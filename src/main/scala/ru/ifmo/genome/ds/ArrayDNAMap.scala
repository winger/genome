package ru.ifmo.genome.ds

import java.nio.ByteBuffer
import ru.ifmo.genome.dna.DNASeq
import collection.mutable.BitSet
import akka.dispatch.{ Promise, Future}
import akka.event.Logging
import akka.actor.{Actor, ActorSystem}
import akka.pattern._
import ru.ifmo.genome.scripts.ActorsHome
import akka.event.slf4j.SLF4JLogging


/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object Messages {
  sealed trait DNAMapMessages

  case class size() extends DNAMapMessages
  case class apply(key: DNASeq) extends DNAMapMessages
  case class getAll(key: DNASeq) extends DNAMapMessages
  case class update[T](key: DNASeq, v: T) extends DNAMapMessages
  case class update1[T](key: DNASeq, v0: T, f: T => T) extends DNAMapMessages
  case class putNew[T](key: DNASeq,  v: T) extends DNAMapMessages
  case class deleteAll[T](p: (DNASeq, T) => Boolean) extends DNAMapMessages
  case class contains[T](key: DNASeq) extends DNAMapMessages
  case class mapReduce[T, T1, T2](map: ((DNASeq, T)) => Option[T1], reduce: Seq[T1] => T2) extends DNAMapMessages
}

trait DNAMapActor[T] extends Actor with SLF4JLogging {
  self: DNAMap[T] =>
  def receive = {
    case Messages.size() => size pipeTo sender
    case Messages.apply(key) => apply(key) pipeTo sender
    case Messages.getAll(key) => getAll(key) pipeTo sender
    case Messages.update(key, v: T) => update(key, v)
    case Messages.update1(key, v: T, f: (T => T)) => update(key, v, f)
    case Messages.putNew(key, v: T) => putNew(key, v)
    case Messages.deleteAll(p: ((DNASeq, T) => Boolean)) => deleteAll(p) pipeTo sender
    case Messages.contains(key) => contains(key) pipeTo sender
    case Messages.mapReduce(map: (((DNASeq, T)) => Option[_]), reduce: Function1[Seq[_], _]) =>
      mapReduce(map, reduce) pipeTo sender
    case x => log.error("Unknown message: " + x.toString)
  }
}

trait DNAMap[T] {
  def size: Future[Int]
  def apply(key: DNASeq): Future[Option[T]]
  def getAll(key: DNASeq): Future[Iterable[T]]
  def update(key: DNASeq, v: T)
  def update(key: DNASeq, v0: T, f: T => T)
  def putNew(key: DNASeq,  v: T)
  def deleteAll(p: (DNASeq, T) => Boolean): Future[Unit]
  def contains(key: DNASeq): Future[Boolean]
  def mapReduce[T1, T2](map: ((DNASeq, T)) => Option[T1], reduce: Seq[T1] => T2) : Future[T2]
  def foreach(f: ((DNASeq, T)) => Unit): Future[Unit] = mapReduce(p => {f(p); None}, (_:Seq[Unit]) => ())
}

class ArrayDNAMap[T](k: Byte)(implicit mf: ClassManifest[T], as: ActorSystem) extends DNAMap[T] {
  import ArrayDNAMap._

  val logger = Logging(as, classOf[ArrayDNAMap[T]])
  
  //TODO fix resize policy

  private def bins = container.bins
  private val sizeOfK = sizeOf(k)
  def size = Promise successful container.size
  private var container = new Container(16)
  
  private class Container(val bins: Int) {
    var size = 0
    val mask = bins - 1
    val keys: ByteBuffer = ByteBuffer.allocateDirect(sizeOfK * bins)
    val ar = if (mf.erasure == classOf[Unit]) null else new Array[T](bins)
    val set = new BitSet(bins)
    val del = new BitSet(bins)

    private def getAr(i: Int): T = {
      if (ar == null) Unit.asInstanceOf[T] else ar(i)
    }

    private def setAr(i: Int, v: T) {
      if (ar != null) ar(i) = v
    }
    
    def apply(key: DNASeq) = {
      var i = improve(key.hashCode) & mask
      var ans: Option[T] = None
      while (ans.isEmpty && set(i)) {
        if (!del(i) && DNASeq.read(keys, i * sizeOfK, k) == key) {
          ans = Some(getAr(i))
        } else {
          i = (i + 1) & mask
        }
      }
      ans
    }
    
    def getAll(key: DNASeq) = {
      var ans: List[T] = Nil
      var i = improve(key.hashCode) & mask
      while (set(i)) {
        if (!del(i) && DNASeq.read(keys, i * sizeOfK, k) == key) {
          ans ::= getAr(i)
        }
        i = (i + 1) & mask
      }
      ans
    }
    
    def update(key: DNASeq, v: T) {
      var i = improve(key.hashCode) & mask
      while (!del(i) && set(i) && DNASeq.read(keys, i * sizeOfK, k) != key) {
        i = (i + 1) & mask
      }
      if (del(i) || !set(i)) {
        set += i
        del -= i
        key.write(keys, i * sizeOfK)
        size += 1
      }
      setAr(i, v)
    }
    
    def update(key: DNASeq, v0: T, update: T => T) {
      var i = improve(key.hashCode) & mask
      var firstPos = -1
      while (set(i) && (del(i) || DNASeq.read(keys, i * sizeOfK, k) != key)) {
        if (del(i)) {
          firstPos = i
        }
        i = (i + 1) & mask
      }
      if (!set(i)) {
        if (firstPos != -1) {
          i = firstPos
          del -= i
        }
        set += i
        key.write(keys, i * sizeOfK)
        size += 1
        setAr(i, v0)
      } else {
        setAr(i, update(ar(i)))
      }
    }

    def putNew(key: DNASeq, v: T) {
      var i = improve(key.hashCode) & mask
      while (!del(i) && set(i)) {
        i = (i + 1) & mask
      }
      set += i
      del -= i
      size += 1
      key.write(keys, i * sizeOfK)
      setAr(i, v)
    }
    
    def deleteAll(p: (DNASeq, T) => Boolean) {
      var i = 0
      while (i < bins) {
        if (set(i) && !del(i) && p(DNASeq.read(keys, i * sizeOfK, k), getAr(i))) {
          del += i
          size -= 1
        }
        i += 1
      }
    }
    
    def iterator: Iterator[(DNASeq, T)] = {
      for (i <- (0 until bins).iterator if set(i) && !del(i))
        yield (DNASeq.read(keys, i * sizeOfK, k), getAr(i))
    }
  }

  def apply(key: DNASeq): Future[Option[T]] = {
    assert(key.length == k)
    Promise successful container(key)
  }

  def getAll(key: DNASeq): Future[Iterable[T]] = {
    assert(key.length == k)
    Promise successful container.getAll(key)
  }
  
  def update(key: DNASeq, v: T) {
    assert(key.length == k)
    container.update(key, v)
    rescale()
    Promise successful ()
  }

  def update(key: DNASeq, v0: T, update: T => T) {
    assert(key.length == k)
    container.update(key, v0, update)
    rescale()
    Promise successful ()
  }

  def putNew(key: DNASeq, v: T) {
    assert(key.length == k)
    container.putNew(key, v)
    rescale()
    Promise successful ()
  }

  def deleteAll(p: (DNASeq, T) => Boolean) = Future {
    container.deleteAll(p)
    rescale()
  }
  
  def rescale() {
    if (bins > 16 && container.size < bins * minLoadFactor || bins * maxLoadFactor < container.size) {
      var newBins = 16
      while (newBins * maxLoadFactor < container.size) {
        newBins *= 2
      }
      logger.info("rescaling to " + newBins)
      val newContainer = new Container(newBins)
      for ((key, v) <- container.iterator) {
        newContainer.putNew(key, v)
      }
      container = newContainer
    }
  }
  
  def contains(key: DNASeq) = apply(key).map(_.isDefined)
  
  def mapReduce[T1, T2](f: ((DNASeq, T)) => Option[T1], reduce: Seq[T1] => T2): Future[T2] = Future {
    //reduce(container.iterator.flatMap(f(_).toIterator).toList)
    val list = {
      for (chunk <- container.iterator.grouped(ActorsHome.chunkSize))
        yield chunk.par.map(f(_)).flatten.seq
    }.flatten
    reduce(list.toSeq)
  }

}

object ArrayDNAMap {
  val minLoadFactor = 0.3
  val maxLoadFactor = 0.7
  
  def sizeOf(k: Byte) = {
    if (k <= 32) {
      8
    } else if (k <= 64) {
      16
    } else {
      (k + 3) / 4
    }
  }
  
  def minPow2(x: Double) = {
    var n = 1
    while (n < x) {
      n *= 2
    }
    n
  }

  def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }
}

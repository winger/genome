package ru.ifmo.genome.ds

import java.nio.ByteBuffer
import ru.ifmo.genome.dna.DNASeq
import collection.mutable.BitSet


/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

class DNAMap[T](k: Byte)(implicit mf: ClassManifest[T]) {
  import DNAMap._
  
  //TODO fix resize policy

  def bins = container.bins
  val sizeOfK = sizeOf(k)
  def size = container.size
  var container = new Container(16)
  
  class Container(val bins: Int) {
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
        }
        i += 1
      }
    }
    
    def iterator: Iterator[(DNASeq, T)] = {
      for (i <- (0 until bins).iterator if set(i) && !del(i))
        yield (DNASeq.read(keys, i * sizeOfK, k), getAr(i))
    }
  }

  def apply(key: DNASeq): Option[T] = {
    assert(key.length == k)
    container(key)
  }

  def getAll(key: DNASeq): Iterable[T] = {
    assert(key.length == k)
    container.getAll(key)
  }
  
  def update(key: DNASeq, v: T) {
    assert(key.length == k)
    container.update(key, v)
    rescale()
  }

  def update(key: DNASeq, v0: T, update: T => T) {
    assert(key.length == k)
    container.update(key, v0, update)
    rescale()
  }

  def putNew(key: DNASeq, v: T) {
    assert(key.length == k)
    container.putNew(key, v)
    rescale()
  }

  def deleteAll(p: (DNASeq, T) => Boolean) {
    container.deleteAll(p)
    rescale()
  }
  
  def rescale() {
    if (bins > 16 && size < bins * minLoadFactor || bins * maxLoadFactor < size) {
      var newBins = 16
      while (newBins * maxLoadFactor < size) {
        newBins *= 2
      }
      val newContainer = new Container(newBins)
      for ((key, v) <- container.iterator) {
        newContainer.putNew(key, v)
      }
      container = newContainer
    }
  }
  
  def contains(key: DNASeq) = apply(key).isDefined

}

object DNAMap {
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

package ru.ifmo.genome.dna

import collection.mutable.Builder
import java.io.{OutputStream, ByteArrayOutputStream}
import akka.routing.MurmurHash
import ru.ifmo.genome.ds.MultiHash
import java.util.Arrays
import collection.{GenSeqLike, IndexedSeqLike}
import collection.generic.{SeqFactory, CanBuildFrom}
import ru.ifmo.genome.dna.DNASeq.GenCanBuildFrom
import java.nio.ByteBuffer
import com.esotericsoftware.kryo.DefaultSerializer
import com.esotericsoftware.kryo.serializers.FieldSerializer

/**
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

//TODO: add native serializators
//TODO: optimize foreach/iterator/etc.
abstract class DNASeq extends IndexedSeq[Base] with IndexedSeqLike[Base, DNASeq] with Serializable with MultiHash {
  override protected[this] def newBuilder = DNASeq.newBuilder
  override def toString = map(_.toString).mkString

  def toByteArray: Array[Byte]

  def complement: DNASeq = map(_.complement)
  def revComplement: DNASeq = complement.reverse
  
  def write(buf: ByteBuffer, pos: Int)

  override def multiHashCode(seed: Int) = {
    val murmur = new MurmurHash[Base](seed)
    foreach(murmur)
    murmur.hash
  }
}

@DefaultSerializer(classOf[FieldSerializer[_]])
class ArrayDNASeq(val data: Array[Byte], val length: Int) extends DNASeq {
  import DNASeq._

//  assert(data.length > 16 && data.length == (length + n1 - 1) / n1)
  private def this() = this(null, 0) // only for serialization

  def apply(i: Int) = {
    if (i < 0 || length <= i) {
      throw new IndexOutOfBoundsException
    }
    Base.fromInt((data(i / n1) >> (i % n1 * bits) & mask).toInt)
  }

  def toByteArray = data

  override def multiHashCode(seed: Int) = {
    val murmur = new MurmurHash[Byte](seed)
    data.foreach(murmur)
    murmur.hash
  }
  
  def write(buf: ByteBuffer, pos: Int) {
    for(i <- 0 until data.length) buf.put(pos + i, data(i))
  }

  //optimizations

  override def equals(o: Any) = o match {
    case that: ArrayDNASeq => length == that.length && Arrays.equals(data, that.data)
    case _ => super.equals(o)
  }
}

@DefaultSerializer(classOf[FieldSerializer[_]])
class Long1DNASeq(val long: Long, val len: Byte) extends DNASeq {
  import DNASeq._

//  assert(len <= n && (len == n || (long & ~((1L << (bits * len)) - 1)) == 0))
  private def this() = this(0L, 0) // only for serialization

  def apply(i: Int) = {
    if (i < 0 || len <= i) {
      throw new IndexOutOfBoundsException
    }
    Base.fromInt((long >> (i * bits) & mask).toInt)
  }

  override def length = len

  def toByteArray = {
    val ar = new Array[Byte]((bits * length + 7) / 8)
    for (i <- 0 until ar.length) {
      ar(i) = (long >> (8 * i)).toByte
    }
    ar
  }

  def write(buf: ByteBuffer, pos: Int) {
    buf.putLong(pos, long)
  }

  //optimizations

  override def hashCode = long.##

  override def multiHashCode(seed: Int) = {
    val t = (long ^ (long >> 32)) * seed
    (t ^ (t >> 32)).toInt
  }

  override def equals(o: Any) = o match {
    case that: Long1DNASeq => length == that.length && long == that.long
    case _ => super.equals(o)
  }

  //TODO: add those to Long2DNASeq
  
  def subseq(l: Int, r: Int): Long1DNASeq = {
    assert(0 <= l && l <= r && r <= length)
    val mask = if (r - l == n) -1L else (1L << (bits * (r - l))) - 1
    new Long1DNASeq((long >>> (bits * l)) & mask, (r - l).toByte)
  }

  override def take(n: Int) = subseq(0, (n max 0) min length)

  override def drop(n: Int) = subseq((n max 0) min length, length)

  override def sliding[B](size: Int, step: Int): Iterator[DNASeq] = {
    if (length < size) {
      Iterator.single(this)
    } else {
      for (i <- Iterator.range(0, length - size + 1, step)) yield subseq(i, i + size)
    }
  }

  override def +:[B >: Base, That](elem: B)(implicit bf: CanBuildFrom[DNASeq, B, That]): That = {
    if (bf.isInstanceOf[DNASeq.GenCanBuildFrom] && elem.isInstanceOf[Base] && length < n) {
      val newLong = (long << bits) | elem.asInstanceOf[Base].toInt
      val newLength = (length + 1).toByte
      new Long1DNASeq(newLong, newLength).asInstanceOf[That]
    } else {
      super.+:(elem)(bf)
    }
  }

  override def :+[B >: Base, That](elem: B)(implicit bf: CanBuildFrom[DNASeq, B, That]): That = {
    if (bf.isInstanceOf[DNASeq.GenCanBuildFrom] && elem.isInstanceOf[Base] && length < n) {
      val newLong = long | (elem.asInstanceOf[Base].toInt.toLong << (length * bits))
      val newLength = (length + 1).toByte
      new Long1DNASeq(newLong, newLength).asInstanceOf[That]
    } else {
      super.+:(elem)(bf)
    }
  }

  override def reverse = {
    var i = long
    i = (i & 0x3333333333333333L) << 2 | (i >>> 2) & 0x3333333333333333L;
    i = (i & 0x0f0f0f0f0f0f0f0fL) << 4 | (i >>> 4) & 0x0f0f0f0f0f0f0f0fL;
    i = (i & 0x00ff00ff00ff00ffL) << 8 | (i >>> 8) & 0x00ff00ff00ff00ffL;
    i = (i << 48) | ((i & 0xffff0000L) << 16) |
      ((i >>> 16) & 0xffff0000L) | (i >>> 48);
    new Long1DNASeq(i >>> (bits * (n - length)), length.toByte)
  }

  override def complement = {
    val mask = if (length == 64) -1L else (1L << (bits * length)) - 1
    new Long1DNASeq(long ^ mask, length.toByte)
  }
}

@DefaultSerializer(classOf[FieldSerializer[_]])
class Long2DNASeq(val long1: Long, val long2: Long, val len: Byte) extends DNASeq {
  import DNASeq._

//  assert(n < len && len <= 2 * n && (len == 2 * n || (long2 & ~((1L << (bits * (len - n))) - 1)) == 0))
  private def this() = this(0L, 0L, 0) // only for serialization

  def apply(i: Int) = {
    if (i < 0 || len <= i) {
      throw new IndexOutOfBoundsException
    }
    val data = if (i < n) long1 else long2
    Base.fromInt((data >> (i % n * bits) & mask).toInt)
  }
  
  override def length = len

  def toByteArray = {
    val ar = new Array[Byte]((bits * length + 7) / 8)
    for (i <- 0 until ar.length) {
      val long = if (i < 8) long1 else long2
      ar(i) = (long >> (8 * (i % 8))).toByte
    }
    ar
  }

  def write(buf: ByteBuffer, pos: Int) {
    buf.putLong(pos, long1)
    buf.putLong(pos + 8, long2)
  }

  //optimizations

  override def multiHashCode(seed: Int) = {
    val t = (long1 ^ (long1 >> 32)) * seed
    val t1 = (long2 ^ (long2 >> 32) ^ t) * seed
    (t1 ^ (t1 >> 32)).toInt
  }

  override def equals(o: Any) = o match {
    case that: Long2DNASeq => length == that.length && long1 == that.long1 && long2 == that.long2
    case _ => super.equals(o)
  }

}

object DNASeq {
  private[dna] val bits = 2
  private[dna] val n = 64 / bits
  private[dna] val n1 = 8 / bits
  private[dna] val mask = (1L << bits) - 1

  val writeBuffer = new Array[Byte](8)

  def writeLong(v: Long, out: OutputStream) {
    writeBuffer(0) = (v >>> 0).asInstanceOf[Byte]
    writeBuffer(1) = (v >>> 8).asInstanceOf[Byte]
    writeBuffer(2) = (v >>> 16).asInstanceOf[Byte]
    writeBuffer(3) = (v >>> 24).asInstanceOf[Byte]
    writeBuffer(4) = (v >>> 32).asInstanceOf[Byte]
    writeBuffer(5) = (v >>> 40).asInstanceOf[Byte]
    writeBuffer(6) = (v >>> 48).asInstanceOf[Byte]
    writeBuffer(7) = (v >>> 56).asInstanceOf[Byte]
    out.write(writeBuffer, 0, 8)
  }

  def newBuilder = new Builder[Base, DNASeq] {
    var l1, l2 = 0L
    var b = 0
    var count = 0
    var byteStream: ByteArrayOutputStream = null

    def +=(elem: Base) = {
      if (count == 2 * n) {
        if (byteStream == null) {
          byteStream = new ByteArrayOutputStream()
        }
        writeLong(l1, byteStream)
        writeLong(l2, byteStream)
      }
      if (count > 2 * n && count % n1 == 0) {
        byteStream.write(b)
        b = 0
      }
      if (count < n) {
        l1 |= elem.toInt.toLong << (count * bits)
      } else if (count < 2 * n) {
        l2 |= elem.toInt.toLong << (count % n * bits)
      } else {
        b |= elem.toInt << (count % n1 * bits)
      }
      count += 1
      this
    }

    def clear() {
      l1 = 0
      l2 = 0
      count = 0
      byteStream = null
    }

    def result() = {
      if (count <= n) new Long1DNASeq(l1, count.toByte)
      else if (count <= 2 * n) new Long2DNASeq(l1, l2, count.toByte)
      else {
        byteStream.write(b)
        new ArrayDNASeq(byteStream.toByteArray, count)
      }
    }
  }

  def apply(xs: Base*) = (newBuilder ++= xs).result()

  def apply(ar: Array[Byte], length: Int): DNASeq = {
    if (ar.length <= 16) {
      var l1 = 0L
      for (i <- 0 until (8 min ar.length)) {
        l1 |= (ar(i).toLong & 0xff) << (8 * i).toLong
      }
      if (ar.length <= 8) {
        new Long1DNASeq(l1, length.toByte)
      } else {
        var l2 = 0L
        for (i <- 8 until ar.length) {
          l2 |= (ar(i).toLong & 0xff) << (8 * (i - 8)).toLong
        }
        new Long2DNASeq(l1, l2, length.toByte)
      }
    } else {
      new ArrayDNASeq(ar, length)
    }
  }
  
  def read(buf: ByteBuffer, pos: Int, k: Byte): DNASeq = {
    if (k <= 32) {
      new Long1DNASeq(buf.getLong(pos), k)
    } else if (k <= 64) {
      new Long2DNASeq(buf.getLong(pos), buf.getLong(pos + 8), k)
    } else {
      val ar = new Array[Byte]((k + 3) / 4)
      for (i <- 0 until ar.length) {
        ar(i) = buf.get(pos + i)
      }
      new ArrayDNASeq(ar, k)
    }
  }
  
  class GenCanBuildFrom extends CanBuildFrom[DNASeq, Base, DNASeq] {
    def apply() = newBuilder
    def apply(from: DNASeq) = newBuilder
  }

  implicit def canBuildFrom: CanBuildFrom[DNASeq, Base, DNASeq] = new GenCanBuildFrom
}
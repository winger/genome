package ru.ifmo.genome.dna

import collection.IndexedSeqLike
import collection.mutable.{ArrayBuilder, Builder}
import collection.generic.CanBuildFrom
import com.sun.xml.internal.messaging.saaj.util.ByteOutputStream
import java.nio.ByteBuffer
import java.io.{OutputStream, DataOutputStream, ByteArrayOutputStream}

/**
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

//TODO: optimize foreach/iterator/etc.
abstract class SmallDNASeq extends IndexedSeq[Base] with IndexedSeqLike[Base, SmallDNASeq] with Serializable {
  override protected[this] def newBuilder = SmallDNASeq.newBuilder
  override def toString = map(_.toString).mkString

  def toByteArray: Array[Byte]
}

private class ArrayDNASeq(val data: Array[Byte], val length: Int) extends SmallDNASeq {
  import SmallDNASeq._

  assert(data.length > 16 && data.length == (length + n1 - 1) / n1)

  def apply(i: Int) = {
    if (i < 0 || length <= i) {
      throw new IndexOutOfBoundsException
    }
    Base.fromInt((data(i / n1) >> (i % n1 * bits) & mask).toInt)
  }

  def toByteArray = data
}

private class Long1DNASeq(val long: Long, val len: Byte) extends SmallDNASeq {
  import SmallDNASeq._

  assert(len <= n)

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
}

private class Long2DNASeq(val long1: Long, val long2: Long, val len: Byte) extends SmallDNASeq {
  import SmallDNASeq._

  assert(n < len && len <= 2 * n)

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
}

object SmallDNASeq {

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

  private[dna] def newBuilder = new Builder[Base, SmallDNASeq] {
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

  def apply(ar: Array[Byte], length: Int): SmallDNASeq = {
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

  implicit def canBuildFrom = new CanBuildFrom[SmallDNASeq, Base, SmallDNASeq] {
    def apply() = newBuilder
    def apply(from: SmallDNASeq) = newBuilder
  }
}
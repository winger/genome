package ru.ifmo.genome.dna

import collection.IndexedSeqLike
import collection.mutable.{ArrayBuilder, Builder}
import collection.generic.CanBuildFrom

/**
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

//TODO: optimize foreach/iterator/etc.
abstract class SmallDNASeq extends IndexedSeq[Base] with IndexedSeqLike[Base, SmallDNASeq] {
  override protected[this] def newBuilder = SmallDNASeq.newBuilder
  override def toString = map(_.toString).mkString
}

private class ArrayDNASeq(val data: Array[Long], val length: Int) extends SmallDNASeq {
  import SmallDNASeq._

  assert(data.length > 2 && data.length == (length + n - 1) / n)

  def apply(i: Int) = {
    if (i < 0 || length <= i) {
      throw new IndexOutOfBoundsException
    }
    Base.fromInt((data(i / n) >> (i % n * bits) & mask).toInt)
  }
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
}

object SmallDNASeq {

  private[dna] val bits = 2
  private[dna] val n = 64 / bits
  private[dna] val mask = (1L << bits) - 1

  def apply(xs: Base*) = (newBuilder ++= xs).result()

  def newBuilder = new Builder[Base, SmallDNASeq] {
    var l1, l2 = 0L
    var count = 0
    var longBuilder: ArrayBuilder[Long] = null

    def +=(elem: Base) = {
      if (count == 2 * n) {
        if (longBuilder == null) longBuilder = ArrayBuilder.make()
        longBuilder += l1
      }
      if (count >= 2 * n && count % n == 0) {
        longBuilder += l2
        l2 = 0
      }
      if (count < n) {
        l1 |= elem.toInt.toLong << (count * bits)
      } else {
        l2 |= elem.toInt.toLong << (count % n * bits)
      }
      count += 1
      this
    }

    def clear() {
      l1 = 0
      l2 = 0
      count = 0
      if (longBuilder != null) {
        longBuilder.clear()
      }
    }

    def result() = {
      if (count <= n) new Long1DNASeq(l1, count.toByte)
      else if (count <= 2 * n) new Long2DNASeq(l1, l2, count.toByte)
      else {
        longBuilder += l2
        new ArrayDNASeq(longBuilder.result(), count)
      }
    }
  }

  implicit def canBuildFrom = new CanBuildFrom[SmallDNASeq, Base, SmallDNASeq] {
    def apply() = newBuilder
    def apply(from: SmallDNASeq) = newBuilder
  }
}
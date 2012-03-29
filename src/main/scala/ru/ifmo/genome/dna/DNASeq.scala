package ru.ifmo.genome.dna

import collection.mutable.WrappedArray

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

trait DNASeq extends Seq[Nucleotide] {
  def length: Int
  def apply(i: Int): Nucleotide
  def update(i: Int, v: Nucleotide)
  override def reverse: DNASeq = new DNASeqWrapper(this, true, false)
  def complement: DNASeq = new DNASeqWrapper(this, false, true)
  def revComplement: DNASeq = reverse.complement
}

private class DNASeqWrapper[T <: DNASeq](backedSeq: T, isReversed: Boolean, isComplement: Boolean) extends DNASeq {
  override def apply(i: Int) = complementIfNeeded(backedSeq.apply(index(i)))

  override def update(i: Int, v: Nucleotide) { backedSeq.update(index(i), complementIfNeeded(v)) }

  private def index(i: Int) = if (isReversed) length - i - 1 else i

  private def complementIfNeeded(v: Nucleotide) = if (isComplement) v.complement else v

  def length = backedSeq.length
  
  override def reverse = new DNASeqWrapper[T](backedSeq, !isReversed, isComplement)
  override def complement = new DNASeqWrapper[T](backedSeq, isReversed, !isComplement)

  def iterator = {
    val it = if (isReversed) backedSeq.reverseIterator else backedSeq.iterator
    new Iterator[Nucleotide] {
      def next() = complementIfNeeded(it.next())
      def hasNext = it.hasNext
    }
  }
}

class ArrayDNASeq(len: Int) extends DNASeq {
  val ar = new Array[Byte]((len + 3) / 4)

  def length = len

  def apply(i: Int) = Nucleotide(BitsHelper.bitsAt(ar, i))

  def update(i: Int, v: Nucleotide) { BitsHelper.updateBitsAt[Int](ar, ar(_) = _, i, v.byte) }

  def iterator = new Iterator[Nucleotide] {
    var i: Int = 0

    def next() = {
      assert(hasNext)
      val next = apply(i)
      i += 1
      next
    }

    def hasNext = i < len
  }
}

object BitsHelper {

  private def byteIndex[A](i: A)(implicit int: Integral[A]) = int.quot(i, int.fromInt(8))

  private def bitIndex[A](i: A)(implicit int: Integral[A]) = int.toInt(int.quot(i, int.fromInt(8)))

  def bitsAt[A](byteSource: A => Byte, i: A, count: Int = 2)(implicit int: Integral[A]): Byte = {
    val byte = byteIndex(i)
    val bit = bitIndex(i)
    assert(bit + count <= 8)
    ((byteSource(byte) >> bit) & ((1 << count) - 1)).toByte
  }

  def updateBitsAt[A](byteSource: A => Byte, updateSource: (A, Byte) => Unit, i: A, v: Byte, count: Int = 2)(implicit int: Integral[A]) {
    val byte = byteIndex(i)
    val bit = bitIndex(i)
    assert(bit + count <= 8)
    assert((v & ((1 << count) - 1)) == v)
    val oldValue = byteSource(byte)
    updateSource(byte, (oldValue & (((1 << count) - 1) << bit) | v << bit).toByte)
  }

}
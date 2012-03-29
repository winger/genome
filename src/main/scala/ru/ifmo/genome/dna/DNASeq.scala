package ru.ifmo.genome.dna

import collection.mutable.Seq

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

private case class RevCompWrapper[T <: DNASeq](backedSeq: T, isReversed: Boolean, isComplement: Boolean) extends DNASeq {
  override def apply(i: Int) = complement(backedSeq.apply(index(i)))

  override def update(i: Int, v: Nucleotide) = backedSeq.update(index(i), complement(v))

  def index(i: Int) = if (isReversed) length - i - 1 else i

  def complement(v: Nucleotide) = if (isComplement) v.complement else v

  def length = backedSeq.length

  def iterator = buildIterator(if (isReversed) backedSeq.reverseIterator : backedSeq.iterator)

  def buildIterator(it: Iterator[Nucleotide]) = new Iterator[Nucleotide] {
    def next = complement(it.next())
    def hasNext = it.hasNext
  }
}

trait DNASeq extends Seq[Nucleotide] {
  override def reverse = this match {
    case RevCompWrapper(seq, rev, comp) => null
    case _ => null
  }
  def complement: IndexedSeq[Nucleotide]
}

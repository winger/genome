package ru.ifmo.genome.dna

import collection.IndexedSeqLike
import collection.generic.CanBuildFrom


/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

class DNASeq[T <: IndexedSeqLike[Base, T]](seq: T)(implicit bf: CanBuildFrom[T, Base, T]) {
  def complement: T = seq.map(_.complement)
  def revComplement: T = complement.reverse
}

object DNASeq {
  implicit def seq2dna[T <: IndexedSeqLike[Base, T]](seq: T)(implicit bf: CanBuildFrom[T, Base, T]) = new DNASeq[T](seq)
}
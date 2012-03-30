package ru.ifmo.genome.dna

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */
sealed abstract class Base(val toInt: Int) {
  def complement = Base.complement(this)
}

object Base {
  case object A extends Base(0)
  case object T extends Base(1)
  case object G extends Base(2)
  case object C extends Base(3)

  val fromInt = Array[Base](A, T, G, C)
  val complement = Map[Base, Base](A -> T, T -> A, G -> C, C -> G)
  val fromChar = Map[Char, Base](fromInt.map(b => b.toString.apply(0) -> b): _*)
  
  assert(fromInt.forall(b => b == b.complement.complement))
  assert(fromInt.forall(b => b == fromInt(b.toInt)))
}
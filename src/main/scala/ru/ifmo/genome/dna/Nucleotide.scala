package ru.ifmo.genome.dna

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */
sealed abstract class Nucleotide(val complement: Nucleotide, val byte: Byte, val name: String) {
  val char = name(0)

  Nucleotide.nucleotidesByByte += byte -> this
  Nucleotide.nucleotidesByName += name -> this
  Nucleotide.nucleotidesByChar += char -> this
  
  override val toString = name
}

object Nucleotide {
  case object A extends Nucleotide(T, 0, "A")
  case object T extends Nucleotide(A, 1, "T")
  case object G extends Nucleotide(C, 2, "G")
  case object C extends Nucleotide(G, 3, "C")

  var nucleotidesByByte = Map[Byte, Nucleotide]()
  var nucleotidesByChar = Map[Char, Nucleotide]()
  var nucleotidesByName = Map[String, Nucleotide]()

  var values = Array(A, T, G, C)
  
  implicit def apply(byte: Byte) = nucleotidesByByte(byte)
  implicit def apply(name: String) = nucleotidesByName(name)
  implicit def apply(char: Char) = nucleotidesByChar(char)
}
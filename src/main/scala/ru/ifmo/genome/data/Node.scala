package ru.ifmo.genome.data

import ru.ifmo.genome.dna.{Base, DNASeq}


/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

abstract class Node(val seq: DNASeq)

case class TerminalNode(override val seq: DNASeq) extends Node(seq) {
  var inEdges = List[Edge]()
  var outEdges = Map[Base, Edge]()
}

case class EdgeNode(override val seq: DNASeq, from: Node, base: Base, dist: Long) extends Node(seq)
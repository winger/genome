package ru.ifmo.genome.data

import ru.ifmo.genome.dna.{Base, SmallDNASeq}


/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

abstract class Node(val seq: SmallDNASeq)

case class TerminalNode(override val seq: SmallDNASeq) extends Node(seq) {
  var inEdges = List[Edge]()
  var outEdges = Map[Base, Edge]()
}

class EdgeNode(seq: SmallDNASeq, val from: Node, val base: Base, val dist: Long) extends Node(seq)
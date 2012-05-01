package ru.ifmo.genome.data

import ru.ifmo.genome.dna.DNASeq

/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

@SerialVersionUID(1L)
case class Edge(startSeq: DNASeq, endSeq: DNASeq, seq: DNASeq, nodeMap: collection.Map[DNASeq, Node]) {
  @transient lazy val start: TerminalNode = nodeMap(startSeq).asInstanceOf[TerminalNode]
  @transient lazy val end: TerminalNode = nodeMap(endSeq).asInstanceOf[TerminalNode]
}

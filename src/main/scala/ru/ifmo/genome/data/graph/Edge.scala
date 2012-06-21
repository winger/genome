package ru.ifmo.genome.data.graph

import ru.ifmo.genome.dna.DNASeq

/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

@SerialVersionUID(1L)
case class Edge(id: Long, startId: Long, endId: Long, seq: DNASeq) {
  private def this() = this(-1, 0, 0, null)

  override def equals(obj: Any) = obj match {
    case Edge(oid, _, _, _) => id == oid
    case _ => false
  }

  override def hashCode() = id.##

  def start(implicit g: Graph) = g.getNode(startId)

  def end(implicit g: Graph) = g.getNode(endId)
}

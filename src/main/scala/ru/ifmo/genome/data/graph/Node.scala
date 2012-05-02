package ru.ifmo.genome.data.graph

import ru.ifmo.genome.dna.{Base, DNASeq}
import collection.immutable
import com.esotericsoftware.kryo.{Kryo, Serializer, DefaultSerializer}
import com.esotericsoftware.kryo.io.{Input, Output}


/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

class NodeSerializer extends Serializer[Node] {
  def write(kryo: Kryo, out: Output, node: Node) {
    out.writeLong(node.id)
    kryo.writeClassAndObject(out, node.seq)
    out.writeInt(node.inEdgeIds.size)
    for (inEdgeId <- node.inEdgeIds) {
      out.writeLong(inEdgeId)
    }
    out.writeInt(node.outEdgeIds.size)
    for ((base, outEdgeId) <- node.outEdgeIds) {
      out.writeByte(base.toInt)
      out.writeLong(outEdgeId)
    }
  }

  override def read(kryo: Kryo, in: Input, node: Node) {
    node.seq = kryo.readClassAndObject(in).asInstanceOf[DNASeq]
    for (it <- 0 until in.readInt()) {
      node.inEdgeIds += in.readLong()
    }
    for (it <- 0 until in.readInt()) {
      node.outEdgeIds += Base.fromInt(in.readByte()) -> in.readLong()
    }
  }

  override def create(kryo: Kryo, in: Input, p3: Class[Node]) = new Node(in.readLong(), null, Set.empty[Long], Map.empty[Base, Long])
}

@DefaultSerializer(classOf[NodeSerializer])
@SerialVersionUID(1L)
case class Node(id: Long, var seq: DNASeq, var inEdgeIds: immutable.Set[Long], var outEdgeIds: immutable.Map[Base, Long]) {
  private def this() = this(-1, null, null, null) // for serialization only

  override def equals(obj: Any) = obj match {
    case Node(oid, _, _, _) => id == oid
    case _ => false
  }

  override def hashCode() = id.##

  def inEdges(implicit g: Graph) = inEdgeIds.view.map(g.getEdge)

  def outEdges(implicit g: Graph) = outEdgeIds.mapValues(g.getEdge)
}
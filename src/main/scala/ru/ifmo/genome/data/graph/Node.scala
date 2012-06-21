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

  def read(kryo: Kryo, in: Input, typ: Class[Node]) = {
    kryo.reference(new Object())
    val id = in.readLong()
    val seq = kryo.readClassAndObject(in).asInstanceOf[DNASeq]
    val inEdgeIds = for (it <- 0 until in.readInt()) yield in.readLong()
    val outEdgeIds = for (it <- 0 until in.readInt()) yield Base.fromInt(in.readByte()) -> in.readLong()
    new Node(id, seq, inEdgeIds.toSet, outEdgeIds.toMap)
  }
}

@DefaultSerializer(classOf[NodeSerializer])
@SerialVersionUID(1L)
case class Node(id: Long, var seq: DNASeq, var inEdgeIds: immutable.Set[Long], var outEdgeIds: immutable.Map[Base, Long]) {

  override def equals(obj: Any) = obj match {
    case Node(oid, _, _, _) => id == oid
    case _ => false
  }

  override def hashCode() = id.##

  def inEdges(implicit g: Graph) = inEdgeIds.view.map(g.getEdge)

  def outEdges(implicit g: Graph) = outEdgeIds.mapValues(g.getEdge)
}
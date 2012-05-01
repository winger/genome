package ru.ifmo.genome.data

import ru.ifmo.genome.dna.DNASeq
import com.esotericsoftware.kryo.io.{Output, Input}
import com.esotericsoftware.kryo.util.ObjectMap
import com.esotericsoftware.kryo.{DefaultSerializer, Kryo, Serializer}
import java.lang.Class

/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

class EdgeSerializer extends Serializer[Edge] {
  {setImmutable(true)}
  
  def write(kryo: Kryo, out: Output, edge: Edge) {
    kryo.writeClassAndObject(out, edge.start.seq)
    kryo.writeClassAndObject(out, edge.end.seq)
    kryo.writeClassAndObject(out, edge.seq.seq)
  }


  override def create(p1: Kryo, p2: Input, p3: Class[Edge]) = new Edge(null, null, null)

  override def read(kryo: Kryo, in: Input, edge: Edge) {
    val graph = kryo.getGraphContext.asInstanceOf[ObjectMap[Class[_], AnyRef]].get(classOf[Graph]).asInstanceOf[Graph]
    edge.start = graph.nodeMap(kryo.readClassAndObject(in).asInstanceOf[DNASeq]).asInstanceOf[TerminalNode]
    edge.end = graph.nodeMap(kryo.readClassAndObject(in).asInstanceOf[DNASeq]).asInstanceOf[TerminalNode]
    edge.seq = kryo.readClassAndObject(in).asInstanceOf[DNASeq]
    edge.start.outEdges += edge.seq(0) -> edge
    edge.end.inEdges ::= edge
  }
}

@DefaultSerializer(classOf[EdgeSerializer])
@SerialVersionUID(1L)
case class Edge(var start: TerminalNode, var end: TerminalNode, var seq: DNASeq)

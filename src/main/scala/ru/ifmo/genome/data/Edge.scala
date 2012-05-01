package ru.ifmo.genome.data

import ru.ifmo.genome.dna.DNASeq
import com.esotericsoftware.kryo.io.{Output, Input}
import com.esotericsoftware.kryo.util.ObjectMap
import com.esotericsoftware.kryo.{DefaultSerializer, Kryo, Serializer}

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
  
  override def create(kryo: Kryo, in: Input, cls: Class[Edge]) = {
    val graph = kryo.getGraphContext.asInstanceOf[ObjectMap[Class[_], AnyRef]].get(classOf[Graph]).asInstanceOf[Graph]
    val start = graph.nodeMap(kryo.readClassAndObject(in).asInstanceOf[DNASeq]).asInstanceOf[TerminalNode]
    val end = graph.nodeMap(kryo.readClassAndObject(in).asInstanceOf[DNASeq]).asInstanceOf[TerminalNode]
    val seq = kryo.readClassAndObject(in).asInstanceOf[DNASeq]
    val edge = new Edge(start, end, seq)
    start.outEdges += seq(0) -> edge
    end.inEdges ::= edge
    edge
  }
}

@DefaultSerializer(classOf[EdgeSerializer])
@SerialVersionUID(1L)
case class Edge(start: TerminalNode, end: TerminalNode, seq: DNASeq)

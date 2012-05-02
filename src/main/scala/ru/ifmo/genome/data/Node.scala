package ru.ifmo.genome.data

import ru.ifmo.genome.dna.{Base, DNASeq}
import com.esotericsoftware.kryo.io.{Output, Input}
import com.esotericsoftware.kryo.{Serializer, DefaultSerializer, Kryo, KryoSerializable}
import java.lang.Class


/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

abstract class Node extends Serializable {
  def seq: DNASeq
}

class TerminalNodeSerializer extends Serializer[TerminalNode] {
  {setAcceptsNull(true)}

  def write(kryo: Kryo, out: Output, node: TerminalNode) {
    kryo.writeClassAndObject(out, node.seq)
  }

  override def create(kryo: Kryo, in: Input, cls: Class[TerminalNode]) =
    new TerminalNode(null)

  override def read(kryo: Kryo, in: Input, node: TerminalNode) {
    node.seq = kryo.readClassAndObject(in).asInstanceOf[DNASeq]
  }
}

@DefaultSerializer(classOf[TerminalNodeSerializer])
@SerialVersionUID(1L)
case class TerminalNode(var seq: DNASeq) extends Node {
  var inEdges = List[Edge]()
  var outEdges = Map[Base, Edge]()
}

@SerialVersionUID(1L)
case class EdgeNode(seq: DNASeq, edge: List[(Edge, Int)]) extends Node
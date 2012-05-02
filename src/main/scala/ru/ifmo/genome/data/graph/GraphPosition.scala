package ru.ifmo.genome.data.graph

import ru.ifmo.genome.dna.{Base, DNASeq}
import com.esotericsoftware.kryo.io.{Output, Input}
import com.esotericsoftware.kryo.{Serializer, DefaultSerializer, Kryo, KryoSerializable}
import java.lang.Class


/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

sealed abstract class GraphPosition

case class NodeGraphPosition(node: Node) extends GraphPosition

case class EdgeGraphPosition(edge: Edge, dist: Int) extends GraphPosition
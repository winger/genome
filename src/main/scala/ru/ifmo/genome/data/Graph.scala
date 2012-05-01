package ru.ifmo.genome.data

import ru.ifmo.genome.util.ConsoleProgress
import ru.ifmo.genome.dna.{Base, DNASeq}
import java.io._
import collection.mutable.{ArrayBuffer, ArrayStack, HashSet}
import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{KryoSerializable, Kryo}
import com.esotericsoftware.kryo.util.ObjectMap

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

@SerialVersionUID(1L)
class Graph(val termKmers: collection.mutable.Set[DNASeq], val nodeMap: collection.mutable.Map[DNASeq, Node]) extends KryoSerializable {
  import Graph._
  import formatter._
  
  //TODO make external serializator
  private def this() = this(collection.mutable.Set[DNASeq](), collection.mutable.Map[DNASeq, Node]()) // for serialization only

  def retain(nodes: Seq[TerminalNode]) {
    val set = nodes.map(_.seq).toSet
    termKmers.retain(set)
    def retain(node: Node): Boolean = node match {
      case TerminalNode(seq) => set.contains(seq)
      case EdgeNode(_, from, _, _) => retain(from)
    }
    nodeMap.retain((_, node) => retain(node))
  }

  def components: Seq[Seq[TerminalNode]] = {
    val col = collection.mutable.HashSet[TerminalNode]()

    def dfs(node: TerminalNode): Seq[TerminalNode] = {
      val stack = ArrayStack[TerminalNode](node)
      val visited = ArrayBuffer[TerminalNode]()
      while (!stack.isEmpty) {
        val node = stack.pop()
        visited += node
        val neighbours = (node.inEdges.map(e => e.start)
          ++ node.outEdges.values.map(e => e.end)).map(_.asInstanceOf[TerminalNode]).filterNot(col)
        col ++= neighbours
        stack ++= neighbours
      }
      visited
    }

    for (read <- termKmers.toSeq; node = nodeMap(read).asInstanceOf[TerminalNode] if !col(node))
      yield dfs(node)
  }

  def writeDot(out: PrintWriter) {
    out.println("digraph G {")
    for (kmer <- termKmers; node = nodeMap(kmer).asInstanceOf[TerminalNode]; edge <- node.outEdges.values) {
      out.println("%s -> %s [label=%d]".format(edge.start.seq, edge.end.seq, edge.seq.length))
    }
    out.println("}")
    out.close()
  }
  
  def getPos(seq: DNASeq): Graph.Pos = getPos(nodeMap(seq))

  def getPos(n: Node): Graph.Pos = n match {
    case n@TerminalNode(_) => Left(n)
    case EdgeNode(_, from, base, dist) => getPos(from) match {
      case Left(tNode) => Right((tNode.outEdges(base), dist))
      case Right((edge, d1)) => Right((edge, d1 + dist))
    }
  }

  def edges = termKmers.flatMap(x => nodeMap(x).asInstanceOf[TerminalNode].outEdges.values).toSeq

  def write(file: File) {
    val kryo = new Kryo()
    val out = new Output(new FileOutputStream(file))
    kryo.writeObject(out, this)
    out.close()
  }

  def write(kryo: Kryo, out: Output) {
    for (node <- nodeMap.values) {
      kryo.writeClassAndObject(out, node)
    }
    kryo.writeClassAndObject(out, null)
    for (seq <- termKmers; node = nodeMap(seq).asInstanceOf[TerminalNode];
         edge <- node.outEdges.values) {
      kryo.writeObjectOrNull(out, edge)
    }
    kryo.writeClassAndObject(out, null)
  }

  def read(kryo: Kryo, in: Input) {
    var count = 0
    for (node <- Iterator.continually(kryo.readClassAndObject(in).asInstanceOf[Node]).takeWhile(_ != null)) {
      count += 1
      nodeMap(node.seq) = node
      if (node.isInstanceOf[TerminalNode]) {
        termKmers += node.seq
      }
    }
    logger.info("Nodes count: " + count + " " + nodeMap.size)
    kryo.getGraphContext.asInstanceOf[ObjectMap[Class[_], AnyRef]].put(classOf[Graph], this)
    Iterator.continually(kryo.readObjectOrNull(in, classOf[Edge])).takeWhile(_ != null).size
  }
}

object Graph {
  val (logger, formatter) = ZeroLoggerFactory.newLogger(Graph)
  import formatter._

  type Pos = Either[TerminalNode, (Edge, Long)]

  val chunkSize = 1024

  def buildGraph(k: Int, kmers: collection.Set[DNASeq]) = {

    def contains(x: DNASeq) = kmers.contains(x) || kmers.contains(x.revComplement)

    def incoming(x: DNASeq) = {
      for (base <- Base.fromInt if contains(base +: x.take(k - 1))) yield {
        base
      }
    }

    def outcoming(x: DNASeq) = {
      for (base <- Base.fromInt if contains(x.drop(1) :+ base)) yield {
        base
      }
    }

    val termKmers = {
      val t = kmers.par.filter {
        read =>
          val in = incoming(read).size
          val out = outcoming(read).size
          (in != 1 || out != 1) && (in != 0 || out != 0)
      }.seq
      t ++ t.map(_.revComplement)
    }

    logger.info("Terminal reads: " + termKmers.size)

    logger.info("Split reads: " + termKmers.count(read => incoming(read).size > 1 || outcoming(read).size > 1))

    val nodeMap = collection.mutable.Map[DNASeq, Node]()

    val progress = new ConsoleProgress("building graph", 80)

    for (read <- termKmers) {
      nodeMap(read) = new TerminalNode(read)
      progress(nodeMap.size.toDouble / kmers.size / 2d)
    }

    def buildEdges(read: DNASeq) {
      val node = nodeMap(read).asInstanceOf[TerminalNode]
      for (base <- outcoming(read)) {
        val builder = DNASeq.newBuilder += base
        var seq = read.drop(1) :+ base
        var length = 1
        while (!nodeMap.contains(seq)) {
          nodeMap(seq) = new EdgeNode(seq, node, base, length)
          val out = outcoming(seq)
          assert(out.size == 1, seq + " " + out.toSeq)
          builder += out(0)
          length += 1
          seq = seq.drop(1) :+ out(0)
        }
        val node1 = nodeMap(seq).asInstanceOf[TerminalNode]
        val edge = new Edge(node, node1, builder.result())
        node1.asInstanceOf[TerminalNode].inEdges ::= edge
        node.outEdges += base -> edge
      }
    }

    for (chunk <- termKmers.iterator.grouped(chunkSize)) {
      for (read <- chunk) {
        buildEdges(read)
      }
      progress(nodeMap.size.toDouble / kmers.size / 2d)
    }

    for (chunk <- kmers.iterator.grouped(chunkSize)) {
      for (read <- chunk ++ chunk.map(_.revComplement)) {
        if (!nodeMap.contains(read)) {
          val in = incoming(read).size
          val out = outcoming(read).size
          if (in == 1 && out == 1) {
            nodeMap(read) = new TerminalNode(read)
            buildEdges(read)
            val node = nodeMap(read).asInstanceOf[TerminalNode]
            logger.info(read + " " + node.inEdges + " " + node.outEdges)
          } else {
            assert(in == 0 && out == 0)
          }
        }
      }
    }
    
    progress.done()

    logger.info("Graph nodes: " + nodeMap.size)

    new Graph(HashSet[DNASeq]() ++ termKmers, nodeMap)
  }
  
  def apply(file: File): Graph = {
    val kryo = new Kryo()
    val in = new Input(new FileInputStream(file))
    val graph = kryo.readObject(in, classOf[Graph])
    in.close()
    graph
  }

}
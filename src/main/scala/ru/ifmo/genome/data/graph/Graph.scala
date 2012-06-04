package ru.ifmo.genome.data.graph

import ru.ifmo.genome.util.ConsoleProgress
import com.esotericsoftware.kryo.io.{Input, Output}
import java.io._
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.ConcurrentHashMap
import scala.collection._
import ru.ifmo.genome.dna.{DNASeq, Base}
import com.esotericsoftware.kryo.{Kryo, KryoSerializable}
import scala.{Long, collection, Int}
import ru.ifmo.genome.ds.DNAMap

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

trait Graph {
  implicit val g: Graph = this

  def retain(retainNodes: collection.Set[Node])
  def getNode(id: Long): Node
  def getEdge(id: Long): Edge
  def getNodes: Traversable[Node]
  def getEdges: Traversable[Edge]
  def addNode(seq: DNASeq): Node
  def addEdge(start: Node, end: Node, seq: DNASeq): Edge
  def removeNode(node: Node)
  def removeEdge(edge: Edge)
  def replaceStart(edgeId: Long, newStart: Node)
  def replaceEnd(edgeId: Long, newEnd: Node)
  def simplifyGraph()

  def neighbours(node: Node, count: Int): Set[Node] = {
    val col = mutable.Set[Node]()

    val queue = mutable.Queue[Node](node)
    val visited = mutable.Set[Node]()
    while (!queue.isEmpty && visited.size < count) {
      val node = queue.dequeue()
      visited += node
      val neighbours = (node.inEdges.map(e => e.start) ++ node.outEdges.values.map(e => e.end)).filterNot(col).toSeq
      col ++= neighbours
      queue ++= neighbours
    }
    visited
  }

  def components: Traversable[Set[Node]] = {
    val col = mutable.Set[Node]()

    def dfs(startNode: Node): Set[Node] = {
      val stack = mutable.ArrayStack[Node](startNode)
      val visited = mutable.Set[Node]()
      while (!stack.isEmpty) {
        val node = stack.pop()
        visited += node
        val neighbours = (node.inEdges.map(e => e.start)
          ++ node.outEdges.values.map(e => e.end)).filterNot(col).toSeq
        col ++= neighbours
        stack ++= neighbours
      }
      visited
    }

    for (node <- getNodes if !col(node)) yield dfs(node)
  }

  def writeDot(out: PrintWriter) {
    out.println("digraph G {")
    for (edge <- getEdges) {
      val label = {
        if (edge.seq.length <= 50) {
          edge.seq.toString
        } else {
          edge.seq.length.toString
        }
      }
      out.println("%s -> %s [label=%s]".format(edge.startId, edge.endId, label))
    }
    out.println("}")
    out.close()
  }

  def getGraphMap: DNAMap[GraphPosition] = {
    val k = getNodes.head.seq.size.toByte
    val nodeMap = new DNAMap[GraphPosition](k)
    def add(seq: DNASeq, pos: GraphPosition) {
      val s0 = nodeMap.size
      nodeMap.putNew(seq, pos)
      assert(nodeMap.size - s0 == 1)
    }
    val total = getEdges.map(_.seq.size).sum + getNodes.size - getEdges.size
    val progress = new ConsoleProgress("Node map", 80)
    for (node <- getNodes) {
      add(node.seq, new NodeGraphPosition(node))
    }
    for (edge <- getEdges) {
      val start = edge.seq.head
      var seq = edge.start.seq.drop(1) :+ start
      var dist = 1
      for (base <- edge.seq.tail) {
        add(seq, new EdgeGraphPosition(edge, dist))
        seq = seq.drop(1) :+ base
        dist += 1
      }
      progress(nodeMap.size.toDouble / total)
    }
    progress.done()
    println(nodeMap.size + " " + total)
    nodeMap
  }

  private def similar(a: DNASeq, b: DNASeq) = {
    math.abs(a.length - b.length) * 5 < (a.length max b.length) //TODO fix this
  }
  
  def removeBubbles() {
    //TODO store bubble info
    val maxerrors = 3
    for (node <- getNodes) {
//      def dfs(e1: Edge, e1dist: Int, e2: Edge, e2dist: Int, errors: Int): Boolean = {
//        if (errors > maxerrors) {
//          false
//        } else {
//          if (e1dist == e1.seq.length && e2dist == e2.seq.length && e1.end == e2.end) {
//            true
//          } else {
//
//          }
//        }
//      }
      val out = node.outEdges.values.toArray
      var toRemove = Set[Edge]()
      for (i <- 0 until out.size if !toRemove(out(i)); j <- i + 1 until out.size) {
        if (out(i).end == out(j).end && similar(out(i).seq, out(j).seq)) {
          toRemove += out(j)
        }
      }
      toRemove.foreach(removeEdge)
    }
  }
}

@SerialVersionUID(1L)
class MapGraph extends Graph with KryoSerializable {
  import JavaConversions._
  private val nodes: mutable.ConcurrentMap[Long, Node] = new ConcurrentHashMap[Long, Node]
  private val edges: mutable.ConcurrentMap[Long, Edge] = new ConcurrentHashMap[Long, Edge]

  val nodeIdGen = new AtomicLong(0L)
  val edgeIdGen = new AtomicLong(0L)

  def retain(retainNodes: collection.Set[Node]) {
    val retainIds = retainNodes.map(_.id)
    nodes.retain{case (id, _) => retainIds(id)}
    edges.retain{case (_, Edge(_, startId, endId, _)) => retainIds(startId) && retainIds(endId)}
  }

  def getNode(id: Long) = nodes(id)
  def getEdge(id: Long) = edges(id)
  def getNodes = nodes.values
  def getEdges = edges.values

  def addNode(seq: DNASeq): Node = {
    val newNode = new Node(nodeIdGen.incrementAndGet(), seq, immutable.Set.empty[Long], immutable.Map.empty[Base, Long])
    nodes(newNode.id) = newNode
    newNode
  }

  def addEdge(start: Node, end: Node, seq: DNASeq): Edge = {
    val newEdge = new Edge(edgeIdGen.incrementAndGet(), start.id, end.id, seq)
    start.synchronized(start.outEdgeIds += seq.head -> newEdge.id)
    end.synchronized(end.inEdgeIds += newEdge.id)
    edges(newEdge.id) = newEdge
    newEdge
  }


  def removeNode(node: Node) {
    nodes -= node.id
  }

  def removeEdge(edge: Edge) {
    nodes.get(edge.startId).foreach(n => n.synchronized(n.outEdgeIds -= edge.seq(0)))
    nodes.get(edge.endId).foreach(n => n.synchronized(n.inEdgeIds -= edge.id))
    edges -= edge.id
  }

  def replaceStart(edgeId: Long, newStart: Node) {
    val edge = getEdge(edgeId)
    edges(edge.id) = new Edge(edge.id, newStart.id, edge.endId, edge.seq)
    edge.start.outEdgeIds -= edge.seq(0)
    newStart.outEdgeIds += edge.seq(0) -> edge.id
  }

  def replaceEnd(edgeId: Long, newEnd: Node) {
    val edge = getEdge(edgeId)
    edges(edge.id) = new Edge(edge.id, edge.startId, newEnd.id, edge.seq)
    edge.end.inEdgeIds -= edge.id
    newEnd.inEdgeIds += edge.id
  }

  def simplifyGraph() {
    for (node <- getNodes) {
      val in = node.inEdges.toArray
      val out = node.outEdges.values.toArray
      if (in.size == 0 && out.size == 0) {
        removeNode(node)
      } else if (in.size == 1 && out.size == 1) {
        val e1 = in(0)
        val e2 = out(0)
        if (e1 == e2) {
          removeEdge(e1)
        } else {
          removeEdge(e1)
          removeEdge(e2)
          addEdge(e1.start, e2.end, e1.seq ++ e2.seq)
        }
        removeNode(node)
      }
    }
  }

  def write(file: File) {
    val kryo = new Kryo()
    val out = new Output(new FileOutputStream(file))
    kryo.writeObject(out, this)
    out.close()
  }

  def write(kryo: Kryo, out: Output) {
    out.writeInt(nodes.size)
    for (node <- getNodes) {
      kryo.writeObject(out, node)
    }
    out.writeInt(edges.size)
    for (edge <- getEdges) {
      kryo.writeObject(out, edge)
    }
  }

  def read(kryo: Kryo, in: Input) {
    for (it <- 0 until in.readInt()) {
      val node = kryo.readObject(in, classOf[Node])
      nodes(node.id) = node
    }
    for (it <- 0 until in.readInt()) {
      val edge = kryo.readObject(in, classOf[Edge])
      edges(edge.id) = edge
    }
    nodeIdGen.set(nodes.keys.max)
    edgeIdGen.set(edges.keys.max)
  }
}

object Graph {
  val (logger, formatter) = ZeroLoggerFactory.newLogger(Graph)

  import formatter._

  val chunkSize = 1024

  def buildGraph(k: Int, kmersFreq : collection.Map[DNASeq, Int]) = {
    var kmers = kmersFreq.keySet.toSet

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

//    val restoreDepth = 2
//    def restore(kmer: DNASeq, depth: Int = 0): (Boolean, Set[DNASeq]) = {
//      if (depth > 0 && (kmers(kmer) || kmers(kmer.revComplement))) {
//        (true, Set.empty[DNASeq])
//      } else {
//        var found = false
//        var set = Set.empty[DNASeq]
//        if (depth < restoreDepth && (kmersFreq.contains(kmer) || kmersFreq.contains(kmer.revComplement))) {
//          for (base <- Base.fromInt) {
//            val kmer1 = kmer.drop(1) :+ base
//            val (found1, set1) = restore(kmer1, depth + 1)
//            found |= found1
//            set ++= set1
//          }
//        }
//        if (found) set += kmer
//        (found, set)
//      }
//    }
//    val restoredKmers = {
//      val progress = new ConsoleProgress("restoring kmers", 80)
//      var count = 0d
//      for (chunk <- kmers.grouped(1024)) yield {
//        val set = for (kmer <- chunk.par) yield {
//          val (_, set) = restore(kmer, 0)
//          set
//        }
//        count += chunk.size
//        progress(count / kmers.size)
//        set.seq.flatten
//      }
//    }.flatten.toSet
//    logger.info("Restored kmers: " + restoredKmers.size)
//
//    kmers ++= restoredKmers

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

    val graph: Graph = new MapGraph

    val progress = new ConsoleProgress("building graph", 80)

    val nodeMap = {
      for (read <- termKmers) yield {
        read -> graph.addNode(read)
      }
    }.toMap

    def buildEdges(read: DNASeq) {
      val node = nodeMap(read)
      for (base <- outcoming(read)) {
        val builder = DNASeq.newBuilder += base
        var seq = read.drop(1) :+ base
        var length = 1
        while (!nodeMap.contains(seq)) {
          val out = outcoming(seq)
          assert(out.size == 1, seq + " " + out.toSeq)
          builder += out(0)
          length += 1
          seq = seq.drop(1) :+ out(0)
        }
        val node1 = nodeMap(seq)
        graph.addEdge(node, node1, builder.result())
      }
    }

    var count = 0d
    for (chunk <- termKmers.iterator.grouped(chunkSize)) {
      for (read <- chunk) {
        buildEdges(read)
      }
      count += chunk.size
      progress(count / termKmers.size)
    }
    //TODO: perfect cycles are ignored

    progress.done()

    logger.info("Graph nodes: " + nodeMap.size)

    graph
  }

  def apply(file: File): Graph = {
    val kryo = new Kryo()
    val in = new Input(new FileInputStream(file))
    val graph = kryo.readObject(in, classOf[MapGraph])
    in.close()
    graph
  }

}

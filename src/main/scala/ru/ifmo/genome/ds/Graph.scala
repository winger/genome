package ru.ifmo.genome.ds

import ru.ifmo.genome.util.ConsoleProgress
import ru.ifmo.genome.dna.{Base, DNASeq}
import ru.ifmo.genome.data.{Edge, EdgeNode, TerminalNode, Node}
import collection.mutable.HashSet

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

class Graph(val termKmers: collection.mutable.Set[DNASeq], val nodeMap: collection.mutable.Map[DNASeq, Node]) {
  
  def retain(seq: Seq[TerminalNode]) {
    val set = seq.map(_.seq).toSet
    nodeMap.retain((seq, _) => set(seq))
    termKmers.retain(set)
  }

  def components: Seq[Seq[TerminalNode]] = {
    val col = collection.mutable.HashSet[TerminalNode]()

    def dfs(node: TerminalNode): Iterator[TerminalNode] = {
      if (!col(node)) {
        col += node
        Iterator.single(node) ++
          node.outEdges.values.flatMap(e => dfs(e.end)) ++
          node.inEdges.flatMap(e => dfs(e.start))
      } else {
        Iterator.empty
      }
    }

    for (read <- termKmers.toSeq; node = nodeMap(read).asInstanceOf[TerminalNode] if !col(node))
      yield dfs(node).toList
  }
}

object Graph {
  val (logger, formatter) = ZeroLoggerFactory.newLogger(Graph)
  import formatter._

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

    val termKmers = kmers.par.filter {
      read =>
        val in = incoming(read).size
        val out = outcoming(read).size
        (in != 1 || out != 1) && (in != 0 || out != 0)
    }.seq

    logger.info("Terminal reads: " + termKmers.size)

    logger.info("Split reads: " + termKmers.count(read => incoming(read).size > 1 || outcoming(read).size > 1))
    
    val nodeMap = collection.mutable.Map[DNASeq, Node]()

    val progress = new ConsoleProgress("building graph", 80)

    for (read <- termKmers) {
      nodeMap(read) = new TerminalNode(read)
      nodeMap(read.revComplement) = new TerminalNode(read.revComplement)
      progress(nodeMap.size.toDouble / kmers.size / 2d)
    }

    def buildEdges(read: DNASeq) {
      val node = nodeMap(read).asInstanceOf[TerminalNode]
      for (base <- outcoming(read)) {
        val builder = DNASeq.newBuilder += base
        var seq = read.drop(1) :+ base
        var length = 1
        while (!nodeMap.contains(seq)) {
          nodeMap(seq) = new EdgeNode(read, node, base, length)
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
        val read1 = read.revComplement
        if (read1 != read) {
          buildEdges(read.revComplement)
        }
      }
      progress(nodeMap.size.toDouble / kmers.size / 2d)
    }

    //TODO perfect cycles are missing

    progress.done()

    logger.info("Graph nodes: " + nodeMap.size)

    new Graph(HashSet[DNASeq]() ++ termKmers, nodeMap)
  }
  
}
import collection.mutable.Builder
import java.io._
import java.lang.Object._
import ru.ifmo.genome.data._
import ru.ifmo.genome.dna._
import ru.ifmo.genome.dna.DNASeq._
import ru.ifmo.genome.util.ConsoleProgress
import scala.App

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object SimpleGraphProcessor extends App {
  val infile = new File(args(0))
  val outfile = new File(args(1))

  val data = PairedEndData(infile)
  val k = 32

  var readsFreq = collection.mutable.Map[SmallDNASeq, Int]()
  def add(x: SmallDNASeq) {
    val rcx = x.revComplement
    val y = if (x.hashCode < rcx.hashCode) x else rcx
    readsFreq(y) = readsFreq.getOrElse(y, 0) + 1
  }

  val progress = new ConsoleProgress("graph", 80)

  var count = 0
  for ((p1, p2) <- data.getPairs) {
    add(p1.take(k))
    add(p2.take(k).reverse)
    count += 1
    progress(count.toDouble / data.count)
  }

  readsFreq = readsFreq.filter(_._2 > 1)

//  val hist = collection.mutable.Map[Int, Int]()
//  for ((_, count) <- readsFreq) {
//    hist(count) = hist.getOrElse(count, 0) + 1
//  }
//  println(hist.toSeq.sortBy(_._1))
//  println(hist.map(_._2).sum)

  val reads: collection.Set[SmallDNASeq] = readsFreq.keySet ++ readsFreq.keySet.map(_.revComplement)

  def contains(x: SmallDNASeq) = reads.contains(x)

  def incoming(x: SmallDNASeq) = {
    for (base <- Base.fromInt if contains(base +: x.drop(1))) yield {
      base
    }
  }

  def outcoming(x: SmallDNASeq) = {
    for (base <- Base.fromInt if contains(x.take(k - 1) :+ base)) yield {
      base
    }
  }

  val graph = collection.mutable.Map[SmallDNASeq, Node]()

  def rewind(read: SmallDNASeq, from: SmallDNASeq = null): SmallDNASeq = {
    val inEdges = incoming(read)
    if (read == from || inEdges.size != 1) {
      read
    } else {
      rewind(inEdges(0) +: read.drop(1), if (from == null) read else from)
    }
  }

  class EdgeBuilder(val from: Node, val first: Base) {
    val seq: Builder[Base, SmallDNASeq] = SmallDNASeq.newBuilder += first
    var length: Long = 1L
  }

  val out = new PrintWriter(outfile)

  def dfs(read: SmallDNASeq, edgeBuilder: EdgeBuilder = null): Edge = {
    val outEdges = outcoming(read)
    if (edgeBuilder != null && outEdges.size == 1 && incoming(read).size == 1) {
      graph(read) = new EdgeNode(read, edgeBuilder.from, edgeBuilder.first, edgeBuilder.length)
      edgeBuilder.length += 1
      edgeBuilder.seq += outEdges(0)
      dfs(read.drop(1) :+ outEdges(0), edgeBuilder)
    } else {
      val node = {
        graph.get(read) match {
          case None => {
            val n = new TerminalNode(read)
            for (base <- outEdges) {
              n.outEdges += base -> dfs(read.drop(1) :+ base, new EdgeBuilder(n, base))
            }
            n
          }
          case Some(n@TerminalNode(_)) => n
        }
      }
      if (edgeBuilder != null) {
        val edge = new Edge(edgeBuilder.from, node, edgeBuilder.seq.result())
        node.inEdges ::= edge
        out.println(edge.seq.toString)
        out.flush()
        edge
      } else {
        null
      }
    }
  }

  {
    var progress = new ConsoleProgress("building graph", 80)
    var count = 0d
    for (read <- reads) {
      if (!graph.contains(read)) {
        dfs(rewind(read))
      }
      count += 1
      progress(count / reads.size)
    }
    progress.done()
  }

  out.close()

  progress.done()
}
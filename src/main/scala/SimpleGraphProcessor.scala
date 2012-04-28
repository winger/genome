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
    add(p2.take(k).revComplement)
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

  val reads: collection.Set[SmallDNASeq] = readsFreq.keySet
  println(reads.size)

  def contains(x: SmallDNASeq) = reads.contains(x) || reads.contains(x.revComplement)

  def incoming(x: SmallDNASeq) = {
    for (base <- Base.fromInt if contains(base +: x.take(k - 1))) yield {
      base
    }
  }

  def outcoming(x: SmallDNASeq) = {
    for (base <- Base.fromInt if contains(x.drop(1) :+ base)) yield {
      base
    }
  }

  val graph = collection.mutable.Map[SmallDNASeq, Node]()

  val out = new PrintWriter(outfile)

  val termReads = reads.filter(read => incoming(read).size != 1 || outcoming(read).size != 1)
  println(termReads.size)
  
  println(termReads.filter(read => incoming(read).size > 1 || outcoming(read).size > 1).size)

  {
    val progress = new ConsoleProgress("building graph", 80)

    for (read <- termReads) {
      graph(read) = new TerminalNode(read)
      graph(read.revComplement) = new TerminalNode(read.revComplement)
     progress(graph.size.toDouble / reads.size / 2.)
    }
    
    def buildEdges(read: SmallDNASeq) {
      val node = graph(read).asInstanceOf[TerminalNode]
      for (base <- outcoming(read)) {
        val builder = SmallDNASeq.newBuilder += base
        var seq = read.drop(1) :+ base
        var length = 1
        while (!graph.contains(seq)) {
          graph(seq) = new EdgeNode(read, node, base, length)
          val out = outcoming(seq)
          assert(out.size == 1, seq + " " + out)
          builder += out(0)
          length += 1
          seq = seq.drop(1) :+ out(0)
        }
        val node1 = graph(seq)
        val edge = new Edge(node, node1, builder.result())
        node1.asInstanceOf[TerminalNode].inEdges ::= edge
        node.outEdges += base -> edge
        out.println(edge.seq)
      }
    }

    for (read <- termReads) {
      buildEdges(read)
      buildEdges(read.revComplement)
      progress(graph.size.toDouble / reads.size / 2d)
    }

    //TODO perfect cycles are missing
    
    progress.done()
  }

  out.close()

  progress.done()
}
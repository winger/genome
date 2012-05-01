package ru.ifmo.genome.scripts

import java.io._
import scala.App
import ru.ifmo.genome.data._
import ru.ifmo.genome.util.ConsoleProgress
import ru.ifmo.genome.dna.DNASeq
import collection.parallel.ParSeq
import collection.immutable.Range.Inclusive
import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}
import java.util.concurrent.atomic.AtomicInteger

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object GraphSimplifier extends App {
  val (logger, formatter) = ZeroLoggerFactory.newLogger(GraphBuilder)
  import formatter._

  logger.info("Started")

  val infile = new File(args(0))
  val datafile = new File(args(1))
  val outfile = new File(args(2))

  val range: Inclusive = 180 to 250

  val data = PairedEndData(datafile)
  
  val graph = Graph(infile)
  val k = graph.termKmers.head.length

//  graph.writeDot(new PrintWriter(new FileWriter(outfile)))
  
  logger.info("K = " + k)
  
  logger.info("Graph nodes: " + graph.nodeMap.size)

  var annPairs = {
    val progress = new ConsoleProgress("annotate pairs", 80)
    
    var count = 0d
    val ps = {
      for (chunk <- data.getPairs.grouped(1024)) yield {
        val chunkRes: ParSeq[Iterable[(Graph.Pos, Graph.Pos)]] = {
          for ((p1: DNASeq, p2: DNASeq) <- chunk.par if p1.length >= k && p2.length >= k) yield {
            graph.nodeMap.get(p1.take(k)) zip graph.nodeMap.get(p2.take(k).revComplement) map {
              case (n1, n2) => (graph.getPos(n1), graph.getPos(n2))
            }
          }
        }
        count += chunk.size
        progress(count / data.count)

        chunkRes.seq.flatten
      }
    }.flatten.toList
    
    progress.done()
    
    ps.toSeq
  }

  logger.info("Pairs before: " + annPairs.size)

  val distsHist = collection.mutable.HashMap[Long, Int]()
  
  annPairs = annPairs flatMap { case (pos1, pos2) =>
    if (pos1.isRight && pos2.isRight) {
      val (edge1, dist1) = pos1.right.get
      val (edge2, dist2) = pos2.right.get
      val delta = (dist2 - dist1) + k
      if (edge1 == edge2 && range.contains(delta)) {
        distsHist(delta) = distsHist.getOrElse(delta, 0) + 1
        None
      } else {
        Some((pos1, pos2))
      }
    } else {
      Some((pos1, pos2))
    }
  }
  
  logger.info("Dists hist: " + distsHist.toSeq.sortBy(_._1))
  
  logger.info("Pairs after: " + annPairs.size)

  {
    import collection.JavaConversions._
    val hist: ConcurrentMap[Int, AtomicInteger] = new ConcurrentHashMap[Int, AtomicInteger]()

    val progress = new ConsoleProgress("walk pairs", 80)

    var count = 0d
    for (chunk <- annPairs.grouped(1024)) {
      for ((pos1, pos2) <- chunk.par) {
        println(pos1 + " " + pos2)
        val (node2, dist2) = pos2 match {
          case Left(n) => (n, 0L)
          case Right((edge, dist)) => (edge.start, dist)
        }
        def dfs(node1: TerminalNode, dist1: Long): Int = {
          if (dist1 + dist2 > range.last) {
            0
          } else {
            val cur = if (node1 == node2 && range.contains(dist1 + dist2)) 1 else 0
            cur + node1.outEdges.values.map(e => dfs(e.end, dist1 + e.seq.length)).sum
          }
        }
        val (node0, dist0) = pos1 match {
          case Left(n) => (n, 0L)
          case Right((edge, dist)) => (edge.end, edge.seq.length - dist)
        }
        val paths = dfs(node0, dist0)
        val count = {
          if (!hist.contains(paths)) {
            hist.putIfAbsent(paths, new AtomicInteger(0))
          }
          hist(paths)
        }
        count.incrementAndGet()
      }
      count += chunk.size
      progress(count / annPairs.size)
    }

    progress.done()
    
    logger.info("Paths hist: " + hist.toSeq.sortBy(_._1).map(p => (p._1, p._2.get)))
  }
}
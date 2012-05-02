package ru.ifmo.genome.scripts

import java.io._
import scala.App
import ru.ifmo.genome.data._
import ru.ifmo.genome.util.ConsoleProgress
import ru.ifmo.genome.dna.DNASeq
import collection.parallel.ParSeq
import collection.immutable.Range.Inclusive
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import collection.mutable.PriorityQueue
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.tensor.sparse.SparseVector
import scalala.tensor.dense.DenseVector

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
  val cutoff = 75

  val data = PairedEndData(datafile)
  
  val graph = Graph(infile)
  val k = graph.termNodes.head.seq.length
  logger.info("K = " + k)
  logger.info("Graph nodes: " + graph.nodeMap.size)
  
  val reachable: collection.Map[TerminalNode, collection.Map[TerminalNode, Int]] = {
    for (node <- graph.termNodes) yield {
      var queue = new PriorityQueue[(Int, TerminalNode)]()(new Ordering[(Int, TerminalNode)]{
        def compare(x: (Int, TerminalNode), y: (Int, TerminalNode)): Int = {
          x._1 - y._1
        }
      })
      val set = collection.mutable.Map[TerminalNode, Int]()
      queue += 0 -> node
      while (!queue.isEmpty) {
        val (dist, u) = queue.dequeue()
        if (!set.contains(u)) {
          set += u -> dist
          for (edge <- u.inEdges) {
            val dist2 = dist + u.seq.length
            if (dist2 <= range.last) {
              queue += dist2 -> edge.start
            }
          }
        }
      }
      node -> set
    }
  }.toMap

  logger.info("Reachable sets: " + reachable.map(_._2.size).sum)

  var annPairs: Iterator[(Graph.Pos, Graph.Pos)] = {
    lazy val progress = new ConsoleProgress("walk pairs", 80)
    var count = 0d
    val annPairs = {
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
    }.flatten
    annPairs
  }
  
  annPairs = annPairs flatMap { case (pos1, pos2) =>
    if (pos1 == null || pos2 == null) {
      logger.warning("Skipped pair because of multiple locations")
      None
    } else {
      if (pos1.isRight && pos2.isRight) {
        val (edge1, dist1) = pos1.right.get
        val (edge2, dist2) = pos2.right.get
        val delta = (dist2 - dist1) + k
        if (edge1 == edge2 && range.contains(delta)) {
          None
        } else {
          Some((pos1, pos2))
        }
      } else {
        Some((pos1, pos2))
      }
    }
  }

  {
    import collection.JavaConversions._
    val pathsMap: collection.mutable.ConcurrentMap[(Edge, Edge), AtomicInteger] = new ConcurrentHashMap[(Edge, Edge), AtomicInteger]()

    var count = 0d
    for (chunk <- annPairs.grouped(1024)) {
      for ((pos1, pos2) <- chunk.par) {
        var pathEdges = collection.mutable.Set[(Edge, Edge)]()
        val (node2, dist2) = pos2 match {
          case Left(n) => (n, 0)
          case Right((edge, dist)) =>
            (edge.start, dist)
        }
        val startEdge = pos1.fold(_ => null, _._1)
        val endEdge = pos2.fold(_ => null, _._1)
        def dfs(node1: TerminalNode, dist1: Long, prevEdge: Edge): Int = {
          if (dist1 + dist2 + reachable(node2).getOrElse(node1, range.last + 1) > range.last) {
            0
          } else {
            val cur = if (node1 == node2 && range.contains(dist1 + dist2)) {
              if (prevEdge != null && endEdge != null) {
                pathEdges += prevEdge -> endEdge
              }
              1
            } else 0
            cur + node1.outEdges.values.map { e =>
              val count = dfs(e.end, dist1 + e.seq.length, e)
              if (count > 0 && prevEdge != null) {
                pathEdges += prevEdge -> e
              }
              count
            }.sum
          }
        }
        val (node0, dist0) = pos1 match {
          case Left(n) => (n, 0)
          case Right((edge, dist)) =>
            (edge.end, edge.seq.length - dist)
        }
        val paths = dfs(node0, dist0, startEdge)
        for (es <- pathEdges) {
          val counter = {
            if (!pathsMap.contains(es)) pathsMap.putIfAbsent(es, new AtomicInteger())
            pathsMap(es)
          }
          counter.incrementAndGet()
        }
      }
      count += chunk.size
    }
    
    val outf = new PrintWriter(new FileWriter(outfile))

    var newNodes = 0
    for (node <- graph.termNodes) {
      val in = node.inEdges
      val out = node.outEdges.values.toSeq
      assert(in.size > 0 || out.size > 0)
      val matrix = Array.tabulate(in.size, out.size) { (i, j) =>
        pathsMap.get((in(i), out(j))).map(_.get).getOrElse(0)
      }
      val colLeft = new Array[Boolean](in.size)
      val colRight = new Array[Boolean](out.size)
      for (i <- 0 until in.size if !colLeft(i)) {
        def dfsLeft(i: Int): (Set[Int], Set[Int]) = {
          assert(!colLeft(i))
          colLeft(i) = true
          var (l, r) = (Set(i), Set.empty[Int])
          for (j <- 0 until out.size if !colRight(j) && matrix(i)(j) >= cutoff) {
            val (l1, r1) = dfsRight(j)
            l ++= l1
            r ++= r1
          }
          (l, r)
        }
        def dfsRight(j: Int): (Set[Int], Set[Int]) = {
          assert(!colRight(j))
          colRight(j) = true
          var (l, r) = (Set.empty[Int], Set(j))
          for (i <- 0 until in.size if !colLeft(i) && matrix(i)(j) >= cutoff) {
            val (l1, r1) = dfsLeft(i)
            l ++= l1
            r ++= r1
          }
          (l, r)
        }
        val (l, r) = dfsLeft(i)
        if (l.size > 0 && r.size > 0 && (l.size > 1 || r.size > 1)) {
          newNodes += 1
        }
      }
      outf.println(node.seq + " -> " + matrix.deep.toString + " " + in.map(_.seq.size) + " " + out.map(_.seq.size).toList)
    }
    
    outf.close()

    logger.info("New nodes count: " + newNodes)
  }
  
//  val edgesToRemove = edgeMap.filter{case (e, c) => c.get - e.seq.length <= cutoffAdd && e.seq.length <= 100}.map(_._1)
//  logger.info("Total edges: " + graph.edges.size)
//  logger.info("Edges to remove: " + edgesToRemove.size)
//
//  for (edge <- edgesToRemove) {
//    edge.start.outEdges -= edge.seq(0)
//    edge.end.inEdges = edge.end.inEdges.filterNot(_ == edge)
//  }
//
//  logger.info("Terminal nodes before: " + graph.termNodes.size)
//  for (node <- graph.termNodes) {
//    val in = node.inEdges.size
//    val out = node.outEdges.size
//    if (in == 1 && out == 1) {
//      graph.termKmers -= node.seq
//      graph.nodeMap -= node.seq
//      val e1 = node.inEdges.head
//      val (_, e2) = node.outEdges.head
//      val e = new Edge(e1.start, e2.end, e1.seq ++ e2.seq)
//      e.start.outEdges += e.seq.head -> e
//      e.end.inEdges = e.end.inEdges.map(ee => if (ee == e2) e else ee)
//    }
//  }
//  graph.nodeMap.retain{case (seq, _) => graph.termKmers(seq)}
//  logger.info("Terminal nodes after: " + graph.termNodes.size)
//
//  logger.info("Component sizes:" + graph.components.map(_.flatMap(_.outEdges.values).map(_.seq.size).sum).filter(_ != 0))
//  val maxComponent = graph.components.maxBy(_.size)
//  logger.info("Max component size: " + maxComponent.size)
//  graph.retain(maxComponent)
//  graph.write(outfile)
//
//  hist(DenseVector(edgeMap.filter((1000 to 1100) contains _._1.seq.size).values.map(_.get).toSeq: _*), 1000)
}
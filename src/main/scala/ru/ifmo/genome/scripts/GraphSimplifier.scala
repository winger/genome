package ru.ifmo.genome.scripts

import ru.ifmo.genome.data._
import graph._
import ru.ifmo.genome.util.ConsoleProgress
import ru.ifmo.genome.dna.DNASeq
import scala.collection.parallel.ParSeq
import scala.collection.immutable.Range.Inclusive
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.tensor.sparse.SparseVector
import scalala.tensor.dense.DenseVector
import java.io._
import scala._
import collection.mutable.PriorityQueue
import scala.Predef._

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
//  val range: Inclusive = 160 to 270
  val cutoff = 50

  val data = PairedEndData(datafile)

  implicit val graph = Graph(infile)
  val k = graph.getNodes.head.seq.length
  logger.info("K = " + k)
  logger.info("Graph nodes: " + graph.getNodes.size)

  for (it <- 0 until 5) {
    System.gc()

    for (edge <- graph.getEdges) {
      assert(edge.start.outEdgeIds(edge.seq(0)) == edge.id && edge.end.inEdgeIds(edge.id))
    }
    for (node <- graph.getNodes) {
      for (edge <- node.inEdges) {
        assert(edge.end == node)
      }
      for ((base, edge) <- node.outEdges) {
        assert(edge.start == node && edge.seq(0) == base)
      }
    }

    val reachable: collection.Map[(Node, Node), Int] = {
      for (node <- graph.getNodes.par) yield {
        var queue = new PriorityQueue[(Int, Node)]()(new Ordering[(Int, Node)]{
          def compare(x: (Int, Node), y: (Int, Node)): Int = {
            y._1 - x._1
          }
        })
        var set = collection.immutable.Map[Node, Int]()
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
        set.map{case (node1, dist) => (node, node1) -> dist}
      }
    }.seq.flatten.toMap
  
    logger.info("Reachable sets: " + reachable.size)
  
    val graphMap = graph.getGraphMap

    var notFoundPairs1, notFoundPairs2 = 0

    var annPairs: Iterator[(Iterable[GraphPosition], Iterable[GraphPosition])] = {
      lazy val progress = new ConsoleProgress("walk pairs", 80)
      var count = 0d
      val annPairs = {
        for (chunk <- data.getPairs.grouped(1024)) yield {
          val chunkRes: ParSeq[Iterable[(Iterable[GraphPosition], Iterable[GraphPosition])]] = {
            for ((p1: DNASeq, p2: DNASeq) <- chunk.par if p1.length >= k && p2.length >= k) yield {
              if (!graphMap.contains(p1.take(k)) || !graphMap.contains(p2.take(k).revComplement)) {
                notFoundPairs1 += 1
              }
              if (!graphMap.contains(p2.take(k)) || !graphMap.contains(p1.take(k).revComplement)) {
                notFoundPairs2 += 1
              }
              (graphMap.get(p1.take(k)) zip graphMap.get(p2.take(k).revComplement)) ++
              (graphMap.get(p2.take(k)) zip graphMap.get(p1.take(k).revComplement))
            }
          }
          count += chunk.size
          progress(count / data.count)
          chunkRes.seq.flatten
        }
      }.flatten
      annPairs
    }
  
    annPairs = annPairs flatMap { case pair@(positions1, positions2) =>
      val condition = positions1.exists {
        case EdgeGraphPosition(edge1, dist1) => positions2.exists {
          case EdgeGraphPosition(edge2, dist2) => edge1 == edge2 && range.contains((dist2 - dist1) + k)
          case _ => false
        }
        case _ => false
      }
      if (condition) {
        None
      } else {
        Some(pair)
      }
    }
  
    {
      import scala.collection.JavaConversions._
      val pathsMap: collection.mutable.ConcurrentMap[(Edge, Edge), AtomicInteger] = new ConcurrentHashMap[(Edge, Edge), AtomicInteger]()
  
      var count = 0d
      var badPairs = 0
      for (chunk <- annPairs.grouped(1024)) {
        for ((positions1, positions2) <- chunk.par; pos1 <- positions1; pos2 <- positions2) {
          var pathEdges = collection.mutable.Set[(Edge, Edge)]()
          val (node2, dist2) = pos2 match {
            case NodeGraphPosition(n) => (n, 0)
            case EdgeGraphPosition(edge, dist) =>
              (edge.start, dist)
          }
          val startEdge = pos1 match { case EdgeGraphPosition(edge, _) => edge; case _ => null }
          val endEdge = pos2 match { case EdgeGraphPosition(edge, _) => edge; case _ => null }
          def dfs(node1: Node, dist1: Int, prevEdge: Edge): Int = {
            if (dist1 + dist2 + reachable.getOrElse((node2, node1), range.last + 1) > range.last) {
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
            case NodeGraphPosition(n) => (n, 0)
            case EdgeGraphPosition(edge, dist) =>
              (edge.end, edge.seq.length - dist)
          }
          val paths = dfs(node0, dist0, startEdge)
          if (paths == 0) {
            badPairs += 1
          }
          for (es <- pathEdges) {
            val counter = {
              if (!pathsMap.contains(es)) pathsMap.putIfAbsent(es, new AtomicInteger(0))
              pathsMap(es)
            }
            counter.incrementAndGet()
          }
        }
        count += chunk.size
      }
  
      logger.info("Bad pairs: " + badPairs)
      logger.info("Not found pairs: " + notFoundPairs1 + " " + notFoundPairs2)
  
      val outf = new PrintWriter(new FileWriter(outfile))
  
      val toRemove = collection.mutable.Map[Long, Int]()
  
      for (node <- graph.getNodes; in = node.inEdgeIds.toArray; out = node.outEdgeIds.values.toArray
           if in.size > 0 && out.size > 0) {
        val matrix = Array.tabulate(in.size, out.size) { (i, j) =>
          pathsMap.get((graph.getEdge(in(i)), graph.getEdge(out(j)))).map(_.get).getOrElse(0)
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
          if (r.size == 0) {
            toRemove(in(i)) = toRemove.getOrElse(in(i), 0) + 1
          } else {
            val newNode = graph.addNode(node.seq)
            for (i <- l; e = in(i)) graph.replaceEnd(e, newNode)
            for (i <- r; e = out(i)) graph.replaceStart(e, newNode)
          }
        }
        for (edgeId <- (0 until out.size).filterNot(colRight).map(out(_))) {
          toRemove(edgeId) = toRemove.getOrElse(edgeId, 0) + 1
        }
        outf.println(node.seq + " -> " + matrix.deep.toString + " " + in.map(graph.getEdge(_).seq.size).toList + " " + out.map(graph.getEdge(_).seq.size).toList)
      }
  
      logger.info("Edges before: " + graph.getEdges.size)
      toRemove.iterator.collect{case (edgeId, 2) => edgeId}.foreach(id => graph.removeEdge(graph.getEdge(id)))
      graph.removeBubbles()
      graph.simplifyGraph()
      logger.info("Edges after: " + graph.getEdges.size)
  
      val components = graph.components
  
      logger.info("Total edges length: " + graph.getEdges.map(_.seq.length).sum)
  
      val hist2 = components.groupBy { comp =>
        (comp.size, comp.flatMap(_.outEdges.values).toSeq.map(_.seq.size).sum)
      }.map(p => (p._1, p._2.size)).toSeq.sortBy(_._1)
      logger.info("Components histogram: " + hist2)
      
//      val maxComponent = components.maxBy(_.size)
//      logger.info("Max component size: " + maxComponent.size)
//      graph.retain(maxComponent)
  
      outf.close()
    }
  }

  graph.writeDot(new PrintWriter(outfile))

}
package ru.ifmo.genome.scripts

import ru.ifmo.genome.data._
import graph._
import ru.ifmo.genome.util.ConsoleProgress
import ru.ifmo.genome.dna.DNASeq
import scala.collection.parallel.ParSeq
import scala.collection.immutable.Range.Inclusive
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.PriorityQueue
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.tensor.sparse.SparseVector
import scalala.tensor.dense.DenseVector
import java.io._
import scala._

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

  implicit val graph = Graph(infile)
  val k = graph.getNodes.head.seq.length
  logger.info("K = " + k)
  logger.info("Graph nodes: " + graph.getNodes.size)

  val reachable: collection.Map[Node, collection.Map[Node, Int]] = {
    for (node <- graph.getNodes) yield {
      var queue = new PriorityQueue[(Int, Node)]()(new Ordering[(Int, Node)]{
        def compare(x: (Int, Node), y: (Int, Node)): Int = {
          x._1 - y._1
        }
      })
      val set = collection.mutable.Map[Node, Int]()
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

  val graphMap = graph.getGraphMap

  var annPairs: Iterator[(List[GraphPosition], List[GraphPosition])] = {
    lazy val progress = new ConsoleProgress("walk pairs", 80)
    var count = 0d
    val annPairs = {
      for (chunk <- data.getPairs.grouped(1024)) yield {
        val chunkRes: ParSeq[Iterable[(List[GraphPosition], List[GraphPosition])]] = {
          for ((p1: DNASeq, p2: DNASeq) <- chunk.par if p1.length >= k && p2.length >= k) yield {
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
        val endEdge = pos1 match { case EdgeGraphPosition(edge, _) => edge; case _ => null }
        def dfs(node1: Node, dist1: Int, prevEdge: Edge): Int = {
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

    val outf = new PrintWriter(new FileWriter(outfile))

    val toRemove = collection.mutable.Set[Long]()

    var newNodes = 0
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
          toRemove += in(i)
        } else {
          val newNode = graph.addNode(node.seq)
          for (i <- l; e = in(i)) graph.replaceEnd(e, newNode)
          for (i <- r; e = out(i)) graph.replaceStart(e, newNode)
        }
        if (l.size > 0 && r.size > 0 && (l.size > 1 || r.size > 1)) {
          newNodes += 1
        }
      }
      graph.removeNode(node)
      toRemove ++= (0 until out.size).filterNot(colRight).map(out(_))
      outf.println(node.seq + " -> " + matrix.deep.toString + " " + in.map(graph.getEdge(_).seq.size).toList + " " + out.map(graph.getEdge(_).seq.size).toList)
    }

    logger.info("Edges before: " + graph.getEdges.size)
    toRemove.foreach(id => graph.removeEdge(graph.getEdge(id)))
//    graph.simplifyGraph()
    logger.info("Edges after: " + graph.getEdges.size)

    val components = graph.components

    logger.info("Total edges length: " + graph.getEdges.map(_.seq.length).sum)

    val hist = components.groupBy(_.size).map(p => (p._1, p._2.size)).toSeq.sortBy(_._1)
    logger.info("Components histogram: " + hist)

    val hist2 = components.groupBy { comp =>
      comp.flatMap(_.outEdges.values).map(_.seq.size).sum
    }.map(p => (p._1, p._2.size)).toSeq.sortBy(_._1)
    logger.info("Components histogram 2: " + hist2)

    outf.close()

    logger.info("New nodes count: " + newNodes)
  }

}
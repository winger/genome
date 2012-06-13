package ru.ifmo.genome.scripts

import ru.ifmo.genome.data._
import graph._
import ru.ifmo.genome.util.ConsoleProgress
import ru.ifmo.genome.dna.DNASeq
import scala.collection.parallel.ParSeq
import scala.collection.immutable.Range.Inclusive
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import akka.event.Logging
import akka.kernel.Bootable

//import scalala.library.Plotting._
//import scalala.tensor.dense.DenseVector
import java.io._
import scala._
import collection.mutable.{SynchronizedMap, PriorityQueue}
import scala.Predef._
import akka.dispatch.Await
import akka.util.Duration

/**
* Author: Vladislav Isenbaev (isenbaev@gmail.com)
*/

object GraphSimplifier extends Bootable {
  val logger = Logging(ActorsHome.system, getClass.getSimpleName)

  def startup() {
    import ActorsHome.system
  
    logger.info("Started")
    
    val config = system.settings.config.getConfig("genome")
  
    val infile = new File(config.getString("inputFile"))
    val datafile = new File(config.getString("dataFile"))
    val outfile = new File(config.getString("outputFile"))
    val graphfile = new File(config.getString("graphFile"))
  
    val range: Inclusive = 180 to 250
  //  val range: Inclusive = 160 to 270
    val cutoff = 200
  
    val data = PairedEndData(datafile)
  
    implicit val graph = Graph(infile)
    val k = graph.getNodes.head.seq.length
    logger.info("K = " + k)
  
    for (it <- 0 until 1) {
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
  
      val cache = new collection.mutable.HashMap[Node, collection.Map[Node, Int]]() with SynchronizedMap[Node, collection.Map[Node, Int]]
      def reachable(node: Node): collection.Map[Node, Int] = {
        if (cache.contains(node)) {
          cache(node)
        } else {
          var queue = new PriorityQueue[(Int, Node)]()(new Ordering[(Int, Node)]{
            def compare(x: (Int, Node), y: (Int, Node)): Int = {
              y._1 - x._1
            }
          })
          var set = collection.mutable.Map[Node, Int]()
          queue += 0 -> node
          while (!queue.isEmpty) {
            val (dist, u) = queue.dequeue()
            if (!set.contains(u)) {
              set += u -> dist
              for (edge <- u.inEdges) {
                val dist2 = dist + edge.seq.length
                if (dist2 <= range.last) {
                  queue += dist2 -> edge.start
                }
              }
            }
          }
          if (cache.size < 50000) {
            cache += node -> set
          }
          set
        }
      }
    
      val graphMap = graph.getGraphMap
  
      var notFoundPairs1, notFoundPairs2 = 0
  
      var annPairs: Iterator[(Iterable[GraphPosition], Iterable[GraphPosition])] = {
        lazy val progress = new ConsoleProgress("walk pairs", 80)
        var count = 0d
        val annPairs = {
          for (chunk <- data.getPairs.grouped(1024)) yield {
            val chunkRes: ParSeq[Iterable[(Iterable[GraphPosition], Iterable[GraphPosition])]] = {
              for ((p1: DNASeq, p2: DNASeq) <- chunk.par if p1.length >= k && p2.length >= k) yield {
                if (!Await.result(graphMap.contains(p1.take(k)), Duration.Inf) || !Await.result(graphMap.contains(p2.take(k).revComplement), Duration.Inf)) {
                  notFoundPairs1 += 1
                }
                if (!Await.result(graphMap.contains(p2.take(k)), Duration.Inf) || !Await.result(graphMap.contains(p1.take(k).revComplement), Duration.Inf)) {
                  notFoundPairs2 += 1
                }
                Iterable(
                  (Await.result(graphMap.getAll(p1.take(k)), Duration.Inf), Await.result(graphMap.getAll(p2.take(k).revComplement), Duration.Inf)),
                  (Await.result(graphMap.getAll(p2.take(k)), Duration.Inf), Await.result(graphMap.getAll(p1.take(k).revComplement), Duration.Inf))
                )
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
        var reachables = 0d
  
        import scala.collection.JavaConversions._
        val pathsMap: collection.mutable.ConcurrentMap[(Edge, Edge), AtomicInteger] = new ConcurrentHashMap[(Edge, Edge), AtomicInteger]()
    
        var count = 0d
        var badPairs = 0
        for (chunk <- annPairs.grouped(1024)) {
          for ((positions1, positions2) <- chunk.par) {
            var pathEdges = collection.mutable.Set[(Edge, Edge)]()
            val pairs = for (pos1 <- positions1; pos2 <- positions2) yield (pos1, pos2)
            var good = false
            pairs.foreach{ case (pos1, pos2) =>
              val (node2, dist2) = pos2 match {
                case NodeGraphPosition(n) => (n, 0)
                case EdgeGraphPosition(edge, dist) =>
                  (edge.start, dist)
              }
              val startEdge = pos1 match { case EdgeGraphPosition(edge, _) => edge; case _ => null }
              val endEdge = pos2 match { case EdgeGraphPosition(edge, _) => edge; case _ => null }
              val reachableSet = reachable(node2)
              reachables += reachableSet.size
              val memo = collection.mutable.Map[(Edge, Int), Boolean]()
              def dfs(node1: Node, dist1: Int, prevEdge: Edge): Boolean = {
                if (memo.contains((prevEdge, dist1 / 5))) {
                  memo((prevEdge, dist1 / 5))
                } else if (dist1 + dist2 + reachableSet.getOrElse(node1, range.last + 1) > range.last) {
                  false
                } else {
                  var cur = if (node1 == node2 && range.contains(dist1 + dist2)) {
                    if (prevEdge != null && endEdge != null) {
                      pathEdges += prevEdge -> endEdge
                    }
                    true
                  } else false
                  node1.outEdges.values.foreach { e =>
                    val res = dfs(e.end, dist1 + e.seq.length, e)
                    if (res && prevEdge != null) {
                      pathEdges += prevEdge -> e
                    }
                    cur |= res
                  }
                  memo((prevEdge, dist1 / 5)) = cur
                  cur
                }
              }
              val (node0, dist0) = pos1 match {
                case NodeGraphPosition(n) => (n, 0)
                case EdgeGraphPosition(edge, dist) =>
                  (edge.end, edge.seq.length - dist)
              }
              val paths = dfs(node0, dist0, startEdge)
              good |= paths
            }
            for (es <- pathEdges) {
              val counter = {
                if (!pathsMap.contains(es)) pathsMap.putIfAbsent(es, new AtomicInteger(0))
                pathsMap(es)
              }
              counter.incrementAndGet()
            }
            if (!good) {
              badPairs += 1
            }
          }
          count += chunk.size
        }
    
        logger.info("Bad pairs: " + badPairs)
        logger.info("Not found pairs: " + notFoundPairs1 + " " + notFoundPairs2)
    
        val outf = new PrintWriter(new FileWriter(outfile))
    
        val toRemove = collection.mutable.Set[Long]()
    
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
          }
          toRemove ++= (0 until out.size).filterNot(colRight).map(out(_))
          outf.println(node.id + " -> " + matrix.deep.toString + " " + in.map(graph.getEdge(_).seq.size).toList + " " + out.map(graph.getEdge(_).seq.size).toList)
        }
    
        logger.info("Edges before: " + graph.getEdges.size)
        toRemove.foreach(id => graph.removeEdge(graph.getEdge(id)))
  //      graph.removeBubbles()
        graph.simplifyGraph()
        logger.info("Edges after: " + graph.getEdges.size)
    
        val components = graph.components
    
        logger.info("Total edges length: " + graph.getEdges.map(_.seq.length).sum)
    
        val hist2 = components.groupBy { comp =>
          (comp.size, comp.flatMap(_.outEdges.values).toSeq.map(_.seq.size).sum)
        }.map(p => (p._1, p._2.size)).toSeq.sortBy(_._1)
        logger.info("Components histogram: " + hist2)
        
        val maxComponent = components.maxBy(_.size)
        logger.info("Max component size: " + maxComponent.size)
    
        outf.close()
  
        val lengths = graph.getEdges.map(_.seq.size).toSeq.groupBy(identity).map(p => (p._1, p._2.size))
        logger.info("" + lengths)
  
        {
          var i = 0
          val out = new PrintWriter("contigs")
          for (e <- graph.getEdges) {
            out.println(e.seq)
            out.println(">abacaba" + i)
            i += 1
          }
          out.close()
        }
        
  //      hist(DenseVector(graph.getEdges.map(_.seq.size).filter(_ > 100).toSeq: _*), 100)
      }
    }
    graph.asInstanceOf[MapGraph].write(graphfile)
  
  //  graph.retain(graph.neighbours(graph.getNodes.head, 100))
  //  graph.writeDot(new PrintWriter(graphfile))
  
    ActorsHome.system.shutdown()
  }

  def shutdown() {}

  def main(args: List[String]) {startup()}
}
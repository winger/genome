package ru.ifmo.genome.scripts

import ru.ifmo.genome.data._
import graph._
import ru.ifmo.genome.util.ConsoleProgress
import ru.ifmo.genome.dna.DNASeq
import scala.collection.parallel.ParSeq
import scala.collection.immutable.Range.Inclusive
import akka.event.Logging
import akka.kernel.Bootable
import akka.actor._
import akka.pattern.{ask, pipe}
import akka.remote.RemoteScope
import akka.util.{Timeout, Duration}
import akka.util.duration._
import akka.dispatch.{Future, Await}
import java.util.concurrent.atomic.{AtomicLong, AtomicInteger}
import akka.event.slf4j.SLF4JLogging
import collection.mutable.{Set, PriorityQueue}
import collection.immutable.List
import java.util.concurrent.{Semaphore, CountDownLatch, ConcurrentHashMap}

//import scalala.library.Plotting._
//import scalala.tensor.dense.DenseVector
import java.io._
import scala._
import scala.Predef._

/**
* Author: Vladislav Isenbaev (isenbaev@gmail.com)
*/

class WalkingActor(range_ : (Int, Int), graph_ : Graph) extends Actor {
  implicit val graph = graph_
  import context.dispatcher

  val logger = Logging(context.system, this)

  val range = range_._1 to range_._2

  val cache = new collection.mutable.LinkedHashMap[Node, collection.Map[Node, Int]]()

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
      //TODO: use LRU cache
      if (cache.size < 50000) {
        cache += node -> set
      }
      set
    }
  }

  var processed = new AtomicInteger(0)
  var totalTime = new AtomicLong(0L)

  def receive = {
    case (pos1: GraphPosition, pos2: GraphPosition) => Future {
      val time0 = System.nanoTime
      var pathEdges = collection.mutable.Set[(Long, Long)]()
      val (node2, dist2) = pos2 match {
        case NodeGraphPosition(nodeId) => (graph.getNode(nodeId), 0)
        case EdgeGraphPosition(edgeId, dist) =>
          (graph.getEdge(edgeId).start, dist)
      }
      val startEdge = pos1 match { case EdgeGraphPosition(edgeId, _) => graph.getEdge(edgeId); case _ => null }
      val endEdge = pos2 match { case EdgeGraphPosition(edgeId, _) => graph.getEdge(edgeId); case _ => null }
      val reachableSet = reachable(node2)
      val memo = collection.mutable.Map[(Edge, Int), Boolean]()
      def dfs(node1: Node, dist1: Int, prevEdge: Edge): Boolean = {
        if (memo.contains((prevEdge, dist1))) {
          memo((prevEdge, dist1))
        } else if (dist1 + dist2 + reachableSet.getOrElse(node1, range.last + 1) > range.last) {
          false
        } else {
          var cur = if (node1 == node2 && range.contains(dist1 + dist2)) {
            if (prevEdge != null && endEdge != null) {
              pathEdges += prevEdge.id -> endEdge.id
            }
            true
          } else false
          node1.outEdges.values.foreach { e =>
            val res = dfs(e.end, dist1 + e.seq.length, e)
            if (res && prevEdge != null) {
              pathEdges += prevEdge.id -> e.id
            }
            cur |= res
          }
          memo((prevEdge, dist1)) = cur
          cur
        }
      }
      val (node0, dist0) = pos1 match {
        case NodeGraphPosition(nodeId) => (graph.getNode(nodeId), 0)
        case EdgeGraphPosition(edgeId, dist) =>
          (graph.getEdge(edgeId).end, graph.getEdge(edgeId).seq.length - dist)
      }
      val ret = (dfs(node0, dist0, startEdge), pathEdges)
      val processedNew = processed.incrementAndGet()
      val totalTimeNew = totalTime.getAndAdd(System.nanoTime - time0)
      if (processedNew % 10000 == 0) {
        logger.info("Processed " + processedNew + " events, average time is " + (totalTimeNew.toDouble / processedNew))
      }
      ret
    } pipeTo sender
  }
}

class GraphSimplifier extends Bootable {
  val logger = Logging(ActorsHome.system, getClass.getSimpleName)

  def startup() {
    import ActorsHome.system
  
    logger.info("Started")

    implicit val timeout = Timeout(1000000 seconds)
    
    val config = system.settings.config.root().toConfig.getConfig("genome")
  
    val infile = new File(config.getString("inputFile"))
    val datafile = new File(config.getString("dataFile"))
    val outfile = new File(config.getString("outputFile"))
    val graphfile = new File(config.getString("graphFile"))
  
    implicit val range: Inclusive = 180 to 250
  //  val range: Inclusive = 160 to 270
    val cutoff = ActorsHome.conf.root.toConfig.getInt("genome.cutoff")
  
    val data = PairedEndData(datafile)
  
    implicit val graph = Graph(infile)
    val k = graph.getNodes.head.seq.length
    logger.info("K = " + k)
  
    for (it <- 0 until 1) {
      System.gc()
  
      for (edge <- graph.getEdges) {
        assert(edge.start.outEdgeIds(edge.seq(0)) == edge.id && edge.end.inEdgeIds(edge.id),
          edge + " " + edge.start + " " + edge.end)
      }
      for (node <- graph.getNodes) {
        for (edge <- node.inEdges) {
          assert(edge.end == node)
        }
        for ((base, edge) <- node.outEdges) {
          assert(edge.start == node && edge.seq(0) == base)
        }
      }

      val partitions: Array[ActorRef] = {
        import collection.JavaConverters._
        val nodes = system.settings.config.getStringList("genome.storageNodes").asScala.map(AddressFromURIString(_))

        def creator(range: (Int, Int), graph: Graph) = {
          new (() => WalkingActor) {
            def apply() = new WalkingActor(range, graph)
          }
        }

        nodes.map { address =>
          val props = Props(creator((range.head, range.last), graph)).withDeploy(Deploy(scope = RemoteScope(address)))
          system.actorOf(props)
        }
      }.toArray
    
      val graphMap = graph.getGraphMap

      val takeFirst = ActorsHome.conf.root.toConfig.getInt("genome.takeFirst")

      def annotate(pair: (List[GraphPosition], List[GraphPosition])) = {
        val (positions1, positions2) = pair
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

      import scala.collection.JavaConversions._
      val pathsMap: collection.mutable.ConcurrentMap[(Long, Long), AtomicInteger] = new ConcurrentHashMap[(Long, Long), AtomicInteger]()

      var badPairs = 0
      lazy val progress = new ConsoleProgress("walk pairs", 80)
      val tasksIterator = for ((p1: DNASeq, p2: DNASeq) <- data.getPairs.take(takeFirst) if p1.length >= k && p2.length >= k) yield { () =>
        val f1 = graphMap.getAll(p1.take(k))
        val f2 = graphMap.getAll(p2.take(k).revComplement)
        val f3 = graphMap.getAll(p2.take(k))
        val f4 = graphMap.getAll(p1.take(k).revComplement)
        for (s1 <- f1; s2 <- f2; s3 <- f3; s4 <- f4) yield {
          for ((p1, p2) <- List((s1.toList, s2.toList), (s3.toList, s4.toList)).flatMap(annotate(_).toTraversable)) {
            val pairs = for (pos1 <- p1; pos2 <- p2) yield (pos1, pos2)
            val futures = for ((pos1, pos2) <- pairs) yield {
              val endNode = pos2 match {
                case NodeGraphPosition(nodeId) => graph.getNode(nodeId)
                case EdgeGraphPosition(edgeId, _) => graph.getEdge(edgeId).start
              }
              def fix(i: Int) = if (i < 0) i + partitions.size else i
              val actorId = fix(endNode.hashCode % partitions.size)
              val actorRef = partitions(actorId)
              (actorRef ? (pos1, pos2)).mapTo[(Boolean, collection.mutable.Set[(Long, Long)])]
            }
            for (list <- Future.sequence(futures) if !list.isEmpty) {
              val (goods, pathEdgesList) = list.unzip
              val good = goods.exists(identity)
              val pathEdges = pathEdgesList.reduce(_ ++ _)
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
          }
        }
      }
      
      def executeAll(genFutures: TraversableOnce[() => Future[Unit]], total: Long, progress: ConsoleProgress) {
        val concurrency = 500
        val semaphore = new Semaphore(concurrency)
        var count = 0
        for (genFuture <- genFutures) {
          semaphore.acquire()
          genFuture().onComplete(_ => semaphore.release())
          count += 1
          progress(count.toDouble / total)
        }
        semaphore.acquire(concurrency)
      }
      
      executeAll(tasksIterator, data.count max takeFirst, progress)
    
      {
        logger.info("Bad pairs: " + badPairs)
    
        val outf = new PrintWriter(new FileWriter(outfile))
    
        val toRemove = collection.mutable.Set[Long]()
    
        for (node <- graph.getNodes; in = node.inEdgeIds.toArray; out = node.outEdgeIds.values.toArray
             if in.size > 0 && out.size > 0) {
          val matrix = Array.tabulate(in.size, out.size) { (i, j) =>
            pathsMap.get((graph.getEdge(in(i)).id, graph.getEdge(out(j)).id)).map(_.get).getOrElse(0)
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

}

object GraphSimplifier {
  def main(args: Array[String]) {new GraphSimplifier().startup()}
}
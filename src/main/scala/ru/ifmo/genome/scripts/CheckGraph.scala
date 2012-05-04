package ru.ifmo.genome.scripts

import ru.ifmo.genome.data._
import graph._
import java.io._
import scala._
import io.Source
import ru.ifmo.genome.dna.{DNASeq, Base}

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object CheckGraph extends App {
  val (logger, formatter) = ZeroLoggerFactory.newLogger(GraphBuilder)
  import formatter._

  logger.info("Started")

  val infile = new File(args(0))
  val datafile = new File(args(1))

  implicit val graph = Graph(infile)
  val k = graph.getNodes.head.seq.length
  logger.info("K = " + k)
  logger.info("Graph nodes: " + graph.getNodes.size)

  System.gc()

  val contigs = graph.getEdges.map(_.seq.size).filter(_ > 200).toSeq.sorted
  logger.info("Contigs count " + contigs.size)
  logger.info("Contigs size " + contigs.sum)
  logger.info("Contigs N50 " + contigs(contigs.size / 2))
  logger.info("Contigs max " + contigs.last)

  val graphMap = graph.getGraphMap

//  logger.info("" + graphMap.get(DNASeq("ATCGTCGGTTCCGCCATTG".map(Base.fromChar): _*)))
//  logger.info("" + graph.getNode(120864).outEdges)

  for (line <- Source.fromFile(datafile).getLines() if !line.startsWith(">")) {
    for (str <- line.sliding(k) if str.forall(Base.fromChar.contains(_))) {
      val read = DNASeq(str.map(Base.fromChar): _*)
      if (!graphMap.contains(read)) {
        logger.info("Not found " + line.length + " " + read)
      }
    }
  }

}
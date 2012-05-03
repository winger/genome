package ru.ifmo.genome.scripts

import java.io._
import scala.App
import ru.ifmo.genome.data._
import graph.{MapGraph, Graph}

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object GraphBuilder extends App {
  val (logger, formatter) = ZeroLoggerFactory.newLogger(GraphBuilder)
  import formatter._

  logger.info("Started")

  val infile = new File(args(0))
  val outfile = new File(args(1))

  val data = PairedEndData(infile)
  val k = 19
  val rounds = 2

  val kmersFreq = FreqFilter.extractFilteredKmers(data, k, rounds)

  logger.info("Good reads count: " + kmersFreq.size)

  implicit val graph = Graph.buildGraph(k, kmersFreq)
  val components = graph.components

  logger.info("Total edges length: " + graph.getEdges.map(_.seq.length).sum)

  val hist = components.groupBy(_.size).map(p => (p._1, p._2.size)).toSeq.sortBy(_._1)
  logger.info("Components histogram: " + hist)

  val hist2 = components.groupBy { comp =>
    comp.flatMap(_.outEdges.values).toSeq.map(_.seq.size).sum
  }.map(p => (p._1, p._2.size)).toSeq.sortBy(_._1)
  logger.info("Components histogram 2: " + hist2)

//  val edgesHist = graph.edges.groupBy(_.seq.length).map(p => (p._1, p._2.size)).toSeq.sortBy(_._1)
//  logger.info("Edges histogram: " + edgesHist)

  val maxComponent = components.maxBy(_.size)
  logger.info("Max component size: " + maxComponent.size)
  graph.retain(maxComponent)

  graph.asInstanceOf[MapGraph].write(outfile)
}
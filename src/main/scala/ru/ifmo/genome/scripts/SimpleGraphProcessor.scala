package ru.ifmo.genome.scripts

import java.io._
import ru.ifmo.genome.data._
import scala.App
import ru.ifmo.genome.ds.Graph

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object SimpleGraphProcessor extends App {
  val (logger, formatter) = ZeroLoggerFactory.newLogger(SimpleGraphProcessor)
  import formatter._

  logger.info("Started")

  val infile = new File(args(0))
  val outfile = new File(args(1))

  val data = PairedEndData(infile)
  val k = 23
  val rounds = 3

  val kmersFreq = FreqFilter.extractFilteredKmers(data, k, rounds)

  val kmers = kmersFreq.keySet
  logger.info("Good reads count: " + kmers.size)

  val graph = Graph.buildGraph(k, kmers)
  val components = graph.components

  val hist = components.groupBy(_.size).map(p => (p._1, p._2.size)).toSeq.sortBy(_._1)
  logger.info("Components histogram: " + hist)

  val maxComponent = components.maxBy(_.size)
  graph.retain(maxComponent)

  //    val largestComponent
}
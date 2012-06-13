package ru.ifmo.genome.scripts

import java.io._
import scala.App
import ru.ifmo.genome.data._
import graph.{MapGraph, Graph}
import akka.kernel.Bootable
import akka.event.Logging

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object GraphBuilder extends Bootable {

  def startup() {
    val logger = Logging(ActorsHome.system, getClass.getSimpleName)
  
    logger.info("Started")

    val config = ActorsHome.system.settings.config

    val infile = new File(config.getString("genome.inputFile"))
    val outfile = new File(config.getString("genome.outputFile"))
  
    val data = PairedEndData(infile)
    val k = 19.toByte
    val rounds = 3
  
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
  
    ActorsHome.system.shutdown()
  }

  def shutdown() {}
  
  def main(args: Array[String]) {startup()}
}
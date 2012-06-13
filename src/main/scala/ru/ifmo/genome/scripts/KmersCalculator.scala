package ru.ifmo.genome.scripts

import java.io._
import java.nio.ByteBuffer
import ru.ifmo.genome.data.PairedEndData
import ru.ifmo.genome.dna._
import ru.ifmo.genome.util.ConsoleProgress
import sun.nio.ch.ChannelInputStream
import io.Source
import akka.event.Logging

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object KmersCalculator extends App {
  val logger = Logging(ActorsHome.system, getClass.getSimpleName)

  val infile = new File(args(0))

  val k = 19

  val in = Source.fromFile(infile)
  val sb = in.getLines().drop(1).flatMap(_.toCharArray).toSeq
  logger.info("Length: " + sb.length)
  val seq = DNASeq(sb.map(Base.fromChar): _*)
  logger.info("Kmers: " + seq.sliding(k).toSet.size)
}
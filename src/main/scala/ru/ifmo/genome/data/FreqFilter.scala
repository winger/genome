package ru.ifmo.genome.data

import ru.ifmo.genome.dna.DNASeq
import ru.ifmo.genome.util.ConsoleProgress
import ru.ifmo.genome.ds.{DNAMap, PartitionedDNAMap}
import ru.ifmo.genome.scripts.ActorsHome._
import akka.event.Logging
import ru.ifmo.genome.scripts.ActorsHome

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object FreqFilter {
  val logger = Logging(ActorsHome.system, getClass.getSimpleName)

  val chunkSize = 1024

  def extractFilteredKmers(data: PairedEndData, k: Byte, rounds: Int) = {
    val kmersFreq: DNAMap[Int] = new PartitionedDNAMap[Int](k)

    def add(seq: DNASeq) {
      if (seq.length >= k) {
        for (x <- seq.sliding(k)) {
          val rcx = x.revComplement
          val y = if (x.hashCode < rcx.hashCode) x else rcx
          kmersFreq.update(y, 1, _ + 1)
        }
      }
    }

    val progress = new ConsoleProgress("kmers", 80)

    val max = 0

    var count = 0
    for (chunk <- data.getPairs.take(max).grouped(chunkSize)) {
      for ((p1, p2) <- chunk.par) {
        add(p1)
        add(p2)
      }
      count += chunk.size
      progress(count.toDouble / (data.count min max))
    }

    progress.done()

    kmersFreq.deleteAll((k, v) => v < rounds)

    kmersFreq
  }

  //  val hist = collection.mutable.Map[Int, Int]()
  //  for ((_, count) <- readsFreq) {
  //    hist(count) = hist.getOrElse(count, 0) + 1
  //  }
  //  logger.info("Reads count histogram: " + hist.toSeq.sortBy(_._1))
}
package ru.ifmo.genome.data

import ru.ifmo.genome.dna.DNASeq
import ru.ifmo.genome.util.ConsoleProgress
import ru.ifmo.genome.scripts.ActorsHome._
import akka.event.Logging
import ru.ifmo.genome.scripts.ActorsHome
import akka.dispatch.{Future, Promise, Await}
import ru.ifmo.genome.ds.{Messages, DNAMap, PartitionedDNAMap}
import akka.pattern.ask
import akka.util.{Timeout, Duration}
import akka.util.duration._
import akka.remote.RemoteScope
import akka.actor.{AddressFromURIString, Deploy, Props, Actor}

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object FreqFilter {
  val logger = Logging(ActorsHome.system, getClass.getSimpleName)

  val chunkSize = ActorsHome.chunkSize

  def extractFilteredKmers(data: PairedEndData, k: Byte, rounds: Int) = {
    val kmersFreq: DNAMap[Int] = new PartitionedDNAMap[Int](k)

    def add(seq: DNASeq) {
      if (seq.length >= k) {
        for (x <- seq.sliding(k).toSeq) {
          val rcx = x.revComplement
          val y = if (x.hashCode < rcx.hashCode) x else rcx
          kmersFreq.update(y, 1, _ + 1)
        }
      }
    }

    val progress = new ConsoleProgress("kmers", 80)

    val max = ActorsHome.conf.root.toConfig.getInt("genome.takeFirst")

    var count = 0

    for (chunk <- data.getPairs.take(max).grouped(chunkSize)) {
      for ((p1, p2) <- chunk) {
        add(p1)
        add(p2)
      }
      count += chunk.size
      progress(count.toDouble / (data.count min max))
    }

    progress.done()

    Await.ready(kmersFreq.deleteAll((k, v) => v < rounds), Duration.Inf)
    
    kmersFreq
  }

  //  val hist = collection.mutable.Map[Int, Int]()
  //  for ((_, count) <- readsFreq) {
  //    hist(count) = hist.getOrElse(count, 0) + 1
  //  }
  //  logger.info("Reads count histogram: " + hist.toSeq.sortBy(_._1))
}
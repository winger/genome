package ru.ifmo.genome.data

import ru.ifmo.genome.ds.BloomFilter
import ru.ifmo.genome.dna.DNASeq
import ru.ifmo.genome.util.ConsoleProgress
import collection.mutable.ConcurrentMap
import java.util.concurrent.ConcurrentHashMap

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object FreqFilter {
  val (logger, formatter) = ZeroLoggerFactory.newLogger(FreqFilter)

  import formatter._

  val chunkSize = 1024

  def extractFilteredKmers(data: PairedEndData, k: Int, rounds: Int) = {
    val filter = {
      val filters = Array.fill(rounds)(new BloomFilter[DNASeq](60000000, 1e-1))

      var cc = 0
      
      def add(seq: DNASeq) {
        if (seq.length >= k) {
          for (x <- seq.sliding(k)) {
            val rcx = x.revComplement
            val y = if (x.hashCode < rcx.hashCode) x else rcx
            var index = 0
            while (index < rounds && filters(index).add(y)) {
              index += 1
            }
          }
        }
      }

      val progress = new ConsoleProgress("filter", 80)

      var count = 0
      for (chunk <- data.getPairs.grouped(chunkSize)) {
        for ((p1, p2) <- chunk.par) {
          if (p1.length >= k && p2.length >= k) {
            cc += 1
          }
          add(p1)
          add(p2)
        }
        count += chunk.size
        progress(count.toDouble / data.count)
      }

      progress.done()
      
      logger.info("CC: " + cc)

      filters(rounds - 1)
    }

    import collection.JavaConversions._
    val kmersFreq: ConcurrentMap[DNASeq, Int] = new ConcurrentHashMap[DNASeq, Int]

    def add(seq: DNASeq) {
      if (seq.length >= k) {
        for (x <- seq.sliding(k)) {
          val rcx = x.revComplement
          val y = if (x.hashCode < rcx.hashCode) x else rcx
          if (filter.contains(y)) {
            var done = false
            if (!kmersFreq.contains(y)) {
              done |= kmersFreq.putIfAbsent(y, 1).isEmpty
            }
            while (!done) {
              val v = kmersFreq(y)
              done |= kmersFreq.replace(y, v, v + 1)
            }
          }
        }
      }
    }

    val progress = new ConsoleProgress("kmers", 80)

    var count = 0
    for (chunk <- data.getPairs.grouped(chunkSize)) {
      for ((p1, p2) <- chunk.par) {
        add(p1)
        add(p2)
      }
      count += chunk.size
      progress(count.toDouble / data.count)
    }

    progress.done()

    logger.info("Filter false-positives: " + kmersFreq.count(_._2 < rounds))

    kmersFreq.filter(_._2 >= rounds)
  }

  //  val hist = collection.mutable.Map[Int, Int]()
  //  for ((_, count) <- readsFreq) {
  //    hist(count) = hist.getOrElse(count, 0) + 1
  //  }
  //  logger.info("Reads count histogram: " + hist.toSeq.sortBy(_._1))
}
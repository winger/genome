package ru.ifmo.genome.scripts

import java.io._
import java.nio.ByteBuffer
import ru.ifmo.genome.data.PairedEndData
import ru.ifmo.genome.dna._
import ru.ifmo.genome.util.ConsoleProgress
import sun.nio.ch.ChannelInputStream

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object Convert2bin extends App {
  val (logger, formatter) = ZeroLoggerFactory.newLogger(Convert2bin)
  import formatter._

  val infile = new File(args(0))
  val outfile = new File(args(1) + ".bin")

  val insert = 200
  val n = 36
  val k = 23

  {
    val inCh = new FileInputStream(infile).getChannel
    val in = new BufferedReader(new InputStreamReader(new ChannelInputStream(inCh)))
    val out = new FileOutputStream(outfile).getChannel

    val progress = new ConsoleProgress("convert to binary", 80)

    var kmers = 0L
    var shortReads = 0L
    
    def write(seq: DNASeq) {
      out.write(ByteBuffer.wrap(Array(seq.length.toByte)))
      out.write(ByteBuffer.wrap(seq.toByteArray))
    }

    def filtered(withQ: Seq[(Char, Char)]): DNASeq = {
      val filtered = withQ.takeWhile(p => Base.fromChar.contains(p._1))
//      val (s1, s2) = filtered1.splitAt(k)
//      val filtered = s1 ++ s2.takeWhile(p => p._2 - '!' >= 20)
      val seq = DNASeq(filtered.map(p => Base.fromChar(p._1)): _*)
      if (filtered.length >= k) {
        kmers += filtered.length - k + 1
      }
      seq
    }

    def processRead(): Boolean = {
      if (in.readLine() == null) {
        false
      } else {
        val (line1, line2) = in.readLine().splitAt(n)
        in.readLine()
        val (quality1, quality2) = in.readLine().splitAt(n)
        val seq1 = filtered(line1 zip quality1)
        val seq2 = filtered(line2 zip quality2)
        if (seq1.length < k || seq2.length < k) {
          shortReads += 1
        }
        write(seq1)
        write(seq2)
        progress(inCh.position().toDouble / inCh.size())
        true
      }
    }

    var count = 0
    while (processRead()) {
      count += 1
    }
    progress.done()

    out.close()

    new PairedEndData(count, insert, outfile).write(new File(args(1)))

    logger.info("Kmers count: " + kmers)

    logger.info("Short reads count: " + shortReads)
  }


  logger.info("Result size: " + outfile.length)
}
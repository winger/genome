import collection.mutable.ArrayBuffer
import io.{Source, BufferedSource}
import java.io._
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.util.logging.Logger
import ru.ifmo.genome.data.PairedEndData
import ru.ifmo.genome.dna._
import ru.ifmo.genome.util.ConsoleProgress
import sun.java2d.pipe.BufferedTextPipe
import sun.nio.ch.ChannelInputStream

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object Convert2bin extends App {
  val log = Logger.getLogger("Convert2bin")

  val infile = new File(args(0))
  val outfile = new File(args(1) + ".bin")

  val insert = 200
  val n = 36
  val k = 25

  {
    val inCh = new FileInputStream(infile).getChannel
    val in = new BufferedReader(new InputStreamReader(new ChannelInputStream(inCh)))
    val out = new FileOutputStream(outfile).getChannel

    val progress = new ConsoleProgress("convert to binary", 80)

    var kmers = 0L

    def addFiltered(withQ: Seq[(Char, Char)]) {
      val filtered = withQ.takeWhile(p => Base.fromChar.contains(p._1) && p._2 - '!' >= 30)
      val seq = DNASeq(filtered.map(p => Base.fromChar(p._1)): _*)
      out.write(ByteBuffer.wrap(Array(seq.length.toByte)))
      out.write(ByteBuffer.wrap(seq.toByteArray))
      if (filtered.length >= k) {
        kmers += filtered.length - k + 1
      }
    }

    def processRead(): Boolean = {
      if (in.readLine() == null) {
        false
      } else {
        val (line1, line2) = in.readLine().splitAt(n)
        in.readLine()
        val (quality1, quality2) = in.readLine().splitAt(n)
        addFiltered(line1 zip quality1)
        addFiltered(line2 zip quality2)
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

    println(kmers)
  }


  println(outfile.length)
}
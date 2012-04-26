import collection.mutable.ArrayBuffer
import io.{Source, BufferedSource}
import java.io._
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.util.logging.Logger
import ru.ifmo.genome.data.PairedEndData
import ru.ifmo.genome.dna._
import ru.ifmo.genome.util.ConsoleProgress
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
  
  val badLines = ArrayBuffer[String]()

  {
    val inCh = new FileInputStream(infile).getChannel
    val in = new BufferedReader(new InputStreamReader(new ChannelInputStream(inCh)))
    val out = new FileOutputStream(outfile).getChannel

    val progress = new ConsoleProgress("convert to binary", 80)

    def processRead(): Boolean = {
      if (in.readLine() == null) {
        false
      } else {
        val line: String = in.readLine()
        val seq = line.map(Base.fromChar.get(_))
        if (seq.forall(_.isDefined)) {
          val read = SmallDNASeq(seq.flatten: _*)
          val (p1, p2) = read.splitAt(n)
          assert(p1.length == p2.length)
          assert(p1 == SmallDNASeq(p1.toByteArray, p1.length))
          out.write(ByteBuffer.wrap(p1.toByteArray))
          out.write(ByteBuffer.wrap(p2.toByteArray))
        } else {
          badLines += line
        }
        in.readLine()
        val quality = in.readLine()

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

    new PairedEndData(count, insert, n, outfile).write(new File(args(1)))
  }


  println(badLines.size + " " + outfile.length)
}
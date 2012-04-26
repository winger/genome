import collection.mutable.ArrayBuffer
import io.{Source, BufferedSource}
import java.io._
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.util.logging.Logger
import ru.ifmo.genome.dna._
import ru.ifmo.genome.util.ConsoleProgress
import sun.nio.ch.ChannelInputStream

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object HashChecker extends App {
  val infile = new File(args(0))
  val outfile = new File(args(1))

  val insert = 200
  val k = 32
  val n = 36

  val hash1 = {
    val inCh = new FileInputStream(infile).getChannel
    val in = new BufferedReader(new InputStreamReader(new ChannelInputStream(inCh)))

    val progress = new ConsoleProgress("hash1", 80)

    var hashes = ArrayBuffer[Int]()

    def processRead(): Boolean = {
      if (in.readLine() == null) {
        false
      } else {
        val line: String = in.readLine()
        val seq = line.map(Base.fromChar.get(_))
        if (seq.forall(_.isDefined)) {
          val read = SmallDNASeq(seq.flatten: _*)
          val (p1, p2) = read.splitAt(n)
          hashes += p1.hashCode
          hashes += p2.hashCode
        }
        in.readLine()
        val quality = in.readLine()

        progress(inCh.position().toDouble / inCh.size())
        true
      }
    }

    while (processRead()) {}
    progress.done()

    hashes
  }

  val hash2 = {
    val inCh = new FileInputStream(outfile).getChannel

    val progress = new ConsoleProgress("hash2", 80)

    var hashes = ArrayBuffer[Int]()

    def readBytes(count: Int): Array[Byte] = {
      var bb = ByteBuffer.allocate(count)
      var bytes = 0
      while (bytes < count) {
        val c = inCh.read(bb)
        if (c == -1) {
          bb = null
          bytes = count
        } else {
          bytes += c
        }
      }
      if (bb == null) {
        null
      } else {
        bb.array()
      }
    }

    def processRead(): Boolean = {
      val str = readBytes((n + 3) / 4)
      if (str == null) {
        false
      } else {
        val seq: SmallDNASeq = SmallDNASeq(str, n)
        hashes += seq.hashCode
        progress(inCh.position().toDouble / inCh.size())
        true
      }
    }

    while (processRead()) {}
    progress.done()

    hashes
  }

  assert(hash1 == hash2)
}
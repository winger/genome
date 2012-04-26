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

object SimpleGraphProcessor extends App {
  val infile = new File(args(0))
  val outfile = new File(args(1))

  val insert = 200
  val k = 32
  val n = 36
}
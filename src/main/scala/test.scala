import collection.mutable.ArrayBuffer
import java.io._
import java.util.logging.Logger
import ru.ifmo.genome.dna._

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object Test extends App {
  val log = Logger.getLogger("Test")

  val file = new File(args(0))
  val in = new BufferedReader(new FileReader(file))
  val insert = 200
  val k = 32
  val n = 36
  
  val badLines = ArrayBuffer[String]()

  def processRead(): Boolean = {
    if (in.readLine() != null) {
      val line: String = in.readLine()
      val seq = line.map(Base.fromChar.get(_))
      if (seq.forall(_.isDefined)) {
        val read = SmallDNASeq(seq.flatten: _*)
        //println(read)
      } else {
        badLines += line
      }
      in.readLine()
      val quality = in.readLine()
      true
    } else {
      false
    }
  }
  
  while (processRead()) {}
  
  println(badLines.size)
}
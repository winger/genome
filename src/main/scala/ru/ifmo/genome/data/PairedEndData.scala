package ru.ifmo.genome.data

import java.io._
import ru.ifmo.genome.dna.DNASeq

/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

@SerialVersionUID(1L)
class PairedEndData(val count: Long, val insert: Int, val bin: File) extends Serializable {

  def write(f: File) {
    val stream = new ObjectOutputStream(new FileOutputStream(f))
    stream.writeObject(this)
    stream.close()
  }

  def getPairs = {
    val in = new FileInputStream(bin)
    var buf = Array.ofDim[Byte](32)

    def read() = {
      val len = in.read()
      val byteLen = (len + 3) / 4
      while (buf.length < byteLen) {
        buf = Array.ofDim(buf.length * 2)
      }
      in.read(buf, 0, byteLen)
      DNASeq(buf.take(byteLen), len)
    }

    for (i <- 0L until count)
      yield (read(), read())
  }
}

object PairedEndData {
  def apply(f: File): PairedEndData =
    new ObjectInputStream(new FileInputStream(f)).readObject().asInstanceOf[PairedEndData]
}
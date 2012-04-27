package ru.ifmo.genome.data

import java.io._
import ru.ifmo.genome.dna.SmallDNASeq

/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

class PairedEndData(val count: Long, val insert: Int, val len: Int, val bin: File) extends Serializable {
  val serialVersionUID = 1;

  def write(f: File) {
    val stream = new ObjectOutputStream(new FileOutputStream(f))
    stream.writeObject(this)
    stream.close()
  }

  def getPairs = {
    val in = new FileInputStream(bin)
    val buf = new Array[Byte]((len + 3) / 4)

    def read() = {
      in.read(buf)
      SmallDNASeq(buf, len)
    }

    for (i <- 0L until count)
      yield (read(), read())
  }
}

object PairedEndData {
  def apply(f: File): PairedEndData =
    new ObjectInputStream(new FileInputStream(f)).readObject().asInstanceOf[PairedEndData]
}
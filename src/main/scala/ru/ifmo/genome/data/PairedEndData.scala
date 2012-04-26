package ru.ifmo.genome.data

import java.io._

/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

class PairedEndData(val count: Long, val insert: Int, val len: Int, val bin: File) extends Serializable {
  def write(f: File) {
    val stream = new ObjectOutputStream(new FileOutputStream(f))
    stream.writeObject(this)
    stream.close()
  }
}

object PairedEndData {
  def apply(f: File): PairedEndData =
    new ObjectInputStream(new FileInputStream(f)).readObject().asInstanceOf[PairedEndData]
}
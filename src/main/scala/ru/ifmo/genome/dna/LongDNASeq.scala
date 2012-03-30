package ru.ifmo.genome.dna

import collection.mutable.IndexedSeqLike
import java.nio.ByteBuffer

/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

class LongDNASeq private(data: ByteBuffer, val len: Long) extends IndexedSeq[Base] with IndexedSeqLike[Base, SmallDNASeq] {

}

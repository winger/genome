package ru.ifmo.genome.ds

import util.Random
import collection.mutable.BitSet

/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

trait MultiHash {
  def hashCode(seed: Int): Int
  override def hashCode = hashCode(42)
}

class BloomFilter[T <: MultiHash](val n: Long, p: Double) {
  import BloomFilter._

  private val (k, m) = calcParams(n, p)
  println(k, m)
  private var count = 0
  private val set = new BitSet(m)

  def add(seq: T) {
    count += 1
    assert(count <= n)
    hashes_(seq).foreach(set += _)
  }

  def contains(seq: T) = hashes_(seq).forall(set)

  private def hashes_(seq: T) = hashes(seq, k).map(_ & (m - 1))
}

object BloomFilter {
  def calcParams(n: Long, p: Double) = {
    val ln2 = math.log(2)
    val m0 = math.round(-n * math.log(p) / ln2 / ln2)
    val m = Iterator.iterate(1)(2 * _).find(_ >= m0).get
    val k = math.round(m.toDouble / n * ln2).toInt
    (k, m)
  }

  val seeds = {
    val rnd = new Random(42)
    Stream.continually(rnd.nextInt())
  }

  def hashes(a: MultiHash, k: Int): Seq[Int] = {
    seeds.take(k).map(a.hashCode(_))
  }
}
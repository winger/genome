package ru.ifmo.genome.ds

import util.Random
import collection.mutable.BitSet
import java.util.concurrent.atomic.AtomicInteger

/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

trait MultiHash {
  def multiHashCode(seed: Int): Int
  override def hashCode = multiHashCode(42)
}

class BloomFilter[T <: MultiHash](val n: Long, p: Double) {
  import BloomFilter._

  private val (k, m) = calcParams(n, p)
  private val count = new AtomicInteger()
  private val set = new BitSet(m)
  
  private val mySeeds = Array[Int](seeds.take(k): _*)
  
  private val locks = Array.fill[AnyRef](32)(new AnyRef)

  def add(seq: T): Boolean = {
    locks(seq.hashCode & (locks.size - 1)).synchronized {
      var add = false
      hashes_(seq).foreach { i =>
        add |= !set(i)
        set += i
      }
      if (add) {
        assert(count.incrementAndGet() <= n)
      }
      add
    }
  }

  def contains(seq: T) = hashes_(seq).forall(set)

  private def hashes_(seq: T) = hashes(seq, mySeeds).map(improve(_) & (m - 1))
}

object BloomFilter {
  def calcParams(n: Long, p: Double) = {
    val ln2 = math.log(2)
    val m0 = math.round(-n * math.log(p) / ln2 / ln2)
    val m = Iterator.iterate(1)(2 * _).find(_ >= m0).get
    val k = math.round(m.toDouble / n * ln2).toInt
    (k, m)
  }

  def improve(hcode: Int) = {
    //from hashtable
    var i = hcode * 0x9e3775cd
    i = java.lang.Integer.reverseBytes(i)
    i * 0x9e3775cd
  }

  val seeds = {
    val rnd = new Random(42)
    Stream.continually(rnd.nextInt())
  }

  def hashes(a: MultiHash, seeds: Array[Int]): Seq[Int] = {
    seeds.map(a.multiHashCode(_))
  }
}
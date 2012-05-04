package ru.ifmo.genome.ds

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicInteger
import ru.ifmo.genome.dna.DNASeq
import collection.mutable.{ArrayBuilder, BitSet}


/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

class DNAMap[T](k: Byte, n: Int)(implicit mf: ClassManifest[T]) {
  import DNAMap._

  val maxSize = minPow2(n  / maxLoadFactor)
  val mask = maxSize - 1
  val sizeOfK = sizeOf(k)
  val count = new AtomicInteger(0)
  val keys: ByteBuffer = ByteBuffer.allocateDirect(sizeOfK * maxSize)
  val ar = new Array[T](maxSize)
  val set = new BitSet(maxSize)

  //TODO make thread-safe
//  val lockSegments = Array.fill(64)(new AnyRef())

  def get(key: DNASeq): Option[T] = {
    assert(key.length == k)
    var i = improve(key.hashCode) & mask
    var ans: Option[T] = None
    while (ans.isEmpty && set(i)) {
      if (DNASeq.read(keys, i * sizeOfK, k) == key) {
        ans = Some(ar(i))
      } else {
        i = (i + 1) & mask
      }
    }
    ans
  }

  def getAll(key: DNASeq): Iterable[T] = {
    assert(key.length == k)
    var ans: List[T] = Nil
    var i = improve(key.hashCode) & mask
    while (set(i)) {
      if (DNASeq.read(keys, i * sizeOfK, k) == key) {
        ans ::= ar(i)
      }
      i = (i + 1) & mask
    }
    ans
  }
  
  def put(key: DNASeq, v: T) {
    assert(count.incrementAndGet() <= maxSize * maxLoadFactor)
    var i = improve(key.hashCode) & mask
    while (set(i) && DNASeq.read(keys, i * sizeOfK, k) != key) {
      i = (i + 1) & mask
    }
    if (!set(i)) {
      set += i
      key.write(keys, i * sizeOfK)
    }
    ar(i) = v
  }

  def putNew(key: DNASeq, v: T) {
    assert(count.incrementAndGet() <= maxSize * maxLoadFactor)
    var i = improve(key.hashCode) & mask
    while (set(i)) {
      i = (i + 1) & mask
    }
    set += i
    key.write(keys, i * sizeOfK)
    ar(i) = v
  }
  
  def contains(key: DNASeq) = get(key).isDefined
  
  def size = count.get

}

object DNAMap {
  val maxLoadFactor = 0.5
  
  def sizeOf(k: Byte) = {
    if (k <= 32) {
      8
    } else if (k <= 64) {
      16
    } else {
      (k + 3) / 4
    }
  }
  
  def minPow2(x: Double) = {
    var n = 1
    while (n < x) {
      n *= 2
    }
    n
  }

  private[DNAMap] def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }
}

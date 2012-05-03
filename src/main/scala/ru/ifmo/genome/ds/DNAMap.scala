package ru.ifmo.genome.ds

import java.nio.{ByteBuffer, LongBuffer}
import java.util.concurrent.atomic.AtomicInteger
import ru.ifmo.genome.dna.DNASeq
import collection.mutable.BitSet


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
    var i = key.hashCode & mask
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
  
  def put(key: DNASeq, v: T) {
    assert(count.incrementAndGet() <= maxSize * maxLoadFactor)
    var i = key.hashCode & mask
    while (set(i) && DNASeq.read(keys, i * sizeOfK, k) != key) {
      i = (i + 1) & mask
    }
    if (!set(i)) {
      set += i
      key.write(keys, i * sizeOfK)
    }
    ar(i) = v
  }
  
  def contains(key: DNASeq) = get(key).isDefined
  
  def size = count.get

}

object DNAMap {
  val maxLoadFactor = 0.5
  
  def sizeOf(k: Byte) = {
    if (k <= 32) {
      4
    } else if (k <= 64) {
      8
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
}
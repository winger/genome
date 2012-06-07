package ru.ifmo.genome.ds

import ru.ifmo.genome.dna.DNASeq
import akka.actor.{TypedProps, TypedActor, ActorSystem}
import akka.dispatch.Future
import sys.Prop.Creator

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

class PartitionedDNAMap[T](k: Byte, partitionsCount: Int)(implicit mf: Manifest[T], as: ActorSystem) extends DNAMap[T] {
  assert((partitionsCount & (partitionsCount - 1)) == 0)

  val partitions: Array[DNAMap[T]] =
    Array.fill(partitionsCount)(TypedActor(as).typedActorOf(TypedProps[DNAMap[T]](classOf[DNAMap[T]], new ArrayDNAMap[T](k))))
//  val partitions: Array[DNAMap[T]] = Array.fill(partitionsCount)(new ArrayDNAMap[T](k))

  def size = partitions.map(_.size).sum

  def apply(key: DNASeq) = partition(key).apply(key)

  def getAll(key: DNASeq) = partition(key).getAll(key)

  def update(key: DNASeq, v: T) {
    partition(key).update(key, v)
  }

  def update(key: DNASeq, v0: T, f: (T) => T) {
    partition(key).update(key, v0, f)
  }

  def putNew(key: DNASeq, v: T) {
    partition(key).putNew(key, v)
  }
  
  def deleteAll(p: (DNASeq, T) => Boolean) {
    partitions.foreach(_.deleteAll(p))
  }

  def contains(key: DNASeq) = partition(key).contains(key)

  def foreach(f: ((DNASeq, T)) => Unit) = Future.traverse(partitions.toList)(_.foreach(f)).map(_ => ())
  
  private def partition(key: DNASeq): DNAMap[T] = {
    partitions(key.hashCode & (partitionsCount - 1))
  }
}
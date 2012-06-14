package ru.ifmo.genome.ds

import ru.ifmo.genome.dna.DNASeq
import akka.dispatch.Future
import akka.remote.RemoteScope
import akka.actor._
import akka.japi.Creator

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

class PartitionedDNAMap[T](k: Byte)(implicit mf: Manifest[T]) extends DNAMap[T] with Serializable {
  import ru.ifmo.genome.scripts.ActorsHome.system

  import collection.JavaConverters._
  val partitions: Array[DNAMap[T]] = {
    val nodes = system.settings.config.getStringList("genome.storageNodes").asScala.map(AddressFromURIString(_))

    val constructor = new Creator[DNAMap[T]] with Serializable {
      def create() = new ArrayDNAMap[T](k)
    }

    nodes.map { address =>
      val props = new TypedProps[DNAMap[T]](classOf[DNAMap[T]], constructor) {
        override def actorProps() = super.actorProps().withDeploy(Deploy(scope = RemoteScope(address)))
      }
      TypedActor(system).typedActorOf(props)
    }
  }.toArray
//  val partitions: Array[DNAMap[T]] = Array.fill(partitionsCount)(new ArrayDNAMap[T](k))

  def size = Future.traverse(partitions.toList)(_.size).map(_.sum)

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
  
  def deleteAll(p: (DNASeq, T) => Boolean): Future[Unit] = {
    Future.traverse(partitions.toList)(_.deleteAll(p)).map(_ => ())
  }

  def contains(key: DNASeq) = partition(key).contains(key)

  def mapReduce[T1, T2](map: ((DNASeq, T)) => Option[T1], reduce: Seq[T1] => T2) = {
    Future.traverse(partitions.toList)(_.mapReduce(map, identity[Seq[T1]])).map(list => reduce(list.flatten))
  }
  
  private def partition(key: DNASeq): DNAMap[T] = {
    def fix(i: Int) = if (i < 0) i + partitions.size else i
    partitions(fix(key.hashCode % partitions.size))
  }
}
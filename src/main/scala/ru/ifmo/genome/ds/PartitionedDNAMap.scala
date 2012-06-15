package ru.ifmo.genome.ds

import ru.ifmo.genome.dna.DNASeq
import akka.dispatch.Future
import akka.remote.RemoteScope
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._
import java.util.concurrent.Semaphore

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

class PartitionedDNAMap[T](k: Byte)(implicit mf: Manifest[T]) extends DNAMap[T] with Serializable {
  import ru.ifmo.genome.scripts.ActorsHome.system

  import collection.JavaConverters._

  implicit val timeout = Timeout(1000000 seconds)

  val partitions: Array[ActorRef] = {
    val nodes = system.settings.config.getStringList("genome.storageNodes").asScala.map(AddressFromURIString(_))

    nodes.map { address =>
      val props = Props(new ArrayDNAMap[T](k) with DNAMapActor[T]).withDeploy(Deploy(scope = RemoteScope(address)))
      system.actorOf(props)
    }
  }.toArray
//  val partitions: Array[DNAMap[T]] = Array.fill(partitionsCount)(new ArrayDNAMap[T](k))

  def size = Future.traverse(partitions.toList)(_ ? Messages.size).mapTo[List[Int]].map(_.sum)

  def apply(key: DNASeq) = (partition(key) ? Messages.apply(key)).mapTo[Option[T]]

  def getAll(key: DNASeq) = (partition(key) ? Messages.getAll(key)).mapTo[List[T]]

  def update(key: DNASeq, v: T) {
    partition(key) ! Messages.update(key, v)
  }

  def update(key: DNASeq, v0: T, f: (T) => T) {
    partition(key) ! Messages.update1(key, v0, f)
  }

  def putNew(key: DNASeq, v: T) {
    partition(key) ! Messages.putNew(key, v)
  }
  
  def deleteAll(p: (DNASeq, T) => Boolean): Future[Unit] = {
    Future.traverse(partitions.toList)(_ ? Messages.deleteAll(p)).map(_ => ())
  }

  def contains(key: DNASeq) = (partition(key) ? Messages.contains(key)).mapTo[Boolean]

  def mapReduce[T1, T2](map: ((DNASeq, T)) => Option[T1], reduce: Seq[T1] => T2) = {
    Future.traverse(partitions.toList)(_ ? Messages.mapReduce(map, identity[Seq[T1]]))
      .map(_.asInstanceOf[List[Seq[T1]]]).map(list => reduce(list.flatten))
  }
  
  private def partition(key: DNASeq): ActorRef = {
    def fix(i: Int) = if (i < 0) i + partitions.size else i
    partitions(fix(key.hashCode % partitions.size))
  }
}
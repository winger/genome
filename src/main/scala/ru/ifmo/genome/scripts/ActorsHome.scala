package ru.ifmo.genome.scripts

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import akka.kernel.Bootable
import akka.event.Logging

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

class ActorsHome extends Bootable {
  def startup() {
    ActorsHome.system
  }

  def shutdown() {}
}

object ActorsHome {
  val conf: Config = ConfigFactory.load()
  implicit val system = ActorSystem()

  val logger = Logging(system, getClass.getSimpleName)
  logger.info("" + (conf.root.get("genome")))

  val chunkSize = conf.getInt("genome.chunkSize")

  def main(args: Array[String]) {}
}

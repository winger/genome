package ru.ifmo.genome.scripts

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object ActorsHome {
  val (logger, formatter) = ZeroLoggerFactory.newLogger(ActorsHome)
  import formatter._

  val conf = ConfigFactory.load("application.conf")
  implicit val system = ActorSystem("main", conf)
//  logger.info("" + system.settings);
}

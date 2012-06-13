import sbt._
import Keys._
import akka.sbt.AkkaKernelPlugin
import akka.sbt.AkkaKernelPlugin.{Dist, outputDirectory, distJvmOptions}

object PegasBuild extends Build {

  lazy val Pegas = Project(
    id = "pegas",
    base = file("."),
    settings = (defaultSettings ++ AkkaKernelPlugin.distSettings ++ Seq(
      distJvmOptions in Dist := "-Xmx1G",
      outputDirectory in Dist := file("target/dist"),
      exportJars := true
    )).asInstanceOf[Seq[Project.Setting[_]]]
  )

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "ru.ifmo",
    version := "0.0.1",
    scalaVersion := "2.9.1",
    crossPaths := false
  )

  lazy val AkkaVersion = "2.1-SNAPSHOT"

  lazy val defaultSettings = buildSettings ++ Seq(
    resolvers ++= Seq(
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/snapshots/",
      "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
      "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
      "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
    ),
    libraryDependencies ++= Seq(
      "com.typesafe.akka" % "akka-kernel" % AkkaVersion,
      "com.typesafe.akka" % "akka-remote" % AkkaVersion,
      "com.typesafe.akka" % "akka-slf4j" % AkkaVersion,
      "ch.qos.logback" % "logback-classic" % "1.0.0",
      "com.romix.akka" % "akka-kryo-serialization" % "0.1-SNAPSHOT",
      "com.esotericsoftware.kryo" % "kryo" % "2.14-SNAPSHOT"
//      , "org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT"
    ),
    // compile options
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
    javacOptions  ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")
  )
}
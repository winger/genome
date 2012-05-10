import sbt._
import Keys._
import akka.sbt.AkkaKernelPlugin
import akka.sbt.AkkaKernelPlugin.{Dist, outputDirectory, distJvmOptions}

object PegasBuild extends Build {

  lazy val Pegas = Project(
    id = "pegas",
    base = file("."),
    settings = defaultSettings ++ AkkaKernelPlugin.distSettings ++ Seq(
      distJvmOptions in Dist := "-Xmx1G",
      outputDirectory in Dist := file("target/dist")
    )
  )

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "ru.ifmo",
    version := "0.0.1",
    scalaVersion := "2.9.1",
    crossPaths := false
  )

  lazy val AkkaVersion = "2.0.1"

  lazy val defaultSettings = buildSettings ++ Seq(
    resolvers ++= Seq(
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
      "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
      "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
    ),
    libraryDependencies ++= Seq(
      "com.typesafe.akka" % "akka-kernel" % AkkaVersion,
      "com.typesafe.akka" % "akka-slf4j" % AkkaVersion,
      "ch.qos.logback" % "logback-classic" % "1.0.0",
      "com.dongxiguo" %% "zero-log" % "0.1.1",
      "com.jsuereth" %% "scala-arm" % "1.2",
      "javax.transaction" % "jta" % "1.0.1B" % "provided->default",
      "org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT"
    ),
    // compile options
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
    javacOptions  ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")
  )
}
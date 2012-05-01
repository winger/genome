name := "master"

version := "1.0"

scalaVersion := "2.9.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

resolvers += "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0"

libraryDependencies += "com.dongxiguo" %% "zero-log" % "0.1.1"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.2"

libraryDependencies += "javax.transaction" % "jta" % "1.0.1B" % "provided->default"

libraryDependencies += "org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT"

libraryDependencies += "com.googlecode" % "kryo" % "1.04"
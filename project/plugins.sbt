resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")

addSbtPlugin("com.typesafe.akka" % "akka-sbt-plugin" % "2.1-SNAPSHOT")
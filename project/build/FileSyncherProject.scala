import sbt._

class FileSyncherProject(info: ProjectInfo) extends DefaultProject(info) {
  val scala_arm     = "com.github.jsuereth.scala-arm" % "scala-arm_2.8.1" % "0.2"
  val scalaiocore   = "com.github.scala-incubator.io" %% "core" % "0.1.1"
  val scalaiofile   = "com.github.scala-incubator.io" %% "file" % "0.1.1"
}

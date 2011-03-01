import sbt._

class FileSyncherProject(info: ProjectInfo) extends DefaultProject(info) {
  val scala_arm = "com.github.jsuereth.scala-arm" % "scala-arm_2.8.1" % "0.2"
}

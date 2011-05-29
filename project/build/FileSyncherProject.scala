import sbt._

class FileSyncherProject(info: ProjectInfo) extends DefaultProject(info) {

  val scalaTest = "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "test"
  val mockito   = "org.mockito" % "mockito-all" % "1.8.5" % "test"
}

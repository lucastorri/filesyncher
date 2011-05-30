import sbt._

class FileSyncherProject(info: ProjectInfo) extends DefaultProject(info) with AkkaProject with IdeaProject {

  val scalaIO = "com.github.scala-incubator.io" % "core_2.9.0" % "0.1.2"

  val scalaTest = "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "test"
  val mockito   = "org.mockito" % "mockito-all" % "1.8.5" % "test"

}

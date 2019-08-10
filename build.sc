import mill._, scalalib._, scalajslib._
import ammonite.ops._


object shared extends Module{
  object jvm extends ScalaModule{
    def scalaVersion = "2.12.8"
    def millSourcePath = super.millSourcePath / up
    def ivyDeps = Agg(
      ivy"com.lihaoyi::fastparse:2.1.3",
      ivy"com.lihaoyi::scalatags::0.7.0"
    )
  }
  object js extends ScalaJSModule{
    def scalaVersion = "2.12.8"
    def scalaJSVersion = "0.6.28"
    def millSourcePath = super.millSourcePath / up
    def platformSegment = "js"
    def ivyDeps = Agg(
      ivy"com.lihaoyi::fastparse::2.1.3",
      ivy"com.lihaoyi::scalatags::0.7.0"
    )
  }
}

object server extends ScalaModule{
  def scalaVersion = "2.12.8"
  def moduleDeps = Seq(shared.jvm)
  object test extends Tests{
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.8")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

object client extends ScalaJSModule{
  def scalaVersion = "2.12.8"
  def scalaJSVersion = "0.6.28"
  def moduleDeps: Seq[ScalaJSModule] = Seq(shared.js)
  def platformSegment = "js"
  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::0.9.7",
    ivy"com.lihaoyi::scalatags::0.7.0"
  )
}



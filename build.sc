import mill._, scalalib._, scalajslib._
import ammonite.ops._

object partex extends Module{
  object shared extends Module{
    object jvm extends ScalaModule{
      def scalaVersion = "2.12.8"
      def millSourcePath = super.millSourcePath / up
      def ivyDeps = Agg(
        ivy"com.lihaoyi::fastparse:2.1.0",
        ivy"com.lihaoyi::scalatags::0.6.7"
      )
    }
    object js extends ScalaJSModule{
      def scalaVersion = "2.12.8"
      def scalaJSVersion = "0.6.27"
      def millSourcePath = super.millSourcePath / up
      def platformSegment = "js"
      def ivyDeps = Agg(
        ivy"com.lihaoyi::fastparse::2.1.0",
        ivy"com.lihaoyi::scalatags::0.6.7"
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
    def scalaJSVersion = "0.6.27"
    def moduleDeps: Seq[ScalaJSModule] = Seq(shared.js)
    def platformSegment = "js"
    def ivyDeps = Agg(
      ivy"org.scala-js::scalajs-dom::0.9.6",
      ivy"com.lihaoyi::scalatags::0.6.7"
    )
  }

}

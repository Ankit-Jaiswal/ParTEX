import mill._, scalalib._

object partex extends ScalaModule{
  def scalaVersion="2.12.6"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse::1.0.0",
    ivy"com.lihaoyi::scalatags::0.6.7"
  )
}

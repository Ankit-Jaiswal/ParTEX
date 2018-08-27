import mill._, scalalib._

object partex extends SbtModule{
  def scalaVersion="2.12.6"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse::1.0.0"
  )
}

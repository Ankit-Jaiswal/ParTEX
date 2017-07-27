package partex

import fastparse.all._

object detex {
  val ws = P(" "|"\n"|"\t")

  val document: P[Unit] = P((!beginDoc ~ AnyChar).rep ~ beginDoc ~ body.rep ~ endDoc)
  val beginDoc = P("\\begin{document}" ~ ws.rep)
  val endDoc = P("\\end{document}" ~ ws.rep)


  val body: P[Unit] = P(text | enclosure | !"\\end{" ~ command)

  val text: P[Unit] = P((!command ~ AnyChar).rep(1))

  val enclosure: P[Unit] = P(begin ~ body ~ end)
  val begin = P("\\begin{" ~ (!"}" ~ AnyChar).rep(1) ~ "}" ~ (!ws ~ AnyChar).rep ~ ws.rep )
  val end = P("\\end{" ~ (!"}" ~ AnyChar).rep(1) ~ "}" ~ (!ws ~ AnyChar).rep ~ ws.rep )

  val command: P[Unit] = P("\\" ~  (!"{" ~ AnyChar).rep(1) ~ cmdName ~ (!ws ~ AnyChar).rep ~ ws.rep )
  val cmdName = P("{" ~ (!"}" ~ AnyChar).rep ~ "}" )

}


/*
class ParTEX(filename: String) {
  val fullname = "../grad-school-notes/Algebra/" + filename
  val content = scala.io.Source.fromFile(fullname).getLines mkString "\n"
  val tree = detex.document.parse(content)
}
*/

object partex {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("../grad-school-notes/Algebra/group_theory.tex").getLines mkString "\n"
    println(detex.document.parse(input))
  }
}


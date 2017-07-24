package partex

import fastparse.all._

object detex {
  val ws = P(" "|"\n"|"\t")

  val document: P[Unit] = P((!"\\begin{document}" ~ AnyChar).rep ~ enclosure ~ AnyChar.rep ~ End)

  val body: P[Unit] = P(text | enclosure | !"\\end{" ~ command)

  val text: P[Unit] = P((!command ~ AnyChar).rep)

  val enclosure: P[Unit] = P(begin ~ body.rep ~ end)
  val begin = P("\\begin{" ~ (!"}" ~ AnyChar).rep ~ "}" ~ (!ws ~ AnyChar).rep ~ ws )
  val end = P("\\end{" ~ (!"}" ~ AnyChar).rep ~ "}" ~ (!ws ~ AnyChar).rep ~ ws )

  val command: P[Unit] = P("\\" ~  (!"{" ~ AnyChar).rep(1) ~ cmdName ~ (!ws ~ AnyChar).rep ~ ws )
  val cmdName = P("{" ~ (!"}" ~ AnyChar).rep ~ "}" )

}



case class ParTEX(filename: String) {
  val fullname = "../grad-school-notes/Algebra/" + filename
  val content = scala.io.Source.fromFile(fullname).getLines mkString "\n"
  val tree = detex.document.parse(content)
}

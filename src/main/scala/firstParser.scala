package ParTEX

import fastparse.all._

object latexParser {
  val command = P("\\" ~ (!(" "|"{"|"[") ~ AnyChar).rep)

  val beginDef = P("\\begin{" ~ ("defn*"|"defn") ~ "}")
  val endDef = P("\\end{" ~ ("defn*"|"defn") ~ "}")
  val defn = P((!beginDef ~ AnyChar).rep ~ beginDef ~ (!endDef ~ AnyChar).rep.! ~ endDef)

  val document = P(environ ~ text)
  val environ = Pass
  val text = P(defn.rep)
}


case class ParTEX(filename: String) {
  val fullname = "../grad-school-notes/Algebra/" + filename
  val input = scala.io.Source.fromFile(fullname).getLines mkString "\n"
  val tree = latexParser.document.parse(input)
}

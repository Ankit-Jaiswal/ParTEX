package partex

import fastparse.all._
import texLang._

object detex {
  val ws = P(" "|"\n"|"\t"|"\\:")
  val alpha = P(CharIn('a' to 'z'))

  val document: P[Document] = P( ((!beginDoc ~ AnyChar).rep ~ beginDoc ~ body ~ endDoc).
    map((b:Body) => Document(b)) )
  val beginDoc: P[Unit] = P("\\begin{document}" ~ ws.rep)
  val endDoc: P[Unit] = P("\\end{document}" ~ ws.rep)


  val body: P[Body] = P(bodyElem.rep.map((bs:Seq[BodyElem]) => Body(bs)) )
  val bodyElem: P[BodyElem] = P(text | enclosure | !"\\end{" ~ command)

  val text: P[Text] = P( (!command ~ (mathBlock|AnyChar.!)).rep(1).
    map(_.reduceLeft(_+_)).
    map((s:String) => Text(s)) )

  val mathBlock: P[String] = P(doubleDollar | singleDollar | roundBracket | sqBracket)
  val doubleDollar : P[String] = P("$$" ~ (!"$$" ~ AnyChar).rep(1).! ~ "$$")
  val singleDollar : P[String] = P("$" ~ (!"$" ~ AnyChar).rep(1).! ~ "$")
  val roundBracket: P[String] = P("\\(" ~ (!
    "\\)" ~ AnyChar).rep(1).! ~ "\\)")
  val sqBracket : P[String] = P("\\[" ~ (!"\\]" ~ AnyChar).rep(1).! ~ "\\]")

  val enclosure: P[Enclosure] = P( (begin ~ body ~ end).
    map((t:(String,Body)) => Enclosure(t._1,t._2)) )
  val begin: P[String] = P("\\begin" ~ cmdName ~ box.rep ~ (&("\\")|ws.rep) )
  val end: P[Unit] = P("\\end" ~ box.rep(1) ~ (&("\\")|ws.rep) )

  val command: P[Command] = P( ("\\" ~ (alpha|"*").rep(1).! ~ cmdName ~ box.rep ~ (&("\\")|ws.rep)).
    map((t:(String,String)) => Command(t._1,t._2)) )
  val cmdName: P[String] = P("{" ~ (!"}" ~ AnyChar).rep.! ~ "}" )

  val box: P[Unit] = P(curlyBox|sqBox)
  val curlyBox: P[Unit] = P("{" ~ (!"}" ~ AnyChar).rep ~ "}" )
  val sqBox: P[Unit] = P("[" ~ (!"]" ~ AnyChar).rep ~ "]" )

}


/*
class ParTEX(filename: String) {
  val fullname = "../grad-school-notes/Algebra/" + filename
  val content = scala.io.Source.fromFile(fullname).getLines mkString "\n"
  val tree = detex.document.parse(content)
}
*/

object partex {

  val input1 = scala.io.Source.fromFile("../grad-school-notes/Algebra/group_theory.tex").getLines mkString "\n"

  def main(args: Array[String]): Unit = {
    println(detex.document.parse(input2))
  }

  val input2 =
"""
\documentclass[master.tex]{subfiles}

%%%%%%%%%%%%%% BEGIN CONTENT: %%%%%%%%%%%%%%

\begin{document}
\begin{defn*}[1.3.6]
A sequence of homomorphisms
\[\ldots \to \ldots  G_{i-1} \xrightarrow{f_{i-1}}G_i \xrightarrow{f_i} G_{i+1} \to \ldots \to \ldots\]
is called \emph{exact} if \(\text{im}(f_{i-1})=\ker(f_i)\) for each \(i\). A sequence is a short exact sequence
(SES) if it is exact and takes the form:
\[1 \to N \xrightarrow{f_1} G \xrightarrow{f_2} Q \to 1.\]
\end{defn*}
\end{document}
"""

}

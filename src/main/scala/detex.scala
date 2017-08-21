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


  val body: P[Body] = P(bodyElem.rep.map(_.toVector).map((bs:Vector[BodyElem]) => Body(bs)) )

  val bodyElem: P[BodyElem] = P(text | enclosure | !"\\end{" ~ command)

  val text: P[Text] = P( (!command ~ reservedCmd.? ~ (mathBlock|AnyChar.!)).rep(1).
    map(_.reduceLeft(_+_)).
    map((s:String) => Text(s)) )

  val reservedCmd: P[Unit] = P("\\" ~ ("item"|"maketitle") )

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

  val command: P[Command] = P( ("\\" ~ (alpha| "*").rep(1).! ~ cmdName ~ box.rep ~ (&("\\")|ws.rep)).
    map((t:(String,String)) => Command(t._1,t._2)) )
  val cmdName: P[String] = P("{" ~ (!"}" ~ AnyChar).rep.! ~ "}" )

  val box: P[Unit] = P(curlyBox|sqBox)
  val curlyBox: P[Unit] = P("{" ~ (!"}" ~ AnyChar).rep ~ "}" )
  val sqBox: P[Unit] = P("[" ~ (!"]" ~ AnyChar).rep ~ "]" )

}


object samplesIO {

  val input1 = scala.io.Source.fromFile("../grad-school-notes/Algebra/group_theory.tex").mkString


  def main(args: Array[String]): Unit = {
    println(detex.document.parse(input2))
  }

  val input2 =
"""
\documentclass[master.tex]{subfiles}

%%%%%%%%%%%%%% BEGIN CONTENT: %%%%%%%%%%%%%%

\begin{document}
  This follows from the second part of the remark above.
  Now for some remakrs about centralizers.
  \begin{rmk*}[1.1.3]
    \begin{enumerate}
    \item If $A \subgroup G$, then $A$ is abelian if and only if $A \subset
      C_G(A)$.
    \item Furthermore, if $A \subgroup Z(G)$, then $A \normsubgroup G$.
      This follows using basic commutativity arguments.
    \end{enumerate}
  \end{rmk*}
\end{document}
"""

}

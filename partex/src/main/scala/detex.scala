package partex

import fastparse.all._
import TargetLang._

object DeTeX {
  val ws = P(" "|"\n"|"\t"|"\\:")
  val alpha = P(CharIn('a' to 'z'))

  val document: P[Document] = P( ((!beginDoc ~ AnyChar).rep ~ beginDoc ~ body ~ endDoc).
    map((b:Body) => Document(b)) )
  val beginDoc: P[Unit] = P("\\begin{document}" ~ ws.rep)
  val endDoc: P[Unit] = P("\\end{document}" ~ ws.rep)


  val body: P[Body] = P(bodyElem.rep.map(_.toVector).map((bs:Vector[BodyElem]) => Body(bs)) )

  val bodyElem: P[BodyElem] = P(text | enclosure | !"\\end{" ~ command)

  val text: P[Text] =
    P( (reserved.? ~ !command ~ comment.? ~ (wrapper|spSym|mathBlock|AnyChar.!)).rep(1).
    map(_.reduceLeft(_+_)).
    map((s:String) => Text(s)) )

  val comment: P[Unit] = P("%" ~ (!"\n" ~ AnyChar).rep ~ "\n")
  val reserved: P[Unit] = P(resvdWord|resvdCmd)
  val resvdWord: P[Unit] = P("\\" ~
    ("bigskip"|"break"|"centering"|"clearpage"|"hfill"|"justify"|"item"|
      "maketitle"|"medskip"|"noindent"|"newline"|"newpage"|
      "par"|"smallskip"|"vfill"|"\\") )
  val resvdCmd: P[Unit] = P("\\" ~ ("hspace"|"vspace") ~ curlyBox)
  val wrapper: P[String] = P("\\" ~ ("emph"|"textbf"|"textit"|"underline") ~ cmdName)
  val spSym: P[String] = P("\\" ~ ("#"|"$"|"%"|"&"|"{"|"}"|"~").!)

  val mathBlock: P[String] = P(doubleDollar | singleDollar | roundBracket | sqBracket)
  val doubleDollar : P[String] = P("$$" ~ (!"$$" ~ AnyChar).rep(1).! ~ "$$")
  val singleDollar : P[String] = P("$" ~ (!"$" ~ AnyChar).rep(1).! ~ "$")
  val roundBracket: P[String] = P("\\(" ~ (!"\\)" ~ AnyChar).rep(1).! ~ "\\)")
  val sqBracket : P[String] = P("\\[" ~ (!"\\]" ~ AnyChar).rep(1).! ~ "\\]")

  val enclosure: P[Enclosure] = P( (begin ~ body ~ end).
    map((t:(String,Body)) => Enclosure(t._1,t._2)) )
  val begin: P[String] = P("\\begin" ~ cmdName ~ box.rep ~ (&("\\")|ws.rep) )
  val end: P[Unit] = P("\\end" ~ box.rep(1) ~ (&("\\")|ws.rep) )

  val command: P[Command] = P( !wrapper ~
    ("\\" ~ (alpha| "*").rep(1).! ~ cmdName ~ box.rep ~ (&("\\")|ws.rep)).
    map((t:(String,String)) => Command(t._1,t._2)) )
  val cmdName: P[String] = P("{" ~ (!"}" ~ AnyChar).rep.! ~ "}" )

  val box: P[Unit] = P(curlyBox|sqBox)
  val curlyBox: P[Unit] = P("{" ~ (!"}" ~ AnyChar).rep ~ "}" )
  val sqBox: P[Unit] = P("[" ~ (!"]" ~ AnyChar).rep ~ "]" )

}


object SourcesIO {

//  val input1 = scala.io.Source.fromFile("../grad-school-notes/Algebra/group_theory.tex").mkString
//  val input2 = scala.io.Source.fromFile("../stacks-project/algebra.tex").mkString

  def main(args: Array[String]): Unit = {
    println(DeTeX.document.parse(input3))
  }

  val input3 =
"""
\documentclass[master.tex]{subfiles}

%%%%%%%%%%%%%% BEGIN CONTENT: %%%%%%%%%%%%%%

\begin{document}
\section{Testing}
  This follows from the second part of the \textit{remark} above.
  % parsing comments
  Now for some remakrs \% about centralizers.
  \vspace{1cm}
  \begin{rmk*}[1.1.3]
    This is a example of nested enclosure. \\
    Also test the line break token.
    \begin{enumerate}
    \item If $A \subgroup G$, \medskip then $A$ is abelian if and only if $A \subset
      C_G(A)$.
    \item Furthermore, if $A \subgroup Z(G)$, then $A \normsubgroup G$. \newline
      This follows using \emph{basic} commutativity arguments.
    \end{enumerate}
  \end{rmk*}
\end{document}
"""

}

/*****
STATUS - issue#1 fixed.
       - refrences and \labels are still to be added.

*/

package partex

import fastparse.all._
import TargetLang._

object DeTeX {
  val ws = P(" "|"\n"|"\t"|"\\:")
  val alpha = P(CharIn('a' to 'z'))


  val document: P[Document] = P( ((!beginDoc ~ AnyChar).rep ~ beginDoc ~ body ~ endDoc).
    map((b: Body) => Document(b)) )
  val beginDoc: P[Unit] = P("\\begin{document}" ~ (&("\\")|ws.rep))
  val endDoc: P[Unit] = P("\\end{document}" ~ (&("\\")|ws.rep))


  val body: P[Body] = P(bodyElem.rep.map(_.toVector).map((bs: Vector[BodyElem]) => Body(bs)) )

  val bodyElem: P[BodyElem] = P(mathBlock | list | environment| paragraph | !"\\end{" ~ command)

  val mathBlock: P[MathBlock] = P((displayEnv | mathEnv | doubleDollar | sqBracket).
    map((s: String) => MathBlock(s)))
  val displayEnv: P[String] = P("\\begin{displaymath}" ~ ws.rep ~ (!"\\end{displaymath}" ~ AnyChar).rep(1).! ~
    ws.rep ~ "\\end{displaymath}")
  val mathEnv: P[String] = P("\\begin{" ~ ("equation*}"|"equation}") ~ ws.rep ~
    (!("\\end{"~("equation*}"|"equation}")) ~ AnyChar).rep(1).! ~ ws.rep ~ "\\end{"~("equation*}"|"equation}") )
  val doubleDollar : P[String] = P("$$" ~ ws.rep ~ (!"$$" ~ AnyChar).rep(1).! ~ ws.rep ~ "$$")
  val sqBracket : P[String] = P("\\[" ~ ws.rep ~ (!"\\]" ~ AnyChar).rep(1).! ~ ws.rep ~ "\\]")


  val list: P[List] = P((beginLs ~ lsItem.rep.map(_.toVector) ~ endLs).
    map((xs: Vector[Body]) => List(xs)))
  val lsItem: P[Body] = P("\\item" ~ ws.rep ~ body)
  val beginLs: P[Unit] = P("\\begin{" ~ lsType ~ "}" ~ box.rep ~ (&("\\")|ws.rep))
  val endLs: P[Unit] = P("\\end{" ~ lsType ~ "}" ~ (&("\\")|ws.rep))
  val lsType: P[Unit] = P("itemize"|"enumerate"|"description")

  val paragraph: P[Paragraph] = P(fragment.rep(1).map(_.toVector).
    map((frgs: Vector[Fragment]) => Paragraph(frgs)))

  val fragment: P[Fragment] = P(inlineEq|text)

  val text: P[Text] =
    P( ( reserved | wrapper | spSym |
      !("{"|"}"|"$"|"\\("|"\\["|"\\item") ~ !command ~ AnyChar.! ).rep(1).
    map(_.reduceLeft(_+_)).
    map((s: String) => Text(s)) )

  val reserved: P[String] = P(resvdWord|resvdCmd).!.map((s: String) => "")
  val resvdWord: P[Unit] = P("\\" ~ StringIn("bigskip","break","centering",
      "clearpage","cleardoublepage","footnotesize","hfill","indent","justify",
      "large","Large","LARGE","huge","Huge","leftskip","listoffigures",
      "listoftables","maketitle","medskip","normalsize","noindent","newline",
      "newpage","parindent","parfillskip","parskip","par","raggedleft",
      "ragggedright","rightskip","scriptsize","smallskip","small",
      "tableofcontents","tiny","vfill","\\*")
      | "\\\\" ~ " ".rep )
  val resvdCmd: P[Unit] = P("\\" ~ StringIn("hspace","linespread","setlength","vspace")
    ~ curlyBox.rep)
  val wrapper: P[String] = P("\\" ~ StringIn("emph","lowercase","textbf","textit","textnormal",
    "textrm","textsf","texttt","textup","textsl","textsc","textmd","textlf","textsubscript",
    "textsuperscript","uppercase","underline") ~ cmdName)
  val spSym: P[String] = P("\\" ~ StringIn("#","$","%","^","&","{","}","~").!)

  val inlineEq: P[InlineEq] = P((inlineEnv | singleDollar | roundBracket ).
    map((s: String)=> InlineEq(s)))
  val inlineEnv: P[String] = P("\\begin{math}" ~ ws.rep ~ (!"\\end{math}" ~ AnyChar).rep(1).! ~
    ws.rep ~ "\\end{math}")
  val singleDollar : P[String] = P("$" ~ ws.rep ~ (!"$" ~ AnyChar).rep(1).! ~ ws.rep ~ "$")
  val roundBracket: P[String] = P("\\(" ~ ws.rep ~ (!"\\)" ~ AnyChar).rep(1).! ~ ws.rep ~ "\\)")

  val environment: P[Environment] = P( withoutName | withName )
  val withoutName: P[Environment] = P("{" ~ body ~ "}").map((b: Body) => Environment("None",b))
  val withName: P[Environment] = P( (begin ~ body ~ end).
    map((t:(String,Body)) => Environment(t._1,t._2)) )
  val begin: P[String] = P("\\begin" ~ cmdName ~ box.rep ~ (&("\\")|ws.rep) )
  val end: P[Unit] = P("\\end" ~ box.rep(1) ~ (&("\\")|ws.rep) )

  val command: P[Command] = P( !wrapper ~
    ("\\" ~ (alpha| "*").rep(1).! ~ cmdName ~ box.rep ~ (&("\\")|ws.rep)).
    map((t:(String,String)) => Command(t._1,t._2)) )
  val cmdName: P[String] = P("{" ~ (curlyBox | !"}" ~ AnyChar).rep.! ~ "}" )

  val box: P[Unit] = P(curlyBox|sqBox)
  val curlyBox: P[Unit] = P("{" ~ (!"}" ~ AnyChar).rep ~ "}" )
  val sqBox: P[Unit] = P("[" ~ (!"]" ~ AnyChar).rep ~ "]" )

}

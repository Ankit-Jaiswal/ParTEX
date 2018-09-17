/*****
STATUS - expansion of target language is in progress.
       - refrences and \labels are still to be added.

*/

package partex

import fastparse.all._
import TargetLang._

object DeTeX {
  val ws = P(" "|"\n"|"\t"|"\\:")
  val alpha: P[Unit] = P( CharIn('a' to 'z') | CharIn('A' to 'Z') )
  val num: P[Unit] = P( CharIn('0' to '9') )
  val alias: P[String] = P("[" ~ (!"]" ~ AnyChar).rep.! ~ "]")
  val label: P[String] = P("\\label{" ~ (!"}" ~ AnyChar).rep(1).! ~ "}" ~ (&("\\")|ws.rep))

  val document: P[Document] = P(topmatter ~ (!beginDoc ~ AnyChar).rep ~ beginDoc ~ body ~ endDoc).
    map((t:(Vector[MetaData],Body)) => Document(t._1,t._2))

  val topmatter: P[Vector[MetaData]] = P( (!(metaToken|beginDoc) ~ AnyChar).rep ~
    meta ).rep.map(_.toVector)

  val meta: P[MetaData] = P(abs | title | author | address | email | date)
  val abs: P[Abstract] = P("\\begin{abstract}" ~ alias.? ~ ws.rep ~ (!"\\end{abstract}" ~ AnyChar).rep.! ~
    "\\end{abstract}").map((t:(Option[String],String)) => Abstract(t._1,t._2))
  val title: P[Title] = P("\\title" ~ alias.? ~ cmdName).map((t:(Option[String],String)) => Title(t._1,t._2))
  val author: P[Author] = P("\\author" ~ cmdName).map((s: String) => Author(s))
  val address: P[Address] = P("\\address" ~ cmdName).map((s: String) => Address(s))
  val email: P[Email] = P("\\email" ~ cmdName).map((s: String) => Email(s))
  val date: P[Date] = P("\\date" ~ cmdName).map((s: String) => Date(s))
  val metaToken: P[Unit] = P("\\" ~
    StringIn("begin{abstract}","title{","author{","address{","email{","date{"))
  val beginDoc: P[Unit] = P("\\begin{document}" ~ (&("\\")|ws.rep))
  val endDoc: P[Unit] = P("\\end{document}" ~ (&("\\")|ws.rep))



  val body: P[Body] = P((bodyElem ~ ws.rep).rep.map(_.toVector).map((bs: Vector[BodyElem]) => Body(bs)) )

  val bodyElem: P[BodyElem] = P(meta | heading | graphics | mathBlock | list | environment| paragraph | !"\\end{" ~ command)

  val heading: P[Heading] = P("\\" ~ StringIn("subsubsection","subsection","section","chapter","part").! ~
      "*".? ~ cmdName ~ alias.? ~ (&("\\")|ws.rep) ~ label.?).
      map((t:(String,String,Option[String],Option[String])) => Heading(t._1,t._3,t._4,t._2))

  val graphics: P[Graphics] = P("\\includegraphics" ~ spec.? ~ name ~ (&("\\")|ws.rep)).
    map((t:(Option[Vector[String]],String)) => Graphics(t._2,t._1))
  val name: P[String] = P("{" ~ (!"}" ~ AnyChar).rep(1).! ~ "}")
  val spec: P[Vector[String]] = P("[" ~ (attr ~ "=" ~ value).!.rep(sep= ",") ~ "]").map(_.toVector)
  val attr: P[Unit] = P(ws.rep ~ StringIn("scale","height","width","angle") ~ ws.rep)
  val value: P[Unit] = P(ws.rep ~ (alpha|num| ".").rep ~ ws.rep)

  val mathBlock: P[MathBlock] = P((displayEnv | mathEnv | doubleDollar | sqBracket).
    map((s: String) => MathBlock(s)))
  val displayEnv: P[String] = P("\\begin{displaymath}" ~ ws.rep ~ (!"\\end{displaymath}" ~ AnyChar).rep(1).! ~
    ws.rep ~ "\\end{displaymath}")
  val mathEnv: P[String] = P("\\begin{" ~ ("equation*}"|"equation}") ~ ws.rep ~
    (!("\\end{"~("equation*}"|"equation}")) ~ AnyChar).rep(1).! ~ ws.rep ~ "\\end{"~("equation*}"|"equation}") )
  val doubleDollar : P[String] = P("$$" ~ ws.rep ~ (!"$$" ~ AnyChar).rep(1).! ~ ws.rep ~ "$$")
  val sqBracket : P[String] = P("\\[" ~ ws.rep ~ (!"\\]" ~ AnyChar).rep(1).! ~ ws.rep ~ "\\]")


  val list: P[List] = P(begin ~ lsItem.rep.map(_.toVector) ~ end).
    map((t:(String,Vector[Body])) => List(t._1,t._2))
  val lsItem: P[Body] = P("\\item" ~ box.rep ~ ws.rep ~ body)
  val begin: P[String] = P("\\begin" ~ cmdName ~ box.rep ~ (&("\\")|ws.rep) )
  val end: P[Unit] = P("\\end" ~ box.rep(1) ~ (&("\\")|ws.rep) )

  val paragraph: P[Paragraph] = P(fragment.rep(1).map(_.toVector).
    map((frgs: Vector[Fragment]) => Paragraph(frgs)))

  val fragment: P[Fragment] = P(inlineEq|text)

  val text: P[Text] =
    P( ( reserved | wrapper | spSym |
      !("{"|"}"|"$"|"\\("|"\\["|"\\item"|"\\includegraphics") ~
      !command ~ AnyChar.! ).rep(1).
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

  val command: P[Command] = P( !wrapper ~
    ("\\" ~ (alpha| "*").rep(1).! ~ cmdName ~ box.rep ~ (&("\\")|ws.rep)).
    map((t:(String,String)) => Command(t._1,t._2)) )
  val cmdName: P[String] = P("{" ~ (curlyBox | !"}" ~ AnyChar).rep.! ~ "}" )

  val box: P[Unit] = P(curlyBox|sqBox)
  val curlyBox: P[Unit] = P("{" ~ (curlyBox | !"}" ~ AnyChar).rep ~ "}" )
  val sqBox: P[Unit] = P("[" ~ (sqBox | !"]" ~ AnyChar).rep ~ "]" )

}


object ExampleRun {
  def main(args: Array[String]): Unit = {
    val first = new SourcesIO("")
    println(first.parse)
  }

}

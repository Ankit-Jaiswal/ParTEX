/*****
STATUS - expansion of target language is in progress.
       - refrences and \labels are still to be added.

*/

package partex

object ParsingRules {
  val all = DeTeX(Map())
}

case class DeTeX(thmList: Map[String,String]) {
  import fastparse.all._
  import TargetLang._

  val ws = P(" "|"\n"|"\t"|"\\:")
  val alpha: P[Unit] = P( CharIn('a' to 'z') | CharIn('A' to 'Z') )
  val num: P[Unit] = P( CharIn('0' to '9') )
  val alias: P[String] = P("[" ~ (!"]" ~ AnyChar).rep.! ~ "]")
  val label: P[String] = P("\\label{" ~ (!"}" ~ AnyChar).rep(1).! ~ "}" ~ (&("\\")|ws.rep))
  val box: P[Unit] = P(curlyBox|sqBox)
  val curlyBox: P[Unit] = P("{" ~ (curlyBox | !"}" ~ AnyChar).rep ~ "}" )
  val sqBox: P[Unit] = P("[" ~ (sqBox | !"]" ~ AnyChar).rep ~ "]" )

  def isMeta(b: BodyElem) : Boolean = b match {
    case _: MetaData => true
    case _ => false
  }


  val document: P[Document] = P(topmatter ~ (!beginDoc ~ AnyChar).rep ~ beginDoc ~ body ~ endDoc).
    map((t:(Vector[MetaData],Body)) =>
    Document(t._1 ++ t._2.elems.filter(isMeta).asInstanceOf[Vector[MetaData]] , Body(t._2.elems.filterNot(isMeta))))

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

  val bodyElem: P[BodyElem] = P(meta | heading | graphics | theorem | proof | displaymath |
    list | environment| paragraph | !"\\end{" ~ command)


  val heading: P[Heading] = P("\\" ~ StringIn("subsubsection","subsection","section","chapter","part").! ~
      "*".? ~ cmdName ~ alias.? ~ (&("\\")|ws.rep) ~ label.?).
      map((t:(String,String,Option[String],Option[String])) => Heading(t._1,t._3,t._4,t._2))

  val graphics: P[Graphics] = P("\\includegraphics" ~ imgSpec.? ~ filename ~ (&("\\")|ws.rep)).
    map((t:(Option[Map[String,String]],String)) => Graphics(t._1,t._2))
  val filename: P[String] = P("{" ~ (!"}" ~ AnyChar).rep(1).! ~ "}")
  val imgSpec: P[Map[String,String]] =
    P("[" ~ (attr ~ "=" ~ value).rep(sep= ",") ~ "]").map(_.toVector).map(_.toMap)
  val attr: P[String] = P(ws.rep ~ StringIn("scale","height","width","angle").! ~ ws.rep)
  val value: P[String] = P(ws.rep ~ (alpha|num| "."|"{"|"}").rep.! ~ ws.rep)

  val theorem: P[Theorem] = P("\\begin{" ~ thmToken.! ~ "}" ~ alias.? ~ (&("\\")|ws.rep) ~ label.? ~
    body ~ end).map((t:(String,Option[String],Option[String],Body)) => Theorem(thmList(t._1),t._2,t._3,t._4))
  val thmToken: P[Unit] = thmList.keys.toList.foldLeft(P("****"))((p: P[Unit],s: String) => P(p | s))

  val proof: P[Proof] = P("\\begin{proof}" ~ alias.? ~ (&("\\")|ws.rep) ~ label.? ~
    body ~ "\\end{proof}").map((t:(Option[String],Option[String],Body)) => Proof(t._1,t._2,t._3))

  val displaymath: P[DisplayMath] = P(displayEnv | mathEnv | doubleDollar | sqBracket).
    map((t:(Option[String],String)) => DisplayMath(t._1,t._2))
  val displayEnv: P[(Option[String],String)] = P("\\begin{displaymath}" ~ (&("\\")|ws.rep) ~
    label.? ~ (!"\\end{displaymath}" ~ AnyChar).rep(1).! ~ (&("\\")|ws.rep) ~ "\\end{displaymath}")
  val mathEnv: P[(Option[String],String)] = P("\\begin{" ~ ("equation*}"|"equation}") ~ (&("\\")|ws.rep) ~
    label.? ~ (!("\\end{"~("equation*}"|"equation}")) ~ AnyChar).rep(1).! ~ (&("\\")|ws.rep) ~
    "\\end{"~("equation*}"|"equation}") )
  val doubleDollar : P[(Option[String],String)] =
    P("$$" ~ ws.rep ~ label.? ~ (!"$$" ~ AnyChar).rep(1).! ~ ws.rep ~ "$$")
  val sqBracket : P[(Option[String],String)] =
    P("\\[" ~ ws.rep ~ label.? ~ (!"\\]" ~ AnyChar).rep(1).! ~ ws.rep ~ "\\]")

  val codeBlock: P[CodeBlock] = P(inputCode|writtenCode)
  val inputCode: P[CodeBlock] = P("\\lstinputlisting" ~ codeSpec.? ~ filename ~ (&("\\")|ws.rep)).
    map((t:(Option[Map[String,String]],String)) => CodeBlock(t._1,t._2))
  val writtenCode: P[CodeBlock] = P("\\begin{" ~ StringIn("verbatim","lstlisting") ~
    "*".? ~ "}" ~ codeSpec.? ~ ws.rep ~ code ~ end).
    map((t:(Option[Map[String,String]],String)) => CodeBlock(t._1,t._2))
  val codeSpec: P[Map[String,String]] =
    P("[" ~ (key ~ "=" ~ value).rep(sep= ",") ~ "]").map(_.toVector).map(_.toMap)
  val key: P[String] = P(ws.rep ~ StringIn("language","label","caption").! ~ ws.rep)
  val code: P[String] = P(!("\\end{" ~ StringIn("verbatim","lstlisting") ~ "*".? ~ "}") ~ AnyChar).rep.!


  val list: P[List] = P(begin ~ lsItem.rep.map(_.toVector) ~ end).
    map((t:(String,Vector[Body])) => List(t._1,t._2))
  val lsItem: P[Body] = P("\\item" ~ box.rep ~ ws.rep ~ body)
  val begin: P[String] = P("\\begin" ~ cmdName ~ box.rep ~ (&("\\")|ws.rep) )
  val end: P[Unit] = P("\\end" ~ curlyBox.rep(1) ~ (&("\\")|ws.rep) )

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

}


object ExampleRun {
  def main(args: Array[String]): Unit = {
    val first = new SourcesIO("group_theory.tex")
    println(first.parse)
  }

}

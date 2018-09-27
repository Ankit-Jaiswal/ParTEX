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
  val alias: P[String] = P("[" ~ (!"]" ~ AnyChar).rep.! ~ "]" ~ (&("\\")|ws.rep))
  val box: P[Unit] = P(curlyBox|sqBox)
  val curlyBox: P[Unit] = P("{" ~ (curlyBox | !"}" ~ AnyChar).rep ~ "}" )
  val sqBox: P[Unit] = P("[" ~ (sqBox | !"]" ~ AnyChar).rep ~ "]" )
  val label: P[String] = P("\\label{" ~ (!"}" ~ AnyChar).rep(1).! ~ "}" ~ (&("\\")|ws.rep))
  val caption: P[String] = P("\\caption" ~ sqBox ~ "{" ~ (!("}"|"\\label{") ~ AnyChar).rep(1).! ~
    label.? ~ "}" ~ (&("\\")|ws.rep)).map((t:(String,Option[String])) => t._1)

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
  val abs: P[Abstract] = P("\\begin{abstract}" ~ alias.? ~ (!"\\end{abstract}" ~ AnyChar).rep.! ~
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
    codeBlock | figure | table | tabular | list | environment | paragraph | !"\\end{" ~ command)


  val heading: P[Heading] = P("\\" ~ StringIn("subsubsection","subsection","section","chapter","part").! ~
      "*".? ~ cmdName ~ alias.? ~ label.?).
      map((t:(String,String,Option[String],Option[String])) => Heading(t._1,t._3,t._4,t._2))

  val graphics: P[Graphics] = P("\\includegraphics" ~ imgSpec.? ~ name ~ (&("\\")|ws.rep)).
    map((t:(Option[Map[String,String]],String)) => Graphics(t._1,t._2))
  val name: P[String] = P("{" ~ (!"}" ~ AnyChar).rep(1).! ~ "}")
  val imgSpec: P[Map[String,String]] =
    P("[" ~ (attr ~ "=" ~ value).rep(sep= ",") ~ "]").map(_.toVector).map(_.toMap)
  val attr: P[String] = P(ws.rep ~ StringIn("scale","height","width","angle").! ~ ws.rep)
  val value: P[String] = P(ws.rep ~ (alpha|num| "."|"{"|"}").rep.! ~ ws.rep)

  val theorem: P[Theorem] = P("\\begin{" ~ thmToken.! ~ "}" ~ alias.? ~ label.? ~
    body ~ end).map((t:(String,Option[String],Option[String],Body)) => Theorem(thmList(t._1),t._2,t._3,t._4))
  val thmToken: P[Unit] = thmList.keys.toList.foldLeft(P("****"))((p: P[Unit],s: String) => P(p | s))

  val proof: P[Proof] = P("\\begin{proof}" ~ alias.? ~ label.? ~
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
  val inputCode: P[CodeBlock] = P("\\lstinputlisting" ~ codeSpec.? ~ name ~ (&("\\")|ws.rep)).
    map((t:(Option[Map[String,String]],String)) => CodeBlock(t._1,t._2))
  val writtenCode: P[CodeBlock] = P("\\begin{" ~ StringIn("verbatim","lstlisting") ~
    "*".? ~ "}" ~ codeSpec.? ~ ws.rep ~ code ~ end).
    map((t:(Option[Map[String,String]],String)) => CodeBlock(t._1,t._2))
  val codeSpec: P[Map[String,String]] =
    P("[" ~ (key ~ "=" ~ value).rep(sep= ",") ~ "]").map(_.toVector).map(_.toMap)
  val key: P[String] = P(ws.rep ~ StringIn("language","label","caption").! ~ ws.rep)
  val code: P[String] = P(!("\\end{" ~ StringIn("verbatim","lstlisting") ~ "*".? ~ "}") ~ AnyChar).rep.!

  val figure: P[Figure] = P("\\begin{" ~ StringIn("SCfigure","wrapfigure","figure") ~ "}" ~
    (!end ~ AnyChar).rep.! ~ end).map((s: String) =>
    Figure( P((!"\\includegraphics" ~ AnyChar).rep ~ graphics).parse(s).get.value ,
      P((!"\\caption" ~ AnyChar).rep ~ caption).?.parse(s).get.value ,
      P((!"\\label" ~ AnyChar).rep ~ label).?.parse(s).get.value ))

  val table: P[Table] = P("\\begin{table}" ~ (!beginTb ~ AnyChar).rep.! ~ tabular ~
    (!"\\end{table}" ~ AnyChar).rep.! ~ "\\end{table}").map((t:(String,Table,String)) =>
    Table(P((!"\\caption" ~ AnyChar).rep ~ caption).?.parse(t._1 +"\n"+ t._3).get.value ,
      P((!"\\label" ~ AnyChar).rep ~ label).?.parse(t._1 +"\n"+ t._3).get.value ,
      t._2.tb ))

  val tabular: P[Table] = P(beginTb ~ (!endTb ~ AnyChar).rep.! ~ endTb).map((s: String) =>
    Table(None, None, s.split('\n').mkString.split("""\\\\""").toVector.map((r: String) =>
      Rows(r.split("&").toVector.map((c: String) => cell.parse(c).get.value)))))
  val beginTb: P[Unit] = P("\\begin{" ~ StringIn("longtabu","tabulary","tabularx","tabular*",
    "tabular") ~ "}" ~ box.rep ~ (&("\\")|ws.rep))
  val endTb: P[Unit] = P("\\end{" ~ StringIn("longtabu","tabulary","tabularx","tabular*",
    "tabular") ~ "}" ~ (&("\\")|ws.rep))
  val cell: P[TableElem] = P(parBox | multiCol | multiRow | paragraph)
  val parBox: P[ParBox] = P("\\parbox" ~ sqBox ~ curlyBox ~ "{" ~ cell ~ "}").
    map((c: TableElem) => ParBox(c))
  val multiCol: P[MultiCol] = P("\\multicolumn" ~ span ~ curlyBox ~ "{" ~ cell ~ "}").
    map((t:(Int,TableElem)) => MultiCol(t._1,t._2))
  val multiRow: P[MultiRow] = P("\\multirow" ~ span ~ curlyBox ~ "{" ~ cell ~ "}").
    map((t:(Int,TableElem)) => MultiRow(t._1,t._2))
  val span: P[Int] = P("{" ~ num.rep(1).! ~ "}").map(_.toInt)

  val list: P[TexList] = P(ordered | unordered | custom)
  val ordered: P[Ordered] = P("\\begin{" ~ StringIn("enumerate","tasks").! ~ "}" ~
    box.rep ~ (&("\\")|ws.rep) ~ item.rep.map(_.toVector) ~ end).
    map((t:(String,Vector[Item])) => Ordered(t._1,t._2))
  val unordered: P[Unordered] = P("\\begin{" ~ StringIn("itemize","description","labelling").! ~ "}" ~
    box.rep ~ (&("\\")|ws.rep) ~ item.rep.map(_.toVector) ~ end).
    map((t:(String,Vector[Item])) => Unordered(t._1,t._2))
  val custom: P[Custom] = P(begin ~ item.rep.map(_.toVector) ~ end).
    map((t:(String,Vector[Item])) => Custom(t._1,t._2))
  val item: P[Item] = P("\\item" ~ ws.rep ~ alias.? ~ label.? ~ body ~
    ws.rep ~ label.?).map((t:(Option[String],Option[String],Body,Option[String])) =>
      Item(t._1, if(t._2.isEmpty){t._4} else {t._2} , t._3))
  val begin: P[String] = P("\\begin" ~ cmdName ~ box.rep ~ (&("\\")|ws.rep) )
  val end: P[Unit] = P("\\end" ~ curlyBox.rep(1) ~ (&("\\")|ws.rep) )

  val paragraph: P[Paragraph] = P(fragment.rep(1).map(_.toVector).
    map((frgs: Vector[Fragment]) => Paragraph(frgs)))

  val fragment: P[Fragment] = P(inlineMath | phantom | text)

  val text: P[Text] =
    P( ( reserved | wrapper | spSym |
      !StringIn("{","}","$","\\(","\\[","\\item","\\includegraphics","\\phantomsection") ~
      !command ~ AnyChar.! ).rep(1).
    map(_.reduceLeft(_+_)).
    map((s: String) => Text(s)) )

  val reserved: P[String] = P(resvdWord|resvdCmd).!.map((s: String) => "")
  val resvdWord: P[Unit] = P("\\" ~ StringIn("addsec","addpart","addchap","addcontentsline",
      "bfseries","bigskip","break","centering","clearpage","cleardoublepage",
      "footnotesize","hfill","hline","itshape","indent","justify",
      "large","Large","LARGE","huge","Huge","leftskip","listoffigures",
      "listoftables","maketitle","medskip","normalsize","noindent","newline",
      "newpage","parindent","parfillskip","parskip","par","raggedleft",
      "ragggedright","rightskip","scriptsize","smallskip","small","setcounter",
      "tableofcontents","tabularnewline","textwidth","tiny","vfill","\\*")
      | "\\\\" ~ ws.rep )
  val resvdCmd: P[Unit] = P("\\" ~ StringIn("hspace","linespread","setlength","vspace",
    "cline","noalign","rowfont","markright","markboth") ~ curlyBox.rep)
  val wrapper: P[String] = P("\\" ~ StringIn("emph","lowercase","textbf","textit","textnormal",
    "textrm","textsf","texttt","textup","textsl","textsc","textmd","textlf","textsubscript",
    "textsuperscript","uppercase","underline") ~ cmdName)
  val spSym: P[String] = P("\\" ~ StringIn("#","$","%","^","&","{","}","~").!)

  val inlineMath: P[InlineMath] = P((inlineEnv | singleDollar | roundBracket ).
    map((s: String)=> InlineMath(s)))
  val inlineEnv: P[String] = P("\\begin{math}" ~ ws.rep ~ (!"\\end{math}" ~ AnyChar).rep(1).! ~
    ws.rep ~ "\\end{math}")
  val singleDollar : P[String] = P("$" ~ ws.rep ~ (!"$" ~ AnyChar).rep(1).! ~ ws.rep ~ "$")
  val roundBracket: P[String] = P("\\(" ~ ws.rep ~ (!"\\)" ~ AnyChar).rep(1).! ~ ws.rep ~ "\\)")

  val phantom: P[Phantom] = P("\\phantomsection" ~ ws.rep ~ label.?).
    map((o: Option[String]) => Phantom(o))

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
    val first = new SourcesIO("")
    println(first.parse)
  }

}

/*****
STATUS - parser is complete and running succesfully.
       - a basic .html output is added as a method to SourcesIO.
       - Numbering is added as a method to Document.
       - a scalatest is also needed to be framed.

*/

package partex

object ParsingRules {
  val all = DeTeX(Map())
}

case class DeTeX(thmList: Map[String,(Option[String],String,Option[String])]) {
  import fastparse._, NoWhitespace._
  import TargetLang._

  def ws[_:P]: P[Unit] = P(" " | "\n" | "\t" | "\\:")
  def alpha[_:P]: P[Unit] = P( CharIn("a-z") | CharIn("A-Z") )
  def num[_:P]: P[Unit] = P( CharIn("0-9") )
  def alias[_:P]: P[String] = P("[" ~ (!("]") ~ AnyChar).rep.! ~ "]" ~ (&("\\")|ws.rep))
  def box[_:P]: P[Unit] = P(curlyBox|sqBox)
  def curlyBox[_:P]: P[Unit] = P("{" ~ (curlyBox | !("}") ~ AnyChar).rep ~ "}" )
  def sqBox[_:P]: P[Unit] = P("[" ~ (sqBox | !("]") ~ AnyChar).rep ~ "]" )
  def label[_:P]: P[String] = P("\\label{" ~ (!("}") ~ AnyChar).rep(1).! ~ "}" ~ (&("\\")|ws.rep))
  def caption[_:P]: P[String] = P("\\caption" ~ sqBox.? ~ "{" ~ (!("}" | "\\label{") ~ AnyChar).rep(1).! ~
    label.? ~ "}" ~ (&("\\")|ws.rep)).map((t:(String,Option[String])) => t._1)

  def isMeta(b: BodyElem) : Boolean = b match {
    case _: MetaData => true
    case _ => false
  }


  def document[_:P]: P[Document] = P(topmatter ~ (!beginDoc ~ AnyChar).rep ~ beginDoc ~ body ~ endDoc).
    map((t:(Vector[MetaData],Body)) =>
    Document(t._1 ++ t._2.elems.filter(isMeta).asInstanceOf[Vector[MetaData]] , Body(t._2.elems.filterNot(isMeta))))

  def topmatter[_:P]: P[Vector[MetaData]] = P( (!(metaToken|beginDoc) ~ AnyChar).rep ~
    meta ).rep.map(_.toVector)

  def meta[_:P]: P[MetaData] = P(abs | title | author | address | email | date)
  def abs[_:P]: P[Abstract] = P("\\begin{abstract}" ~ alias.? ~ paragraph ~
    "\\end{abstract}").map((t:(Option[String],Paragraph)) => Abstract(t._1,t._2))
  def title[_:P]: P[Title] = P("\\title" ~ alias.? ~ cmdName).map((t:(Option[String],String)) =>
    Title(t._1,parse(t._2,text(_)).get.value.s))
  def author[_:P]: P[Author] = P("\\author" ~ cmdName).map((s: String) => Author(s))
  def address[_:P]: P[Address] = P("\\address" ~ cmdName).map((s: String) => Address(s))
  def email[_:P]: P[Email] = P("\\email" ~ cmdName).map((s: String) => Email(s))
  def date[_:P]: P[Date] = P("\\date" ~ cmdName).map((s: String) => Date(s))
  def metaToken[_:P]: P[Unit] = P("\\" ~
    StringIn("begin{abstract}","title{","author{","address{","email{","date{"))
  def beginDoc[_:P]: P[Unit] = P("\\begin{document}" ~ (&("\\")|ws.rep))
  def endDoc[_:P]: P[Unit] = P("\\end{document}" ~ (&("\\")|ws.rep))



  def body[_:P]: P[Body] = P((bodyElem ~ ws.rep).rep.map(_.toVector).map((bs: Vector[BodyElem]) => Body(bs)) )

  def bodyElem[_:P]: P[BodyElem] = P(meta | heading | image | theorem | proof | mathBlock |
    codeBlock | figure | table | tabular | list | bibliography |
    environment | paragraph | !("\\end{" | "\\bibitem{") ~ command)


  def heading[_:P]: P[Heading] = P("\\" ~ StringIn("subsubsection","subsection","section","chapter","part").! ~
      "*".? ~ cmdName ~ (&("\\")|ws.rep) ~ alias.? ~ label.?).
      map((t:(String,String,Option[String],Option[String])) => Heading(t._1,t._3,t._4,t._2))

  def image[_:P]: P[Image] = P("\\includegraphics" ~ imgSpec.? ~ name ~ (&("\\")|ws.rep)).
    map((t:(Option[Map[String,String]],String)) => Image(t._1,t._2))
  def name[_:P]: P[String] = P("{" ~ (!("}") ~ AnyChar).rep(1).! ~ "}")
  def imgSpec[_:P]: P[Map[String,String]] =
    P("[" ~ (attr ~ "=" ~ value).rep(sep= ",") ~ "]").map(_.toVector).map(_.toMap)
  def attr[_:P]: P[String] = P(ws.rep ~ StringIn("scale","height","width","angle").! ~ ws.rep)
  def value[_:P]: P[String] = P(ws.rep ~ (alpha | num | "." | "{" | "}").rep.! ~ ws.rep)

  def theorem[_:P]: P[Theorem] = P("\\begin{" ~ thmToken.! ~ "}" ~ (&("\\")|ws.rep) ~ alias.? ~ label.? ~
    body ~ end).map((t:(String,Option[String],Option[String],Body)) =>
    Theorem(
      thmList(t._1)._2,
      thmList(t._1)._1.map((k: String) => if(thmList.keys.toVector.contains(k)) {thmList(k)._2} else {k}),
      thmList(t._1)._3,
      t._2,
      t._3,
      t._4 ))

  def thmToken[_:P]: P[Unit] = thmList.keys.toList.foldLeft(P("****"))((p: P[Unit],s: String) => P(p | s))

  def proof[_:P]: P[Proof] = P("\\begin{proof}" ~ (&("\\")|ws.rep) ~ alias.? ~ label.? ~
    body ~ "\\end{proof}").map((t:(Option[String],Option[String],Body)) => Proof(t._1,t._2,t._3))

  def mathBlock[_:P]: P[MathBlock] = P(displaymath | eqMatrix | multiline)
  def displaymath[_:P]: P[DisplayMath] = P(displayEnv | mathEnv | doubleDollar | sqBracket).
    map((t:(Option[String],String)) => DisplayMath(t._1,t._2))
  def displayEnv[_:P]: P[(Option[String],String)] = P("\\begin{displaymath}" ~
    (&("\\")|ws.rep) ~ label.? ~ (!("\\end{displaymath}") ~ AnyChar).rep(1).! ~
    (&("\\")|ws.rep) ~ "\\end{displaymath}")
  def mathEnv[_:P]: P[(Option[String],String)] = P("\\begin{" ~ ("math}" | "equation*}" | "equation}") ~
    (&("\\")|ws.rep) ~ label.? ~ (!("\\end{"~("math}" | "equation*}" | "equation}")) ~ AnyChar).rep(1).! ~
    (&("\\")|ws.rep) ~ "\\end{"~("math}" | "equation*}" | "equation}") )
  def doubleDollar[_:P]: P[(Option[String],String)] =
    P("$$" ~ ws.rep ~ label.? ~ (!("$$") ~ AnyChar).rep(1).! ~ ws.rep ~ "$$")
  def sqBracket[_:P]: P[(Option[String],String)] =
    P("\\[" ~ ws.rep ~ label.? ~ (!("\\]") ~ AnyChar).rep(1).! ~ ws.rep ~ "\\]")

  def eqMatrix[_:P]: P[EqMatrix] = P("\\begin{" ~ StringIn("align","eqnarray","gathered","gather").! ~
    "*".? ~ "}" ~ (&("\\")|ws.rep) ~ label.? ~ (!("\\end{" ~ StringIn("align","eqnarray","gathered","gather") ~
    "*".? ~ "}") ~ AnyChar).rep(1).! ~ (&("\\")|ws.rep) ~ "\\end{" ~ StringIn("align","eqnarray","gathered","gather") ~
    "*".? ~ "}").map((t:(String,Option[String],String)) => EqMatrix(t._1,t._2,t._3))

  def multiline[_:P]: P[MultiLine] = P("\\begin{multiline" ~ "*".? ~ "}" ~ (&("\\")|ws.rep) ~
    label.? ~ (!("\\end{multiline" ~ "*".? ~ "}") ~ AnyChar).rep(1).! ~
    (&("\\")|ws.rep) ~ "\\end{multiline" ~ "*".? ~ "}")
    .map((t:(Option[String],String)) => MultiLine(t._1,t._2))

  def codeBlock[_:P]: P[CodeBlock] = P(inputCode|writtenCode).
    map((t:(Option[String],String)) =>
    CodeBlock(t._1.getOrElse("").split(',').find(_.contains("label")).map(_.split('=')(1).trim) , t._2))
  def inputCode[_:P]: P[(Option[String],String)] = P("\\lstinputlisting" ~ codeSpec.? ~ name ~ (&("\\")|ws.rep))
  def writtenCode[_:P]: P[(Option[String],String)] = P("\\begin{" ~ StringIn("verbatim","lstlisting","alltt") ~
    "*".? ~ "}" ~ codeSpec.? ~ ws.rep ~ code ~ end)
  def code[_:P]: P[String] = P(!("\\end{" ~ StringIn("verbatim","lstlisting") ~ "*".? ~ "}") ~ AnyChar).rep.!
  def codeSpec[_:P]: P[String] = P("[" ~ (!("]") ~ AnyChar).rep.! ~ "]")

  def figure[_:P]: P[Figure] = P("\\begin{" ~ StringIn("SCfigure","wrapfigure","figure") ~ "}" ~
    (!("\\end{" ~ StringIn("SCfigure","wrapfigure","figure")) ~ AnyChar).rep.! ~ end).map((s: String) =>
    Figure( parse(s, extractGraphics(_)).get.value, parse(s, extractCaption(_)).get.value, parse(s, extractLabel(_)).get.value ))
  def extractGraphics[_:P]: P[Graphics] = P((!("\\begin{tikzcd}" | "\\includegraphics") ~ AnyChar).rep ~ graphics)
  def extractCaption[_:P]: P[Option[String]] = P((!("\\caption") ~ AnyChar).rep ~ caption).?
  def extractLabel[_:P]: P[Option[String]] = P((!("\\label") ~ AnyChar).rep ~ label).?
  def graphics[_:P]: P[Graphics] = P(figMath | image)
  def figMath[_:P]: P[FigMath] = P("\\begin{tikzcd}" ~ (!("\\end{tikzcd}") ~ AnyChar).rep.! ~ "\\end{tikzcd}")
    .map((s: String) => FigMath(s))


  def table[_:P]: P[Table] = P("\\begin{table}" ~ (!beginTb ~ AnyChar).rep.! ~ tabular ~
    (!("\\end{table}") ~ AnyChar).rep.! ~ "\\end{table}").map((t:(String,Table,String)) =>
    Table(parse(t._1+ "\n"+ t._3, extractCaption(_)).get.value, parse(t._1 + "\n"+ t._3, extractLabel(_)).get.value, t._2.tb ))

  def tabular[_:P]: P[Table] = P(beginTb ~ (!endTb ~ AnyChar).rep.! ~ endTb).map((s: String) =>
    Table(None, None, s.split('\n').mkString.split("""\\\\""").toVector.map((r: String) =>
      Rows(r.split("&").toVector.map((c: String) => parse(c, cell(_)).get.value)))))
  def beginTb[_:P]: P[Unit] = P("\\begin{" ~ StringIn("longtabu","tabulary","tabularx","tabular*",
    "tabular") ~ "}" ~ box.rep ~ (&("\\")|ws.rep))
  def endTb[_:P]: P[Unit] = P("\\end{" ~ StringIn("longtabu","tabulary","tabularx","tabular*",
    "tabular") ~ "}" ~ (&("\\")|ws.rep))
  def cell[_:P]: P[TableElem] = P(parBox | multiCol | multiRow | paragraph)
  def parBox[_:P]: P[ParBox] = P("\\parbox" ~ sqBox ~ curlyBox ~ "{" ~ cell ~ "}").
    map((c: TableElem) => ParBox(c))
  def multiCol[_:P]: P[MultiCol] = P("\\multicolumn" ~ span ~ curlyBox ~ "{" ~ cell ~ "}").
    map((t:(Int,TableElem)) => MultiCol(t._1,t._2))
  def multiRow[_:P]: P[MultiRow] = P("\\multirow" ~ span ~ curlyBox ~ "{" ~ cell ~ "}").
    map((t:(Int,TableElem)) => MultiRow(t._1,t._2))
  def span[_:P]: P[Int] = P("{" ~ num.rep(1).! ~ "}").map(_.toInt)

  def list[_:P]: P[TexList] = P(ordered | unordered | custom)
  def ordered[_:P]: P[Ordered] = P("\\begin{" ~ StringIn("enumerate","tasks").! ~ "}" ~
    box.rep ~ (&("\\")|ws.rep) ~ item.rep.map(_.toVector) ~ end).
    map((t:(String,Vector[Item])) => Ordered(t._1,t._2))
  def unordered[_:P]: P[Unordered] = P("\\begin{" ~ StringIn("itemize","description","labelling").! ~ "}" ~
    box.rep ~ (&("\\")|ws.rep) ~ item.rep.map(_.toVector) ~ end).
    map((t:(String,Vector[Item])) => Unordered(t._1,t._2))
  def custom[_:P]: P[Custom] = P(begin ~ item.rep.map(_.toVector) ~ end).
    map((t:(String,Vector[Item])) => Custom(t._1,t._2))
  def item[_:P]: P[Item] = P("\\item" ~ ws.rep ~ alias.? ~ label.? ~ body ~
    ws.rep ~ label.?).map((t:(Option[String],Option[String],Body,Option[String])) =>
      Item(t._1, if(t._2.isEmpty){t._4} else {t._2} , t._3))
  def begin[_:P]: P[String] = P("\\begin" ~ cmdName ~ box.rep ~ (&("\\")|ws.rep) )
  def end[_:P]: P[Unit] = P("\\end" ~ curlyBox.rep(1) ~ (&("\\")|ws.rep) )

  def paragraph[_:P]: P[Paragraph] = P(fragment.rep(1).map(_.toVector).
    map((frgs: Vector[Fragment]) => Paragraph(frgs)))

  def bibliography[_:P]: P[Bibliography] = P("\\begin{thebibliography}" ~ box.rep ~
    (&("\\")|ws.rep) ~ bibItem.rep.map(_.toVector) ~ end).
    map((xs: Vector[BibItem]) => Bibliography(xs))
  def bibItem[_:P]: P[BibItem] = P("\\bibitem" ~ cmdName ~ ws.rep ~ body).
    map((t:(String,Body)) => BibItem(t._1,t._2))



  def fragment[_:P]: P[Fragment] = P(inlineMath | phantom | quoted | cite | hypertarget |
    hyperlink | ref | note | styled | text)

  def text[_:P]: P[Text] =
    P( ( reserved | wrapper | spSym | linebreak | quoteSym |
      !StringIn("{","}","$","\\(","\\[","\\item","\\phantomsection") ~
      !command ~ AnyChar.! ).rep(1).
    map(_.reduceLeft(_+_)).
    map((s: String) => Text(s)) )

  def reserved[_:P]: P[String] = P(resvdWord|resvdCmd|comment|resvdEnvToken).!.map((s: String) => "")
  def resvdWord[_:P]: P[Unit] = P("\\" ~ StringIn("addsec","addpart","addchap","addcontentsline",
      "bfseries","bigskip","break","baselineskip","centering","clearpage","cleardoublepage",
      "doublespacing","footnotemark","footnotesize","frenchspacing","hfill","hline",
      "huge","Huge","itshape","indent","justify","large","Large","LARGE",
      "leftskip","listoffigures","listoftables","maketitle","medskip","normalsize","noindent","newline",
      "newpage","onehalfspacing","parindent","parfillskip","parskip","par","quad","raggedleft",
      "raggedright","rightskip","scriptsize","singlespacing","smallskip","small","setcounter",
      "tableofcontents","tabularnewline","textwidth","tiny","vfill","\\*",":")
      | StringIn("~","{}") )
  def resvdCmd[_:P]: P[Unit] = P("\\" ~ StringIn("hspace","linespread","setlength","setstretch","vspace",
    "cline","comment","noalign","rowfont","markright","markboth","pagenumbering","def",
    "newcommand","renewcommand") ~ box.rep)
  def comment[_:P]: P[Unit] = P("\\begin{comment}" ~ (!("\\end{comment}") ~ AnyChar).rep ~ "\\end{comment}")
  def resvdEnvToken[_:P]: P[Unit] = P("\\" ~ ("begin" | "end") ~ "{" ~
    StringIn("doublespace","spacing","flushleft","flushright","center") ~ "}")

  def wrapper[_:P]: P[String] = P("\\" ~ StringIn("lowercase","textnormal","textrm","textsf","texttt",
    "textup","textsl","textsc","textmd","textlf","uppercase") ~ cmdName)
  def spSym[_:P]: P[String] = P("\\" ~ StringIn("#","$","%","^","&","{","}","~").!)
  def quoteSym[_:P]: P[String] = P(("``").!.map((s: String) => "\"") | ("`").!.map((s: String) => "'"))
  def linebreak[_:P]: P[String] = P("\\\\" ~ ws.rep).!.map((_:String) => "\n")

  def inlineMath[_:P]: P[InlineMath] = P((inlineEnv | singleDollar | roundBracket ).
    map((s: String)=> InlineMath(s)))
  def inlineEnv[_:P]: P[String] = P("\\begin{math}" ~ ws.rep ~ (!("\\end{math}") ~ AnyChar).rep(1).! ~
    ws.rep ~ "\\end{math}")
  def singleDollar[_:P]: P[String] = P("$" ~ ws.rep ~ (!("$") ~ AnyChar).rep(1).! ~ ws.rep ~ "$")
  def roundBracket[_:P]: P[String] = P("\\(" ~ ws.rep ~ (!("\\)") ~ AnyChar).rep(1).! ~ ws.rep ~ "\\)")

  def phantom[_:P]: P[Phantom] = P("\\phantomsection" ~ ws.rep ~ label.?).
    map((o: Option[String]) => Phantom(o))

  def quoted[_:P]: P[Quoted] = P( "\\begin{quote}" ~ (!("\\end{quote}") ~ AnyChar).rep.! ~ "\\end{quote}" |
    "\\begin{quotation}" ~(!("\\end{quotation}") ~ AnyChar).rep.! ~ "\\end{quotation}" |
    "\\begin{verse}" ~ (!("\\end{verse}") ~ AnyChar).rep.! ~ "\\end{verse}" |
    "``" ~ (!("''" | "\"") ~ AnyChar).rep.! ~ ("''" | "\"") |
    "`" ~ (!("'") ~ AnyChar).rep.! ~ "'" ).map((s: String) => Quoted(s))

  def cite[_:P]: P[Citation] = P("\\cite" ~ ("year" | "author" | "p*" | "t*" | "p" | "t").? ~
    sqBox.rep ~ cmdName).map((s: String) => Citation(s))

  def hypertarget[_:P]: P[Hypertarget] = P("\\hypertarget" ~ cmdName ~ cmdName).
    map((t:(String,String)) => Hypertarget(t._1,t._2))
  def hyperlink[_:P]: P[Hyperlink] = P(("\\url" ~ cmdName).map((s: String) => Hyperlink(s,s)) |
    (("\\href" | "\\hyperlink") ~ cmdName ~ cmdName).map((t:(String,String)) => Hyperlink(t._1,t._2)) )

  def ref[_:P]: P[Reference] = P("\\" ~ StringIn("ref","pageref","nameref","autoref","vref","hyperref") ~
    sqBox.? ~ cmdName).map((s: String) => Reference(s))

  def note[_:P]: P[Note] = P("\\" ~ StringIn("footnotetext","footnote","todo","marginpar","marginnote") ~
    sqBox.? ~ "{" ~ paragraph ~ "}" ~ sqBox.?).map((p: Paragraph) => Note(p))

  def styled[_:P]: P[Styled] = P(strong | italics | underline | emph | superscript | subscript)

  def strong[_:P]: P[Strong] = P("\\textbf{" ~ paragraph ~ "}").map((p: Paragraph) => Strong(p))
  def italics[_:P]: P[Italic] = P("\\textit{" ~ paragraph ~ "}").map((p: Paragraph) => Italic(p))
  def underline[_:P]: P[Underline] = P("\\underline{" ~ paragraph ~ "}").map((p: Paragraph) => Underline(p))
  def emph[_:P]: P[Emph] = P("\\emph{" ~ paragraph ~ "}" | "{\\em" ~ paragraph ~ "}").
    map((p: Paragraph) => Emph(p))
  def superscript[_:P]: P[Superscript] = P("\\textsuperscript{" ~ paragraph ~ "}").map((p: Paragraph) => Superscript(p))
  def subscript[_:P]: P[Subscript] = P("\\textsubscript{" ~ paragraph ~ "}").map((p: Paragraph) => Subscript(p))

  def environment[_:P]: P[Environment] = P( withoutName | withName )
  def withoutName[_:P]: P[Environment] = P("{" ~ body ~ "}").map((b: Body) => Environment("None",b))
  def withName[_:P]: P[Environment] = P( (begin ~ body ~ end).
    map((t:(String,Body)) => Environment(t._1,t._2)) )

  def command[_:P]: P[Command] = P( !wrapper ~ ("\\" ~ (alpha| "*").rep(1).! ~
    sqBox.rep ~ cmdName ~ box.rep ~ (&("\\")|ws.rep)).
    map((t:(String,String)) => Command(t._1,t._2)) )
  def cmdName[_:P]: P[String] = P("{" ~ (cmdName | !"}" ~ AnyChar).rep.! ~ "}" )

}

package partex
import fastparse._, NoWhitespace._
import ParsingRules.all.curlyBox

object MathParser{
  import MathLang._
  def ws[_:P]: P[Unit] = P(resrvdWd | " " | "\t" | "\n")
  def resrvdWd[_:P]: P[Unit] = P(StringIn("\\displaystyle","\\nolimits","\\textstyle",
    "\\quad","\\qquad","\\thinmuskip","\\thickmuskip","\\scriptstyle",
    "\\scriptscriptstyle","\\left","\\right","\\middle","\\bigl","\\bigr","\\Bigl","\\Bigr",
    "\\biggl","\\biggr","\\Biggl","\\Biggr","\\underbrace","\\,","\\:","\\;","\\!","\\ "))

  def parseMath(s: String) : Parsed[Vector[MathPhrase]] = parse(s, fullMathLine(_))
  def getMath(s:String): Option[Vector[MathPhrase]] = parseMath(s).fold({case (_, _, _) => None}, {case (exp, _) => Some(exp)})

  def fullMathLine[_:P] : P[Vector[MathPhrase]] =
    P(mathLine.rep(sep= ",").map(_.toVector.flatten) ~ ("."|ws).rep ~ End)

  def mathLine[_:P]: P[Vector[MathPhrase]] =
    xyMatrix |
    suchThat.map((st: SuchThat) => Vector(st)) |
    mapsTo.map((mp: MapsTo) => Vector(mp)) |
    P(!arrMatrix ~ (multiline|splitline)) |
    P(expr ~ (("\\mathrel{" | "\\mathbin{").? ~ binRelation ~ "}".? ~ expr).rep.map(_.toVector)).map(
      (t:(Expr,Vector[(String,Expr)])) =>
        if (t._2.size > 0) {
          (("=",t._1) +: t._2).sliding(2).toVector.map(
            (p: Vector[(String,Expr)]) => getMathPhrase(p(0)._2, p(1)._1 , p(1)._2)
          )
        }
        else { Vector(t._1) }
    )

  def xyMatrix[_:P]: P[Vector[MathPhrase]] = P(ws.rep ~ ("\\vcenter{" ~ ws.rep).? ~
    "\\xymatrix{" ~ ws.rep ~ xyCell.rep(sep= "&").map(_.toVector).rep(sep= "\\\\").map(_.toVector) ~
    ("\\\\"|ws).rep ~ "}" ~ (ws.rep ~ "}").?)
    .map((xxs: Vector[Vector[(Expr,Vector[(String,Vector[SymAttr])])]]) =>
      xxs.map((xs: Vector[(Expr,Vector[(String,Vector[SymAttr])])]) =>
        xs.map((t: (Expr,Vector[(String,Vector[SymAttr])])) =>
          if(t._2.isEmpty) { Vector(t._1) }
          else {
            t._2.map((ar: (String,Vector[SymAttr])) => {
              val nwIndex = ar._1.split("").toVector.foldLeft((xxs.indexOf(xs),xs.indexOf(t)))(move)
              if(xxs.isDefinedAt(nwIndex._1) && xxs(nwIndex._1).isDefinedAt(nwIndex._2))
                {MapsTo(t._1, Vector(xxs(nwIndex._1)(nwIndex._2)._1), ar._2)}
              else {MapsTo(t._1,Vector(t._1),ar._2)}
            })
          }
        ).flatten
      ).flatten
    )
  def xyCell[_:P]: P[(Expr,Vector[(String,Vector[SymAttr])])] =
    P((expr|ws.rep.!.map(Variable(_,Vector()))) ~ arrow.rep.map(_.toVector))
  def arrow[_:P]: P[(String,Vector[SymAttr])] = P("\\ar[" ~ CharIn("a-z").rep.! ~
    "]" ~ ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
  def move(i:(Int,Int), c: String): (Int,Int) =
    if(c == "l") {(i._1,i._2-1)}
    else if(c == "r") {(i._1,i._2+1)}
    else if(c == "u") {(i._1-1,i._2)}
    else if(c == "d") {(i._1+1,i._2)}
    else {i}

  def suchThat[_:P]: P[SuchThat] = P(expr ~ ":" ~ mathLine.rep(min= 1, sep= ",").map(_.toVector.flatten)).map(
    (t:(Expr,Vector[MathPhrase])) => SuchThat(t._1,t._2)
  )

  def mapsTo[_:P]: P[MapsTo] = P(expr ~ (mapSym ~ expr).rep(1).map(_.toVector)).map(
      (t: (Expr,Vector[Expr])) => MapsTo(t._1,t._2,Vector())
    )
  def mapSym[_:P]: P[Unit] = StringIn("\\to","\\mapsto","\\rightarrow","\\longrightarrow","\\longmapsto")


  def multiline[_:P]: P[Vector[MathPhrase]] = P(ws.rep ~ "\\begin{array}" ~ curlyBox.? ~
    (!("\\end{array}") ~ AnyChar).rep.! ~ "\\end{array}" ~ curlyBox.? ~ ws.rep)
    .map((s: String) =>
    getMath(s.split("&").reduce(_+_).split("""\\\\""").reduce(_+", "+_))
      .getOrElse(Vector(MathText("Multiline rule failed.",Vector()))))

  def splitline[_:P]: P[Vector[MathPhrase]] = P(ws.rep ~ "\\begin{split}" ~
    (!("\\end{split}") ~ AnyChar).rep.! ~ "\\end{split}" ~ ws.rep)
    .map((s: String) =>
    getMath(s.split("""\\\\""").reduce(_+_)).getOrElse(Vector(MathText("Splitline rule failed.",Vector()))))


  def binRelation[_:P]: P[String] = P(StringIn("=","\\approx","\\cong","\\equiv","\\propto","\\in",
    "\\neq","\\ne","<","\\leqslant","\\leq",">","\\geqslant","\\geq","\\simeq","\\sim","\\subset",
    "\\subseteq","\\not\\subset","\\nsubseteq","\\supset","\\supseteq","\\not\\supset",
    "\\nsupseteq"/*,"\\to","\\mapsto","\\rightarrow","\\longrightarrow","\\longmapsto"*/).!)
  def getMathPhrase(e1: Expr, r: String, e2: Expr): MathPhrase =
    if (r == "=") { Equality(e1,e2) }
    else if (Vector("\\neq","\\ne").contains(r)) { Inequality(e1,e2) }
    else if (Vector("\\leqslant","\\leq").contains(r)) { LessThanEqual(e1,e2) }
    else if (Vector("\\geqslant","\\geq").contains(r)) { GreaterThanEqual(e1,e2) }
//    else if (Vector("\\to","\\mapsto","\\rightarrow","\\longrightarrow","\\longmapsto").contains(r))
//      { MapsTo(e1,e2,Vector()) }
    else if (r == "\\approx") { Approx(e1,e2) }
    else if (r == "\\cong") { Congruent(e1,e2) }
    else if (r == "\\equiv") { Equivalent(e1,e2) }
    else if (r == "\\propto") { Proportional(e1,e2) }
    else if (r == "\\simeq") { SimilarEq(e1,e2) }
    else if (r == "\\sim") { Similar(e1,e2) }
    else if (r == "<") { LessThan(e1,e2) }
    else if (r == ">") { GreaterThan(e1,e2) }
    else if (r == "\\subset") { SubsetPrpr(e1,e2) }
    else if (r == "\\subseteq") { Subset(e1,e2) }
    else if (r == "\\not\\subset") { NotSubsetPrpr(e1,e2) }
    else if (r == "\\nsubseteq") { NotSubset(e1,e2) }
    else if (r == "\\supset") { SupsetPrpr(e1,e2) }
    else if (r == "\\supseteq") { Supset(e1,e2) }
    else if (r == "\\not\\supset") { NotSupsetPrpr(e1,e2) }
    else if (r == "\\nsupseteq") { NotSupset(e1,e2) }
    else { BelongsTo(e1,e2) }


  def expr[_:P]: P[Expr] = P(ws.rep ~ signed ~ (binOperation ~ expr).?).map(
    (t:(Expr,Option[(String,Expr)])) => t._2 match {
      case Some(("+",e)) => Add(t._1,e)
      case Some(("-",e)) => Subtract(t._1,e)
      case Some(("\\pm",e)) => PlusMinus(t._1,e)
      case Some(("\\cap",e)) => Intersection(t._1,e)
      case Some(("\\cup",e)) => Union(t._1,e)
      case Some(("\\setminus",e)) => SetMinus(t._1,e)
      case _ => t._1
    }
  )
  def binOperation[_:P]: P[String] = P(("\\mathbin{" | "\\mathrel{").? ~
    StringIn("+","-","\\pm","\\cap","\\cup","\\setminus").! ~ "}".?)
  def binary[_:P]: P[String] = P(binRelation | binOperation)

  def signed[_:P]: P[Expr] = P(posOrNeg | negative | positive)
  def posOrNeg[_:P]: P[Signed] = P("\\pm" ~ ws.rep ~ term).map((e: Expr) => PosOrNeg(e))
  def negative[_:P]: P[Signed] = P("-" ~ ws.rep ~ term).map((e: Expr) => Negative(e))
  def positive[_:P]: P[Signed] = P("+".? ~ ws.rep ~ term).map((e: Expr) => Positive(e))

  def term[_:P]: P[Expr] = P(factor ~ (!(binary|mapSym) ~ (("*"| "\\times" | "\\cdot") ~ ws.rep).? ~ term).?).map(
    (t:(Expr,Option[Expr])) => t._2 match {
      case Some(e) => Multiply(t._1,e)
      case _ => t._1
    }
  )
  def factor[_:P]: P[Expr] = P(funcOpr ~ (("/"| "\\div")~ ws.rep ~ funcOpr).?).map(
    (t:(Expr,Option[Expr])) => t._2 match {
      case Some(e) => Divide(t._1,e)
      case _ => t._1
    }
  )
  def funcOpr[_:P]: P[Expr] = P(token ~ ("(" ~ expr.rep(sep= ",").map(_.toVector) ~ ")" ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep).?).map(
      (t:(Expr,Option[(Vector[Expr],Vector[SymAttr])])) => t._2 match {
        case Some((xe,xa)) => FuncOperation(t._1,xe,xa)
        case _ => t._1
      }
    )
  def token[_:P]: P[Expr] = P(cases | matrix | arrMatrix | set | tuple | paren | sqrt |
    fraction | binomial | decimal | numeral | mathText | formatted | symbol | variable)

  def numeral[_:P]: P[Numeral] = P(CharIn("0-9").rep(1).! ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(String,Vector[SymAttr])) => Numeral(t._1,t._2))
  def decimal[_:P]: P[Decimal] = P((CharIn("0-9").rep(1) ~ "." ~ CharIn("0-9").rep(1)).! ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(String,Vector[SymAttr])) => Decimal(t._1,t._2))
  def variable[_:P]: P[Variable] = P(CharIn("a-zA-Z").! ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(String,Vector[SymAttr])) => Variable(t._1,t._2))
  def mathText[_:P]: P[MathText] = P("\\" ~
    StringIn("textnormal","textrm","textit","textbf","textsf","text") ~
    "{" ~ (!("}") ~ AnyChar).rep.! ~ "}" ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(String,Vector[SymAttr])) => MathText(t._1,t._2))
  def symbol[_:P]: P[Sym] = P(!arrow ~ "\\" ~ symName ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(String,Vector[SymAttr])) => Sym(t._1,t._2))
  def paren[_:P]: P[Expr] = P("(" ~ expr ~ ")" ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(Expr,Vector[SymAttr])) => Paren(t._1,t._2))
  def fraction[_:P]: P[Fraction] = P("\\" ~
    ("tfrac" | "dfrac" | "frac" | "sfrac" | "cfrac" | "nicefrac") ~
    "{" ~ expr ~ "}{" ~ expr ~ "}" ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(Expr,Expr,Vector[SymAttr])) => Fraction(t._1,t._2,t._3))
  def sqrt[_:P]: P[Sqrt] = P("\\sqrt" ~ ("[" ~ expr ~ "]").? ~ "{" ~ expr ~ "}" ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(Option[Expr],Expr,Vector[SymAttr])) => Sqrt(t._1,t._2,t._3))
  def binomial[_:P]: P[Binomial] = P("\\binom" ~ "{" ~ expr ~ "}{" ~ expr ~ "}" ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(Expr,Expr,Vector[SymAttr])) => Binomial(t._1,t._2,t._3))
  def formatted[_:P]: P[Formatted] = P((
    "\\" ~ StringIn("mathnormal","mathrm","mathit","mathbf","mathsf","mathtt",
      "mathfrak","mathcal","mathbb","mathscr","mathord","mathop").! ~ expr |
    "\\" ~ symName ~ "{" ~ expr ~ "}" |
    "{" ~ "\\" ~ symName ~ ws.rep ~ expr ~ "}") ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(String,Expr,Vector[SymAttr])) => Formatted(t._1,t._2,t._3))
  def set[_:P] = setByElems | setByProps
  def tuple[_:P]: P[Tuple] = P("(" ~ expr.rep(min= 2, sep= ",").map(_.toVector) ~ ")" ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(Vector[Expr],Vector[SymAttr])) => Tuple(t._1,t._2))
  def cases[_:P]: P[Cases] = P("\\begin{cases}" ~
    (expr ~ ",".? ~ ws.rep ~ "&" ~ mathLine).rep(sep= "\\\\").map(_.toVector) ~
    ws.rep ~ "\\end{cases}" ~ ws.rep)
    .map(_.map((t:(Expr,Vector[MathPhrase])) => if(t._2.isEmpty) {t._1} else {SuchThat(t._1,t._2)}))
    .map((xs: Vector[MathPhrase]) => Cases(xs))
  def matrix[_:P]: P[Matrix] = P("\\begin{" ~
    StringIn("matrix","vmatrxix","Vmatrix","bmatrix","Bmatrix","pmatrix") ~ "}" ~
    mathLine.rep(sep= "&").map(_.toVector).rep(sep= "\\\\").map(_.toVector) ~
    ws.rep ~ "\\end{" ~ StringIn("matrix","vmatrxix","Vmatrix","bmatrix","Bmatrix","pmatrix") ~ "}" ~ ws.rep)
    .map(Matrix(_))
  def arrMatrix[_:P]: P[Matrix] = P("\\begin{array}" ~ curlyBox.? ~
    mathLine.rep(sep= "&").map(_.toVector).rep(sep= "\\\\").map(_.toVector) ~
    ws.rep ~ "\\end{array}" ~ curlyBox.? ~ ws.rep)
    .map(Matrix(_))


  def setByElems[_:P]: P[SetByElems] = P("\\{" ~ mathLine.rep(sep= ",").map(_.toVector.flatten) ~ "\\}" ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(Vector[MathPhrase],Vector[SymAttr])) => SetByElems(t._1,t._2))
  def setByProps[_:P]: P[SetByProps] = P("\\{" ~ suchThat ~ "\\}" ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(SuchThat,Vector[SymAttr])) => SetByProps(t._1,t._2))

  def symName[_:P]: P[String] = P(CharIn("a-zA-Z") | CharIn("0-9")).rep(1).!
  def symAttr[_:P]: P[SymAttr] = P( subLine | superLine | subscript | superscript | sqBox /*| limits*/)
  def subLine[_:P]: P[Subscript] = P("_" ~ " ".rep ~ "{" ~ mathLine.rep(sep= ",").map(_.toVector.flatten) ~ "}")
    .map((xs: Vector[MathPhrase]) => Subscript(xs))
  def superLine[_:P]: P[Superscript] = P("^" ~ " ".rep ~ "{" ~ mathLine.rep(sep= ",").map(_.toVector.flatten) ~ "}")
    .map((xs: Vector[MathPhrase]) => Superscript(xs))
  def subscript[_:P]: P[Subscript] = P("_" ~ " ".rep ~ singleChar)
    .map((xs: Vector[MathPhrase]) => Subscript(xs))
  def superscript[_:P]: P[Superscript] = P(apostrophe | "^" ~ " ".rep ~ singleChar)
    .map((xs: Vector[MathPhrase]) => Superscript(xs))
  def sqBox[_:P]: P[SqBox] = P("[" ~ mathLine.rep(sep= ",").map(_.toVector.flatten) ~ "]")
    .map((xs: Vector[MathPhrase]) => SqBox(xs))
//  def limits[_:P]: P[Limits] = P("\\limits" ~ " ".rep ~ (!symArg ~ symAttr).rep.map(_.toVector))
//    .map((xs: Vector[SymAttr]) => Limits(xs))

  def apostrophe[_:P]: P[Vector[MathPhrase]] = P(" ".rep ~ "'".!)
    .map((s: String) => Vector(Positive(Variable(s,Vector()))))
  def singleChar[_:P]: P[Vector[MathPhrase]] = P(
    ("\\" ~ symName).map((s: String) => Vector(Positive(Sym(s,Vector())))) |
    CharIn("0-9").!.map((s:String) => Vector(Positive(Numeral(s,Vector())))) |
    AnyChar.!.map((s: String) => Vector(Positive(Variable(s,Vector()))))
  )


}

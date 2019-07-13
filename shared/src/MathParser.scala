package partex
import fastparse._, NoWhitespace._

object MathParser{
  import MathLang._
  def ws[_:P]: P[Unit] = P(resrvdWd | " " | "\n" )
  def resrvdWd[_:P]: P[Unit] = P(StringIn("\\begin{split}","\\end{split}","\\displaystyle",
    "\\textstyle","\\quad","\\qquad","\\thinmuskip","\\thickmuskip","\\scriptstyle",
    "\\scriptscriptstyle","\\left","\\right","\\middle","\\bigl","\\bigr","\\Bigl","\\Bigr",
    "\\biggl","\\biggr","\\Biggl","\\Biggr","\\underbrace","&","\\\\","\\,","\\:","\\;","\\!","\\ "))

  def mathLine[_:P]: P[MathLine] = P(expr ~ (binRelation ~ expr).rep.map(_.toVector)).map(
    (t:(Expr,Vector[(String,Expr)])) =>
      if (t._2.size > 0) {
        MathLine(
          (("=",t._1) +: t._2).sliding(2).toVector.map(
            (p: Vector[(String,Expr)]) => getMathPhrase(p(0)._2, p(1)._1 , p(1)._2)
          )
        )
      }
      else { MathLine(Vector(t._1)) }
  )

  def fullMathLine[_:P] : P[Vector[MathLine]] = P(mathLine.rep(sep= ",").map(_.toVector) ~ End)

  def parseMath(s: String) : Parsed[Vector[MathLine]] = parse(s, fullMathLine(_))

  def getMath(s:String): Option[Vector[MathLine]] = parseMath(s).fold({case (_, _, _) => None}, {case (exp, _) => Some(exp)})

  def binRelation[_:P]: P[String] = P(StringIn("=","\\neq","\\ne","<","\\leqslant",
    "\\leq",">","\\geqslant","\\geq","\\subset","\\subseteq","\\not\\subset","\\nsubseteq",
    "\\supset","\\supseteq","\\not\\supset","\\nsupseteq").!)
  def getMathPhrase(e1: Expr, r: String, e2: Expr): MathPhrase =
    if (r == "=") { Equality(e1,e2) }
    else if (Vector("\\neq","\\ne").contains(r)) { Inequality(e1,e2) }
    else if (Vector("\\leqslant","\\leq").contains(r)) { LessThanEqual(e1,e2) }
    else if (Vector("\\geqslant","\\geq").contains(r)) { GreaterThanEqual(e1,e2) }
    else if (r == "<") { LessThan(e1,e2) }
    else if (r == ">") { GreaterThan(e1,e2) }
    else if (r == "\\subset") { SubsetPrpr(e1,e2) }
    else if (r == "\\subseteq") { Subset(e1,e2) }
    else if (r == "\\not\\subset") { NotSubsetPrpr(e1,e2) }
    else if (r == "\\nsubseteq") { NotSubset(e1,e2) }
    else if (r == "\\supset") { SupsetPrpr(e1,e2) }
    else if (r == "\\supseteq") { Supset(e1,e2) }
    else if (r == "\\not\\supset") { NotSupsetPrpr(e1,e2) }
    else { NotSupset(e1,e2) }

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
  def binOperation[_:P]: P[String] = P(StringIn("+","-","\\pm","\\cap","\\cup","\\setminus").!)
  def binary[_:P]: P[String] = P(binRelation | binOperation)

  def signed[_:P]: P[Expr] = P(posOrNeg | negative | positive)
  def posOrNeg[_:P]: P[Signed] = P("\\pm" ~ ws.rep ~ term).map((e: Expr) => PosOrNeg(e))
  def negative[_:P]: P[Signed] = P("-" ~ ws.rep ~ term).map((e: Expr) => Negative(e))
  def positive[_:P]: P[Signed] = P("+".? ~ ws.rep ~ term).map((e: Expr) => Positive(e))

  def term[_:P]: P[Expr] = P(factor ~ (!binary ~ (("*"| "\\times" | "\\cdot") ~ ws.rep).? ~ term).?).map(
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
  def token[_:P]: P[Expr] = P(set | paren | sqrt | fraction | decimal | numeral |
    mathText | formatted | symbol | variable)

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
  def symbol[_:P]: P[Sym] = P("\\" ~ symName ~
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
  def formatted[_:P]: P[Formatted] = P((
    "\\" ~ StringIn("mathnormal","mathrm","mathit","mathbf","mathsf","mathtt",
      "mathfrak","mathcal","mathbb","mathscr").! ~ expr |
    "\\" ~ symName ~ "{" ~ expr ~ "}" |
    "{" ~ "\\" ~ symName ~ ws.rep ~ expr ~ "}") ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(String,Expr,Vector[SymAttr])) => Formatted(t._1,t._2,t._3))
  def set[_:P]: P[Set] = P("\\{" ~ expr.rep(sep= ",").map(_.toVector) ~ "\\}" ~
    ws.rep ~ symAttr.rep.map(_.toVector) ~ ws.rep)
    .map((t:(Vector[Expr],Vector[SymAttr])) => Set(t._1,t._2))

  def symName[_:P]: P[String] = P(CharIn("a-zA-Z") | CharIn("0-9")).rep(1).!
  def symAttr[_:P]: P[SymAttr] = P( subExpr | superExpr | subscript | superscript /*| limits*/)
  def subExpr[_:P]: P[Subscript] = P("_" ~ " ".rep ~ "{" ~ expr ~ "}").map((e: Expr) => Subscript(e))
  def superExpr[_:P]: P[Superscript] = P("^" ~ " ".rep ~ "{" ~ expr ~ "}").map((e: Expr) => Superscript(e))
  def subscript[_:P]: P[Subscript] = P("_" ~ " ".rep ~ singleChar).map((e: Expr) => Subscript(e))
  def superscript[_:P]: P[Superscript] = P(apostrophe | "^" ~ " ".rep ~ singleChar).map((e: Expr) => Superscript(e))
//  def limits[_:P]: P[Limits] = P("\\limits" ~ " ".rep ~ (!symArg ~ symAttr).rep.map(_.toVector))
//    .map((xs: Vector[SymAttr]) => Limits(xs))

  def apostrophe[_:P]: P[Expr] = P(" ".rep ~ "'".!).map((s: String) => Variable(s,Vector()))
  def singleChar[_:P]: P[Expr] = P(
    ("\\" ~ symName).map((s: String) => Sym(s,Vector())) |
    CharIn("0-9").!.map((s:String) => Numeral(s,Vector())) |
    AnyChar.!.map((s: String) => Variable(s,Vector()))
  )


}

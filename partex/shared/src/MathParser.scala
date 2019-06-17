package partex.shared
import fastparse._, NoWhitespace._

object MathParser{
  import MathLang._
  def ws[_:P]: P[Unit] = P(" " | "\\quad" | "\n" | "\\ " )


  def numeral[_:P]: P[Numeral] = P(CharIn("0-9").rep(1).! ~
    " ".rep ~ symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(String,Vector[SymAttr])) => Numeral(t._1,t._2))
  def decimal[_:P]: P[Decimal] = P((CharIn("0-9").rep(1) ~ "." ~ CharIn("0-9").rep(1)).! ~
    " ".rep ~ symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(String,Vector[SymAttr])) => Decimal(t._1,t._2))
  def variable[_:P]: P[Variable] = P(CharIn("a-zA-Z").! ~
    " ".rep ~ symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(String,Vector[SymAttr])) => Variable(t._1,t._2))
  def mathText[_:P]: P[MathText] = P("\\text{" ~ (!("}") ~ AnyChar).rep.! ~ "}" ~
    " ".rep ~ symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(String,Vector[SymAttr])) => MathText(t._1,t._2))
  def symbol[_:P]: P[Sym] = P("\\" ~ symName ~
    " ".rep ~ symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(String,Vector[SymAttr])) => Sym(t._1,t._2))
  def paren[_:P]: P[Expr] = P("(" ~ expr ~ ")" ~
    " ".rep ~ symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(Expr,Vector[SymAttr])) => Paren(t._1,t._2))
  def fraction[_:P]: P[Fraction] = P("\\" ~ ("tfrac" | "dfrac" | "frac") ~ "{" ~ expr ~ "}{" ~ expr ~ "}" ~
    " ".rep ~ symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(Expr,Expr,Vector[SymAttr])) => Fraction(t._1,t._2,t._3))
  def sqrt[_:P]: P[Sqrt] = P("\\sqrt" ~ ("[" ~ expr ~ "]").? ~ "{" ~ expr ~ "}" ~
    " ".rep ~ symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(Option[Expr],Expr,Vector[SymAttr])) => Sqrt(t._1,t._2,t._3))


  def symName[_:P]: P[String] = P(CharIn("a-zA-Z") | CharIn("0-9")).rep(1).!
  def symAttr[_:P]: P[SymAttr] = P( /*symArg |*/ subExpr | superExpr | subscript | superscript /*| limits*/)
//  def symArg[_:P]: P[SymArg] = P(("{" | "(") ~ expr ~ ("}" | ")")).map((e: Expr) => SymArg(e))
  def subExpr[_:P]: P[Subscript] = P("_" ~ " ".rep ~ "{" ~ expr ~ "}").map((e: Expr) => Subscript(e))
  def superExpr[_:P]: P[Superscript] = P("^" ~ " ".rep ~ "{" ~ expr ~ "}").map((e: Expr) => Superscript(e))
  def subscript[_:P]: P[Subscript] = P("_" ~ " ".rep ~ singleChar).map((e: Expr) => Subscript(e))
  def superscript[_:P]: P[Superscript] = P("^" ~ " ".rep ~ singleChar).map((e: Expr) => Superscript(e))
//  def limits[_:P]: P[Limits] = P("\\limits" ~ " ".rep ~ (!symArg ~ symAttr).rep.map(_.toVector))
//    .map((xs: Vector[SymAttr]) => Limits(xs))

  def singleChar[_:P]: P[Expr] = P(
    ("\\" ~ symName).map((s: String) => Sym(s,Vector())) |
    CharIn("0-9").!.map((s:String) => Numeral(s,Vector())) |
    AnyChar.!.map((s: String) => Variable(s,Vector()))
  )

  def equation[_:P]: P[Equation] = P(expr ~ "=" ~ expr).map((t:(Expr,Expr)) => Equation(t._1,t._2))

  def expr[_:P]: P[Expr] = P(ws.rep ~ (add | sub | plusMinus | signed))
  def add[_:P]: P[Expr] = P(signed ~ "+" ~ expr).map((t:(Expr,Expr)) => Add(t._1,t._2))
  def sub[_:P]: P[Expr] = P(signed ~ "-" ~ expr).map((t:(Expr,Expr)) => Subtract(t._1,t._2))
  def plusMinus[_:P]: P[Expr] = P(signed ~ "\\pm" ~ expr).map((t:(Expr,Expr)) => PlusMinus(t._1,t._2))
  def signed[_:P]: P[Expr] = P(posOrNeg | negative | positive)
  def posOrNeg[_:P]: P[Signed] = P("\\pm" ~ ws.rep ~ multiply).map((e: Expr) => PosOrNeg(e))
  def negative[_:P]: P[Signed] = P("-" ~ ws.rep ~ multiply).map((e: Expr) => Negative(e))
  def positive[_:P]: P[Signed] = P("+".? ~ ws.rep ~ multiply).map((e: Expr) => Positive(e))

  def multiply[_:P]: P[Expr] = P(
    (divide ~ (("*"| "\\times" | "\\cdot") ~ ws.rep).? ~ multiply).map((t:(Expr,Expr)) => Multiply(t._1,t._2)) |
    divide
  )
  def divide[_:P]: P[Expr] = P(
    (token ~ ("/"| "\\div") ~ ws.rep ~ token).map((t:(Expr,Expr)) => Divide(t._1,t._2)) |
    token
  )
  def token[_:P]: P[Expr] = P(paren | sqrt | fraction | decimal | numeral | mathText | symbol | variable)

}
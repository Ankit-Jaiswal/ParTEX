package partex.shared
import fastparse._, NoWhitespace._

object MathParser{
  import MathLang._

  def ws[_:P]: P[Unit] = P(" " | "\n")
  def numeral[_:P]: P[Numeral] = P(CharIn("0-9").rep(1).! ~ symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(String,Vector[SymAttr])) => Numeral(t._1,t._2))
  def letter[_:P]: P[Letter] = P(CharIn("a-zA-Z").! ~ symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(String,Vector[SymAttr])) => Letter(t._1,t._2))
  def mathText[_:P]: P[MathText] = P("\\text{" ~ (!("}") ~ AnyChar).rep.! ~ "}" ~
    symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(String,Vector[SymAttr])) => MathText(t._1,t._2))
  def symbol[_:P]: P[Sym] = P("\\" ~ symName ~ symAttr.rep.map(_.toVector) ~ (&("\\")|ws.rep))
    .map((t:(String,Vector[SymAttr])) => Sym(t._1,t._2))

  def symName[_:P]: P[String] = P(CharIn("a-zA-Z") | CharIn("0-9")).rep(1).!
  def symAttr[_:P]: P[SymAttr] = P(symArg | subExpr | superExpr | subscript | superscript)
  def symArg[_:P]: P[SymArg] = P(("{" | "(") ~ addSub ~ ("}" | ")")).map((e: AddSub) => SymArg(e))
  def subExpr[_:P]: P[SubExpr] = P("_{" ~ addSub ~ "}").map((e: AddSub) => SubExpr(e))
  def superExpr[_:P]: P[SuperExpr] = P("^{" ~ addSub ~ "}").map((e: AddSub) => SuperExpr(e))
  def subscript[_:P]: P[Subscript] = P("_" ~ singleChar).map((t: Token) => Subscript(t))
  def superscript[_:P]: P[Superscript] = P("^" ~ singleChar).map((t: Token) => Superscript(t))
  def singleChar[_:P]: P[Token] = P(
    CharIn("0-9").!.map((s: String) => Numeral(s,Vector())) |
    CharIn("a-zA-Z").!.map((s: String) => Letter(s,Vector())) |
    ("\\" ~ symName).map((s: String) => Sym(s,Vector()))
  )

  def expr[_:P]: P[Expr] = P(ws.rep ~ equalled).map((e: Equalled) => Expr(e))
  def equalled[_:P]: P[Equalled] = P(addSub.rep(min=1, sep= "=").map(_.toVector))
    .map((xs: Vector[AddSub]) => Equalled(xs))
  def addSub[_:P]: P[AddSub] = P(signed.rep(1).map(_.toVector))
    .map((xs: Vector[Signed]) => AddSub(xs))
  def signed[_:P]: P[Signed] = P(("+" | "-").!.? ~ multiplied)
    .map((t:(Option[String],Multiplied)) => Signed(t._1,t._2))
  def multiplied[_:P]: P[Multiplied] = P(divided ~ ("*".? ~ divided).rep.map(_.toVector))
    .map((t:(Divided,Vector[Divided])) => Multiplied(t._1 +: t._2))
  def divided[_:P]: P[Divided] = P(ws.rep ~ token ~ ("/" ~ token).?)
    .map((t:(Token,Option[Token])) => Divided(t._1,t._2))
  def token[_:P]: P[Token] = P(numeral | letter | mathText | symbol | paren)
  def paren[_:P]: P[Paren] = P("(" ~ addSub ~ ")" ~ symAttr.rep.map(_.toVector))
    .map((t:(AddSub,Vector[SymAttr])) => Paren(t._1,t._2))

}

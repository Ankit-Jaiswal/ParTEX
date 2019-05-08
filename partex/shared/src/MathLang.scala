package partex.shared

object MathLang {
  case class Expr(e: Equalled)
  case class Equalled(xs: Vector[AddSub])
  case class AddSub(xs: Vector[Signed])
  case class Signed(s: Option[String], value: Multiplied)
  case class Multiplied(xs: Vector[Divided])
  case class Divided(t1: Token, t2: Option[Token])
  sealed trait Token{
    val xs: Vector[SymAttr]
  }

  case class Numeral(s: String, xs: Vector[SymAttr]) extends Token
  case class Letter(s: String, xs: Vector[SymAttr]) extends Token
  case class Sym(name: String, xs: Vector[SymAttr]) extends Token
  case class MathText(s: String, xs: Vector[SymAttr]) extends Token
  case class Paren(value: AddSub, xs: Vector[SymAttr]) extends Token

  sealed trait SymAttr
  case class SymArg(value: AddSub) extends SymAttr
  case class SubExpr(value: AddSub) extends SymAttr
  case class SuperExpr(value: AddSub) extends SymAttr
  case class Subscript(value: Token) extends SymAttr
  case class Superscript(value: Token) extends SymAttr

}

package partex.shared

object MathLang {
  case class Equation(e1: Expr, e2: Expr)

  sealed trait Expr
  case class Numeral(s: String, xs: Vector[SymAttr]) extends Expr
  case class Identifier(s: String, xs: Vector[SymAttr]) extends Expr
  case class Sym(name: String, xs: Vector[SymAttr]) extends Expr
  case class MathText(s: String, xs: Vector[SymAttr]) extends Expr
  sealed trait Signed extends Expr
  case class Paren(e: Expr, xs: Vector[SymAttr]) extends Expr
  case class Add(e1: Expr, e2: Expr) extends Expr
  case class Subtract(e1: Expr, e2: Expr) extends Expr
  case class Multiply(e1: Expr, e2: Expr) extends Expr
  case class Divide(e1: Expr, e2: Expr) extends Expr

  case class Positive(e: Expr) extends Signed
  case class Negative(e: Expr) extends Signed

  sealed trait SymAttr
  case class SymArg(e: Expr) extends SymAttr
  case class Subscript(e: Expr) extends SymAttr
  case class Superscript(e: Expr) extends SymAttr
  case class Limits(xs: Vector[SymAttr]) extends SymAttr

}

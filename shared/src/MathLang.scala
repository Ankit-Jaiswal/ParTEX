package partex

object MathLang {
  case class MathLine(xs: Vector[MathPhrase])
  sealed trait MathPhrase
  sealed trait BinaryRel
  sealed trait Expr extends MathPhrase
  case class Equality(e1: Expr, e2: Expr) extends MathPhrase
  case class Inequality(e1: Expr, e2: Expr) extends MathPhrase
  case class LessThan(e1: Expr, e2: Expr) extends MathPhrase
  case class GreaterThan(e1: Expr, e2: Expr) extends MathPhrase
  case class LessThanEqual(e1: Expr, e2: Expr) extends MathPhrase
  case class GreaterThanEqual(e1: Expr, e2: Expr) extends MathPhrase
  case class SubsetPrpr(e1: Expr, e2: Expr) extends MathPhrase
  case class Subset(e1: Expr, e2: Expr) extends MathPhrase
  case class NotSubsetPrpr(e1: Expr, e2: Expr) extends MathPhrase
  case class NotSubset(e1: Expr, e2: Expr) extends MathPhrase
  case class SupsetPrpr(e1: Expr, e2: Expr) extends MathPhrase
  case class Supset(e1: Expr, e2: Expr) extends MathPhrase
  case class NotSupsetPrpr(e1: Expr, e2: Expr) extends MathPhrase
  case class NotSupset(e1: Expr, e2: Expr) extends MathPhrase
  case class BelongsTo(e1: Expr, e2: Expr) extends MathPhrase
  case class MapsTo(e1: Expr, e2: Expr) extends MathPhrase
  case class SuchThat(e: Expr, xs: Vector[MathLine]) extends MathPhrase


  case class Numeral(s: String, xs: Vector[SymAttr]) extends Expr
  case class Decimal(s: String, xs: Vector[SymAttr]) extends Expr
  case class Variable(s: String, xs: Vector[SymAttr]) extends Expr
  case class Sym(name: String, xs: Vector[SymAttr]) extends Expr
  case class MathText(s: String, xs: Vector[SymAttr]) extends Expr
  sealed trait Signed extends Expr
  case class Fraction(e1: Expr, e2: Expr, xs: Vector[SymAttr]) extends Expr
  case class Sqrt(optExpr: Option[Expr], e: Expr, xs: Vector[SymAttr]) extends Expr
  case class Paren(e: Expr, xs: Vector[SymAttr]) extends Expr
  case class Add(e1: Expr, e2: Expr) extends Expr
  case class Subtract(e1: Expr, e2: Expr) extends Expr
  case class PlusMinus(e1: Expr, e2: Expr) extends Expr
  case class Multiply(e1: Expr, e2: Expr) extends Expr
  case class Divide(e1: Expr, e2: Expr) extends Expr
  case class FuncOperation(e1: Expr, args: Vector[Expr], xs: Vector[SymAttr]) extends Expr
  case class Formatted(frmt: String, e: Expr, xs: Vector[SymAttr]) extends Expr
  sealed trait Set extends Expr{
    val xs: Vector[SymAttr]
  }
  case class Tuple(elems: Vector[Expr], xs: Vector[SymAttr]) extends Expr
  case class Union(e1: Expr, e2: Expr) extends Expr
  case class Intersection(e1: Expr, e2: Expr) extends Expr
  case class SetMinus(e1: Expr, e2: Expr) extends Expr

  case class Positive(e: Expr) extends Signed
  case class Negative(e: Expr) extends Signed
  case class PosOrNeg(e: Expr) extends Signed

  case class SetByElems(elems: Vector[Expr], xs: Vector[SymAttr]) extends Set
  case class SetByProps(prop: SuchThat, xs: Vector[SymAttr]) extends Set

  sealed trait SymAttr
  case class SymArg(e: Expr) extends SymAttr
  case class Subscript(e: Expr) extends SymAttr
  case class Superscript(e: Expr) extends SymAttr
  case class Limits(xs: Vector[SymAttr]) extends SymAttr

}
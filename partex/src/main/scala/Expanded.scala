package partex

object Expanded {

  case class Document(top: TopMatter, abs: Abstract, body: Body)

  case class TopMatter(title: Option[String], date: Option[String],
    author: Option[String], info: Vector[String])
  case class Abstract(s: Option[String])
  case class Body(elems: Vector[BodyElem])

  sealed trait BodyElem
  case class Header(name: String, value: String) extends BodyElem
  case class Paragraph(frgs: Vector[Fragment]) extends BodyElem
  case class Environment(name: String, value: Body) extends BodyElem
  case class Theorem(name: String, value: Body) extends BodyElem
  case class DisplayMath(value: String) extends BodyElem with Math
  case class Verbatim(value: String) extends BodyElem
  sealed trait TexList extends BodyElem
//  case class Table() extends BodyElem
//  case class Image() extends BodyElem

  case class Ordered(name: String, xs: Vector[Body]) extends TexList
  case class Unordered(name: String,xs: Vector[Body]) extends TexList
  case class Custom(name: String,xs: Vector[Body]) extends TexList

  sealed trait Math

  sealed trait Fragment
  case class Text(s: String) extends Fragment
  case class InlineCmd(name: String, s: String) extends Fragment
  case class InlineMath(s: String) extends Fragment with Math
  sealed trait Styled extends Fragment
//  case class References() extends Fragment
//  case class Note() extends Fragment
//  case class Span() extends Fragment

  case class Emph(s: String) extends Styled
  case class Strong(s: String) extends Styled
  case class Strikeout(s: String) extends Styled
  case class Superscript(s: String) extends Styled
  case class Subscript(s: String) extends Styled
  case class SmallCaps(s: String) extends Styled
  case class Quoted(s: String) extends Styled
  sealed trait Align extends Styled




}

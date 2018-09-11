package partex

object Expanded {

  sealed trait Math
  sealed trait Float
  case class Document(top: TopMatter, body: Body)

  case class TopMatter(title: Option[String], date: Option[String],
    author: Option[String], abs: Option[String])

  case class Body(elems: Vector[BodyElem])

  sealed trait BodyElem
  case class Paragraph(frgs: Vector[Fragment]) extends BodyElem
  case class Command(name: String, s: String) extends BodyElem with Fragment
  case class Header(name: String, value: String) extends BodyElem
  case class Graphics(src: String, width: String) extends BodyElem with Float
  case class Environment(name: String, value: Body) extends BodyElem
  case class Theorem(name: String, value: Body) extends BodyElem
  case class DisplayMath(value: String) extends BodyElem with Math
  case class Figure(caption: Option[String], incl: Graphics) extends BodyElem with Float
  case class Code(value: String) extends BodyElem
  case class Table(caption: Option[String], tb: Vector[Rows]) extends BodyElem with Float
  sealed trait TexList extends BodyElem

  case class Ordered(name: String, xs: Vector[Body]) extends TexList
  case class Unordered(name: String,xs: Vector[Body]) extends TexList
  case class Custom(name: String,xs: Vector[Body]) extends TexList

  case class Rows(tr: Vector[Paragraph])

  sealed trait Fragment
  case class Text(s: String) extends Fragment
  case class InlineMath(s: String) extends Fragment with Math
  case class Quoted(s: String) extends Fragment
  case class References(s: String) extends Fragment
  case class Note(s: String) extends Fragment
  sealed trait Styled extends Fragment

  case class Emph(s: String) extends Styled
  case class Strong(s: String) extends Styled
  case class Italic(s: String) extends Styled
  case class Strikeout(s: String) extends Styled
  case class Superscript(s: String) extends Styled
  case class Subscript(s: String) extends Styled
  case class SmallCaps(s: String) extends Styled


}

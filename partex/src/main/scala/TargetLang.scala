package partex

object TargetLang {

  sealed trait Labelable
  sealed trait Math
  sealed trait Float
  case class Document(top: Vector[MetaData], body: Body)

  case class Body(elems: Vector[BodyElem])

  sealed trait MetaData extends BodyElem
  case class Title(alias: Option[String], s: String) extends MetaData
  case class Abstract(alias: Option[String], s: String) extends MetaData
  case class Author(s: String) extends MetaData
  case class Address(s: String) extends MetaData
  case class Email(s: String) extends MetaData
  case class Date(s: String) extends MetaData
  case class Info(s: String) extends MetaData

  sealed trait BodyElem
  case class Paragraph(frgs: Vector[Fragment]) extends BodyElem with TableElem
  case class Command(name: String, value: String) extends BodyElem
  case class Heading(name: String, alias: Option[String], label: Option[String],
    value: String) extends BodyElem with Labelable
  case class Graphics(spec: Option[Map[String,String]], name: String)
    extends BodyElem with Float
  case class Environment(name: String, value: Body) extends BodyElem
  case class Theorem(name: String, alias: Option[String], label: Option[String],
    value: Body) extends BodyElem with Labelable
  case class Proof(alias: Option[String], label: Option[String], value: Body)
    extends BodyElem with Labelable
  case class DisplayMath(label: Option[String], value: String) extends BodyElem
    with Math with Labelable
  case class CodeBlock(spec: Option[Map[String,String]], value: String) extends BodyElem
  case class Figure(g: Graphics, cap: Option[String], label: Option[String])
    extends BodyElem with Float with Labelable
  case class Table(cap: Option[String], label: Option[String], tb: Vector[Rows])
    extends BodyElem with Float with Labelable
  sealed trait TexList extends BodyElem {
    val name: String
    val xs: Vector[Item]
  }

  case class Ordered(name: String, xs: Vector[Item]) extends TexList
  case class Unordered(name: String,xs: Vector[Item]) extends TexList
  case class Custom(name: String,xs: Vector[Item]) extends TexList
  case class Item(alias: Option[String], label: Option[String], value: Body) extends Labelable


  case class Rows(tr: Vector[TableElem])
  sealed trait TableElem
  case class MultiCol(span: Int, value: TableElem) extends TableElem
  case class MultiRow(span: Int, value: TableElem) extends TableElem
  case class ParBox(value: TableElem) extends TableElem


  sealed trait Fragment
  case class Text(s: String) extends Fragment
  case class InlineMath(s: String) extends Fragment with Math
  case class Phantom(label: Option[String]) extends Fragment with Labelable

}

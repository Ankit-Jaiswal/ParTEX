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
  case class Paragraph(frgs: Vector[Fragment]) extends BodyElem
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

  case class List(name: String, xs: Vector[Body]) extends BodyElem


  case class Rows(tr: Vector[Paragraph])
  case class Caption(s: String, label: Option[String]) extends Labelable


  sealed trait Fragment {
    val s : String
  }
  case class Text(s: String) extends Fragment
  case class InlineEq(s: String) extends Fragment
}

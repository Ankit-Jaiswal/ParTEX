package partex

object TargetLang {

  case class Document(body: Body)
  case class Body(elems: Vector[BodyElem])
  sealed trait BodyElem

  case class Text(s: String) extends BodyElem
  case class Command(name: String, value: String) extends BodyElem
  case class Enclosure(name: String, value: Body) extends BodyElem
}

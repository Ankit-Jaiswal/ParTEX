package partex
import scalatags.Text.all._

object TargetLang {

  sealed trait Labelable
  sealed trait Math
  sealed trait Float
  case class Document(top: Vector[MetaData], bd: Body){
    def toHTML: Frag =
      html(
        head(
          scalatags.Text.tags2.title("First Look !"),
          link(href:="main.css", rel:="stylesheet")
        ),
        body(css("margin"):="0px")(
          div(id:="topmatter")(
            table(tr(
              td(top.collectFirst({case m: Author => m.s})),
              td(top.collectFirst({case m: Title => m.s})),
              td(top.collectFirst({case m: Date => m.s})) )
            ),
            div(id:="abstract")(
              h3("Abstract"),
              p(top.collectFirst({case m: Abstract => m.s}))
            )
          ),
          div(id:="mainbody")(bd.toHTML)
        )
      )
  }

  sealed trait MetaData extends BodyElem{
    val s: String
    def toHTML: Frag = s
  }
  case class Title(alias: Option[String], s: String) extends MetaData
  case class Abstract(alias: Option[String], s: String) extends MetaData
  case class Author(s: String) extends MetaData
  case class Address(s: String) extends MetaData
  case class Email(s: String) extends MetaData
  case class Date(s: String) extends MetaData
  case class Info(s: String) extends MetaData



  case class Body(elems: Vector[BodyElem]){
    def toHTML: Frag =
      div(`class`:="body")(
        for(b <- elems) yield b.toHTML
      )
    }

  sealed trait BodyElem{
    def toHTML: Frag
  }
  case class Paragraph(frgs: Vector[Fragment]) extends BodyElem with TableElem{
    def toHTML: Frag =
      p(`class`:="paragraph")(
        for(f <- frgs) yield f.toHTML
      )
  }
  case class Command(name: String, value: String) extends BodyElem{
    def toHTML: Frag = div(`class`:="command")(h3(name+": "+value))
  }
  case class Heading(name: String, alias: Option[String], label: Option[String],
    value: String) extends BodyElem with Labelable{
      def toHTML: Frag = div(`class`:="heading")(span(`class`:=name)(name, ": ", value))
  }
  case class Graphics(spec: Option[Map[String,String]], name: String)
    extends BodyElem with Float{
      def toHTML: Frag = div(`class`:="graphics")(div(`class`:="img")())
  }
  case class Environment(name: String, value: Body) extends BodyElem{
    def toHTML: Frag =
      div(`class`:="environment")(
        div(`class`:="name")(name),
        div(`class`:="envbody")(value.toHTML)
      )
  }
  case class Theorem(name: String, alias: Option[String], label: Option[String],
    value: Body) extends BodyElem with Labelable{
      def toHTML: Frag =
        div(`class`:="theorem")(
          div(`class`:="name")(name,"\t\t", span(`class`:="alias")(alias)),
          value.toHTML
        )
  }
  case class Proof(alias: Option[String], label: Option[String], value: Body)
    extends BodyElem with Labelable{
      def toHTML: Frag =
        div(`class`:="proof")(
          div(`class`:="name")("proof", span(`class`:="alias")(alias)),
          div(`class`:="pfbody")(value.toHTML)
        )
  }
  case class DisplayMath(label: Option[String], value: String) extends BodyElem
    with Math with Labelable{
      def toHTML: Frag = div(`class`:="displaymath")(value)
  }
  case class CodeBlock(spec: Option[Map[String,String]], value: String) extends BodyElem{
    def toHTML: Frag = div(`class`:="codeBlock")(pre(code(value)))
  }
  case class Figure(g: Graphics, cap: Option[String], label: Option[String])
    extends BodyElem with Float with Labelable{
      def toHTML: Frag =
        div(`class`:="figure")(
          g.toHTML,
          div(`class`:="caption")(cap)
        )
  }
  case class Table(cap: Option[String], label: Option[String], tb: Vector[Rows])
    extends BodyElem with Float with Labelable{
      def toHTML: Frag =
        div(`class`:="table")(
          table( for(row <- tb) yield row.toHTML ),
          div(`class`:="caption")(cap)
        )
  }
  sealed trait TexList extends BodyElem {
    val name: String
    val xs: Vector[Item]
    def toHTML: Frag
  }

  case class Ordered(name: String, xs: Vector[Item]) extends TexList{
    def toHTML: Frag = ol(for(i <- xs) yield i.toHTML)
  }
  case class Unordered(name: String,xs: Vector[Item]) extends TexList{
    def toHTML: Frag = ul(for(i <- xs) yield i.toHTML)
  }
  case class Custom(name: String,xs: Vector[Item]) extends TexList{
    def toHTML: Frag = ul(`class`:="custom")(for(i <- xs) yield i.toHTML)
  }
  case class Item(alias: Option[String], label: Option[String], value: Body) extends Labelable{
    def toHTML: Frag = li(`class`:="item")(value.toHTML)
  }


  case class Rows(rs: Vector[TableElem]){
    def toHTML: Frag = tr(for(e <- rs) yield td(e.toHTML))
  }
  sealed trait TableElem {
    def toHTML: Frag
  }
  case class MultiCol(n: Int, value: TableElem) extends TableElem{
    def toHTML: Frag = span(`class`:="multicol")(value.toHTML)
  }
  case class MultiRow(n: Int, value: TableElem) extends TableElem{
    def toHTML: Frag = span(`class`:="multirow")(value.toHTML)
  }
  case class ParBox(value: TableElem) extends TableElem{
    def toHTML: Frag = span(`class`:="parbox")(value.toHTML)
  }


  sealed trait Fragment{
    def toHTML: Frag
  }
  case class Text(s: String) extends Fragment{
    def toHTML: Frag = s /*span().apply(s.split("\n\n").map((s: String) => span(s).apply(br)))*/
  }
  case class InlineMath(s: String) extends Fragment with Math{
    def toHTML: Frag = span(`class`:="inlinemath")(s)
  }
  case class Phantom(label: Option[String]) extends Fragment with Labelable{
    def toHTML: Frag = span(`class`:="phantom")("this")
  }
  case class Quoted(s: String) extends Fragment{
    def toHTML: Frag = "\""+s+"\""
  }
  case class Citation(s: String) extends Fragment{
    def toHTML: Frag = span(`class`:="citation")(sup(strong(s)))
  }
  case class Hypertarget(l: String, s: String) extends Fragment{
    def toHTML: Frag = span(`class`:="hypertarget")(s)
  }
  case class Hyperlink(l: String, s: String) extends Fragment{
    def toHTML: Frag = span(`class`:="hyperlink")(s)
  }
  case class Reference(s: String) extends Fragment{
    def toHTML: Frag = span(`class`:="reference")("this")
  }
  case class Note(s: String) extends Fragment{
    def toHTML: Frag = span(`class`:="note")(s)
  }
  sealed trait Styled extends Fragment{
    val s: Paragraph
    def toHTML: Frag
  }

  case class Emph(s: Paragraph) extends Styled{
    def toHTML: Frag = em(for(f <- s.frgs) yield f.toHTML)
  }
  case class Strong(s: Paragraph) extends Styled{
    def toHTML: Frag = strong(for(f <- s.frgs) yield f.toHTML)
  }
  case class Italic(s: Paragraph) extends Styled{
    def toHTML: Frag = span(`class`:="italic")(for(f <- s.frgs) yield f.toHTML)
  }
  case class Underline(s: Paragraph) extends Styled{
    def toHTML: Frag = span(`class`:="underline")(for(f <- s.frgs) yield f.toHTML)
  }
  case class Strikeout(s: Paragraph) extends Styled{
    def toHTML: Frag = span(`class`:="strikeout")(for(f <- s.frgs) yield f.toHTML)
  }
  case class Superscript(s: Paragraph) extends Styled{
    def toHTML: Frag = sup(for(f <- s.frgs) yield f.toHTML)
  }
  case class Subscript(s: Paragraph) extends Styled{
    def toHTML: Frag = sub(for(f <- s.frgs) yield f.toHTML)
  }
  case class SmallCaps(s: Paragraph) extends Styled{
    def toHTML: Frag = span(`class`:="smallcaps")(for(f <- s.frgs) yield f.toHTML)
  }

}

package partex
import scalatags.Text.all._

object TargetLang {
  val foot = new scalatags.text.Builder

  sealed trait AllElem
  sealed trait Labelable {
    val label: Option[String]
  }
  sealed trait Math
  sealed trait Float

  case class Document(top: Vector[MetaData], bd: Body){
    val headList = bd.elems.filter(isHeading).asInstanceOf[Vector[Heading]]

    val headNum = headList.filter((h: Heading) => h.name == "section").zipWithIndex
      .map((t:(Heading,Int)) => (t._1,(t._2+1).toString))
      .map(subsecBlock).flatten.toMap ++
      headList.filter((h: Heading) => h.name == "chapter").zipWithIndex
      .map((t:(Heading,Int)) => (t._1,(t._2+1).toString)).toMap ++
      headList.filter((h: Heading) => h.name == "part").zipWithIndex
      .map((t:(Heading,Int)) => (t._1,(t._2+1).toString)).toMap

    val headNumByName = headNum.map((t:(Heading,String)) => (t._1.name+t._1.value -> t._2))

    val labelNum = bd.elems.flatMap(hasLabel)
      .map((l: Labelable) => (l.label.get, getNum(l))).toMap

    def subsecBlock(t:(Heading,String)) = {
      val slice = headList.splitAt(headList.indexOf(t._1)+1)._2
        .takeWhile((h: Heading) => h.name!="section")

      t +: slice.filter((h: Heading) => h.name=="subsection").map((h: Heading) => (h,t._2+".")).zipWithIndex
        .map((tt:((Heading,String),Int)) => (tt._1._1, tt._1._2 + (tt._2+1).toString))
        .map(
          (t:(Heading,String)) => t +:
            slice.splitAt(slice.indexOf(t._1)+1)._2.takeWhile((h: Heading) => h.name!="subsection")
              .map((h: Heading) => (h,t._2+".")).zipWithIndex
              .map((tt:((Heading,String),Int)) => (tt._1._1, tt._1._2 + (tt._2+1).toString))
        ).flatten
      }

    def isHeading(b: BodyElem) = b match {
      case _: Heading => true
      case _ => false
    }

    def hasLabel(e: AllElem): Vector[Labelable] = e match {
      case x: Paragraph => x.frgs.flatMap(hasLabel)
      case x: TexList => x.xs.flatMap(hasLabel)
      case x: Environment => x.value.elems.flatMap(hasLabel)
      case x: Theorem => if(x.label!=None) {Vector(x)} else {Vector()} ++
        x.value.elems.flatMap(hasLabel)
      case x: Proof => if(x.label!=None) {Vector(x)} else {Vector()} ++
        x.value.elems.flatMap(hasLabel)
      case x: Labelable => if(x.label!=None) {Vector(x)} else {Vector()}
      case _ => Vector()
    }

    def getNum(l: Labelable) = l match {
      case h: Heading => headNum(h)
      case _ => "00"
    }

    def mapToJSobjectString(m: Map[String,String]): String =
      m.map((t:(String,String)) => "\""+t._1+"\""+": "+"\""+t._2+"\"").mkString("{", ", ", "};")

    val toHTML: Frag =
      html(
        head(
          scalatags.Text.tags2.title("First Look !"),
          link(href:="main.css", rel:="stylesheet"),
          script(src:="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML"),
          script("const headNum = ", raw(mapToJSobjectString(headNumByName)) ),
          script("const labelNum = ", raw(mapToJSobjectString(labelNum)) )
        ),
        body(css("margin"):="0px")(
          div(id:="topmatter")(
            a(href:="#topmatter")(table(tr(
              td(width:="25%")(top.collectFirst({case m: Author => m.toHTML})),
              td(width:="50%")(top.collectFirst({case m: Title => m.toHTML})),
              td(width:="25%")(top.collectFirst({case m: Date => m.toHTML}))
            ))),
            top.collectFirst({case m: Abstract => m.toHTML})
          ),
          div(id:="mainbody")(bd.toHTML),
          div(id:="footer")(
            ol(
              for(f <- foot.children.take(foot.childIndex)) yield li(f)
            )
          )
        )
      )
  }

  sealed trait MetaData extends BodyElem{
    val toHTML: Frag
  }
  case class Title(alias: Option[String], s: String) extends MetaData{
    val toHTML: Frag = s
  }
  case class Author(s: String) extends MetaData{
    val toHTML: Frag = s
  }
  case class Address(s: String) extends MetaData{
    val toHTML: Frag = s
  }
  case class Email(s: String) extends MetaData{
    val toHTML: Frag = s
  }
  case class Date(s: String) extends MetaData{
    val toHTML: Frag = s
  }
  case class Info(s: String) extends MetaData{
    val toHTML: Frag = s
  }

  case class Abstract(alias: Option[String], p: Paragraph) extends MetaData{
    val toHTML: Frag =
      div(id:="abstract")(
        h3("Abstract"),
        p.toHTML
      )
  }


  case class Body(elems: Vector[BodyElem]){
    val toHTML: Frag =
      div(`class`:="body")(
        for(b <- elems) yield b.toHTML
      )
    }

  sealed trait BodyElem extends AllElem{
    val toHTML: Frag
  }
  case class Paragraph(frgs: Vector[Fragment]) extends BodyElem with TableElem{
    val toHTML: Frag =
      p(`class`:="paragraph")(
        for(f <- frgs) yield f.toHTML
      )
  }
  case class Command(name: String, value: String) extends BodyElem{
    val toHTML: Frag = div(`class`:="command")(h3(name+": "+value))
  }
  case class Heading(name: String, alias: Option[String], label: Option[String],
    value: String) extends BodyElem with Labelable{
      val idValue = name + value.split(" ").mkString("_")
      val toHTML: Frag =
        div(`class`:="heading")(
          span(`class`:=name)(
            span(id:=idValue)(script(
              raw("document.getElementById(\""),idValue, raw("\").innerHTML = "),
              raw("headNum[\""),name+value,raw("\"]")
            )),
            value,
            alias.map((l: String) => "\t\t["+l+"]")
          )/*,
          label.map((l: String) => a(name:=l)()).getOrElse("")*/
        )
  }
  case class Graphics(spec: Option[Map[String,String]], name: String)
    extends BodyElem with Float{
      val toHTML: Frag = div(`class`:="graphics")(div(`class`:="img")())
  }
  case class Environment(name: String, value: Body) extends BodyElem{
    val toHTML: Frag =
      div(`class`:="environment")(
        div(`class`:="name")(name),
        div(`class`:="envbody")(value.toHTML)
      )
  }
  case class Theorem(name: String, alias: Option[String], label: Option[String],
    value: Body) extends BodyElem with Labelable{
      val toHTML: Frag =
        div(`class`:="theorem")(
          div(`class`:="name")(name, alias.map((l: String) => "\t\t["+l+"]")),
          value.toHTML
        )
  }
  case class Proof(alias: Option[String], label: Option[String], value: Body)
    extends BodyElem with Labelable{
      val toHTML: Frag =
        div(`class`:="proof")(
          div(`class`:="name")("proof", alias.map((l: String) => "\t\t["+l+"]")),
          div(`class`:="pfbody")(value.toHTML)
        )
  }
  case class DisplayMath(label: Option[String], value: String) extends BodyElem
    with Math with Labelable{
      val toHTML: Frag = div(`class`:="displaymath")("\\["+value+"\\]")
  }
  case class CodeBlock(label: Option[String], value: String) extends BodyElem with Labelable{
    val toHTML: Frag = div(`class`:="codeBlock")(pre(code(value)))
  }
  case class Figure(g: Graphics, cap: Option[String], label: Option[String])
    extends BodyElem with Float with Labelable{
      val toHTML: Frag =
        div(`class`:="figure")(
          g.toHTML,
          div(`class`:="caption")(cap)
        )
  }
  case class Table(cap: Option[String], label: Option[String], tb: Vector[Rows])
    extends BodyElem with Float with Labelable{
      val toHTML: Frag =
        div(`class`:="table")(
          table( for(row <- tb) yield row.toHTML ),
          div(`class`:="caption")(cap)
        )
  }
  sealed trait TexList extends BodyElem {
    val name: String
    val xs: Vector[Item]
    val toHTML: Frag
  }
  case class Bibliography(xs: Vector[BibItem]) extends BodyElem {
    val toHTML: Frag =
      div(`class`:="bibliography")(
        h2(`class`:="bibtitle")("References"),
        ol(for(i <- xs) yield i.toHTML)
      )
  }
  case class BibItem(name: String, value: Body){
    val toHTML: Frag = li(value.toHTML)
  }

  case class Ordered(name: String, xs: Vector[Item]) extends TexList{
    val toHTML: Frag = ol(for(i <- xs) yield i.toHTML)
  }
  case class Unordered(name: String,xs: Vector[Item]) extends TexList{
    val toHTML: Frag = ul(for(i <- xs) yield i.toHTML)
  }
  case class Custom(name: String,xs: Vector[Item]) extends TexList{
    val toHTML: Frag = ul(`class`:="custom")(for(i <- xs) yield i.toHTML)
  }
  case class Item(alias: Option[String], label: Option[String], value: Body) extends Labelable with AllElem{
    val toHTML: Frag = li(`class`:="item")(value.toHTML)
  }


  case class Rows(rs: Vector[TableElem]){
    val toHTML: Frag = tr(for(e <- rs) yield td(e.toHTML))
  }
  sealed trait TableElem {
    val toHTML: Frag
  }
  case class MultiCol(n: Int, value: TableElem) extends TableElem{
    val toHTML: Frag = span(`class`:="multicol")(value.toHTML)
  }
  case class MultiRow(n: Int, value: TableElem) extends TableElem{
    val toHTML: Frag = span(`class`:="multirow")(value.toHTML)
  }
  case class ParBox(value: TableElem) extends TableElem{
    val toHTML: Frag = span(`class`:="parbox")(value.toHTML)
  }


  sealed trait Fragment extends AllElem{
    val toHTML: Frag
  }
  case class Text(s: String) extends Fragment{
    val toHTML: Frag = span(`class`:="text")(s)
    /*span().apply(s.split("\n\n").map((s: String) => span(s).apply(br)))*/
  }
  case class InlineMath(s: String) extends Fragment with Math{
    val toHTML: Frag = span(`class`:="inlinemath")("\\("+s+"\\)")
  }
  case class Phantom(label: Option[String]) extends Fragment with Labelable{
    val toHTML: Frag = span(`class`:="phantom")("this")
  }
  case class Quoted(s: String) extends Fragment{
    val toHTML: Frag = "\""+s+"\""
  }
  case class Citation(s: String) extends Fragment{
    val toHTML: Frag = span(`class`:="citation")(sup(strong(s)))
  }
  case class Hypertarget(l: String, s: String) extends Fragment{
    val toHTML: Frag = span(`class`:="hypertarget")(s)
  }
  case class Hyperlink(l: String, s: String) extends Fragment{
    val toHTML: Frag = span(`class`:="hyperlink")(a(href:=l)(s))
  }
  case class Reference(s: String) extends Fragment{
    val toHTML: Frag = span(id:="ref"+s, `class`:="reference")(
      script(
        raw("document.getElementById(\"ref"),s,raw("\").innerHTML = "),
        raw("labelNum[\""),s,raw("\"]")
      )
    )
  }
  case class Note(p: Paragraph) extends Fragment{
    val toHTML: Frag = {
      p.toHTML.applyTo(foot)
      span(`class`:="notemark")(a(href:="#footer")(sup("[?]")))
    }
  }
  sealed trait Styled extends Fragment{
    val s: Paragraph
    val toHTML: Frag
  }

  case class Emph(s: Paragraph) extends Styled{
    val toHTML: Frag = em(for(f <- s.frgs) yield f.toHTML)
  }
  case class Strong(s: Paragraph) extends Styled{
    val toHTML: Frag = strong(for(f <- s.frgs) yield f.toHTML)
  }
  case class Italic(s: Paragraph) extends Styled{
    val toHTML: Frag = span(`class`:="italic")(for(f <- s.frgs) yield f.toHTML)
  }
  case class Underline(s: Paragraph) extends Styled{
    val toHTML: Frag = span(`class`:="underline")(for(f <- s.frgs) yield f.toHTML)
  }
  case class Strikeout(s: Paragraph) extends Styled{
    val toHTML: Frag = span(`class`:="strikeout")(for(f <- s.frgs) yield f.toHTML)
  }
  case class Superscript(s: Paragraph) extends Styled{
    val toHTML: Frag = sup(for(f <- s.frgs) yield f.toHTML)
  }
  case class Subscript(s: Paragraph) extends Styled{
    val toHTML: Frag = sub(for(f <- s.frgs) yield f.toHTML)
  }
  case class SmallCaps(s: Paragraph) extends Styled{
    val toHTML: Frag = span(`class`:="smallcaps")(for(f <- s.frgs) yield f.toHTML)
  }

}

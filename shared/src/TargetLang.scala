package partex
import scalatags.Text.all._

object TargetLang {
  val foot = new scalatags.text.Builder

  sealed trait AllElem
  sealed trait Labelable {
    val label: Option[String]
  }
  sealed trait Math{
    val value: String
    val mathOpt = MathParser.getMath(value)
  }
  sealed trait Float

  case class Document(top: Vector[MetaData], bd: Body){
    val headList = bd.elems.collect({case x: Heading => x}).asInstanceOf[Vector[Heading]]

    val headNum = headList.filter((h: Heading) => h.name == "section").zipWithIndex
      .map((t:(Heading,Int)) => (t._1,(t._2+1).toString))
      .map(subsecBlock).flatten.toMap ++
      headList.filter((h: Heading) => h.name == "chapter").zipWithIndex
      .map((t:(Heading,Int)) => (t._1,(t._2+1).toString)).toMap ++
      headList.filter((h: Heading) => h.name == "part").zipWithIndex
      .map((t:(Heading,Int)) => (t._1,(t._2+1).toString)).toMap

    val headNumByName = headNum.map((t:(Heading,String)) => (t._1.name+t._1.value -> t._2))

    val thmNum = bd.elems.collect({case x: Theorem => x}).asInstanceOf[Vector[Theorem]]
      .groupBy((t: Theorem) => t.counter.getOrElse(t.name)).values.toVector
      .map((xs: Vector[Theorem]) => (getNumby(xs), xs))
      .flatMap(
        (t:(Option[String],Vector[Theorem])) =>
        t._2.groupBy(currHead(t._1,_)).mapValues(_.zipWithIndex)
        .flatMap(
          (tt:(String,Vector[(Theorem,Int)])) =>
          tt._2.map((p:(Theorem,Int)) => (p._1, tt._1+(p._2+1).toString))
        )
      ).toMap

    val thmNumByName = thmNum.map((t:(Theorem,String)) =>
      (t._1.name+t._1.value.elems.collectFirst({
        case b: Paragraph =>
        b.frgs.collect({case x:Text => x.s.take(20).split('\n').mkString}).mkString
      }).getOrElse("") -> t._2))

    val eqNum = bd.elems.collect({case x: MathBlock => x})
    .asInstanceOf[Vector[MathBlock]].zipWithIndex
    .map((t:(MathBlock,Int)) => (t._1,"(" +(t._2+1)+ ")")).toMap

    val codeNum = bd.elems.collect({case x: CodeBlock => x})
      .asInstanceOf[Vector[CodeBlock]].zipWithIndex
      .map((t:(CodeBlock,Int)) => (t._1,"Listing "+(t._2+1))).toMap

    val figNum = bd.elems.collect({case x: Figure => x})
      .asInstanceOf[Vector[Figure]].filter(_.cap.nonEmpty).zipWithIndex
      .map((t:(Figure,Int)) => (t._1,"Figure "+(t._2+1))).toMap

    val tableNum = bd.elems.collect({case x: Table => x})
      .asInstanceOf[Vector[Table]].filter(_.cap.nonEmpty).zipWithIndex
      .map((t:(Table,Int)) => (t._1,"Table "+(t._2+1))).toMap

    val labelNum = bd.elems.flatMap(hasLabel)
      .map((l: Labelable) => (l.label.get, getNum(l).getOrElse("02"))).toMap

    def subsecBlock(t:(Heading,String)) = {
      val slice = headList.splitAt(headList.indexOf(t._1)+1)._2
        .takeWhile((h: Heading) => h.name!= "section")

      t +: slice.filter((h: Heading) => h.name== "subsection").map((h: Heading) => (h,t._2+ ".")).zipWithIndex
        .map((tt:((Heading,String),Int)) => (tt._1._1, tt._1._2 + (tt._2+1).toString))
        .map(
          (t:(Heading,String)) => t +:
            slice.splitAt(slice.indexOf(t._1)+1)._2.takeWhile((h: Heading) => h.name!= "subsection")
              .filter((h: Heading) => h.name== "subsubsection")
              .map((h: Heading) => (h,t._2+ ".")).zipWithIndex
              .map((tt:((Heading,String),Int)) => (tt._1._1, tt._1._2 + (tt._2+1).toString))
        ).flatten
    }

    def getNumby(xs: Vector[Theorem]) = {
      val master = xs.find(_.counter == None)
      if (master == None) { xs(0).counter.map(levelup) }
      else { master.get.numberBy }
    }

    def levelup(level: String) =
      if(level == "subsubsection") {"subsection"}
      else if(level == "subsection") {"section"}
      else if(level == "section") {"chapter"}
      else {"failname"}

    def currHead(h: Option[String],t: BodyElem) =
      h.map((s: String) =>
        bd.elems.takeWhile(_!=t).reverse.find((b: BodyElem) => b match {
          case e: Heading => if(e.name==s) {true} else {false}
          case _ => {false}
        }).asInstanceOf[Option[Heading]].map(headNum(_)+ ".").getOrElse("")
      ).getOrElse("")

    def hasLabel(e: AllElem): Vector[Labelable] = e match {
      case x: Paragraph => x.frgs.flatMap(hasLabel)
      case x: TexList => x.xs.flatMap(hasLabel)
      case x: Environment => x.value.elems.flatMap(hasLabel)
      case x: Theorem => if(x.label!=None) {Vector(x)} else {Vector()} ++
        x.value.elems.flatMap(hasLabel)
      case x: Item => if(x.label!=None) {Vector(x)} else {Vector()} ++
        x.value.elems.flatMap(hasLabel)
      case x: Labelable => if(x.label!=None) {Vector(x)} else {Vector()}
      case _ => Vector()
    }

    def getNum(l: Labelable) = l match {
      case x: Heading => headNum.get(x)
      case x: Theorem => thmNum.get(x)
      case x: MathBlock => eqNum.get(x)
      case x: CodeBlock => codeNum.get(x)
      case x: Figure => figNum.get(x)
      case x: Table => tableNum.get(x)
      case x: Item => getList(x,bd).map((_.xs.indexOf(x)+1)).map(_.toString)
      case x: Phantom => bd.elems.find(hasFrag(x,_)).
        map(currHead(Some("section"),_))
      case _ => Some("01")
    }

    def hasFrag(x: Fragment,be: BodyElem): Boolean = be match {
      case e: Paragraph => e.frgs.contains(x)
      case e: Environment => e.value.elems.exists(hasFrag(x,_))
      case e: Proof => e.value.elems.exists(hasFrag(x,_))
      case e: TexList => e.xs.exists(_.value.elems.exists(hasFrag(x,_)))
      case _ => false
    }

    def getList(x: Item,b: Body): Option[TexList] =
      b.elems.collect({
        case e: TexList => if(e.xs.contains(x)) {Some(e)} else {None}
        case e: Environment => getList(x,e.value)
        case e: Proof => getList(x,e.value)
      }).flatten.headOption

    def mapToJSobjectString(m: Map[String,String]): String =
      m.map((t:(String,String)) => "\""+t._1+"\": " + "\""+t._2+"\"").mkString("{", ", ", "};")

    val toHTML: Frag =
      html(
        head(
          scalatags.Text.tags2.title("First Look !"),
          link(href:="main.css", rel:="stylesheet"),
          script(src:="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML"),
          script("const headNum = ", raw(mapToJSobjectString(headNumByName)) ),
          script("const labelNum = ", raw(mapToJSobjectString(labelNum)) ),
          script("const thmNum = ", raw(mapToJSobjectString(thmNumByName)) ),
          script(
            "const eqNum = ",
            raw(mapToJSobjectString(eqNum.map((t:(MathBlock,String)) =>
            (t._1.value.split('\n').mkString,t._2))))
          ),
          script(
            "const codeNum = ",
            raw(mapToJSobjectString(codeNum.map((t:(CodeBlock,String)) =>
            (t._1.value.take(20).split('\n').mkString,t._2))))
          ),
          script(
            "const figNum = ",
            raw(mapToJSobjectString(figNum.map((t:(Figure,String)) => (t._1.cap.get,t._2))))
          ),
          script(
            "const tableNum = ",
            raw(mapToJSobjectString(tableNum.map((t:(Table,String)) => (t._1.cap.get,t._2))))
          )
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
    val toHTML: Frag = div(`class`:="command")(h3(name+ ": " +value))
  }
  case class Heading(name: String, alias: Option[String], label: Option[String],
    value: String) extends BodyElem with Labelable{
      val key = name+value
      val idValue = key.split(" ").mkString("_")
      val toHTML: Frag =
        div(`class`:="heading")(
          span(`class`:=name)(
            span(id:=idValue)(script(
              raw("document.getElementById(\""),idValue,raw("\").innerHTML = "),
              raw("headNum[\""),raw(key),raw("\"];")
            )),
            label.map((l: String) => a(attr("name"):=l)()),
            value,
            alias.map((l: String) => "\t\t[" +l+ "]")
          )
        )
  }
  case class Image(spec: Option[Map[String,String]], name: String)
    extends BodyElem with Float with Graphics{
      val toHTML: Frag = div(`class`:="image")(div(`class`:="img")())
  }
  case class Environment(name: String, value: Body) extends BodyElem{
    val toHTML: Frag =
      div(`class`:="environment")(
        div(`class`:="name")(name),
        div(`class`:="envbody")(value.toHTML)
      )
  }
  case class Theorem(name: String, counter: Option[String], numberBy: Option[String],
    alias: Option[String], label: Option[String], value: Body)
    extends BodyElem with Labelable{
      val key = name +
        value.elems.collectFirst({
          case b: Paragraph =>
          b.frgs.collect({case x:Text => x.s.take(20).split('\n').mkString}).mkString
        }).getOrElse("")
      val idValue = key.split(" ").mkString("_")

      val toHTML: Frag =
        div(`class`:="theorem")(
          div(`class`:="name")(
            label.map((l: String) => a(attr("name"):=l)()),
            name,
            span(id:=idValue)(script(
              raw("document.getElementById(\""),idValue,raw("\").innerHTML = "),
              raw("thmNum[\""),raw(key),raw("\"];")
            )),
            alias.map((l: String) => "\t\t[" +l+ "]")
          ),
          value.toHTML
        )
  }
  case class Proof(alias: Option[String], label: Option[String], value: Body)
    extends BodyElem with Labelable{
      val toHTML: Frag =
        div(`class`:="proof")(
          label.map((l: String) => a(attr("name"):=l)()),
          div(`class`:="name")("proof", alias.map((l: String) => "\t\t[" +l+ "]")),
          div(`class`:="pfbody")(value.toHTML)
        )
  }
  sealed trait MathBlock extends BodyElem with Math with Labelable{
    val label: Option[String]
    val value: String
    val key = value.take(20).split('\n').mkString
    val idValue = key.split(" ").mkString("_")
    val toHTML: Frag
  }
  case class EqMatrix(name: String, label: Option[String], value: String) extends MathBlock{
    val toHTML: Frag = 
    div(`class`:="displaymath")(
      div(id:=idValue)(script(
        raw("document.getElementById(\""),idValue,raw("\").innerHTML = "),
        raw("eqNum[\""),raw(key),raw("\"];")
      )),
      label.map((l: String) => a(attr("name"):=l)()),
      "\\begin{",name,"}", raw(value), "\\end{",name,"}"
    )
  }
  case class MultiLine(label: Option[String], value: String) extends MathBlock{
    val toHTML: Frag =
    div(`class`:="displaymath")(
      div(id:=idValue)(script(
        raw("document.getElementById(\""),idValue,raw("\").innerHTML = "),
        raw("eqNum[\""),raw(key),raw("\"];")
      )),
      label.map((l: String) => a(attr("name"):=l)()),
      "\\[", raw(value), "\\]"
    )
  }
  case class DisplayMath(label: Option[String], value: String) extends MathBlock{
    val toHTML: Frag =
    div(`class`:="displaymath")(
      div(id:=idValue)(script(
        raw("document.getElementById(\""),idValue,raw("\").innerHTML = "),
        raw("eqNum[\""),raw(key),raw("\"];")
      )),
      label.map((l: String) => a(attr("name"):=l)()),
      "\\[", raw(value), "\\]"
    )
  }

  case class CodeBlock(label: Option[String], value: String) extends BodyElem with Labelable{
    val key = value.take(20).split('\n').mkString
    val idValue = key.split(" ").mkString("_")
    val toHTML: Frag =
      div(`class`:="codeBlock")(
        label.map((l: String) => a(attr("name"):=l)()),
        pre(code(value)),
        div(id:=idValue)(script(
          raw("document.getElementById(\""),idValue,raw("\").innerHTML = "),
          raw("codeNum[\""),raw(key),raw("\"];")
        ))
      )
  }
  case class Figure(g: Graphics, cap: Option[String], label: Option[String])
    extends BodyElem with Float with Labelable{
      val toHTML: Frag =
        div(`class`:="figure")(
          label.map((l: String) => a(attr("name"):=l)()),
          g.toHTML,
          cap.map((c: String) => {
            val idValue = c.split(" ").mkString("_")
            div(
              span(id:=idValue)(script(
                raw("document.getElementById(\""),idValue,raw("\").innerHTML = "),
                raw("figNum[\""),raw(c),raw("\"];")
              )),
              span(": ",c)
            )
          }).getOrElse("")
        )
  }
  sealed trait Graphics{
    val toHTML: Frag
  }
  case class FigMath(value: String) extends Graphics with Float{
    val toHTML: Frag = div(`class`:="figmath")(value)
  }

  case class Table(cap: Option[String], label: Option[String], tb: Vector[Rows])
    extends BodyElem with Float with Labelable{
      val toHTML: Frag =
        div(`class`:="table")(
          label.map((l: String) => a(attr("name"):=l)()),
          table( for(row <- tb) yield row.toHTML ),
          cap.map((c: String) => {
            val idValue = c.split(" ").mkString("_")
            div(
              span(id:=idValue)(script(
                raw("document.getElementById(\""),idValue,raw("\").innerHTML = "),
                raw("tableNum[\""),raw(c),raw("\"];")
              )),
              span(": ",c)
            )
          }).getOrElse("")
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
    val toHTML: Frag =
      li(`class`:="item")(
        value.toHTML,
        label.map((l: String) => a(attr("name"):=l)())
      )
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
  case class InlineMath(value: String) extends Fragment with Math{
    val toHTML: Frag = span(`class`:="inlinemath")("\\(", raw(value), "\\)")
  }
  case class Phantom(label: Option[String]) extends Fragment with Labelable{
    val toHTML: Frag =
      span(`class`:="phantom")(
        label.map((l: String) => a(attr("name"):=l)())
      )
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
    val idValue = "ref"+s.split(" ").mkString("_")
    val toHTML: Frag =
      span(`class`:="reference")(
        a(id:=idValue, href:="#"+s)(script(
          raw("document.getElementById(\""),idValue,raw("\").innerHTML = "),
          raw("labelNum[\""),raw(s),raw("\"];")
        ))
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

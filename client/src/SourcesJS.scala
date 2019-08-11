package partex
import org.scalajs.dom
import dom.document, dom.html
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scalatags.JsDom.all._
import org.scalajs.dom.raw.Blob


@js.native
@JSGlobal
class saveAs(bb: Blob, name: String) extends js.Object

@JSExportTopLevel("Reader")
object Reader {
  val reader = new dom.FileReader
  @JSExport
  def handleFile(file: dom.File): Unit = {
    reader.onload = (e: dom.Event) => {
      val rawSrc = new SourcesJS(reader.result.asInstanceOf[String])
      document.getElementById("out").innerHTML = button(id:="parser")("Parse").toString
      document.getElementById("parser").addEventListener("click",rawSrc.parse)
    }
    reader.readAsText(file)
  }
}

class SourcesJS(raw: String) {
  val divided = raw.split("""\\begin\{document\}""")
  val preamble = divided(0)
  val restRaw = "\\begin{document}" + divided(1)
  val parser = new Processor.Resolver(preamble,restRaw)
  val fileContent = parser.content

  def parse(e: dom.Event): Unit = {
    document.getElementById("out").appendChild(p(parser.message).render)
    document.getElementById("out").appendChild(button(id:="converter")("Convert and Download").render)
    document.getElementById("converter").addEventListener("click",convert)
  }

  def convert(e: dom.Event): Unit = {
    document.getElementById("out").innerHTML = p("downloading output file in few seconds...").toString
    val blob = new Blob(js.Array(fileContent))
    new saveAs(blob,"yourdoc.html")
  }

}

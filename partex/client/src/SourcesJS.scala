package partex.client
//import org.scalajs.nodejs.fs.Fs
import org.scalajs.dom
import dom.document, dom.html
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

import partex.shared.Processor

@JSExportTopLevel("Reader")
object Reader {
  val reader = new dom.FileReader
  @JSExport
  def handleFile(file: dom.File): Unit = {
    reader.onload = (e: dom.Event) => {
      val rawSrc = new SourcesJS(reader.result.asInstanceOf[String])
      document.getElementById("out").appendChild(
        button(id:="parser")("Parse").render
      )
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

  def parse(e: dom.Event): Unit = {
    document.getElementById("out").appendChild(p(parser.message).render)
    document.getElementById("out").appendChild(button(id:="converter")("Convert").render)
    document.getElementById("converter").addEventListener("click",convert)
  }

  def convert(e: dom.Event): Unit = {
    document.documentElement.innerHTML = parser.content.split("<html>")(1).split("</html>")(0)
  }

}

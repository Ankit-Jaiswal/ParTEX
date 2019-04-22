package partex.server
import java.io.PrintWriter

class SourcesIO(filename: String) {
  import partex.shared.Processor

  val file = if (filename != "") { getClass.getResource("/"+filename) }
    else { getClass.getResource("/mydoc.tex") }

  val raw = scala.io.Source.fromFile(file.getPath).mkString

  val divided = raw.split("""\\begin\{document\}""")
  val extFilename = Processor.extPreamble(divided(0))
  val external = extFilename match {
    case Some((s,1)) => scala.io.Source.fromFile(getClass.getResource("/"+ s+ ".tex").getPath).mkString
    case Some((s,2)) => scala.io.Source.fromFile(getClass.getResource("/"+s).getPath).mkString
    case Some((s,3)) => scala.io.Source.fromFile(s).mkString
    case _ => ""
  }
  val preamble = external + divided(0)
  val restRaw = if (divided.length > 1) {"\\begin{document}" + divided(1)} else {""}
  val parser = new Processor.Resolver(preamble,restRaw)

  def writeTo(file: String) =
    new PrintWriter(file) { write("<!DOCTYPE html>\n"+parser.content); close }

}

object siteMaker {
  def main(args: Array[String]): Unit = {
    val input = new SourcesIO("polymath.tex")
    input.writeTo("mydoc.html")
  }

}

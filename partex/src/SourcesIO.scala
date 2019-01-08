package partex
import fastparse.all._
import java.io.PrintWriter

class SourcesIO(filename: String) {
  val file = if (filename != "") { getClass.getResource("/"+filename) }
    else { getClass.getResource("/mydoc.tex") }

  val raw = scala.io.Source.fromFile(file.getPath).mkString

  val divided = raw.split("""\\begin\{document\}""")
  val extFilename = Processor.inputFile.?.parse(divided(0)).get.value
  val external = extFilename match {
    case Some((s,1)) => scala.io.Source.fromFile(getClass.getResource("/"+s+".tex").getPath).mkString
    case Some((s,2)) => scala.io.Source.fromFile(getClass.getResource("/"+s).getPath).mkString
    case Some((s,3)) => scala.io.Source.fromFile(s).mkString
    case _ => ""
  }
  val preamble = external + divided(0)
  val restRaw = "\\begin{document}" + divided(1)
  val parser = new Processor.Resolver(preamble,restRaw)

  def writeTo(file: String) = {
    val content = parser.parse match {
      case Parsed.Success(value,_) => value.toHTML.toString
        .replaceAllLiterally("<span class=\"text\"></span>","")
        .replaceAllLiterally("><",">\n<")

      case _: Parsed.Failure =>
        "<html><head><script>alert('Parser Failed!');</script></head></html>"
    }
    new PrintWriter(file) { write("<!DOCTYPE html>\n"+content); close }
  }

}


object siteMaker {
  def main(args: Array[String]): Unit = {
    val input = new SourcesIO("polymath.tex")
    input.writeTo("mydoc.html")
  }

}

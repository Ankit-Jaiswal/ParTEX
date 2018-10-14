package partex
import fastparse.all._
import java.io.PrintWriter

class SourcesIO(filename: String) {
  import ParsingRules.all.alpha
  import ParsingRules.all.num
  import ParsingRules.all.curlyBox
  import ParsingRules.all.sqBox
  import ParsingRules.all.cmdName

  val file = if (filename != "") { getClass.getResource("/"+filename) }
    else { getClass.getResource("/mydoc.tex") }

  val raw = scala.io.Source.fromFile(file.getPath).mkString

/**************************      preamble input      ****************************/

  val inputFile: P[String] = P("\\" ~ ("input"|"include") ~ "{" ~
    (resrcFilename.map((f: String) => scala.io.Source.fromFile(getClass.getResource("/"+f+".tex").getPath).mkString) |
      resrcFile.map((f: String) => scala.io.Source.fromFile(getClass.getResource("/"+f).getPath).mkString) |
      path.map((f: String) => scala.io.Source.fromFile(f).mkString) ) ~
    "}")
  val resrcFilename: P[String] = P(alpha | num | "_").rep.!
  val resrcFile: P[String] = P(alpha | num | "_" | ".").rep.!
  val path: P[String] = P(alpha | num | "_" | "." | "/").rep.!


/********************       extracting \newcommands      ************************/

  val macroParser: P[Map[String,(Vector[String],String)]] =
    P((!defCmd ~ AnyChar).rep ~ usrCmd).rep.map(_.toVector).map(_.toMap)
  val usrCmd: P[(String,(Vector[String],String))] =
    P( defCmd ~ name ~ argBox.? ~ (default.rep.map(_.toVector) ~ definition))
  val defCmd: P[Unit] = P( StringIn("\\def","\\newcommand","\\renewcommand") )
  val name: P[String] = P("{".? ~ ("\\" ~ alpha.rep(1)).! ~ "}".? )
  val argBox: P[Unit] = P("[" ~ num.rep(1) ~ "]" )
  val default: P[String] = P("[" ~ (!"]" ~ AnyChar).rep(1).! ~ "]")
  val definition: P[String] = P("{" ~ (curlyBox | !"}" ~ AnyChar).rep(1).! ~ "}")

/********************     extracting \newtheorems      ************************/

  val nwthmParser: P[Map[String,String]] = P(nwthm.rep).map(_.toVector).map(_.toMap)
  val nwthm: P[(String,String)] = P((!"\\newtheorem{" ~ AnyChar).rep ~
    "\\newtheorem" ~ cmdName ~ sqBox.? ~ cmdName ~ sqBox.?)

/********************          pre-processing          ***********************/

  val divided = raw.split("""\\begin\{document\}""")
  val preamble = inputFile.?.parse(divided(0)).get.value.getOrElse("") + divided(0)
  val rest = "\\begin{document}" + divided(1)

  val usrCmdList: Map[String,(Vector[String],String)] =
    macroParser.parse(preamble) match {
      case Parsed.Success(value,_) => value
      case _: Parsed.Failure => Map()
    }

  val thmList: Map[String,String] =
    nwthmParser.parse(preamble) match {
    case Parsed.Success(value,_) => value
    case _: Parsed.Failure => Map()
  }


/***********************       resolving raw file       **********************/

  val cmdKeys = usrCmdList.keys.toList.sortWith(_>_)
  val calledCmd =
    P((!cmdToken ~ AnyChar).rep ~ (cmdToken ~ !alpha).! ~ boxPara ~ params).rep.map(_.toVector)
  val cmdToken: P[Unit] = cmdKeys.foldLeft(P("****"))((p: P[Unit],s: String) => P(p | s))
  val boxPara: P[Vector[String]] = P("[" ~ (!"]" ~ AnyChar).rep.! ~ "]").rep.map(_.toVector)
  val params: P[Vector[String]] = P("{" ~ (!"}" ~ AnyChar).rep.! ~ "}").rep.map(_.toVector)


  /**********************        main processing        ************************/

  def docString = resolve(rest.split('\n').map(rmvComments).mkString("\n"))

  def rmvComments(l: String) =
    if (l.startsWith("%")) ""
    else """[^\\]%""".r
    .findFirstMatchIn(l)
    .map((m) => m.before.toString + m.group(0).head)
    .getOrElse(l)

  def resolve(s: String): String = {
    val calledCmdList = calledCmd.parse(s) match {
      case Parsed.Success(value,_) => value
      case _: Parsed.Failure => Vector()
    }

    val res = calledCmdList.foldLeft(s)(substitute)

    if (res == s) res else resolve(res)
  }

  def substitute(l: String,t:(String,Vector[String],Vector[String])) =
    if (t._2.length == 0)
    l.replaceAllLiterally(t._1 ++ wrap(t._3),
      resolveDef(t._1, usrCmdList(t._1)._1 ++ t._3) )
    else
    l.replaceAllLiterally(t._1 ++ boxwrap(t._2) ++ wrap(t._3),
      resolveDef(t._1, t._2 ++ t._3) )

  def resolveDef(k: String,params: Vector[String]): String =
    params.foldLeft(usrCmdList(k)._2)((d: String,p: String) =>
      d.replaceAllLiterally("#"++(params.indexOf(p)+1).toString, p))

  def wrap(xs: Vector[String]) = xs.foldLeft("")((l: String,s: String) => l++"{"++s++"}")
  def boxwrap(xs: Vector[String]) = xs.foldLeft("")((l: String,s: String) => l++"["++s++"]")


  def parse = DeTeX(thmList).document.parse(preamble + docString)

  def writeTo(file: String) = {
    val content = this.parse match {
      case Parsed.Success(value,_) => value.toHTML.toString.split("><").mkString(">\n<")
      case _: Parsed.Failure => "<html><head><script>alert('Parser Failed!')</script></head></html>"
    }
    new PrintWriter(file) { write("<!DOCTYPE html>\n"+content); close }
  }

}


object siteMaker {
  def main(args: Array[String]): Unit = {
    val input = new SourcesIO("")
    input.writeTo("mydoc.html")
  }

}

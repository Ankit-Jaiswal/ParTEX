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
    (resrcFilename.map((f: String) =>
      scala.io.Source.fromFile(getClass.getResource("/"+f+".tex").getPath).mkString) |
    resrcFile.map((f: String) =>
      scala.io.Source.fromFile(getClass.getResource("/"+f).getPath).mkString) |
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
  val rest = "\\begin{document}" + divided(1).split('\n').map(rmvComments).mkString("\n")

  val usrCmdList: Map[String,(Vector[String],String)] =
    macroParser.parse(preamble+rest) match {
      case Parsed.Success(value,_) => value
      case _: Parsed.Failure => Map()
    }

  val thmList: Map[String,String] =
    nwthmParser.parse(preamble+rest) match {
    case Parsed.Success(value,_) => value
    case _: Parsed.Failure => Map()
  }


/***********************       resolving raw file       **********************/

  val scanner: P[String] = P(resolver ~ AnyChar.rep.!).map((t:(String,String)) => t._1+t._2)

  val resolver: P[String] = P((!cmdToken ~ AnyChar).rep.! ~ substitutor).
    map((t:(String,String)) => t._1 + t._2).
    rep.map(_.toVector).map((xs: Vector[String]) => if(xs.isEmpty) {""} else {xs.reduceLeft(_+_)})

  val substitutor: P[String] = P(cmdToken.! ~ boxPara ~ params).
    map((t:(String,Vector[String],Vector[String])) =>
    scanner.parse(resolveDef(t._1,t._2,t._3)).get.value)

  val cmdKeys = usrCmdList.keys.toList.sortWith(_>_)
  val cmdToken: P[Unit] = P(cmdKeys.foldLeft(P("****"))((p: P[Unit],s: String) => P(p | s)) ~ !alpha)
  val boxPara: P[Vector[String]] = P("[" ~ (!"]" ~ AnyChar).rep.! ~ "]").rep.map(_.toVector)
  val params: P[Vector[String]] = P("{" ~ (!"}" ~ AnyChar).rep.! ~ "}").rep.map(_.toVector)

  def resolveDef(k: String,default: Vector[String],para: Vector[String]): String = {
    val params = if (default.length == 0) {usrCmdList(k)._1 ++ para} else {default++para}

    params.foldLeft(usrCmdList(k)._2)((d: String,p: String) =>
      d.replaceAllLiterally("#"++(params.indexOf(p)+1).toString, p))
  }

  /**********************        main processing        ************************/

  val docString = scanner.parse(rest).get.value

  def rmvComments(l: String) =
    if (l.startsWith("%")) ""
    else """[^\\]%""".r
    .findFirstMatchIn(l)
    .map((m) => m.before.toString + m.group(0).head)
    .getOrElse(l)

  val parsed = DeTeX(thmList).document.parse(preamble + docString)

  def writeTo(file: String) = {
    val content = this.parsed match {
      case Parsed.Success(value,_) => value.toHTML.toString
        .replaceAllLiterally("<span class=\"text\"></span>","")
        .replaceAllLiterally("><",">\n<")

      case _: Parsed.Failure =>
        "<html><head><script>alert('Parser Failed!')</script></head></html>"
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

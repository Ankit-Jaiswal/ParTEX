package partex.shared
import fastparse.all._

object Processor {
  import ParsingRules.all.alpha
  import ParsingRules.all.num
  import ParsingRules.all.curlyBox
  import ParsingRules.all.sqBox
  import ParsingRules.all.cmdName

  /**************************      preamble input      ****************************/
  val inputFile: P[(String,Int)] = P("\\" ~ ("input"|"include") ~ "{" ~
    (resrcFilename | resrcFile | path) ~ "}")
  val resrcFilename: P[(String,Int)] = P(alpha | num | "_").rep.!
    .map((s: String) => (s,1))
  val resrcFile: P[(String,Int)] = P(alpha | num | "_" | ".").rep.!
    .map((s: String) => (s,2))
  val path: P[(String,Int)] = P(alpha | num | "_" | "." | "/").rep.!
    .map((s: String) => (s,3))

  def extPreamble(s: String) = inputFile.?.parse(s).get.value

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
  val nwthmParser: P[Map[String,(Option[String],String,Option[String])]] =
    P(nwthm.rep).map(_.toVector).map(_.toMap)
  val nwthm: P[(String,(Option[String],String,Option[String]))] =
    P((!"\\newtheorem{" ~ AnyChar).rep ~
    "\\newtheorem" ~ cmdName ~ (counter.? ~ cmdName ~ counter.?))
  val counter: P[String] = P("[" ~ (!"]" ~ AnyChar).rep.! ~ "]")


/***********************       resolving raw file       **********************/
  class Resolver(preamble: String, restRaw: String) {
    val rest = restRaw.split('\n').map(rmvComments).mkString("\n")

    val usrCmdList: Map[String,(Vector[String],String)] =
      macroParser.parse(preamble+rest) match {
        case Parsed.Success(value,_) => value
        case _: Parsed.Failure => Map()
      }

    val thmList: Map[String,(Option[String],String,Option[String])] =
      nwthmParser.parse(preamble+rest) match {
        case Parsed.Success(value,_) => value
        case _: Parsed.Failure => Map()
      }

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

    val docString = scanner.parse(rest).get.value

    def rmvComments(l: String) =
      if (l.startsWith("%")) ""
      else """[^\\]%""".r
      .findFirstMatchIn(l)
      .map((m) => m.before.toString + m.group(0).head)
      .getOrElse(l)

    val parsed = DeTeX(thmList).document.parse(preamble + docString)

    val message = parsed match {
      case Parsed.Success(value,_) => "Parsing Successful :) , please go ahead and convert it to its html version."
      case _: Parsed.Failure => "Parsing Failed :( , Make sure uploaded file is a valid .tex file by compiling it using a LaTeX engine."
    }


    val content = parsed match {
      case Parsed.Success(value,_) => value.toHTML.toString
        .replaceAllLiterally("<span class=\"text\"></span>","")
        .replaceAllLiterally("><",">\n<")

      case _: Parsed.Failure =>
        "<html><head><script>alert('Parser Failed!');</script></head></html>"
    }

  }

}

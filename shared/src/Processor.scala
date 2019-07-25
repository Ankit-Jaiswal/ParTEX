package partex
import fastparse._, NoWhitespace._
import TargetLang.{DisplayMath, MultiLine, EqMatrix, Paragraph, InlineMath}
import partex.MathLang.MathPhrase

object Processor {
  import ParsingRules.all.alpha
  import ParsingRules.all.num
  import ParsingRules.all.curlyBox
  import ParsingRules.all.sqBox
  import ParsingRules.all.cmdName

  /**************************      preamble input      ****************************/
  def importFile[_:P]: P[Option[(String,Int)]] = P("\\" ~ ("input" | "include") ~ "{" ~
    (resrcFilename | resrcFile | path) ~ "}").?
  def resrcFilename[_:P]: P[(String,Int)] = P(alpha | num | "_").rep.!
    .map((s: String) => (s,1))
  def resrcFile[_:P]: P[(String,Int)] = P(alpha | num | "_" | ".").rep.!
    .map((s: String) => (s,2))
  def path[_:P]: P[(String,Int)] = P(alpha | num | "_" | "." | "/").rep.!
    .map((s: String) => (s,3))

  def extPreamble(s: String) = parse(s,importFile(_)).get.value

/********************       extracting \newcommands      ************************/
  def macroParser[_:P]: P[Map[String,(Vector[String],String)]] =
    P((!defCmd ~ AnyChar).rep ~ usrCmd).rep.map(_.toVector).map(_.toMap)
  def usrCmd[_:P]: P[(String,(Vector[String],String))] =
    P( defCmd ~ name ~ argBox.? ~ (default.rep.map(_.toVector) ~ definition))
  def defCmd[_:P]: P[Unit] = P( StringIn("\\def","\\newcommand","\\renewcommand") )
  def name[_:P]: P[String] = P("{".? ~ ("\\" ~ alpha.rep(1)).! ~ "}".? )
  def argBox[_:P]: P[Unit] = P("[" ~ num.rep(1) ~ "]" )
  def default[_:P]: P[String] = P("[" ~ (!("]") ~ AnyChar).rep(1).! ~ "]")
  def definition[_:P]: P[String] = P("{" ~ (curlyBox | !("}") ~ AnyChar).rep(1).! ~ "}")

/********************     extracting \newtheorems      ************************/
  def nwthmParser[_:P]: P[Map[String,(Option[String],String,Option[String])]] =
    P(nwthm.rep).map(_.toVector).map(_.toMap)
  def nwthm[_:P]: P[(String,(Option[String],String,Option[String]))] =
    P((!("\\newtheorem{") ~ AnyChar).rep ~
    "\\newtheorem" ~ cmdName ~ (counter.? ~ cmdName ~ counter.?))
  def counter[_:P]: P[String] = P("[" ~ (!("]") ~ AnyChar).rep.! ~ "]")


/***********************       resolving raw file       **********************/
  class Resolver(preamble: String, restRaw: String) {
    val rest = restRaw.split('\n').map(rmvComments).mkString("\n")

    val usrCmdList: Map[String,(Vector[String],String)] =
      parse(preamble+rest, macroParser(_)) match {
        case Parsed.Success(value,_) => value
        case _: Parsed.Failure => Map()
      }

    val thmList: Map[String,(Option[String],String,Option[String])] =
      parse(preamble+rest, nwthmParser(_)) match {
        case Parsed.Success(value,_) => value
        case _: Parsed.Failure => Map()
      }

    def scanner[_:P]: P[String] = P(resolver ~ AnyChar.rep.!).map((t:(String,String)) => t._1+t._2)

    def resolver[_:P]: P[String] = P((!cmdToken ~ AnyChar).rep.! ~ substitutor).
      map((t:(String,String)) => t._1 + t._2).
      rep.map(_.toVector).map((xs: Vector[String]) => if(xs.isEmpty) {""} else {xs.reduceLeft(_+_)})

    def substitutor[_:P]: P[String] = P(cmdToken.! ~ boxPara ~ params).
      map((t:(String,Vector[String],Vector[String])) =>
      parse(resolveDef(t._1,t._2,t._3), scanner(_)).get.value)

    val cmdKeys = usrCmdList.keys.toList.sortWith(_>_)
    def cmdToken[_:P]: P[Unit] = P(cmdKeys.foldLeft(P("****"))((p: P[Unit],s: String) => P(p | s)) ~ !alpha)
    def boxPara[_:P]: P[Vector[String]] = P("[" ~ (!("]") ~ AnyChar).rep.! ~ "]").rep.map(_.toVector)
    def params[_:P]: P[Vector[String]] = P("{" ~ (!("}") ~ AnyChar).rep.! ~ "}").rep.map(_.toVector)

    def resolveDef(k: String,default: Vector[String],para: Vector[String]): String = {
      val params = if (default.length == 0) {usrCmdList(k)._1 ++ para} else {default++para}

      params.foldLeft(usrCmdList(k)._2)((d: String,p: String) =>
        d.replaceAllLiterally("#"++(params.indexOf(p)+1).toString, p))
    }

    val docString = parse(rest, scanner(_)).get.value

    def rmvComments(l: String) =
      if (l.startsWith("%")) ""
      else """[^\\]%""".r
      .findFirstMatchIn(l)
      .map((m) => m.before.toString + m.group(0).head)
      .getOrElse(l)

    val parsed = parse(preamble+docString, DeTeX(thmList).document(_))

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

    val mathStrings: Vector[String] = parsed match {
      case Parsed.Success(value,_) => value.bd.elems.collect{
        case DisplayMath(label, value) => Vector(value)
        case MultiLine(label, value) => Vector(value.split("""\\\\""").reduce(_+_))
        case EqMatrix(label, value) => value.split("""\\\\""").toVector
          .map(_.split("&").sliding(2,2).toVector.map(_.reduce(_+_))).flatten
        case Paragraph(frgs) => frgs.collect{case InlineMath(value) => value}.toVector
      }.flatten
      case _: Parsed.Failure => Vector()
    }

    lazy val mathParseResults : Map[String, Parsed[Vector[MathPhrase]]] =
      mathStrings.map(s => s -> MathParser.parseMath(s)).toMap

  }

}

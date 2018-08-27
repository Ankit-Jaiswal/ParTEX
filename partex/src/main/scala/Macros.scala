package partex

import fastparse.all._
import TargetLang._

object Macros {
/********************         parsing preamble        ************************/

  val preambleParser: P[Map[String,(Vector[String],String)]] =
    P((!defCmd ~ AnyChar).rep ~ usrCmd).rep.map(_.toVector).map(_.toMap)
  val usrCmd: P[(String,(Vector[String],String))] =
    P( defCmd ~ name ~ argBox.? ~ (default.rep.map(_.toVector) ~ definition))
  val defCmd: P[Unit] = P("\\def" | "\\newcommand" | "\\renewcommand")
  val name: P[String] = P("{".? ~ ("\\" ~ alpha.rep(1)).! ~ "}".? )
  val argBox: P[Unit] = P("[" ~ num ~ "]" )
  val default: P[String] = P("[" ~ (!"]" ~ AnyChar).rep(1).! ~ "]")
  val definition: P[String] = P("{" ~ (!"}" ~ AnyChar).rep(1).! ~ "}")
  val alpha: P[Unit] = P( CharIn('a' to 'z') | CharIn('A' to 'Z') )
  val num: P[Unit] = P( CharIn('0' to '9').rep(1) )


  val usrCmdList: Map[String,(Vector[String],String)] =
    preambleParser.parse(SourcesIO.preamble) match {
      case Parsed.Success(value,_) => value
      case _: Parsed.Failure => Map()
    }



/***********************       resolving raw file       **********************/

  val cmdKeys = usrCmdList.keys.toList.sortWith(_>_)
  val calledCmd = P((!cmdToken ~ AnyChar).rep ~ cmdToken.! ~ params).rep.map(_.toVector)
  val cmdToken: P[Unit] = cmdKeys.foldLeft(P("****"))((p: P[Unit],s: String) => P(p | s))
  val params: P[Vector[String]] = P("{" ~ (!"}" ~ AnyChar).rep.! ~ "}").rep.map(_.toVector)

  def resolve(l: String): String = {
    val calledCmdList = calledCmd.parse(l) match {
      case Parsed.Success(value,_) => value
      case _: Parsed.Failure => Vector()
    }

    val res = calledCmdList.foldLeft(l)((l: String,t: (String,Vector[String])) =>
      l.replaceAllLiterally(t._1 ++ wrapped(t._2), resolveDef(t._1,t._2)) )

    if (res == l) res else resolve(res)
  }

/*  {
    val res = cmdKeys.foldLeft(l)(substitute)
    if (res == l) res else resolve(res)
  }

  def substitute(l: String,k: String): String =  l.replaceAllLiterally( k, usrCmdList(k)._2 )
*/
  def resolveDef(k: String,params: Vector[String]): String =
    params.foldLeft(usrCmdList(k)._2)((d: String,p: String) =>
      d.replaceAllLiterally("#"++(params.indexOf(p)+1).toString, p))


  def wrapped(xs: Vector[String]) = xs.foldLeft("")((l: String,s: String) => l++"{"++s++"}")

}

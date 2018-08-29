package partex
import fastparse.all._
import TargetLang._

class SourcesIO(filename: String) {

  val raw = if (filename != "") {
    val file = getClass.getResource("/"+filename)
    scala.io.Source.fromFile(file.getPath).mkString }
  else
  """
  \documentclass[master.tex]{subfiles}

  \newcommand{\foo}{foobar}
  \newcommand{\jc}{John \foo Cena}
  \newcommand{\name}[2]{My first name is #1 and second name is #2}
  \newcommand{\withDefault}[2][books]{my friends are #1 and #2}
  %%%%%%%%%%%%%% BEGIN CONTENT: %%%%%%%%%%%%%%

  \begin{document}
    \title{How to Structure a LaTeX Document}
    \author{Andrew Roberts}
    \date{December 2004}
    \maketitle

    \section{Testing}
    This follows from \jc the second part of the \textit{remark} above.
    % parsing comments
    \withDefault{Abhijeet}
    Now for some remakrs \% about \foo centralizers. %one more comment.
    \vspace{1cm}
    \begin{rmk*}[1.1.3]
      This is a example of nested environment. \name{Ankit}{Jaiswal} \\
      Also test the line break token. \withDefault[Bhavna]{Abhijeet}
      \begin{enumerate}
      \item If $A \subgroup G$, \medskip then $A$ is abelian if and only if $A \subset
        C_G(A)$.
      \item Furthermore, if $A \subgroup Z(G)$, then $A \normsubgroup G$. \newline
        This follows using \emph{basic} commutativity arguments, which are as follows:
        \begin{itemize}
          % testing nested list
          \item First One.
          % testing comments.
          \item Second One.
        \end{itemize}
      \end{enumerate}
    \end{rmk*}
    \begin{defn*}[1.1.1]
        For $A \subset G$, we set $N_G(A) := \{g \in G | gAg^{-1} = A\}$ and
        $C_G(A) := \{g \in G | gag^{-1} = a, \forall a \in A\}$.
    \end{defn*}
    Note that $C_G(A) \subset N_G(A)$ and $Z(G) = C_G(G)$.


    \section{math section}
    \begin{equation}
    if x = 2 - x then 2x = 2, hence x = 1
    \end{equation}

    {\tiny\begin{theorem}
    If the \emph{tiny} is followed immediately by begin this fails.
    \end{theorem}
    }

  \end{document}
  """



/********************       extracting latex macros      ************************/

  val macroParser: P[Map[String,(Vector[String],String)]] =
    P((!defCmd ~ AnyChar).rep ~ usrCmd).rep.map(_.toVector).map(_.toMap)
  val usrCmd: P[(String,(Vector[String],String))] =
    P( defCmd ~ name ~ argBox.? ~ (default.rep.map(_.toVector) ~ definition))
  val defCmd: P[Unit] = P( StringIn("\\def","\\newcommand","\\renewcommand") )
  val name: P[String] = P("{".? ~ ("\\" ~ alpha.rep(1)).! ~ "}".? )
  val argBox: P[Unit] = P("[" ~ num ~ "]" )
  val default: P[String] = P("[" ~ (!"]" ~ AnyChar).rep(1).! ~ "]")
  val definition: P[String] = P("{" ~ (!"}" ~ AnyChar).rep(1).! ~ "}")
  val alpha: P[Unit] = P( CharIn('a' to 'z') | CharIn('A' to 'Z') )
  val num: P[Unit] = P( CharIn('0' to '9').rep(1) )

  val usrCmdList: Map[String,(Vector[String],String)] =
    macroParser.parse(raw) match {
      case Parsed.Success(value,_) => value
      case _: Parsed.Failure => Map()
    }


/***********************       resolving raw file       **********************/

  val cmdKeys = usrCmdList.keys.toList.sortWith(_>_)
  val calledCmd = P((!cmdToken ~ AnyChar).rep ~ cmdToken.! ~ boxPara ~ params).rep.map(_.toVector)
  val cmdToken: P[Unit] = cmdKeys.foldLeft(P("****"))((p: P[Unit],s: String) => P(p | s))
  val boxPara: P[Vector[String]] = P("[" ~ (!"]" ~ AnyChar).rep.! ~ "]").rep.map(_.toVector)
  val params: P[Vector[String]] = P("{" ~ (!"}" ~ AnyChar).rep.! ~ "}").rep.map(_.toVector)


/********************          pre-processing          ***********************/

  val divided = raw.split("""\\begin\{document\}""")
  val preamble = divided(0)
  val rest = "\\begin{document}" + divided(1)
  def docString = rest.split('\n').map(rmvComments).map(resolve).mkString("\n")

  def rmvComments(l: String) =
    if (l.startsWith("%")) ""
    else """[^\\]%""".r
    .findFirstMatchIn(l)
    .map((m) => m.before.toString + m.group(0).head)
    .getOrElse(l)

  def resolve(l: String): String = {
    val calledCmdList = calledCmd.parse(l) match {
      case Parsed.Success(value,_) => value
      case _: Parsed.Failure => Vector()
    }

    val res = calledCmdList.foldLeft(l)(substitute)

    if (res == l) res else resolve(res)
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


/**********************        main processing        ************************/

  def parse = DeTeX.document.parse(docString)

}

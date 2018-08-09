package partex

import fastparse.all._
import TargetLang._

object SourcesIO {
//  val input1 = scala.io.Source.fromFile("../grad-school-notes/Algebra/group_theory.tex").mkString
//  val input2 = scala.io.Source.fromFile("../stacks-project/algebra.tex").mkString

/**********************             a typical .tex file             ********************/

  val raw =
  """
  \documentclass[master.tex]{subfiles}

  %%%%%%%%%%%%%% BEGIN CONTENT: %%%%%%%%%%%%%%

  \begin{document}
    \title{How to Structure a LaTeX Document}
    \author{Andrew Roberts}
    \date{December 2004}
    \maketitle

    \section{Testing}
    This follows from the second part of the \textit{remark} above.
    % parsing comments
    Now for some remakrs \% about centralizers. %one more comment.
    \vspace{1cm}
    \begin{rmk*}[1.1.3]
      This is a example of nested environment. \\
      Also test the line break token.
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

  \end{document}
  """



/********************          rest of code          ***********************/

  val divided = raw.split("""\\begin\{document\}""")
  val preamble = divided(0)
  val rest = "\\begin{document}" + divided(1)
  val docString = rest.split('\n').map(rmvComments).mkString("\n")

  def main(args: Array[String]): Unit = {
      println(DeTeX.document.parse(docString))
    }

  def rmvComments(l: String) =
    if (l.startsWith("%")) ""
    else """[^\\]%""".r
    .findFirstMatchIn(l)
    .map((m) => m.before.toString + m.group(0).head)
    .getOrElse(l)



/********************          preamble parser        ************************/

  val preambleParser: P[Map[String,(Vector[String],String)]] =
    P((!defCmd ~ AnyChar).rep(1) ~ usrCmd).rep.map(_.toVector).map(_.toMap)
  val usrCmd: P[(String,(Vector[String],String))] =
    P( defCmd ~ name ~ argBox.? ~ (default.rep.map(_.toVector) ~ definition))
  val defCmd: P[Unit] = P("\\def" | "\\newcommand" | "\\renewcommand")
  val name: P[String] = P("{".? ~ ("\\" ~ alpha.rep(1)).! ~ "}".? )
  val argBox: P[Unit] = P("[" ~ num ~ "]" )
  val default: P[String] = P("[" ~ (!"]" ~ AnyChar).rep(1).! ~ "]")
  val definition: P[String] = P("{" ~ (!"}" ~ AnyChar).rep(1).! ~ "}")
  val alpha: P[Unit] = P( CharIn('a' to 'z') | CharIn('A' to 'Z') )
  val num: P[Unit] = P( CharIn('0' to '9').rep(1) )


  val usrCmdList: Map[String,(Vector[String],String)] = preambleParser.parse(preamble).get.value


/************************          resolving raw file       ***********************/
//  val usrCmdToken = P( StringIn(usrCmdList.keys.toList.sortWith(_>_)) )
//def resolve(l: String): String = l

}

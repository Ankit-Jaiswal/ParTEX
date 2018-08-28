package partex

object SourcesIO {
//  val input1 = scala.io.Source.fromFile("../grad-school-notes/Algebra/group_theory.tex").mkString
//  val input2 = scala.io.Source.fromFile("../stacks-project/algebra.tex").mkString

/**********************             a typical .tex file             ********************/

  val raw =
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



/********************          rest of code          ***********************/

  val divided = raw.split("""\\begin\{document\}""")
  val preamble = divided(0)
  val rest = "\\begin{document}" + divided(1)
  val docString = rest.split('\n').map(rmvComments).map(Macros.resolve).mkString("\n")

  def main(args: Array[String]): Unit = {
      println(DeTeX.document.parse(docString))
    }

  def rmvComments(l: String) =
    if (l.startsWith("%")) ""
    else """[^\\]%""".r
    .findFirstMatchIn(l)
    .map((m) => m.before.toString + m.group(0).head)
    .getOrElse(l)



}

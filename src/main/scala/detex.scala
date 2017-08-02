package partex

import fastparse.all._

object detex {
  val ws = P(" "|"\n"|"\t"|"\\:")
  val alpha = P(CharIn('a' to 'z'))

  val document: P[String] = P((!beginDoc ~ AnyChar).rep ~ beginDoc ~ body ~ endDoc)
  val beginDoc: P[Unit] = P("\\begin{document}" ~ ws.rep)
  val endDoc: P[Unit] = P("\\end{document}" ~ ws.rep)


	val body: P[String] = P(bodyElem.rep.map(_.reduceLeft((xs,x) => xs+"\n"+x)) )
  val bodyElem: P[String] = P(text | enclosure | !"\\end{" ~ command)

  val text: P[String] = P((!command ~ AnyChar).rep(1).! )

  val enclosure: P[String] = P(begin ~ body ~ end)
  val begin: P[Unit] = P("\\begin" ~ box.rep(1) ~ (&("\\")|ws.rep) )
  val end: P[Unit] = P("\\end" ~ box.rep(1) ~ (&("\\")|ws.rep) )

  val command: P[String] = P("\\" ~ (alpha|"*").rep(1) ~ cmdName ~ box.rep ~ (&("\\")|ws.rep) )
  val cmdName: P[String] = P("{" ~ (!"}" ~ AnyChar).rep.! ~ "}" )
  
  val box: P[Unit] = P(curlyBox|sqBox)
  val curlyBox: P[Unit] = P("{" ~ (!"}" ~ AnyChar).rep ~ "}" )
  val sqBox: P[Unit] = P("[" ~ (!"]" ~ AnyChar).rep ~ "]" )

}


/*
class ParTEX(filename: String) {
  val fullname = "../grad-school-notes/Algebra/" + filename
  val content = scala.io.Source.fromFile(fullname).getLines mkString "\n"
  val tree = detex.document.parse(content)
}
*/

object partex {

  val input1 = scala.io.Source.fromFile("../grad-school-notes/Algebra/group_theory.tex").getLines mkString "\n"

  def main(args: Array[String]): Unit = {
    println(detex.document.parse(input1))
  }

  val input2 = 
"""
\documentclass[master.tex]{subfiles}

%%%%%%%%%%%%%% BEGIN CONTENT: %%%%%%%%%%%%%%

\begin{document}
\begin{example*}[1.3.4]
  \begin{enumerate}
  \item[] % make spacing nicer
  \item[(a)] \(Z(Q)=\{-1,1\}=[Q,Q]\), hence \(Q\) is nilpotent of class \(2\).
  \item[(b)] \(S_n\) is not nilpotent \(n \ge 3\). Since \(Z(S_n)=1\). \(S_3,S_4\) are solvable. \(S_3^{(1)}=A_3\) and
    \(S_3^{(2)}=1\). \([S_n,S_n]=A_n,[A_n,A_n]=A_n\).  The very last statement is false.  This is because \([A_3,A_3]=1\).  Maybe he meant to add the condition that $ n \geq 5$.
  \item[(c)] \(A_4\) is solvable, non-nilpotent. \(S_n,A_n\) are not solvable \(n \ge 5\).
  \item[(e)] If \(|G| < 60\), then \(G\) is solvable. If \(|G|=60\) and \(G\) is not solvable, then \(G \isom
    A_5\).
  \item[(matt)] Every solvable simple group is solvable of class 1. (And nilpotent of class 1)
  \item[(f)] Let \(F\) be a field, then define
    \[U_n(F) := \left\{
       \left.\left( \begin{array}{c c c}
          1 & & \star\\
           & \ddots \\
          0& & 1
        \end{array}\right)\:\right\rvert \ \star \in F \right\} \le \GL_n.\]
  \(U_n(F)\) is nilpotent.
  \item[(g)] Define \(B_n(F)\) as above, but instead of 1s on the diagonal allow any unit of \(F\). \(B_n(F)\) is
    solvable, but potent for \(n \ge 2\) and \(F \neq F_2\).
  \end{enumerate}
\end{example*}
\end{document}
"""

}


package partex
import fastparse.Parsed
import org.scalatest.FunSuite
import MathLang._
import MathParser.parseMath


class MathTest extends FunSuite {
  test("identifying additions") {
    val parTrial = parseMath("a+b")
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Vector(
        Add(Positive(Variable("a",Vector())),Positive(Variable("b",Vector())))
      )
    )}
  }

  test("handling resrvdWd") {
    val parTrial = parseMath("\\mathrm{e}^{-x}\\, \\mathrm{d}x")
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Vector(
        Positive(
          Multiply(
            Formatted("mathrm", Positive(Variable("e", Vector())), Vector(Superscript(Vector(Negative(Variable("x", Vector())))))),
            Multiply(Formatted("mathrm", Positive(Variable("d", Vector())), Vector()), Variable("x", Vector()))
          )
        )
      )
    )}
  }


  test("handling subscript and superscript") {
    val parTrial = parseMath("x_1^n + x_2^n = x_3^n")
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Vector(
        Equality(
          Add(
            Positive(Variable("x", Vector(Subscript(Vector(Positive(Numeral("1", Vector())))), Superscript(Vector(Positive(Variable("n", Vector()))))))),
            Positive(Variable("x", Vector(Subscript(Vector(Positive(Numeral("2", Vector())))), Superscript(Vector(Positive(Variable("n", Vector())))))))
          ),
          Positive(Variable("x", Vector(Subscript(Vector(Positive(Numeral("3", Vector())))), Superscript(Vector(Positive(Variable("n", Vector())))))))
        )
      )
    )}
  }

    test("identifying function operations") {
    val parTrial = parseMath("\\sin(a + b ) = \\sin(a)\\cos(b) + \\cos(a)\\sin(b)")
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Vector(
        Equality(
          Positive(
            FuncOperation(
              Sym("sin", Vector()),
              Vector(Add(Positive(Variable("a", Vector())), Positive(Variable("b", Vector())))),
              Vector()
            )
          ),
          Add(
            Positive(
              Multiply(
                FuncOperation(Sym("sin", Vector()), Vector(Positive(Variable("a", Vector()))), Vector()),
                FuncOperation(Sym("cos", Vector()), Vector(Positive(Variable("b", Vector()))), Vector())
              )
            ),
            Positive(
              Multiply(
                FuncOperation(Sym("cos", Vector()), Vector(Positive(Variable("a", Vector()))), Vector()),
                FuncOperation(Sym("sin", Vector()), Vector(Positive(Variable("b", Vector()))), Vector())
              )
            )
          )
        )
      )
    )}
  }

  test("handling fractions") {
    val parTrial = parseMath("\\frac{f(x+h)-f(x)}{h}")
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Vector(
        Positive(
          Fraction(
            Subtract(
              Positive(
                FuncOperation(
                  Variable("f", Vector()),
                  Vector(Add(Positive(Variable("x", Vector())), Positive(Variable("h", Vector())))),
                  Vector()
                )
              ),
              Positive(FuncOperation(Variable("f", Vector()), Vector(Positive(Variable("x", Vector()))), Vector()))
            ),
            Positive(Variable("h", Vector())),
            Vector()
          )
        )
      )
    )}
  }

  test("identifying inequalities"){
    val parTrial = parseMath("a \\leq b = c < d")
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Vector(
        LessThanEqual(Positive(Variable("a", Vector())), Positive(Variable("b", Vector()))),
        Equality(Positive(Variable("b", Vector())), Positive(Variable("c", Vector()))),
        LessThan(Positive(Variable("c", Vector())), Positive(Variable("d", Vector())))
      )
    )}
  }

  test("handling formatted variables") {
    val parTrial = parseMath("\\mathcal{T} = {\\mathcal T}")
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Vector(
        Equality(
          Positive(Formatted("mathcal", Positive(Variable("T", Vector())), Vector())),
          Positive(Formatted("mathcal", Positive(Variable("T", Vector())), Vector()))
        )
      )
    )}
  }

  test("identifying sets") {
    val parTrial1 = parseMath("\\mathcal{S} = \\{ 1,2,3 , 4 \\}")
    val parTrial2 = parseMath("\\{ f_1(x), f_2(x), f_3(x), \\cdots \\}")
    if (parTrial1.isInstanceOf[Parsed.Failure] || parTrial2.isInstanceOf[Parsed.Failure])
      {fail("parsing failed")}
    else { assert(parTrial1.get.value ==
      Vector(
        Equality(
          Positive(Formatted("mathcal", Positive(Variable("S", Vector())), Vector())),
          Positive(
            SetByElems(
              Vector(
                Positive(Numeral("1", Vector())),
                Positive(Numeral("2", Vector())),
                Positive(Numeral("3", Vector())),
                Positive(Numeral("4", Vector()))
              ),
              Vector()
            )
          )
        )
      )
    && parTrial2.get.value ==
      Vector(
        Positive(
          SetByElems(
            Vector(
              Positive(
                FuncOperation(Variable("f", Vector(Subscript(Vector(Positive(Numeral("1", Vector())))))), Vector(Positive(Variable("x", Vector()))), Vector())
              ),
              Positive(
                FuncOperation(Variable("f", Vector(Subscript(Vector(Positive(Numeral("2", Vector())))))), Vector(Positive(Variable("x", Vector()))), Vector())
              ),
              Positive(
                FuncOperation(Variable("f", Vector(Subscript(Vector(Positive(Numeral("3", Vector())))))), Vector(Positive(Variable("x", Vector()))), Vector())
              ),
              Positive(Sym("cdots", Vector()))
            ),
            Vector()
          )
        )
      )
    )}
  }

  test("handling set operations") {
    val parTrial = parseMath("\\{ 1, 2, 3 \\} \\cup (A \\setminus B)")
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Vector(
        Union(
          Positive(
            SetByElems(Vector(Positive(Numeral("1", Vector())), Positive(Numeral("2", Vector())), Positive(Numeral("3", Vector()))), Vector())
          ),
          Positive(Paren(SetMinus(Positive(Variable("A", Vector())), Positive(Variable("B", Vector()))), Vector()))
        )
      )
    )}
  }


}

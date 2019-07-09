package partex
import fastparse._ , NoWhitespace._
import org.scalatest.FunSuite
import MathLang._
import MathParser.{expr, mathLine}


class MathTest extends FunSuite {
  test("identifying additions") {
    val parTrial = parse("a+b",expr(_))
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Add(Positive(Variable("a",Vector())),Positive(Variable("b",Vector())))) }
  }

  test("handling resrvdWd") {
    val parTrial = parse("\\mathrm{e}^{-x}\\, \\mathrm{d}x",expr(_))
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Positive(
        Multiply(
          Formatted("mathrm", Positive(Variable("e", Vector())), Vector(Superscript(Negative(Variable("x", Vector()))))),
          Multiply(Formatted("mathrm", Positive(Variable("d", Vector())), Vector()), Variable("x", Vector()))
        )
      )
    )}
  }


  test("handling subscript and superscript") {
    val parTrial = parse("x_1^n + x_2^n = x_3^n",mathLine(_))
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      MathLine(
        Vector(
          Equality(
            Add(
              Positive(Variable("x", Vector(Subscript(Numeral("1", Vector())), Superscript(Variable("n", Vector()))))),
              Positive(Variable("x", Vector(Subscript(Numeral("2", Vector())), Superscript(Variable("n", Vector())))))
            ),
            Positive(Variable("x", Vector(Subscript(Numeral("3", Vector())), Superscript(Variable("n", Vector())))))
          )
        )
      )
    )}
  }

    test("identifying function operations") {
    val parTrial = parse("\\sin(a + b ) = \\sin(a)\\cos(b) + \\cos(a)\\sin(b)",mathLine(_))
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      MathLine(
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
      )
    )}
  }

  test("handling fractions") {
    val parTrial = parse("\\frac{f(x+h)-f(x)}{h}",mathLine(_))
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      MathLine(
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
      )
    )}
  }

  test("identifying inequalities"){
    val parTrial = parse("a \\leq b = c < d",mathLine(_))
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      MathLine(
        Vector(
          LessThanEqual(Positive(Variable("a", Vector())), Positive(Variable("b", Vector()))),
          Equality(Positive(Variable("b", Vector())), Positive(Variable("c", Vector()))),
          LessThan(Positive(Variable("c", Vector())), Positive(Variable("d", Vector())))
        )
      )
    )}
  }

  test("handling formatted variables") {
    val parTrial = parse("\\mathcal{T} = {\\mathcal T}",mathLine(_))
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      MathLine(
        Vector(
          Equality(
            Positive(Formatted("mathcal", Positive(Variable("T", Vector())), Vector())),
            Positive(Formatted("mathcal", Positive(Variable("T", Vector())), Vector()))
          )
        )
      )
    )}
  }

  test("identifying sets") {
    val parTrial1 = parse("\\mathcal{S} = \\{ 1,2,3 , 4 \\}",mathLine(_))
    val parTrial2 = parse("\\{ f_1(x), f_2(x), f_3(x), \\cdots \\}",expr(_))
    if (parTrial1.isInstanceOf[Parsed.Failure] || parTrial2.isInstanceOf[Parsed.Failure])
      {fail("parsing failed")}
    else { assert(parTrial1.get.value ==
      MathLine(
        Vector(
          Equality(
            Positive(Formatted("mathcal", Positive(Variable("S", Vector())), Vector())),
            Positive(
              Set(
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
      )
    && parTrial2.get.value ==
      Positive(
        Set(
          Vector(
            Positive(
              FuncOperation(Variable("f", Vector(Subscript(Numeral("1", Vector())))), Vector(Positive(Variable("x", Vector()))), Vector())
            ),
            Positive(
              FuncOperation(Variable("f", Vector(Subscript(Numeral("2", Vector())))), Vector(Positive(Variable("x", Vector()))), Vector())
            ),
            Positive(
              FuncOperation(Variable("f", Vector(Subscript(Numeral("3", Vector())))), Vector(Positive(Variable("x", Vector()))), Vector())
            ),
            Positive(Sym("cdots", Vector()))
          ),
          Vector()
        )
      )
    )}
  }

  test("handling set operations") {
    val parTrial = parse("\\{ 1, 2, 3 \\} \\cup (A \\setminus B)",expr(_))
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Union(
        Positive(
          Set(Vector(Positive(Numeral("1", Vector())), Positive(Numeral("2", Vector())), Positive(Numeral("3", Vector()))), Vector())
        ),
        Positive(Paren(SetMinus(Positive(Variable("A", Vector())), Positive(Variable("B", Vector()))), Vector()))
      )
    )}
  }


}

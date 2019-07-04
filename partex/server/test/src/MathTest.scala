package partex.server
import fastparse._ , NoWhitespace._
import org.scalatest.FunSuite
import partex.shared.MathLang._
import partex.shared.MathParser.expr
import partex.shared.MathParser.mathLine


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


}

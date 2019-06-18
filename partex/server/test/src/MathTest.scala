package partex.server
import fastparse._ , NoWhitespace._
import org.scalatest.FunSuite
import partex.shared.MathLang._
import partex.shared.MathParser.expr

class MathTest extends FunSuite {
  test("identifying additions") {
    val parTrial = parse("a+b",expr(_))
    if (parTrial.isInstanceOf[Parsed.Failure]) {fail("parsing failed")}
    else { assert(parTrial.get.value ==
      Add(Positive(Variable("a",Vector())),Positive(Variable("b",Vector())))) }
  }
}

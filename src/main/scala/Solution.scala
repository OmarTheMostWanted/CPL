//import com.sun.org.apache.xerces.internal.impl.xpath.regex.ParseException
//
//object Solution {
//
//  /**
//   * Write an s-expression whose result is 42.
//   */
//  def is42 = "( + 42 0)"
//
//
//  /**
//   * Write an s-expression that evaluates to a list containing the numbers
//   * between and including 0 and 3.
//   */
//  def between0and3 = "(cons 0 (cons 1 (cons 2 (cons 3 nil ))))"
//
//  /**
//   * You can also add your own defs below and test them by calling them
//   * from your own set of tests (under the "Test" tab).
//   *
//   * This way you can validate your understanding of a given concept.
//   */
//
//  import org.scalatest.FunSuite
//
//  abstract class Solution extends FunSuite {
//
//    test("Verify correct implementation") {
//      assertResult(NumV(5)) {
//        interp(desugar(parse("5")))
//      }
//    }
//
//    test("Catch erroneous parse behavior") {
//      intercept[ParseException] {
//        parse("()")
//      }
//    }
//
//    test("Catch erroneous interp behavior") {
//      intercept[InterpException] {
//        interp(desugar(parse("(+ true 5)")))
//      }
//    }
//
//    test("test cons"){
//      assertResult(NumV(5)){
//        interp(desugar(parse("(cons 5 nil")))
//      }
//    }
//
//    def parse(s: String): ExprExt
//    def desugar(expr: ExprExt): ExprC
//    def interp(expr: ExprC): Value
//  }
//
//
//
//
//}
//

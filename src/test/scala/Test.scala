//test: Test

import org.scalatest.FunSuite
import week2.basicInterpreter.{ExprExt, _}

class Test extends FunSuite {

  def interp(x: ExprC): Value ={
    Interp.interp(x)
  }

  def desugar(x: ExprExt): ExprC ={
    Desugar.desugar(x)
  }

  def parse(x:String): ExprExt ={
    Parser.parse(x)
  }

  /**
   * Tests for Parsing
   */
  test("Parse 5") {
    assertResult(
      NumExt(5)
    ) {
      Parser.parse("5")
    }
  }

  /**
   * Tests for Desugaring
   */

  test("Desugar 5") {
    assertResult(
      NumC(5)
    ) {
      Desugar.desugar(NumExt(5))
    }
  }

  /**
   * Tests for Interpreting
   */

  test("Interp 5") {
    assertResult(
      NumV(5)
    ) {
      Interp.interp(NumC(5))
    }
  }

  test("Interp 5+true throws InterpException") {
    intercept[InterpException] {
      Interp.interp(PlusC(NumC(5), TrueC()))
    }
  }

  test("Verify correct implementation") {
    assertResult(NumV(5)) {
      Interp.interp(desugar(parse("5")))
    }
  }

  test("Verify correct implementation+") {
    assertResult(NumV(5)) {
      Interp.interp(desugar(parse("(+ 1 4)")))
    }
  }

  test("Verify correct implementation*") {
    assertResult(NumV(5)) {
      Interp.interp(desugar(parse("(* 5 1)")))
    }
  }

  test("Verify correct implementation-") {
    assertResult(NumV(5)) {
      Interp.interp(desugar(parse("(- 6 1)")))
    }
  }

  test("Verify correct implementation-U") {
    assertResult(NumV(6)) {
      Interp.interp(desugar(parse("(* -6 -1)")))
    }
  }

  test("Verify correct implementation NilV") {
    assertResult(NilV()) {
      Interp.interp(desugar(parse("nil")))
    }
  }

  test("Verify correct implementation Cons") {
    assertResult(ConsV(NumV(10), NilV())) {
      Interp.interp(desugar(parse("(cons 10 nil)")))
    }
  }

  test("Verify correct implementation Cons0") {
    assertResult(ConsV(NumV(10), ConsV(NumV(0), NilV()))) {
      Interp.interp(desugar(parse("(cons 10 (cons 0 nil))")))
    }
  }

  test("Verify correct implementation Cons1") {
    assertResult(ConsV(NumV(10), ConsV(NumV(0), ConsV(NumV(0), NilV())))) {
      Interp.interp(desugar(parse("(cons 10 (cons 0 (cons 0 nil)))")))
    }
  }

  test("is-list False") {
    assertResult(BoolV(false)) {
      Interp.interp(desugar(parse("(is-list 1)")))
    }
  }

  test("is-list True") {
    assertResult(BoolV(true)) {
      Interp.interp(desugar(parse("(is-list nil)")))
    }
  }

  test("is-list Exception") {
    assertResult(BoolV(true)) {
      Interp.interp(desugar(parse("(is-list (list 1 nil))")))
    }
  }

  test("is-list True1") {
    assertResult(BoolV(true)) {
      Interp.interp(desugar(parse("(is-list (cons 10 nil))")))
    }
  }

  test("is-nil True 1") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(is-nil nil)")))
    }
  }

  test("is-nil False") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("(is-nil (cons 10 nil))")))
    }
  }

  test("is-nil Exception") {
    intercept[InterpException] {
      interp(desugar(parse("(is-nil 5)")))
    }
  }

  test("Verify correct implementation head") {
    assertResult(NumV(5)) {
      interp(desugar(parse("(head (cons 5 nil))")))
    }
  }


  test("Verify correct implementation headEmpty") {
    intercept[InterpException] {
      interp(desugar(parse("(head 5)")))
    }
  }

  test("Verify correct implementation tailSimple") {
    assertResult(NumV(5)) {
      interp(desugar(parse("(tail (cons 2 5))")))
    }
  }

  // test("Verify correct implementation cond") {
  //   assertResult(CondExt(List((BinOpExt("num>", NumExt(0), NumExt(1)), NumExt(1))))) {
  //     parse("(cond ((num> 0 1) 1) ((num< 1 0) 2))")
  //   }
  // }

  // test("Verify correct implementation tailComplex") {
  //   assertResult(NumV(5)) {
  //     interp(desugar(parse("(tail (cons 2 (cons 6 (cons 0 (cons 7 (cons 8 (cons 9 5)))))))")))
  //   }
  // }

  // test("Verify correct implementation tail") {
  //   assertResult(NilV()) {
  //     interp(desugar(parse("(tail (cons 5 (cons 2 nil)))")))
  //   }
  // }

  test("Verify correct implementation tailEmpty") {
    intercept[InterpException] {
      interp(desugar(parse("(tail 5)")))
    }
  }

  test("list construction") {
    assertResult(ConsV(NumV(1), (ConsV(NumV(2), (ConsV(NumV(3), NilV())))))) {
      interp(desugar(parse("(list 1 2 3)")))
    }
  }

  test("list construction0") {
    assertResult(ConsV(NumV(1), (ConsV(NumV(2), (ConsV(NumV(3), NilV())))))) {
      interp(desugar(parse("(list 1 2 3)")))
    }
  }

  test("list construction1") {
    assertResult(ConsV(NumV(1), NilV())) {
      interp(desugar(parse("(list 1 nil)")))
    }
  }

  test("Catch erroneous parse behavior") {
    intercept[ParseException] {
      parse("()")
    }
  }

  test("Catch erroneous interp behavior") {
    intercept[InterpException] {
      interp(desugar(parse("(+ true 5)")))
    }
  }

}
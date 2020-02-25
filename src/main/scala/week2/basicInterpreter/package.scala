package week2

import scala.util.parsing.combinator._


package object basicInterpreter {

  //S-Expressions
  sealed abstract class SExpr

  case class SSym(sym: String) extends SExpr

  case class SList(list: List[SExpr]) extends SExpr

  case class SNum(num: Int) extends SExpr

  //Abstract Syntax
  sealed abstract class ExprExt

  case class TrueExt() extends ExprExt

  case class FalseExt() extends ExprExt

  case class NumExt(num: Int) extends ExprExt

  case class BinOpExt(s: String, l: ExprExt, r: ExprExt) extends ExprExt

  case class UnOpExt(s: String, e: ExprExt) extends ExprExt

  case class IfExt(c: ExprExt, t: ExprExt, e: ExprExt) extends ExprExt

  case class ListExt(l: List[ExprExt]) extends ExprExt

  case class NilExt() extends ExprExt

  object ExprExt {
    val binOps = Set("+", "*", "-", "and", "or", "num=", "num<", "num>", "cons")
    val unOps = Set("-", "not", "head", "tail", "is-nil", "is-list")
  }

  //Desugared Syntax
  sealed abstract class ExprC

  case class TrueC() extends ExprC

  case class FalseC() extends ExprC

  case class NumC(n: Int) extends ExprC

  case class PlusC(l: ExprC, r: ExprC) extends ExprC

  case class MultC(l: ExprC, r: ExprC) extends ExprC

  case class IfC(c: ExprC, t: ExprC, e: ExprC) extends ExprC

  case class EqNumC(l: ExprC, r: ExprC) extends ExprC

  case class LtC(l: ExprC, r: ExprC) extends ExprC

  case class NilC() extends ExprC

  case class ConsC(l: ExprC, r: ExprC) extends ExprC

  case class HeadC(e: ExprC) extends ExprC

  case class TailC(e: ExprC) extends ExprC

  case class IsNilC(e: ExprC) extends ExprC

  case class IsListC(e: ExprC) extends ExprC


  //Values
  sealed abstract class Value

  case class NumV(v: Int) extends Value

  case class BoolV(v: Boolean) extends Value

  case class NilV() extends Value

  case class ConsV(head: Value, tail: Value) extends Value

  //exceptions
  abstract class ParseException(msg: String = null)

  abstract class DesugarException(msg: String = null)

  abstract class InterpException(msg: String = null)

  case class NotImplementedException(s: String) extends RuntimeException(s)


  //takes in the syntax and generates S-Expressions syntax
  object Reader extends JavaTokenParsers {

    def read(text: String): SExpr = {
      val result = parseAll(sexpr, text)
      result match {
        case Success(r, _) => r
        case Failure(msg, n) =>
          sys.error(msg + " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
        case Error(msg, n) =>
          sys.error(msg + " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
      }
    }

    def sexpr: Parser[SExpr] = (num | symbol | slist)

    def symbol: Parser[SExpr] = not(wholeNumber) ~> "[^()\\s]+".r ^^ SSym

    def slist: Parser[SExpr] = "(" ~> sexpr.+ <~ ")" ^^ SList

    def num: Parser[SExpr] = wholeNumber ^^ { s => SNum(s.toInt) }
  }

  object Parser {
    def parse(str: String): ExprExt = parse(Reader.read(str))

    def parse(sexpr: SExpr): ExprExt = {
      sexpr match {
        case SNum(num) => NumExt(num)
        case SList(list) => {
          list match {
            case Nil => throw new RuntimeException()
            case SSym("if") :: c :: t :: e :: Nil => IfExt(parse(c), parse(t), parse(e)) //?
            case SSym("cond") :: branchs => {
              branchs match {
                case Nil => throw new RuntimeException()
              }
            }
            case SSym(s) :: e :: Nil => UnOpExt(s, parse(e))
            case SSym(s) :: l :: r :: Nil => BinOpExt(s, parse(l), parse(r))
          }
        }
      }
    }
  }

  object Desugar {
    def desugar(e: ExprExt): ExprC = {

      e match {
        case TrueExt() => TrueC()
        case FalseExt() => FalseC()
        case NumExt(n) => NumC(n)
        case BinOpExt(s, l, r) => {
          s match {
            case "+" => PlusC(desugar(l), desugar(r))
            case "-" => PlusC(desugar(l), MultC(NumC(-1), desugar(r)))
            // do not generate terms inside the recursive call
            // PlusC(desugar(l) , desugar(UnOpExt("-" , r)))
            case "*" => MultC(desugar(l), desugar(r))
            case "and" => {
              IfC(desugar(l), desugar(r), FalseC())
            }
            case "or" => {
              IfC(desugar(l), TrueC(), desugar(r))
            }
            case "num=" => EqNumC(desugar(l), desugar(r))
            case "num<" => LtC(desugar(l), desugar(r))
            case "num>" => LtC(desugar(r), desugar(l))
            case "cons" => ConsC(desugar(l), desugar(r))
          }
        }
        case UnOpExt(s, e) => {
          s match {
            case "-" => MultC(NumC(-1), desugar(e))
            case "not" => {
              IfC(desugar(e), FalseC(), TrueC())
            }
            case "head" => HeadC(desugar(e))
            case "tail" => TailC(desugar(e))
            case "is-nil" => IsNilC(desugar(e))
            case "Is-list" => IsListC(desugar(e))
          }
        }
        case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))
        case ListExt(l) => {
          l match {
            case Nil => NilC()
            case e :: Nil => ConsC(desugar(e), NilC())
            case e :: b => ConsC(desugar(e), desugar(ListExt(b)))
          }
        }
        case NilExt() => NilC()
        // case CondExt(l) =>
        // case CondEExt(l) =>
        case _ => UndefinedC()
      }

    }
  }

  object Interp {
    def interp(e: ExprC): Value = {
      e match {
        case NumC(n) => NumV(n)
        case TrueC() => BoolV(true)
        case FalseC() => BoolV(false)
        case PlusC(l, r) => NumV(getIntValue(interp(l)) + getIntValue(interp(r)))
        case MultC(l, r) => NumV(getIntValue(interp(l)) * getIntValue(interp(r)))
        case IfC(c, t, e) => {
          interp(c) match {
            case BoolV(true) => interp(t)
            case BoolV(false) => interp(e)
            //  case _ => throw InterpException()
          }
        }
        case EqNumC(l, r) => {
          BoolV(getIntValue(interp(l)) == getIntValue(interp(r)))
        }
        case LtC(l, r) => {
          BoolV(getIntValue(interp(l)) < getIntValue(interp(r)))
        }
        case NilC() => NilV()
        case ConsC(l, r) => ConsV(interp(l), interp(r))
        case HeadC(e) => {
          e match {
            // case NilC() => NilV()
            case ConsC(l, r) => interp(l)
            // case _ =>
          }
        }
        case TailC(e) => {
          e match {
            // case NilC() => NilV()
            case ConsC(l, r) => interp(e)
            // case _ =>
          }
        }
        case IsNilC(e) => {
          interp(e) match {
            case NilV() => BoolV(true)
            case _ => BoolV(false)
          }
        }
        case IsListC(e) => {
          interp(e) match {
            case NilV() => BoolV(true)
            case ConsV(l, r) => BoolV(true)
            case _ => BoolV(false)
          }
        }
      }
    }

    def getIntValue(v: Value): Int = {
      v match {
        case NumV(n) => n
        // case _ => throw InterpException()
      }
    }
  }

}

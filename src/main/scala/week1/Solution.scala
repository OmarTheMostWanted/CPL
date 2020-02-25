package week1

class Solution {

  sealed abstract class SExpr

  case class SSym(sym: String) extends SExpr

  case class SNum(num: Int) extends SExpr

  case class SList(list: List[SExpr]) extends SExpr

  // define the case classes for SExpr

  import scala.util.parsing.combinator._

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

  // to do: define the other case classes of the ArithC type
  sealed abstract class ArithC

  case class NumC(num: Int) extends ArithC

  case class PlusC(a: ArithC, b: ArithC) extends ArithC

  case class MultC(a: ArithC, b: ArithC) extends ArithC

  case class ParseException(string: String) extends RuntimeException

  object Parser {
    def parse(str: String): ArithC = parse(Reader.read(str))


    def parse(sexpr: SExpr): ArithC = {

      sexpr match {
        case SNum(n) => NumC(n)
        case SList(list) => {

          list match {
            case SNum(n) :: Nil => NumC(n)

            case SSym(s) :: a :: b => {
              if (s == "+") {
                PlusC(parse(a), parse(SList(b)))
              } else if (s == "*") {
                MultC(parse((a)), parse(SList(b)))
              } else throw new RuntimeException("not + nor *")

            }
            case _ => throw new RuntimeException("does not start with a sympol")

          }
        }
        case _ => throw new RuntimeException("not and SNum nor Slist")
      }
    }

    //    def parse(sexpr: SExpr): ArithC = {
    //
    //      print(sexpr)
    //
    //      sexpr match {
    //        case SNum(n) => NumC(n)
    //        case SList(list) => {
    //          list match {
    //            case SSym(s) :: a :: b => {
    //              if (s == "+") {
    //                PlusC(parse(a), parse(SList(b)))
    //              } else if (s == "*") {
    //                MultC(parse((a)), parse(SList(b)))
    //              } else throw new RuntimeException("Something went wrong")
    //
    //            }
    //            case _ => throw new RuntimeException("Something went wrong")
    //
    //          }
    //        }
    //        case _ => throw new RuntimeException("Something went wrong")
    //      }
    //    }

    //    def parse(sexpr: SExpr): ArithC = {
    //      sexpr match {
    //        case SNum(n) => NumC(n)
    //        case SList(list) => {
    //          list match {
    //            case SSym(s) :: SNum(a) :: c => {
    //              if (s == "+") {
    //                PlusC(NumC(a), parse(SList(c)))
    //              } else if (s == "*") {
    //                MultC(NumC(a), parse(SList(c)))
    //              } else throw new RuntimeException("Something went wrong")
    //
    //            }
    //            case _ => throw new RuntimeException("Something went wrong")
    //
    //          }
    //        }
    //        case _ => throw new RuntimeException("Something went wrong")
    //      }
    //    }
  }

}

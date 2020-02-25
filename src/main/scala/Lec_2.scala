class Lec_2 {

  //Processing Programs
  //Syntax is the structure of program phrases in a language

  /*
  BNF Grammar
   E ::=  n      (literal)
       |  E + E  (plus)
       |  E * E  (mult)

       E's are expresions ->Non-terminal symbols
       n are literals ->Terminal symbols
   */

  /*
  Program phrase
            1 + 2 + 3
  Syntax tree
            (1 + (2 + 3)) or ((1 + 2) + 3)

   */

  /*
  Concrete Syntax (String) --(Parse)--> Abstract Syntax Tree (AST)
                                ^
                                | Implements
                                |
                (Grammar + Disambiguation Strategy)
  Parsers are either hand written or generated using grammar.

   */

  //parsing
  sealed abstract class SExpr

  case class SList(list: List[SExpr]) extends SExpr // a list of other slists

  case class SSym(symbol: String) extends SExpr //symbols

  case class SNum(num: Int) extends SExpr //numbers

  case class SString(string: String) extends SExpr


  sealed abstract class ArithC

  case class NumC(num: Int) extends ArithC

  case class PlusC(l: ArithC, r: ArithC) extends ArithC

  case class MultC(l: ArithC, r: ArithC) extends ArithC

}

/*

// concrete Syntax         //S-Expression Syntax                        //Abstract Syntax
(+ 23 (* 5 6))              SList(List(                                 PlusC(
                                       SSym("+"),                             NumC(23),
                                       SNum(23),                              MultC( NumC(5), MumC(6)))
                                       SList(List(
                                                SSym("*"),
                                                SNum(5),
                                                SNum(6)))))

 */


//Semantics is the meaning of program phrases
// semantics : Phrase → Meaning

//Dynamic Semantics

//Operational Semantics: the meaning of a phrase is given by an algorithm for reducing the phrase to a value [Definitional interpreters]
//Denotational Semantics: the meaning of a phrase is this translation to an underlying mathematical domain
//Axiomatic Semantics: the meaning of a phrase is this set of axioms defining how it affects assertions about the program state
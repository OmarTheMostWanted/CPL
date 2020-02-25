package week2.basicInterpreter

sealed abstract class SExpr
case class NumExt(Int) extends SExpr
case class TrueExt(boolean: true) extends SExpr




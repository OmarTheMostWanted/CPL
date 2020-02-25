class Lec_1 {

  /*

  Lec 1

  An interpreter function.
  An important and frequently used method of defining a programming language is to give an interpreter
   for the language that is written in a second, hopefully better understood language.

   Syntax --(parse)--> Abstract Syntax --(desugar)--> Core --(interpret)-->Value

   this includes:
   First class functions
   Recursion
   Mutation
   Lazy Evaluation
   Types and type checking
   Objects and encapsulation


   Functional programming includes: Functions, Algebraic data types, Pattern matching, Recursion.
   Imperative programming includes: Mutation, Objects Sub-typing, Iteration (while, for)

   */

  //declaring and using variables, val cannot be reassigned
  val hello = "hello World" //immutable values

  //hello = "another" does not work  {error: reassignment to val}

  //declaring variables , can be reassigned
  var x = 10 //mutable values
  x = 1


  //the Principle of Least Power: Given a choice of solutions, pick the least powerful solution capable of solving your problem (use val instead of var)

  //defining and calling Functions

  //function_name(parameter_name:parameter_Type): return_Type

  def max(x: Int, y: Int): Int = {
    if (x > y) /*return*/ x
    else y
  }

  //sometimes return type is un necessary and curly brackets are not needed
  def max1(x: Int, y: Int) = if (x > y) /*return*/ x else y //because fuck every other programming language

  max(3, 5) // function call


  //Pattern Matching Scala
  var e = 0
  var e1 = 1
  var e2 = 2
  var e3 = 3

  e match { //match is an expression (returns a value)
    case 1 => e1
    case 2 => e2
    case _ => e3 //this is the default and its necessary: alternatives do not fall through -> throws a MatchError exception if matching fails

  }


  /*

  calling this: scala> rename1("foo")
bar //bar is printed


   */
  def rename1(s: String) = s match {
    case "foo" => println("bar")
    case "bar" => println("baz")
    case "baz" => println("qux")
    case _ => println("huh?")
  }

  /*
  scala> rename2("foo")
res2: String = bar //this would create a var with the result


   */
  def rename2(s: String) = s match {
    case "foo" => "bar"
    case "bar" => "baz"
    case "baz" => "qux"
    case _ => "huh?"
  }

  //Variable Patterns
  def describe(x: Int) = x match {
    case 0 => "zero" // constant pattern
    case somethingElse => "not zero: " + somethingElse // variable pattern
  }

  //Algebraic Data Types using Case Classes

  // ~ Inductive Definitions

  /*Natural numbers
  Natural language:
   •0 is a natural number
   •if n is a natural number, then n + 1 is a natural number
   */

  sealed abstract class Nat

  case class Zero() extends Nat

  case class Successor(n: Nat) extends Nat

  /*
  Integer lists:
  •nil is an integer list
  •if x is an integer, and xs is an integer list, then x in front of xs is a list
   */

  sealed abstract class IntList

  case class Nil() extends IntList //empty list
  case class Construct(x: Int, xs: IntList) extends IntList //a number followed by another IntList

  //how to create a list:
  def a_list: IntList = Construct(1, Nil())

  //or
  var a: IntList = Construct(2, a_list)

  /*
  Pattern Matching with Algebraic Data Types and Recursive Functions a.k.a. functional programming.
   */


  def length(xs: IntList): Int =
    xs match {
      case Nil() => 0 // base case
      case Construct(x, xs1) => 1 + length(xs1) // recursive case
    }

  //sum all elements of a list
  def sum(xs: IntList): Int = {
    xs match {
      case Nil() => 0 //base case
      case Construct(x, xs1) => x + sum(xs1) //recursive case
    }
  }

  //Concatenating Lists
  def concat(xs: IntList, ys: IntList): IntList = {
    xs match {
      case Nil() => ys
      case Construct(x, xs1) => Construct(x, concat(xs1, ys))
    }
  }


  //using generic types

  sealed abstract class List[A] //because we are "unique" and "special" we dont use < > for generic types
  case class Nil_1[A]() extends List[A]

  case class Cons[A](head: A, tail: List[A]) extends List[A]

  // [1, 2]
  def a_list_1: List[Int] = Cons(1, Cons(2, Nil_1()))

  def b_list: List[Boolean] = Cons(true, Cons(false, Nil_1()))


  //def interp(e: ExprC): Value

  //binary trees

  //Algebraic Data Type
  sealed abstract class BinTree

  case class Leaf() extends BinTree

  case class Node(l: BinTree, v: Int, r: BinTree) extends BinTree

  //replace all a value with another.
  def replace(x: Int, y: Int, t: BinTree): BinTree = t match {
    case Leaf() => Leaf()
    case Node(l, n, r) => Node(replace(x, y, l) /*check left*/ , if (n == x) y else n /*check current node*/ , replace(x, y, r) /*check right*/)
  }

  //make a list of the tree
  def flatten(t: BinTree): IntList = t match {
    case Leaf() => Nil()
    case Node(l, n, r) => concat(flatten(l), Construct(n, flatten(r)))
  }

}

package week1

class WeekOne {

  def factorial(n: Int): Int = {

    if (n < 0) return -1

    n match {
      case 0 => 0
      case c => n + factorial(c - 1)
    }
  }

  def fibonacci(n: Int): Int = {
    if (n == 1) n
    else if (n < 1) 0
    else fibonacci(n - 1) + fibonacci(n - 2);
  }


  /**
   * We will now define a slightly less trivial case class,
   * representing a list structure containing integers.
   *
   * Any list can be one of two types:
   * it can be empty, or it can be an element joined to the remaining list.
   *
   * Notice that the structure of these case classes is recursive.
   */
  sealed abstract class IntList

  case class Empty() extends IntList // The empty list, often called Nil
  case class Element(n: Int, tail: IntList) extends IntList // Element is usually called Cons

  /**
   * As an example, let's create a function that lists descending integers from n to 1.
   */
  def listFrom(n: Int): IntList = {
    if (n == 0) Empty()
    else Element(n, listFrom(n - 1))
  }

  /**
   * EXERCISE:
   * Implement the function sumIntList(xs).
   * It should take an IntList, and return the sum of all it's elements.
   * Use pattern matching!
   */
  def sumIntList(xs: IntList): Int = {
    xs match {
      case Empty() => 0
      case Element(x, xs1) => x + sumIntList(xs1)
    }
  }

  /**
   * EXERCISE:
   * Implement the function head(xs).
   * It should return the first element in a list.
   * If the list is empty, throw a NoSuchElementException.
   */
  def head(xs: IntList): Int = {
    xs match {
      case Element(x, xs1) => x
      case Empty() => throw new NoSuchElementException("List is empty")
    }
  }

  /**
   * EXERCISE:
   * Define the function tail(xs).
   * It should accept an IntList and return the same IntList, but without the first element.
   * If the list is empty, throw a NoSuchElementException.
   */
  def tail(xs: IntList): IntList = {
    xs match {
      case Empty() => throw new NoSuchElementException("NoSuchElementException")

      case Element(x, xs1) => xs1
    }
  }

  /**
   * EXERCISE:
   * Define the function concat(xs, ys).
   * It should concatenate two IntLists.
   */
  def concat(xs: IntList, ys: IntList): IntList = {
    xs match {
      case Empty() => ys
      case Element(x, xs1) => Element(x, concat(xs1, ys))
    }
  }


  /**
   * EXERCISE:
   * Define the function take(n, xs).
   * It should return the first n elements of xs.
   * This function should never throw an exception.
   */
  def take(n: Int, xs: IntList): IntList = {

    if (n < 0) {
      return Empty()
    }

    if (n > length(xs)) {
      return xs
    }

    n match {
      case 0 => Empty()
      case c => Element(head(xs), take(c - 1, tail(xs)))
    }
  }

  /**
   * EXERCISE:
   * Define the function drop(n, xs).
   * It should return the list xs, without the first n elements.
   * This function should never throw an exception.
   */
  def drop(n: Int, xs: IntList): IntList = {

    if (n < 0) {
      return Empty()
    }

    if (n > length(xs)) {
      return Empty()
    }


    n match {
      case 0 => xs
      case c => drop(c - 1, tail(xs))
    }
  }

  /**
   * Finds the length of a list.
   */
  def length(xs: IntList): Int = {
    xs match {
      case Empty() => 0
      case Element(x, xs1) => 1 + length(xs1)
    }
  }


  object Solution {

    /**
     * EXERCISE:
     * Define the case classes for Tree.
     * A tree can be a Leaf, or a Node with a value and two child trees.
     * The height function serves as an example for how the tree structure should be organized
     */
    sealed abstract class Tree

    case class Leaf() extends Tree

    case class Node(v: Int, l: Tree, r: Tree) extends Tree

    def height(tree: Tree): Int = tree match {
      case Leaf() => 0
      case Node(elem, left, right) => 1 + Math.max(height(left), height(right))
    }


    /**
     * EXERCISE:
     * Define the following functions.
     *
     * Do not worry about rebalancing the tree.
     *
     * Hint:
     * Pattern matches can be made more specific with arbitrary conditions:
     * `case A(n) if n>3 => print("n is greater than 3!")`
     */
    def insert(e: Int, t: Tree): Tree = {
      t match {
        case Leaf() => Node(e, Leaf(), Leaf())
        case Node(v, l, r) =>
          if (e > v) {
            Node(v, l, insert(e, r))
          } else if (e < v) {
            Node(v, insert(e, l), r)
          } else t
      }
    }

    def contains(e: Int, t: Tree): Boolean = {
      t match {
        case Leaf() => false
        case Node(v, l, r) => {
          if (v == e) true
          else if (e < v) contains(e, l)
          else contains(e, r)
        }
      }
    }

    def size(t: Tree): Int = {
      t match {
        case Leaf() => 0
        case Node(v, l, r) => 1 + size(l) + size(r)
      }
    }
  }

  import java.util.NoSuchElementException

  object Solution1 {

    /**
     * There are several ways to create a List:
     */
    val as = List(1, 2, 3)
    val bs = 1 :: 2 :: 3 :: Nil
    /** ^1   ^^^^^^^^^^^^^2
     * This second line uses the :: function to append it's left argument to it's right argument.
     * (1) is the element to be appended,
     * (2) is an instance of a List
     *
     * Nil is the constructor for an empty list.
     *
     * Therefore we could also do the following:
     */
    val cs = 1 :: List(2, 3, 4)

    /**
     * Watch out, the following two expressions do not create one list,
     * but rather a list containing integers and lists!
     */
    val ds = List(1, 2) :: List(3, 4) // = List(List(1,2),3,4)
    val es = 1 :: List(2, 3) :: 4 :: List(5, 6) :: List(7) // = List(1,List(2,3),4,List(5,6),7)

    /**
     * If you wish to concat two (or more) lists, use the ::: operator
     */
    val fs = List(1, 2) ::: List(3, 4) // = List(1, 2, 3, 4)

    /**
     * To pattern match on a list, we can use the :: function as illustrated above.
     * Note that any part of the pattern match that we do not use can be ignored with an underscore '_'
     * This way it is not required to give each part of a match a name.
     *
     * Note: The [E] in the function signature introduces a type variable for the generic type of List.
     * This means the head function works for any list, not just List[Int] or List[String].
     */
    def head[E](xs: List[E]): E = xs match {
      case Nil => throw new NoSuchElementException
      case head :: _ => head // so the _ can be another number or a list of numbers or a Nil
    }

    /**
     * It's also possible to pattern match on concrete values.
     * This function matches any list that starts with 1, 2 and 3 as it's first elements:
     */
    def headIs123(xs: List[Int]): Boolean = xs match {
      case 1 :: 2 :: 3 :: _ => true
      case _ => false
    }

    /**
     * EXERCISE:
     * Define a function sum(xs).
     * It should sum every second and third element in a list, ignoring all other elements.
     * Use pattern matching!
     *
     * So:
     * sum(List(1,2,3,4,5,6)) == 2+3 + 5+6
     * sum(List(1,2,3,4,5)) == 2+3
     * sum(List(1,2)) == 0
     */
    def sum(xs: List[Int]): Int = {
      xs match {
        case Nil => 0
        case a :: Nil => 0
        case a :: b :: Nil => 0
        case a :: b :: c :: d => b + c + sum(d)
        case _ => 0
      }
    }

    /**
     * A nice feature of Scala is the possibility to treat functions in the same manner as values.
     * This means we can pass functions as arguments, and return functions from within functions.
     *
     * A demonstration of a so called 'higher order function':
     */
    def transformInt(f: (Int => Int), i: Int): Int = f(i)

    //               ^^^^^^^^^^^^^^1                 ^^^^2
    // 1: Argument f is a function that takes in Int and returns an Int
    // 2: Here we apply the function f to i, and return its result.

    /**
     * Higher order functions will be explained with more detail in class. However, to understand
     * some of the functions in the List class, you should know that when you see:
     * f: A => B
     * in the Scala documentation, this means that the argument or variable f is a function.
     *
     * For example, the List class has a function called 'map':
     * https://www.scala-lang.org/api/current/scala/collection/immutable/List.html#map[B](f:A=%3EB):List[B]
     *
     * The map function lets you transform every element inside a list, without using a for-loop!
     * The argument of map is a function that takes a list element, and returns something else.
     *
     */
    val xs = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    def plusOne(x: Int): Int = x + 1

    val timesTwo = (x: Int) => x * 2

    val ys = xs.map(plusOne) // = List(2,3,4,5,6,7,8,9,10,11)
    val zs = xs.map(timesTwo) // = List(2,4,6,8,10,12,14,16,18,20)

    /**
     * EXERCISE:
     * Define your own version of map! Loops are prohibited, so use recursion.
     */
    def myMap[A, B](xs: List[A], f: (A => B)): List[B] = xs match {
      case Nil => Nil
      case head :: tail => f(head) :: (myMap[A, B](tail, f: (A => B)))
    }

    def addOne(a: Int): Int = {
      a + 1
    }

    /**
     * EXERCISE:
     * Take a look at the takeWhile function:
     * https://www.scala-lang.org/api/current/scala/collection/immutable/List.html#takeWhile(p:A=%3EBoolean):List[A]
     *
     * Define the function takeWhileSmallerThanFive, it should take a list and return the first n
     * elements, until the next element (n+1) is greater or equal to 5.
     *
     * Use the takeWhile function!
     */
    def takeWhileSmallerThanFive(xs: List[Int]): List[Int] = {
      xs match {
        case Nil => Nil
        case a :: b => {
          if (a < 5) {
            (a :: takeWhileSmallerThanFive(b))
          }
          else Nil
        }
      }
    }

    /**
     * EXERCISE:
     * Define the function dropWhileSmallerThanFive, it should take a list and discard the first n
     * elements, until the next element (n+1) is greater or equal to 5.
     *
     * Again, use one of Scala's built-in list functions.
     */
      def dropWhileSmallerThanFive(xs: List[Int]): List[Int] = {
        xs match {
          case Nil => Nil
          case a :: b => {
            if (a < 5) {
              dropWhileSmallerThanFive(b)
            }
            else {
              xs
            }
          }
        }
      }

    /**
     * Take a look at the zip function:
     * https://www.scala-lang.org/api/current/scala/collection/immutable/List.html#zip[B](that:scala.collection.GenIterable[B]):List[(A,B)]
     *
     * This function connects two lists by 'zipping' elements into tuples:
     */
    List(1, 2, 3, 4).zip(List(2, 4, 6, 8)) // = List( (1,2), (2,4), (3,6), (4,8) )

    /**
     * EXERCISE:
     * Define zipWith. It should zip two lists, but instead of zipping elements into a tuple,
     * it should use a function to combine two elements.
     *
     * Example: zipWith(List(1, 2, 3),
     * List(10, 11, 12),
     * (x: Int, y: Int) => x+y)
     * Should return: List(11,13,15)
     *
     * Hint: use map and zip.
     */
    def zipWith[A, B, C](xs: List[A], ys: List[B], f: (A, B) => C): List[C] = {
      val ab = xs.zip(ys)


      //map_2(ab, f)

      ab.map(t => t match {
        case (a, b) => f(a, b)
      })

    }


    def map_2[A, B, C](xs: List[(A, B)], o: (A, B) => C): List[C] = {
      xs match {
        case Nil => Nil
        case (a, b) :: c => {
          o(a, b) :: map_2(c, o)
        }
      }
    }


    def sum(a: Int, b: Int): Int = {
      a + b
    }

  }

}

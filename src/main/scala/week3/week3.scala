package week3

class week3 {

  def double(x:Int):Int = (x + x)

  def twice1[A](f: A => A , x:A) = f(f(x))
  def twice2[A](f: A => A): A => A = x => f(f(x))


}

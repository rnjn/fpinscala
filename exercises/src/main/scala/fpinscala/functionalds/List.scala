package in.rnjn.fpinscala.functionalds

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def head[A](list: List[A]): A =
    list match {
      case Nil => sys.error("no head for empty list")
      case Cons(a, _) => a
  }

  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil => Nil
      case Cons(_, as) => as
    }

  def setHead[A](x : A, list: List[A]): List[A] =
    list match {
      case Nil => List(x)
      case Cons(_, as) => Cons(x, as)
    }

  def drop[A](n : Int, list: List[A]): List[A] =
    n match {
      case 0 => list
      case _ => list match {
        case Nil => sys.error("cannot drop any more from this list")
        case _  => drop(n - 1, tail(list))
      }
    }


  def dropWhile[A](f: (A) => Boolean, list: List[A]): List[A] =
  list match {
    case Nil => list
    case Cons(h, t) => if(f(h)) dropWhile(f, t) else list
  }
}

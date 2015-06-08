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

  def setHead[A](x: A, list: List[A]): List[A] =
    list match {
      case Nil => List(x)
      case Cons(_, as) => Cons(x, as)
    }

  @annotation.tailrec
  def drop[A](n: Int, list: List[A]): List[A] =
    n match {
      case 0 => list
      case _ => list match {
        case Nil => sys.error("cannot drop any more from this list")
        case _ => drop(n - 1, tail(list))
      }
    }

  @annotation.tailrec
  def dropWhile[A](f: (A) => Boolean, list: List[A]): List[A] =
    list match {
      case Nil => list
      case Cons(h, t) => if (f(h)) dropWhile(f, t) else list
    }

  //result = l1 + l2
  def append[A](l1: List[A], l2: List[A]): List[A] = {
    l1 match {
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2))
    }
  }

  def init[A](list: List[A]): List[A] =
    list match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A, B](list: List[A], default: B)(f: (A, B) => B): B =
    list match {
      case Nil => default
      case Cons(h, t) => f(h, foldRight(t, default)(f))
    }

  def addFR(list: List[Int]) = foldRight(list, 0)(_ + _)

  def productFR(list: List[Double]) = foldRight(list, 1.0)(_ * _)

  def length(list: List[Int]): Int = foldRight(list, 0)((_, result) => 1 + result)

  def foldLeft[A, B](list: List[A], default: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def g(l: List[A], result: B): B =
      l match {
        case Nil => result
        case Cons(h, t) => g(t, f(result, h))
      }
    g(list, default)
  }

  def addFL(list: List[Int]) = foldLeft(list, 0)((result, n) => result + n)

  def productFL(list: List[Double]) = foldLeft(list, 1.0)(_ * _)

  def lengthFL(list: List[Int]): Int = foldLeft(list, 0)((result, _) => 1 + result)

  def reverse[A](list: List[A]) : List[A] = foldLeft(list, Nil: List[A])((result, x) => Cons(x, result))

def foldRightViaFoldLeft[A,B](list: List[A], default: B)(f: (A, B) => B): B = 
  foldLeft(reverse(list), default)((b,a) => f(a,b))

  def appendFR[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((i, result) => Cons(i, result))

  def appendFL[A](l1: List[A], l2: List[A]): List[A] = foldLeft(reverse(l1), l2)((result, i) => Cons(i, result))

  def concatLists[A](list: List[List[A]]): List[A] =
    foldRight(list, Nil: List[A])((sublist, result) => appendFR(sublist, result))

  def map[A, B](list: List[A], f: (A => B)): List[B] = foldRight(list, Nil: List[B])((i, result) => Cons(f(i), result))


}



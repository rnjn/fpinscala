package in.rnjn.fpinscala.laziness

trait Stream[+A] {

  def head: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def tail: Option[Stream[A]] = this match {
    case Empty => None
    case Cons(h, t) => Some(t())
  }

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Integer): Stream[A] =
      this match {
        case Empty => Empty
        case Cons(h, t) =>
          if(n > 0)
            Cons(h, () => t().take(n - 1))
          else Empty
      }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        if(p(h())) Cons(h, () => t().takeWhile(p))
        else Empty
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
}

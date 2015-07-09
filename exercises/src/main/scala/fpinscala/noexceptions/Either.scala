package in.rnjn.fpinscala.noexceptions

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]  = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
      Either[EE, C] = this.flatMap(x => b.flatMap(y => Right(f(x,y))))

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def traverse[E, A, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    xs.foldRight[Either[E, List[B]]](Right(List()))((x, r) => r.flatMap(l =>  f(x).map(_ :: l)))

  def sequence[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = traverse(xs)(x => x)

}

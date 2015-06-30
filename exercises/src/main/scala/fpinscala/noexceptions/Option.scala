package in.rnjn.fpinscala.noexceptions

import java.util.regex._

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) this else None
  }


}

case class Some[+A](a: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap((m) => mean(xs.map((x) => math.pow(x - m, 2))))
  }
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat).map(p => (s: String) => p.matcher(s).matches)

  def bothMatcher(pat1: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat1)
      g <- mkMatcher(pat2)
    } yield (f(s) && g(s))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y => f(x, y)))

  def bothMatcher_2(pat1: String, pat2: String, s: String): Option[Boolean] = map2(mkMatcher(pat1), mkMatcher(pat2))((f,g) => f(s) && g(s))


  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def sequence[A](xs: List[Option[A]]): Option[List[A]] = xs.foldRight[Option[List[A]]](Some(List()))((x, r) => r.flatMap(l => x.map(v => v :: l)))

  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] =
    xs.foldRight[Option[List[B]]](Some(List()))((x, r) => r.flatMap(l =>  f(x).map(v => v :: l)))

  def traverse2[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] =
    xs.foldRight[Option[List[B]]](Some(List()))((x, r) => map2(f(x), r)(_ :: _))

  def sequence2[A](xs: List[Option[A]]): Option[List[A]] = traverse2(xs)(x => x)

}

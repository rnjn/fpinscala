package in.rnjn.fpinscala.laziness

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def toList: List[A] =
    if(isEmpty) List()
    else uncons.get._1 :: uncons.get._2.toList
}

object Stream {
  def empty[A]: Stream[A] = new Stream[A] { def uncons = None }
  def cons[A](h: => A, t: => Stream[A]) =
    new Stream[A] {
      lazy val uncons = Some((h, t))
    }
  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

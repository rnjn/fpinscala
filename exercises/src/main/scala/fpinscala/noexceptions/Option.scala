package in.rnjn.fpinscala.noexceptions

sealed trait Option[+A]
case class Some[+A](a: A) extends Option[A]
case object None extends Option[Nothing]




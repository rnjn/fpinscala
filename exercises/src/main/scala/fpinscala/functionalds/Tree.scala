package in.rnjn.fpinscala.functionalds

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def firstLeaf[A](t: Tree[A]): A = t match {
    case Leaf(v) => v
    case Branch(l, r) => firstLeaf(l)
  }

  def max[A](tree: Tree[A])(comparer: (A, A) => A): A = {
    def f(t: Tree[A], m: A): A = t match {
      case Leaf(v) => comparer(m, v)
      case Branch(l, r) => comparer(f(l, m), f(r, m))
    }
    f(tree, firstLeaf(tree))
  }
}

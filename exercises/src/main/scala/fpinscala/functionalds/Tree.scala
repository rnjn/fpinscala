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

  def max[A](tree: Tree[A])(comparer: (A, A) => A): A = tree match {
    case Leaf(v) => v
    case Branch(l: Tree[A], r: Tree[A]) => comparer(max(l)(comparer), max(r)(comparer))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(v) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B) : Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def foldTree[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(foldTree(l)(f)(g), foldTree(r)(f)(g))
  }

  def sizef[A](t: Tree[A]) : Int = foldTree(t)((_) => 1 )((x,y) => x + y)

  def maxf(t: Tree[Int]) : Int = foldTree(t)((a) => a)((x,y) => x max y)

  def depthf[A](t: Tree[A]) : Int = foldTree(t)((_) => 0 )((x,y) => 1 + (x max y))

  def mapf[A, B](t: Tree[A])(f: A => B) : Tree[B] = foldTree(t) ((a) => Leaf(f(a)): Tree[B]) ((l,r) => Branch(l, r))

}

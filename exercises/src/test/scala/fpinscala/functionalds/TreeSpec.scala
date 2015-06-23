import org.scalatest._
import in.rnjn.fpinscala.functionalds._
import in.rnjn.fpinscala.functionalds.Tree._

class TreeSpec extends FunSuite {
  test("size of tree with two leaves") {
    val tree = Branch(Leaf(1), Leaf(2))
    assert(2 == Tree.size(tree))
  }

  test("size of a deeper tree") {
    val branch1 = Branch(Leaf(1), Leaf(2))
    val branch2 = Branch(Leaf(3), Leaf(4))
    val branch3 = Branch(Leaf(99), Leaf(100))
    assert(6 == Tree.size(Branch(Branch(branch1, branch3) , branch2 )))
  }

  test("max (tree) finds the biggest number") {
    val branch1 = Branch(Leaf[Int](1), Leaf[Int](2))
    val branch2 = Branch(Leaf[Int](3), Leaf[Int](4))
    val branch3 = Branch(Leaf[Int](99), Leaf[Int](100))
    assert(100 == Tree.max(Branch(Branch(branch1, branch3) , branch2 ))(Integer.max))
  }

}

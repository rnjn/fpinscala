import org.scalatest._
import in.rnjn.fpinscala.functionalds._
import in.rnjn.fpinscala.functionalds.Tree._

class TreeSpec extends FunSuite {
  test("size of tree with two leaves") {
    val tree = Branch(Leaf(1), Leaf(2))
    assert(2 == Tree.size(tree))
    assert(2 == Tree.sizef(tree))
  }

  test("size of a deeper tree") {
    val branch1 = Branch(Leaf(1), Leaf(2))
    val branch2 = Branch(Leaf(3), Leaf(4))
    val branch3 = Branch(Leaf(99), Leaf(100))
    assert(6 == Tree.size(Branch(Branch(branch1, branch3), branch2)))
    assert(6 == Tree.sizef(Branch(Branch(branch1, branch3), branch2)))
  }

  test("max (tree) finds the biggest number") {
    val branch1 = Branch(Leaf[Int](1), Leaf[Int](2))
    val branch2 = Branch(Leaf[Int](3), Leaf[Int](4))
    val branch3 = Branch(Leaf[Int](99), Leaf[Int](100))
    assert(100 == Tree.max(Branch(Branch(branch1, branch3), branch2))(Integer.max))
    assert(100 == Tree.maxf(Branch(Branch(branch1, branch3), branch2)))
  }

  test("depth of a tree") {
    val tree = Branch(Leaf(1), Leaf(2))
    assert(1 == Tree.depth(tree))

    val branch1 = Branch(Leaf(1), Leaf(2))
    val branch2 = Branch(Leaf(3), Leaf(4))
    val branch3 = Branch(Leaf(99), Leaf(100))
    assert(3 == Tree.depth(Branch(Branch(branch1, branch3), branch2)))
    assert(3 == Tree.depthf(Branch(Branch(branch1, branch3), branch2)))
  }

  test("map tree multiply by 2"){
    val tree = Branch(Leaf(1), Leaf(2))
    assert(Branch(Leaf(2), Leaf(4)) == Tree.map(tree)((x: Int) => x * 2))
    assert(Branch(Leaf(2), Leaf(4)) == Tree.mapf(tree)((x: Int) => x * 2))

    val branch1 = Branch(Leaf(1), Leaf(2))
    val branch2 = Branch(Leaf(3), Leaf(4))
    val branch3 = Branch(Leaf(99), Leaf(100))
    val deeperTree = Branch(Branch(branch1, branch3), branch2)
    assert( 200 ==  Tree.max(Tree.map(deeperTree)((x: Int) => x * 2))(Integer.max))
    assert( 200 ==  Tree.max(Tree.mapf(deeperTree)((x: Int) => x * 2))(Integer.max))
  }
}

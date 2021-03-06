import org.scalatest._
import in.rnjn.fpinscala.functionalds._
import in.rnjn.fpinscala.functionalds.List._

class ListSpec extends FunSuite {
  test("sum of elements") {
    assert(sum(List(1, 2, 3)) == 6)
  }

  test("pattern matching") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

  test("head of Nil") {
    intercept[RuntimeException] {
      List.head(Nil)
    }
  }

  test("head of (1, 2, 3) is 1") {
    assert(1 == List.head(List(1, 2, 3)))
  }
  test("tail of Nil is Nil") {
    assert(Nil == List.tail(Nil))
  }

  test("tail of [1,2,3] is [2,3]") {
    val x = List(1, 2, 3);
    assert(List.head(List.tail(x)) == 2)
    assert(List.head(List.tail(List.tail(x))) == 3)
    assert(List.tail(List.tail(List.tail(x))) == Nil)
  }

  test("setHead 4 on () makes (4)") {
    val x = Nil;
    assert(4 == List.head(List.setHead(4, x)))
  }

  test("setHead 4 on (1, 2, 3) makes (4, 2, 3)") {
    val x = List(1, 2, 3);
    assert(4 == List.head(List.setHead(4, x)))
  }

  test("drop 0 elements from (1, 2) to get (1, 2)") {
    assert(List(1, 2) == List.drop(0, List(1, 2)))
  }

  test("drop 2 elements from (1, 2) to get ()") {
    assert(Nil == List.drop(2, List(1, 2)))

  }

  test("drop 2 elements from (1) to get an exception") {
    intercept[RuntimeException] {
      List.drop(2, List(1))
    }
  }

  test("drop 2 elements from (1, 2, 3) to get (3)") {
    assert(List(3) == List.drop(2, List(1, 2, 3)))
  }

  test("drop 4 elements from (1, 2, 3, 4, 5, 6, 7, 8, 9) to get (5, 6, 7, 8, 9)") {
    assert(List(5, 6, 7, 8, 9) == List.drop(4, List(1, 2, 3, 4, 5, 6, 7, 8, 9)))
  }

  test("dropWhile n > 1 elements from () to get () ") {
    assert(Nil == List.dropWhile((n: Int) => n > 1, Nil))
  }

  test("dropWhile n > 1 elements from (1, 2, 3, 4, 5) to get (1, 2, 3, 4, 5) ") {
    assert(List(1, 2, 3, 4, 5) == List.dropWhile((n: Int) => n > 1, List(1, 2, 3, 4, 5)))
  }

  test("dropWhile n < 6 elements from (1, 2, 3, 4, 5) to get Nil") {
    assert(Nil == List.dropWhile((n: Int) => n < 6, List(1, 2, 3, 4, 5)))
  }

  test("dropWhile n < 5 elements from (1, 2, 3, 4, 5, 6, 7, 8, 9) to get (5, 6, 7, 8, 9)") {
    assert(List(5, 6, 7, 8, 9) == List.dropWhile((n: Int) => n < 5, List(1, 2, 3, 4, 5, 6, 7, 8, 9)))
  }

  test("append () to (1, 2, 3) yields (1, 2, 3)") {
    assert(List(1, 2, 3) == List.append(Nil, List(1, 2, 3)))
    assert(List(1, 2, 3) == List.appendFR(Nil, List(1, 2, 3)))
    assert(List(1, 2, 3) == List.appendFL(Nil, List(1, 2, 3)))
  }

  test("append (1, 2, 3) to () yields (1, 2, 3)") {
    assert(List(1, 2, 3) == List.append(List(1, 2, 3), Nil))
    assert(List(1, 2, 3) == List.appendFR(List(1, 2, 3), Nil))
    assert(List(1, 2, 3) == List.appendFL(List(1, 2, 3), Nil))
  }

  test("append () to () yields ()") {
    assert(Nil == List.append(Nil, Nil))
    assert(Nil == List.appendFR(Nil, Nil))
    assert(Nil == List.appendFL(Nil, Nil))
  }

  test("append (1, 2 ,3) to (4, 5, 6) yields (4, 5, 6, 1, 2, 3)") {
    assert(List(4, 5, 6, 1, 2, 3) == List.append(List(4, 5, 6), List(1, 2, 3)))
    assert(List(4, 5, 6, 1, 2, 3) == List.appendFR(List(4, 5, 6), List(1, 2, 3)))
    assert(List(4, 5, 6, 1, 2, 3) == List.appendFL(List(4, 5, 6), List(1, 2, 3)))
  }

  test("init () to get ()") {
    assert(Nil == List.init(Nil))
  }

  test("init (1) to get ()") {
    assert(Nil == List.init(List(1)))
  }

  test("init (1, 2, 3) to get (1, 2)") {
    assert(List(1, 2) == List.init(List(1, 2, 3)))
  }

  test("add (1, 2, 3) should be 6") {
    assert(6 == List.addFR(List(1, 2, 3)))
    assert(6 == List.addFL(List(1, 2, 3)))
  }

  test("product (1, 2, 3, 4) should be 24") {
    assert(24.0 == List.productFR(List(1, 2, 3, 4)))
    assert(24.0 == List.productFL(List(1, 2, 3, 4)))
  }

  test("construct a list using foldRight") {
    assert(List(1, 2, 3) == foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
    assert(List(3, 2, 1) == foldLeft(List(1, 2, 3), Nil: List[Int])((l, h) => Cons(h, l)))

  }

  test("length of () is 0") {
    assert(0 == List.length(Nil))
    assert(0 == List.lengthFL(Nil))
  }

  test("length of (1) is 1") {
    assert(1 == List.length(List(1)))
    assert(1 == List.lengthFL(List(1)))
  }

  test("length of (1, 2, 3, 4) is 4") {
    assert(4 == List.length(List(1, 2, 3, 4)))
    assert(4 == List.lengthFL(List(1, 2, 3, 4)))
  }

  test("reverse of () is ()") {
    assert(Nil == List.reverse(Nil))
  }

  test("reverse of (1) is (1)") {
    assert(List(1) == List.reverse(List(1)))
  }

  test("reverse of (1, 2, 3, 4) is (4, 3, 2, 1)") {
    assert(List(4, 3, 2, 1) == List.reverse(List(1, 2, 3, 4)))
  }

  test("concat () results in ()") {
    assert(Nil == List.concatLists(List()))
  }

  test("concat ((1,2) (3,4) (5,6)) results in (1, 2, 3, 4, 5, 6)") {
    assert(List(1, 2, 3, 4, 5, 6) == List.concatLists(List(List(1, 2), List(3, 4), List(5, 6))))
  }

  test("map - add 1 to every element (1, 2, 3) results in (2, 3, 4)") {
    assert(List(2, 3, 4) == List.map(List(1, 2, 3), (x: Int) => x + 1))
  }

  test("map - toString every element (1, 2, 3) results in ('1', '2', '3')") {
    assert(List("1", "2", "3") == List.map(List(1, 2, 3), (x: Int) => x.toString))
  }

  test("consif 3, even to (1, 2, 3) yields (1, 2, 3)") {
    assert(List(1, 2, 3) == List.consIf(3, List(1, 2, 3), (n: Int) => n % 2 == 0))
  }

  test("consif 4, even to (1, 2, 3) yields (4, 1, 2, 3)") {
    assert(List(4, 1, 2, 3) == List.consIf(4, List(1, 2, 3), (n: Int) => n % 2 == 0))

  }

  test("filter odds out of (1, 2, 3, 4)") {
    assert(List(2, 4) == List.filter(List(1, 2, 3, 4), (n: Int) => n % 2 == 0))
    assert(List(2, 4) == List.filterUsingFlatmap(List(1, 2, 3, 4), (n: Int) => n % 2 == 0))

  }

  test("flatmap () using f(i) => List(i,i) results in ()") {
    assert(Nil == List.flatmap(Nil, (i: Int) => List(i, i)))
  }

  test("flatmap (1, 2, 3) using f(i) => List(i,i) results in (1, 1, 2, 2, 3, 3)") {
    assert(List(1, 1, 2, 2, 3, 3) == List.flatmap(List(1, 2, 3), (i: Int) => List(i, i)))

  }

  def add = (x: Int, y: Int) => x + y

  test("listAdd () and () results in ()") {
    assert(Nil == List.listAdd(Nil, Nil))
    assert(Nil == List.zip(Nil:List[Int], Nil:List[Int])(add))
  }

  test("listAdd (1, 2, 3) and () results in ()") {
    assert(Nil == List.listAdd(List(1, 2, 3), Nil))
    assert(Nil == List.zip(List(1, 2, 3), Nil:List[Int])(add))
  }

  test("listAdd () and (1, 2, 3) results in ()") {
    assert(Nil == List.listAdd(Nil, List(1, 2, 3)))
    assert(Nil == List.zip(Nil:List[Int], List(1, 2, 3))(add))
  }

  test("listAdd (1, 2) and (1, 2, 3) results in ()") {
    intercept[RuntimeException] {
      List.listAdd(List(1, 2), List(1, 2, 3))
    }
  }

  test("listAdd (1, 2, 3) and (4, 5, 6) results in (5, 7, 9)") {
    assert(List(5, 7, 9) == List.listAdd(List(1, 2, 3), List(4, 5, 6)))
    assert(List(5, 7, 9) == List.zip(List(1, 2, 3), List(4, 5, 6))(add))
  }

  test("() is a subsequence of all lists"){
    assert(List.hasSubsequence(List(1), Nil))
  }

  test("(1, 2) is a subsequence of (1, 2, 3)"){
    assert(List.hasSubsequence(List(1, 2, 3), List(1, 2)))
  }


  test("(1, 2, 3) is not a subsequence of (1, 2)"){
    assert(!List.hasSubsequence(List(1, 2), List(1, 2, 3)))
  }

  test("(1), (2, 3) and (3) are subsequences of (1, 2, 3)"){
    assert(List.hasSubsequence(List(1, 2, 3), List(1)))
    assert(List.hasSubsequence(List(1, 2, 3), List(2, 3)))
    assert(List.hasSubsequence(List(1, 2, 3), List(3)))
  }
}


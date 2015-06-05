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

}


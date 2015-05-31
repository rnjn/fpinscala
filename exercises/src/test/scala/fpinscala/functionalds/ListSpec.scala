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
}


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

  test("setHead 4 on () makes (4)"){
    val x = Nil;
    assert(4 == List.head(List.setHead(4, x)))
  }

  test("setHead 4 on (1, 2, 3) makes (4, 2, 3)"){
    val x = List(1, 2, 3);
    assert(4 == List.head(List.setHead(4, x)))
  }

  test("drop 0 elements from (1, 2) to get (1, 2)"){
    assert(List(1, 2) ==  List.drop(0, List(1, 2)))
  }

  test("drop 2 elements from (1, 2) to get ()"){
    assert(Nil ==  List.drop(2, List(1, 2)))

  }

  test("drop 2 elements from (1) to get an exception"){
    intercept[RuntimeException] {
      List.drop(2, List(1))
    }
  }

  test("drop 2 elements from (1, 2, 3) to get (3)"){
    assert(List(3) ==  List.drop(2, List(1, 2, 3)))
  }

  test("drop 4 elements from (1, 2, 3, 4, 5, 6, 7, 8, 9) to get (5, 6, 7, 8, 9)"){
    assert(List(5, 6, 7, 8, 9) ==  List.drop(4, List(1, 2, 3, 4, 5, 6, 7, 8, 9)))
  }

  test("dropWhile n > 1 elements from () to get () "){
    assert(Nil  ==  List.dropWhile( (n: Int) => n > 1, Nil))
  }

  test("dropWhile n > 1 elements from (1, 2, 3, 4, 5) to get (1, 2, 3, 4, 5) "){
    assert(List(1, 2, 3, 4, 5)  ==  List.dropWhile( (n: Int) => n > 1, List(1, 2, 3, 4, 5)))
  }

  test("dropWhile n < 6 elements from (1, 2, 3, 4, 5) to get Nil"){
    assert(Nil  ==  List.dropWhile( (n: Int) => n < 6 , List(1, 2, 3, 4, 5)))
  }

  test("dropWhile n < 5 elements from (1, 2, 3, 4, 5, 6, 7, 8, 9) to get (5, 6, 7, 8, 9)"){
    assert(List(5, 6, 7, 8, 9) ==  List.dropWhile( (n: Int) => n < 5 , List(1, 2, 3, 4, 5, 6, 7, 8, 9)))
  }

}


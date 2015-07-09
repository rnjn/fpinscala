import org.scalatest._
import in.rnjn.fpinscala.noexceptions._

class EitherSpec extends FunSuite {

  test("map Left with fn that doubles results in Left") {
    val a = Left("foo")
    assert(a == a.map((a: Int) => a * 2))
  }

  test("map Right(2) with fn that doubles results in Right(4)") {
    assert(Right(4) == Right(2).map((x: Int) => x * 2))
  }

  test("flatmap Left with fn that doubles results in Left") {
    assert(Left("foo") == Left("foo").flatMap((a: Int) => Right(a * 2)))
  }

  test("flatmap Right(2) with fn that doubles results in Right(4)") {
    assert(Right(4) == Right(2).flatMap((x: Int) => Right(x * 2)))
  }

  test("orElse defaults for Left") {
    assert(Right("bar") == Left("foo").orElse(Right("bar")))
  }

  test("orElse Right(4) gets Right(4)") {
    assert(Right(4) == Right(4).orElse(Right("foo")))
  }

  def multiply(a: Int, b: Int) = a*b

  test("map two lefts yields first left") {
    assert(Left("foo") == Left("foo").map2(Left("bar"))(multiply))
  }

  test("map two - right and left yields left") {
    assert(Left("bar") == Right(2).map2(Left("bar"))(multiply))
  }

  test("map two rights") {
    assert(Right(10) == Right(2).map2(Right(5))(multiply))
  }

  test("merge sequence of eithers to either of sequence"){
    assert(Right(List(1, 2, 3)) == Either.sequence(List(Right(1), Right(2), Right(3))))
  }
  test("merge sequence of eithers to either of sequence : left if any option is left "){
    assert(Left("foo") == Either.sequence(List(Right(1), Right(2), Left("foo"))))
  }

}

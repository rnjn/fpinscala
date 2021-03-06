import org.scalatest._
import in.rnjn.fpinscala.noexceptions._

class OptionSpec extends FunSuite {
  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("no mean for empty list")
    else xs.sum / xs.length

  test("mean of (1, 2, 3) is 2") {
    assert(2 == mean(Seq(1, 2, 3)))
  }

  test("exception when mean of () ") {
    intercept[ArithmeticException] {
      assert(2 == mean(Seq()))
    }
  }

  def meanO1(optionalSeq: Option[Seq[Double]]): Option[Double] =
    optionalSeq match {
      case None => None
      case Some(a) => if (a.isEmpty) None else Some(a.sum / a.length)
    }

  test("mean of Option(1, 2, 3) is Option(2)") {
    assert(Some(2) == meanO1(Some(Seq(1, 2, 3))))
    assert(Some(2) == Option.mean(Seq(1, 2, 3)))
  }

  test("exception when mean of Option() is Option()") {
    assert(None == meanO1(None))
    assert(None == Option.mean(Nil))
  }

  test("map None with fn that doubles results in None") {
    assert(None == None.map((a: Int) => a * 2))
  }

  test("map Option(2) with fn that doubles results in Option(4)") {
    assert(Some(4) == Some(2).map((x: Int) => x * 2))
  }

  test("flatmap None with fn that doubles results in None") {
    assert(None == None.flatMap((a: Int) => Some(a * 2)))
  }

  test("flatmap Option(2) with fn that doubles results in Option(4)") {
    assert(Some(4) == Some(2).flatMap((x: Int) => Some(x * 2)))
  }
  test("getOrElse defaults for Nil") {
    assert("foo" == None.getOrElse("foo"))
  }
  test("getOrElse Option(4) gets 4") {
    assert(4 == Some(4).getOrElse("foo"))
  }
  test("orElse defaults for None") {
    assert(Some("foo") == None.orElse(Some("foo")))
  }
  test("orElse Option(4) gets Option(4)") {
    assert(Some(4) == Some(4).orElse(None))
  }
  test("variance of () is None") {
    assert(None == Option.variance(Nil))
  }

  test("variance of (1,2,3,4) is 1.25") {
    assert(1.25 == Option.variance(Seq(1, 2, 3, 4)).getOrElse(-1))
  }

  test("nothing matches incorrect regex like /z") {
    assert(!Option.mkMatcher("/z").map(f => f("foo")).getOrElse(false))
  }

  test("foo matches [a-z]+") {
    assert(Option.mkMatcher("[a-z]+").map(f => f("foo")).getOrElse(false))
  }

  test("foo matches only one of [a-z]+ and o+"){
    assert(!Option.bothMatcher("[a-z]+", "o+", "foo").getOrElse(false))
    assert(!Option.bothMatcher_2("[a-z]+", "o+", "foo").getOrElse(false))
  }

  test("foo matches [a-z]+ and fo+"){
    assert(Option.bothMatcher("[a-z]+", "fo+", "foo").getOrElse(false))
    assert(Option.bothMatcher_2("[a-z]+", "fo+", "foo").getOrElse(false))
  }

  test("merge sequence of options to option of sequence"){
    assert(Some(List(1, 2, 3)) == Option.sequence(List(Some(1), Some(2), Some(3))))
    assert(Some(List(1, 2, 3)) == Option.sequence2(List(Some(1), Some(2), Some(3))))
  }
  test("merge sequence of options to option of sequence : None if any option is none "){
    assert(None == Option.sequence(List(Some(1), Some(2), None)))
    assert(None == Option.sequence2(List(Some(1), Some(2), None)))
  }

}

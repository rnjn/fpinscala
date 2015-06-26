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

  def meanO(optionalSeq: Option[Seq[Double]]): Option[Double] =
    optionalSeq match {
      case None => None
      case Some(a) => if (a.isEmpty) None else Some(a.sum / a.length)
    }

  test("mean of Option(1, 2, 3) is Option(2)") {
    assert(Some(2) == meanO(Some(Seq(1, 2, 3))))
  }

  test("exception when mean of Option() is Option()") {
    assert(None == meanO(Some(Seq())))
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
}

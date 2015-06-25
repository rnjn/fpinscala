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
    intercept[ArithmeticException]{
      assert(2 == mean(Seq()))
    }
  }

  def meanO(optionalSeq: Option[Seq[Double]]): Option[Double] =
    optionalSeq match {
      case None => None
      case Some(a) => if(a.isEmpty) None else Some(a.sum / a.length)
    }

  test("mean of Option(1, 2, 3) is Option(2)") {
    assert(Some(2) == meanO(Some(Seq(1, 2, 3))))
  }

  test("exception when mean of Option() is Option()") {
      assert(None == meanO(Some(Seq())))
    }
}

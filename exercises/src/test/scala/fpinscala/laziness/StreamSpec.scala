import org.scalatest._
import scala.collection.immutable.List
import in.rnjn.fpinscala.laziness._

class StreamSpec extends FunSuite {
  test("empty stream to empty list") {
    assert(List() == Empty.toList)
  }

  def oneToFour(i: Integer): (() => Stream[Integer]) = {
    def f(): Stream[Integer] = {
      if (i < 4) Cons(() => i + 1, oneToFour(i + 1))
      else Empty
    }
    f
  }

  val oneToFourStream = Cons[Integer](() => 1, oneToFour(1))
  test("natural numbers 1-4 to list") {
    assert(List(1, 2, 3, 4) == oneToFourStream.toList)
  }


}

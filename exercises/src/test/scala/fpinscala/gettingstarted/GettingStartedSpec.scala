import org.scalatest._
import in.rnjn.fpinscala.gettingstarted._

class GettingStartedSpec extends FunSuite {
  test("Abs(n) is >= 0") {
    assert(First.abs(-1) > 0)
  }
  test("Abs(n) for n < 0 is -n") {
    assert(First.abs(-99) == 99)
  }
  test("Abs(n) for n > 0 is n") {
    assert(First.abs(99) == 99)
  }
  test("Abs(n) for n == 0 is 0") {
    assert(First.abs(0) == 0)
  }

}

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

  test("0! is 1") {
    assert(First.factorial(0) == 1)
  }

  test("1! is 1") {
    assert(First.factorial(1) == 1)
  }

  test("5! is 120") {
    assert(First.factorial(5) == 120)
  }

  test("1st fibonacci number is 1") {
    assert(First.fibonacci(1) == 0)
  }

  test("2nd fibonacci number is 1") {
    assert(First.fibonacci(2) == 1)
  }

  test("3rd fibonacci number is 1") {
    assert(First.fibonacci(3) == 1)
  }

  test("4th fibonacci number is 2") {
    assert(First.fibonacci(4) == 2)
  }

  test("5th fibonacci number is 3") {
    assert(First.fibonacci(5) == 3)
  }

  test("10th fibonacci number is 34") {
    assert(First.fibonacci(10) == 34)
  }


  test("20th fibonacci number is 4181") {
    assert(First.fibonacci(20) == 4181)
  }
}


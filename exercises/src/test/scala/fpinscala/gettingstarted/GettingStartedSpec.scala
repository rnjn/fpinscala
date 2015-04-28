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

  test("1st fibonacci number is 0") {
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

  test("5 is the not present in []") {
    assert(First.binarySearch(Array(), 5, First.greaterThan) == -1)
  }

  test("5 is the 1st item in [5]") {
    assert(First.binarySearch(Array(5), 5, First.greaterThan) == 0)
  }

  test("5 is the 1st item in [5 7 9 11]") {
    assert(First.binarySearch(Array(5, 7, 9, 11), 5, First.greaterThan) == 0)
  }

  test("5 is the 3rd item in [1 3 5 7]") {
    assert(First.binarySearch(Array(1, 3, 5, 7), 5, First.greaterThan) == 2)
  }

  test("6 is not in [1 3 5 7]") {
    assert(First.binarySearch(Array(1, 3, 5, 7), 6, First.greaterThan) == -1)
  }

  test("[] is sorted ascending") {
    assert(First.isSorted(Array(), (x: Int, y: Int) => x < y))
  }

  test("[1 3 5 9 7] is not sorted") {
    assert(!First.isSorted(Array(1, 3, 5, 9, 7), First.greaterThan))
  }

  test("[1 3 5 7] is sorted ascending") {
    assert(First.isSorted(Array(1, 3, 5, 7), First.lesserThan))
  }

  test("[z y x w v u] is sorted descending") {
    assert(First.isSorted(Array('z', 'y', 'x', 'w', 'v', 'u'), (x: Char, y: Char) => x > y))
  }

  test("partial: multiples of 10") {
    val multiplyByTen = First.partial1(10, (x: Int, y: Int) => x * y)
    assert(multiplyByTen(5) == 50)
  }

  test("curry multiples of 10") {
    val multiply = First.curry((x: Int, y: Int) => x * y)
    val multiplyByTen = multiply(10)
    assert(multiplyByTen(5) == 50)
  }

  test("uncurry multiples of 10") {
    val multiplyCurried = First.curry((x: Int, y: Int) => x * y)
    val multiply = First.uncurry(multiplyCurried)
    assert(multiply(10, 5) == 50)
  }

  test("compose factorial and fibonacci to find factorial of nth fibonacci : (fib(5))! == 8! == 40320") {
    val factorialOfNthFibonacci = First.compose(First.factorial, First.fibonacci)
    assert(factorialOfNthFibonacci(7) == 40320)
  }
}

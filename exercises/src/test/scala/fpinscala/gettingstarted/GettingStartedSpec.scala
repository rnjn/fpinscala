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

  test("5 is the not present in []"){
    assert(First.binarySearch(Array(), 5) == -1)
  }

  test("5 is the 0th item in [5]"){
    assert(First.binarySearch(Array(5), 5) == 0)
    assert(First.binarySearch(Array(5), 5, (x:Int,y:Int) => x > y) == 0)
  }

  test("5 is the 1st item in [5 7 9 11]"){
    assert(First.binarySearch(Array(5,7,9,11), 5) == 0)
    assert(First.binarySearch(Array(5,7,9,11), 5, (x:Int,y:Int) => x > y) == 0)
  }

  test("5 is the 3rd item in [1 3 5 7]"){
    assert(First.binarySearch(Array(1,3,5,7), 5) == 2)
    assert(First.binarySearch(Array(1,3,5,7), 5, (x:Int,y:Int) => x > y) == 2)
  }

  test("6 is not in [1 3 5 7]"){
    assert(First.binarySearch(Array(1,3,5,7), 6) == -1)
    assert(First.binarySearch(Array(1,3,5,7), 6, (x:Int,y:Int) => x > y) == -1)
  }
}


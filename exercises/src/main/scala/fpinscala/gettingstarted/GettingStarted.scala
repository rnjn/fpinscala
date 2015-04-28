package in.rnjn.fpinscala.gettingstarted

object First {

  def abs(n: Int): Int = if (n < 0) -n else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def facti(x: Int, f: Int): Int = if (x < 1) f else facti(x - 1, x * f)
    facti(n, 1)
  }

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def fibi(x: Int, f1: Int, f2: Int): Int =
      if (x == n) (f1 + f2) else fibi(x + 1, f2, f2 + f1)
    if (n == 1) 0
    else if (n == 2) 1
    else fibi(3, 0, 1)
  }

  def binarySearch(numbers: Array[Double], key: Double) : Int = {
    @annotation.tailrec
    def bs(low: Int, mid: Int, high: Int): Int = {
      if(low > high) -mid -1
      else {
        val median = numbers(mid)
        if(median == key) mid
        else if(median < key) bs(mid + 1, (mid + high)/2, high )
        else bs(low, (low + mid)/2, mid - 1)
      }
    }
    bs(0, numbers.length / 2  , numbers.length - 1)
  }
}

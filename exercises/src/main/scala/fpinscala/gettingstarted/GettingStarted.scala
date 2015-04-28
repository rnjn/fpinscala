package in.rnjn.fpinscala.gettingstarted

object First {

  def greaterThan = (x: Int, y: Int) => x > y

  def lesserThan = (x: Int, y: Int) => x < y

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

  def isSorted[A](numbers: Array[A], comparator: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def f(i: Int): Boolean = {
      if (i == numbers.length - 1) true
      else if (comparator(numbers(i), numbers(i + 1)))
        f(i + 1)
      else
        false
    }
    if (numbers.length == 0) true
    else
      f(0)
  }

  def binarySearch[A](numbers: Array[A], key: A, greaterThan: (A, A) => Boolean): Int = {
    @annotation.tailrec
    def bs(low: Int, high: Int): Int = {
      if (low > high) -1
      else {
        val mid = (low + high) / 2
        val median = numbers(mid)
        if (median == key) mid
        else if (greaterThan(key, median)) bs(mid + 1, high)
        else bs(low, mid - 1)
      }
    }
    if (isSorted(numbers, (x: A, y: A) => !greaterThan(x, y)))
      bs(0, numbers.length - 1)
    else -1
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = (a, b) => f(a)(b)
}

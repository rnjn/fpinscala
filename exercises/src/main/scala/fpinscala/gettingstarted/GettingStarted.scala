package in.rnjn.fpinscala.gettingstarted

object First {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def facti(x: Int, f: Int): Int = {
      if (x < 1) f
      else facti(x - 1, x * f)
    }
    facti(n, 1)
  }

  def fibonacci(n: Int): Int = {
    if (n == 1) 0
    else if (n == 2) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

}

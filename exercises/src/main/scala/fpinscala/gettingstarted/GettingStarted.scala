package in.rnjn.fpinscala.gettingstarted

object First {

  def abs(n: Int): Int = if (n < 0) -n else n

  def factorial(n: Int): Int = {
    def facti(x: Int, f: Int): Int = if (x < 1) f else facti(x - 1, x * f)
    facti(n, 1)
  }

  def fibonacci(n: Int): Int = {
    def fibi(x: Int, f1: Int, f2: Int ): Int =
      if (x == n) (f1 + f2) else fibi(x + 1, f2, f2 + f1 )
    if (n == 1) 0
    else if (n == 2) 1
    else fibi(3, 0, 1 )
  }
}

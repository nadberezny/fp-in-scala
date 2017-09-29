package fpinscala.gettingstarted

object GettingStarted {
  def fact(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def fib(n: Int): Vector[Int] = {
    @annotation.tailrec
    def go(i: Int, numbers: Vector[Int]): Vector[Int] = {
      if (n == i)
        numbers
      else {
        go(i + 1, numbers :+ numbers.takeRight(2).sum)
      }
    }

    if (n < 1) Vector()
    else if (n == 1) Vector(0)
    else go(2, Vector(0, 1))
  }
}

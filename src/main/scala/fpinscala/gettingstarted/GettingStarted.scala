package fpinscala.gettingstarted

object GettingStarted extends App {
  def binarySearch[A](as: Array[A], key: A, gt: (A,A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key,a)) mid2
        else if (greater) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    val last_ix = as.length - 1

    def go(ix: Int, check: Boolean): Boolean = {
      if (last_ix == ix)
        check
      else
        go(ix + 1, gt(as(ix), as(ix + 1)))
    }

    go(0, true)
  }

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

  println(fact(42))
}

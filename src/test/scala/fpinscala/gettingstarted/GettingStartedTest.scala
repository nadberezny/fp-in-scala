package fpinscala.gettingstarted

import org.scalatest.FlatSpec
import GettingStarted._

class GettingStartedTest extends FlatSpec {

  "#binarySearch" should "be polymorphic and return ix of searched key" in {
    val stringGt = (x: String, y: String) => x > y
    val strings = Array("a", "b", "c", "d")
    assert(binarySearch(strings, "c", stringGt) == 2)

    val intGt = (x: Int, y: Int) => x > y
    val ints = Array(1,2,3,4)
    assert(binarySearch(ints, 3, intGt) == 2)
  }

  "#fact" should "return a factorial" in {
    assert(fact(5) == 120)
  }

  "#fib" should "return a Vector of fibonacci sequence" in {
    assert(fib(5) == Vector(0, 1, 1, 2, 3))
  }

  "#is_sorted" should "check whether an Array is sorted" in {
    val sortedInts = Array(1, 2, 3)
    val intLt = (x: Int, y: Int) => x <= y
    assert(isSorted(sortedInts, intLt))

    val intGt = (x: Int, y: Int) => x > y
    assert(!isSorted(sortedInts, intGt))
  }

}

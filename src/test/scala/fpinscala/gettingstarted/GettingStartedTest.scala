package fpinscala.gettingstarted

import org.scalatest.FlatSpec
import GettingStarted._

class GettingStartedTest extends FlatSpec {

  "#fact" should "return a factorial" in {
    assert(fact(5) == 120)
  }

  "#fib" should "return a Vector of fibonacci sequence" in {
    assert(fib(5) == Vector(0, 1, 1, 2, 3))
  }

}

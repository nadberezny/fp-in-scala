package fpinscala.purestate

object PureState extends App {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        val rng = simple(seed2)

        ((seed2 >>> 16).asInstanceOf[Int], rng)
      }
    }
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (randInt, newRng) = rng.nextInt

    if (randInt == Int.MinValue)
      positiveInt(newRng)
    else
      (randInt.abs, newRng)
  }

  val rng = RNG.simple(42)
  val (res, _) = positiveInt(rng)

  println(res)
}


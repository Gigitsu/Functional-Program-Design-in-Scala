package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(scala.math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() >= 0) {
        val sqrtDelta = scala.math.sqrt(delta())

        Set(-b() + sqrtDelta, -b() - sqrtDelta).map(_ / (2 * a()))
      } else Set()
    }
  }
}

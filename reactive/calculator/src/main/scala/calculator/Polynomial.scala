package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    def delta = b()*b() - 4 * a() * c()
    Signal(delta)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def sol(func: ((Double,Double) => Double), a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Double): Double = {
      func(-b(),Math.sqrt(delta))/(2*a())
    }
    def plus(x:Double, y:Double) = x + y
    def minus(x:Double, y:Double) = x - y
    def expr = {
      delta() match {
        case delta if delta > 0 => Set(sol(minus,a,b,c,delta),sol(plus,a,b,c,delta))
        case delta if delta == 0 => Set(sol(plus,a,b,c,delta))
        case _ => Set[Double]()
      }
    }
    Signal(expr)
  }
}

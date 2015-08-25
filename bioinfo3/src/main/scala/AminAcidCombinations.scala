
object AminAcidCombinations {
  def comb : Int => Int = {
    case 0 => 1
    case k if k < 0 => 0
    case k => comb(k-2) + comb(k-3)
  }
  def main(args: Array[String]) : Unit = {
    println(comb(25))
  }
}

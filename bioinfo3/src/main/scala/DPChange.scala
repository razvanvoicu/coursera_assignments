import scala.collection.mutable
import scala.io.Source

object DPChange {

  val cache = new mutable.HashMap[Int, Int]

  def dpChange(amount: Int, denoms: Array[Int]): Int = {
    amount match {
      case 0 => 0
      case _ => cache.get(amount) match {
        case Some(r) => r
        case None => {
          val minCoins = denoms.map(c => if (amount >= c) dpChange(amount - c, denoms) + 1 else Int.MaxValue).min
          cache += ((amount,minCoins))
          minCoins
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines().toArray
    val amount = lines(0).toInt
    val denoms = lines(1).split(',').map(_.toInt)
    println(dpChange(amount, denoms))
  }

}

import scala.collection.mutable
import scala.io.Source

object ManhattanTourist {

  val cache = new mutable.HashMap[(Int,Int),Int]

  def southOrEast(i:Int,j:Int,down:Array[Array[Int]],right:Array[Array[Int]]) : Int = {
    (i,j) match {
      case (0,0) => 0
      case _ => cache.get(i,j) match {
        case Some(r) => r
        case None => {
          val x = if (i>0) southOrEast(i-1,j,down,right)+down(i-1)(j) else Int.MinValue
          val y = if (j>0) southOrEast(i,j-1,down,right)+right(i)(j-1) else Int.MinValue
          val result = Math.max(x,y)
          cache += (((i,j),result))
          result
        }
      }
    }
  }

  def main(args:Array[String]) = {
    val lines = Source.stdin.getLines().toArray
    val size = lines(0).split(' ').map(_.toInt)
    val n = size(0)
    val m = size(1)
    val down = (1 to n).map(lines(_).split(' ').map(_.toInt)).toArray
    val right = (n+2 to 2*n+2).map(lines(_).split(' ').map(_.toInt)).toArray

    println(southOrEast(n,m,down,right))
  }
}

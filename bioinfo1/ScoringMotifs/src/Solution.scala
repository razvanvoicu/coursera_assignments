
import java.util.Scanner

object Solution {
  val in = new Scanner(System.in)

  def idx : Char => Int = {
    case 'A' => 0
    case 'C' => 1
    case 'G' => 2
    case 'T' => 3
    case _ => sys.error("Unknown symbol")
  }

  def score(s:String,i:Int,l:Int,profile:Array[Array[Double]]) : Double =
    s.substring(i,i+l).zipWithIndex.foldLeft(1.0){
      (x,y) => y match {
        case (c,i) => x * profile(idx(c))(i)
      }
    }

  def scores(s:String,l:Int,profile:Array[Array[Double]]) : Array[(Double,String)] = {
    s.tails.take(s.length-l+1).map(x => (score(x,0,l,profile),x.take(l))).toArray
  }

  def maxScore(s:String,l:Int,profile:Array[Array[Double]]) : String =  scores(s,l,profile).max._2

  def main(args:Array[String]) = {
    val input = in.nextLine()
    val length = in.nextInt()
    val profile : Array[Array[Double]] = Iterator.fill(4){Iterator.fill(length){in.nextDouble()}.toArray}.toArray
    println(maxScore(input,length,profile))
  }
}

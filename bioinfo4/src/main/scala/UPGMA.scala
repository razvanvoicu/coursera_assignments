import java.util.Scanner

import scala.io.Source

object UPGMA {

  def main(args: Array[String]) : Unit = {
    val in = Source.stdin.getLines()
    println(upgma(in).mkString(System.getProperty("line.separator")))
  }

  def upgma(in:Iterator[String]): Map[(Int,Int),Double] = {
    val n = in.next.toInt
    var scn : Scanner = null
    val distArr = Array.tabulate[Int](n,n) {
      case (i,j) =>
        if (j == 0) scn = new Scanner(in.next)
        scn.nextInt()
    }
    Map()
  }
}

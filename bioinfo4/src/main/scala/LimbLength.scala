import java.util.Scanner

import scala.io.Source

object LimbLength {
  def main(args: Array[String]) : Unit = {
    val inp = Source.stdin.getLines().toArray
    val n = inp(0).toInt
    val leaf = inp(1).toInt
    val dist: Array[Array[Int]] = inp.drop(2).map{
      case s =>
        val scanner = new Scanner(s)
        def mkElem(b: Boolean) = if (b) (scanner.nextInt(), scanner.hasNextInt, b) else (0,false,false)
        lazy val line : Stream[(Int,Boolean,Boolean)]= mkElem(true) #:: line.map{h => mkElem(h._2)}
        line.takeWhile(_._3).map(_._1).toArray
    }
    val result = {
      for {
        i <- 0 until n
        k <- 0 until n
        if i != leaf
        if k != leaf
        if i != k
      } yield ((dist(i)(leaf) + dist(leaf)(k) - dist(i)(k))/2)
    }.min
    println(result)
  }
}

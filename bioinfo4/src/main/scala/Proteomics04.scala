import java.util.Scanner

import scala.io.Source

object Proteomics04 {

  val masses = Map(
    128 -> 'Q',
    97  -> 'P',
    99  -> 'V',
    131 -> 'M',
    101 -> 'T',
    71  -> 'A',
    129 -> 'E',
    137 -> 'H',
    103 -> 'C',
    113 -> 'L',
    114 -> 'N',
    115 -> 'D',
    147 -> 'F',
    87  -> 'S',
    57  -> 'G',
    163 -> 'Y',
    156 -> 'R',
    186 -> 'W'
  )

  def main(args: Array[String]) : Unit = {
    val line = Source.stdin.getLines.next
    val sc = new Scanner(line)
    val vec = Stream.continually(if(sc.hasNextInt) sc.nextInt else -1)
      .takeWhile(_ != -1).zipWithIndex.filter(_._1 == 1).map(_._2).toList
    val pep = (-1 :: vec.take(vec.length-1)).zip(vec).map{case (i,j) => masses(j - i)}.mkString("")
    println(pep)
  }
}

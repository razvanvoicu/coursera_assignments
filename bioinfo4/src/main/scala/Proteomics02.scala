import java.util.Scanner

import scala.io.Source
import scala.collection.mutable

object Proteomics02 {

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

  val tree = mutable.Map[Int,List[(Int,Int)]]()
  var paths = Set[List[Int]]()

  def getPaths(n:Int,p:List[Int]) : Unit = {
    tree.get(n) match {
      case Some(l) => l.foreach {
        case (j,c) => getPaths(j,p :+ c)
      }
      case None => paths = paths + p
    }
  }

  def idealSpectrum(pep: List[Int]) =
    { 0 :: { pep.tails.toList.tail.map(_.sum) ++ pep.inits.map(_.sum).toList }.filter(_ != 0) }.sorted

  def main(args: Array[String]) : Unit = {
    val line = Source.stdin.getLines.next
    val scanner = new Scanner(line)

    val ints = 0 #:: Stream.iterate(scanner.nextInt) {
      case _ if scanner.hasNextInt => scanner.nextInt
      case _ => -1
    }.takeWhile(_ != -1)

    for {
      i <- ints
      j <- ints
      if i < j && masses.keySet.contains(j-i)
    } tree += i -> ((j,j-i) :: tree.getOrElse(i,List()))

    getPaths(0,List())

    println(paths.map(_.map(masses(_)).mkString("")))
    println(ints.mkString(" "))
    //println(idealSpectrum(paths.head))
    //println(paths.map(idealSpectrum(_).mkString(" ")))
    println("Result")
    println(paths.filter(l => ints == idealSpectrum(l)).map(_.map(masses(_)).mkString("")))
  }
}

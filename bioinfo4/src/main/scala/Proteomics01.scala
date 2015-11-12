import java.util.Scanner

import scala.io.Source

object Proteomics01 {

  val masses = Map(
    128 -> 'K',
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
    val scanner = new Scanner(line)
    val ints = 0 #:: Stream.iterate(scanner.nextInt) {
      case _ if scanner.hasNextInt => scanner.nextInt
      case _ => -1
    }.takeWhile(_ != -1)
    for {
      i <- ints
      j <- ints
      if i < j && masses.keySet.contains(j-i)
    } { println(s"$i->$j:${masses(j-i)}") }
  }
}

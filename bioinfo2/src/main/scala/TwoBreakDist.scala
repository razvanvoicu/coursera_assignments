
import scala.io.Source

object TwoBreakDist {
  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines.toArray
    val pat = "[\\(][^\\(]+[\\)]".r
    val gens = lines.map(s => pat.findAllIn(s))
    gens.foreach(s => println(s.mkString(" ")))
  }
}

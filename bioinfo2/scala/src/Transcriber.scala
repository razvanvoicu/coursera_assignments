import scala.io.Source
import scala.util.matching.Regex

object Transcriber {
  def main(args:Array[String]) : Unit = {
    val line = Source.stdin.getLines().next()
    val codons = new Regex("[A-Z]{3}").findAllMatchIn(line).toArray
    println(codons.map(c => Const.codon(c.toString)).mkString(""))
  }
}

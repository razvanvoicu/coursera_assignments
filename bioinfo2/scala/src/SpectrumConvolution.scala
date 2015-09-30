import scala.io.Source
import java.util.Scanner

object SpectrumConvolution {
	def main(args: Array[String]) : Unit = {
    val input = Source.stdin.getLines.toArray
    val scanner = new Scanner(input(0))
    val experimental = Stream.iterate((scanner.nextInt, true, scanner.hasNextInt)) {
      p => if (p._3) (scanner.nextInt, p._3, scanner.hasNextInt) else (0, false, false)
    }.takeWhile(_._2).map(_._1)
    val conv = { for ( x <- experimental ; y <- experimental) yield (x-y) } filter (_ > 0)
    println(conv.mkString(" "))
  }
}
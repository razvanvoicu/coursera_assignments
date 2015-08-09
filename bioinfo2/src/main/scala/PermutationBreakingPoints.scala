import java.util.Scanner

import scala.io.Source

object PermutationBreakingPoints {
  def main(args: Array[String]): Unit = {
    val line = Source.stdin.getLines.toArray.apply(0)
    val inp = new Scanner(line.substring(1, line.length - 1))
    val perm = Stream.iterate((inp.nextInt, true, inp.hasNextInt)) {
      p => if (p._3) (inp.nextInt, p._3, inp.hasNextInt) else (0, false, false)
    }.takeWhile(_._2).map(_._1).toArray
    val bp = perm.foldLeft((0,0)) {
      case ((k,c),e) => if (k+1 != e) (e,c+1) else (e,c)
    }._2
    println(bp)
  }
}

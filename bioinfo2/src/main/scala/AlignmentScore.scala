import scala.io.Source

object AlignmentScore {
  def main(args: Array[String]): Unit = {
    val mtch = 1
    val mis = 0
    val indel = -2
    val lines = Source.stdin.getLines.toArray
    val score = lines(0).zip(lines(1)).foldLeft(0) {
      case (a,(c1,c2)) if c1 == c2 => a + mtch
      case (a,(c1,c2)) if c1 == '-' || c2 == '-' => a + indel
      case (a,_) => a + mis
    }
    println(score)
  }
}

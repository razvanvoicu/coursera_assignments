import scala.io.Source

object Proteomics03 {
  val masses = Map[Char,Int](
    'Q' -> 128,
    'K' -> 128,
    'P' -> 97,
    'V' -> 99,
    'M' -> 131,
    'T' -> 101,
    'A' -> 71,
    'E' -> 129,
    'H' -> 137,
    'C' -> 103,
    'I' -> 113,
    'L' -> 113,
    'N' -> 114,
    'D' -> 115,
    'F' -> 147,
    'S' -> 87,
    'G' -> 57,
    'Y' -> 163,
    'R' -> 156,
    'W' -> 186
  )

  def main(args: Array[String]): Unit = {
    val line = Source.stdin.getLines.next
    val pep = line.map(masses(_)).toList
    val pepm = pep.inits.map(_.sum).toList
    val vec = Array.fill(pepm(0))(0)
    pepm.take(pepm.length-1).foreach{k => vec(k - 1) = 1}
    println(vec.mkString(" "))
  }
}

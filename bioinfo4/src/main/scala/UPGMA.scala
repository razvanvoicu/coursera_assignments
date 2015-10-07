import scala.io.Source

object UPGMA {

  def main(args: Array[String]) : Unit = {
    val in = Source.stdin.getLines()
    println(upgma(in))
  }

  def upgma(in:Iterator[String]): String = {
    in.mkString(System.getProperty("line.separator"))
  }
}

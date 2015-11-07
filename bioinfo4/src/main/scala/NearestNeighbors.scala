import scala.io.Source

object NearestNeighbors {
  def main(args: Array[String]) : Unit = {
    val lines = Source.stdin.getLines.toArray
    val nodes = lines(0).split(" ")
    val a = nodes(0)
    val b = nodes(1)
    val aTo = lines.filter(_.startsWith(a+"->")).filter(_ != (a + "->" + b))
    val w = aTo(0).split("->").apply(1)
    val x = aTo(1).split("->").apply(1)
    val bTo = lines.filter(_.startsWith(b+"->")).filter(_ != (b + "->" + a))
    val y = bTo(0).split("->").apply(1)
    val z = bTo(1).split("->").apply(1)
    val lines1 = Array.fill(lines.size)("")
    lines.copyToArray(lines1)
    val r1 = lines.tail.map{
      case l if l == a+"->"+x => a+"->"+y
      case l if l == x+"->"+a => y+"->"+a
      case l if l == b+"->"+y => b+"->"+x
      case l if l == y+"->"+b => x+"->"+b
      case l => l
    }
    val r2 = lines1.tail.map{
      case l if l == a+"->"+x => a+"->"+z
      case l if l == x+"->"+a => z+"->"+a
      case l if l == b+"->"+z => b+"->"+x
      case l if l == z+"->"+b => x+"->"+b
      case l => l
    }
    println; println
    r2.foreach(println(_))
    println
    r1.foreach(println(_))
  }
}

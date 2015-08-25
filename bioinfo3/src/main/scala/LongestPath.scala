import scala.io.Source

object LongestPath {

  var longest = 0
  var lPath = List[Int]()

  def longestPath(adj: Array[Array[Int]], len: Int, inPath: List[Int], src: Int, sink: Int): Unit = {
    adj(src).zipWithIndex.filter(_._1 != -1).foreach {
      i => {
        if (i._2 == sink && longest < len + i._1) {
          lPath = src :: inPath
          longest = len + i._1
        }
        longestPath(adj, len + i._1, src :: inPath, i._2, sink)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines.toArray
    val n = lines(1).toInt + 1
    val adj: Array[Array[Int]] = Array.fill(n, n)(-1)
    lines.drop(2).takeWhile(_.split("->").length == 2).foreach {
      s => {
        val split1 = s.split("->").map(_.trim)
        val split2 = split1(1).split(":").map(_.trim)
        val src = split1(0).toInt
        val dest = split2(0).toInt
        val weight = split2(1).toInt
        adj(src)(dest) = weight
      }
    }
    longestPath(adj, 0, List(), 0, n - 1)
    println(longest)
    println((n-1::lPath).reverse.mkString("->"))
  }
}

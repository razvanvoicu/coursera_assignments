import scala.io.Source

object LCS {

  def lcs(a: String, b: String): String = {
    if (a.size==0 || b.size==0) ""
    else if (a==b) a
    else {
      val lengths = Array.ofDim[Int](a.size+1,b.size+1)
      for (i <- 0 until a.size)
        for (j <- 0 until b.size)
          if (a(i) == b(j))
            lengths(i+1)(j+1) = lengths(i)(j) + 1
          else
            lengths(i+1)(j+1) = scala.math.max(lengths(i+1)(j),lengths(i)(j+1))

      // read the substring out from the matrix
      val sb = new StringBuilder()
      var x = a.size
      var y = b.size
      do {
        if (lengths(x)(y) == lengths(x-1)(y))
          x -= 1
        else if (lengths(x)(y) == lengths(x)(y-1))
          y -= 1
        else {
          assert(a(x-1) == b(y-1))
          sb += a(x-1)
          x -= 1
          y -= 1
        }
      } while (x!=0 && y!=0)
      sb.toString.reverse
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines().toArray
    println(lcs(lines(0), lines(1)))
  }
}

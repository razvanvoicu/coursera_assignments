import java.util.Scanner
import scala.io.Source

object UPGMA {

  def main(args: Array[String]) : Unit = {
    val in = Source.stdin.getLines()
    println(upgma(in).map{ case ((i,j),v) => i + "->" + j + f":$v%.3f" }.mkString(System.getProperty("line.separator")))
  }

  def closest(d:Map[(Int,Int),Double]) : (Int,Int) = {
    d.toList.filter{case ((i,j),_) => i != j }.map{case ((i,j),l) => (l,(i,j))}.min._2
  }

  def removed(d:Map[(Int,Int),Double],p:(Int,Int)) = {
    d.filter { case ((i,j),_) => i == p._1 || i == p._2 || j == p._1 || j == p._2 }
  }

  def remove(d:Map[(Int,Int),Double],p:(Int,Int)) = {
    d.filter { case ((i,j),_) => i != p._1 && i != p._2 && j != p._1 && j != p._2 }
  }

  def newGraphEdges(q:Map[(Int,Int),Double],k:Int,c:(Int,Int),s:Map[Int,Double],size:Double) : Map[(Int,Int),Double] = {
    val a = for {
      ((n1,n2),ln) <- q
      ((m1,m2),lm) <- q
      if n1 == c._1 && m1 == c._2 && m2 == n2 && m2 != c._1 && n2 != c._2
      d = (ln*s(n1)+lm*s(m1))/size
      x <- List(((k,n2),d),((n2,k),d))
    } yield x
    a + ((k,k) -> 0.0)
  }

  def treeEdges(a:Map[Int,Double],age:Double,k:Int,c:(Int,Int)) : Map[(Int,Int),Double] = {
    List(
      ((k,c._1),age - a(c._1)),
      ((c._1,k),age - a(c._1)),
      ((k,c._2),age - a(c._2)),
      ((c._2,k),age - a(c._2))
    ).toMap
  }

  def upgma(in:Iterator[String]): Map[(Int,Int),Double] = {
    val n = in.next.toInt
    var scn : Scanner = null
    val distArr = (0 until n).foldLeft(Map[(Int,Int),Double]()) {
      case (m,i) =>
        scn = new Scanner(in.next)
        (0 until n).foldLeft(m) {
          case (m,j) => m + ((i,j) -> scn.nextDouble)
        }
    }
    val ages = (0 until n).map((_,0.0)).toMap
    val sz = (0 until n).map((_,1.0)).toMap
    (n until (2*n-1)).foldLeft((ages,sz,distArr,Map[(Int,Int),Double]())) {
      case ((a,s,d,r),k) =>
        val c = closest(d)
        val p = remove(d,c)
        val q = removed(d,c)
        val age = (k,d(c)/2)
        val size = (k -> (s(c._1) + s(c._2)))
        val edg = newGraphEdges(q,k,c,s,size._2)
        val te = treeEdges(a,age._2,k,c)
        (a + age, s+size, p ++ edg,r ++ te)
    }._4.map{
      case ((i,j),v) => ((i,j),v)
    }
  }
}

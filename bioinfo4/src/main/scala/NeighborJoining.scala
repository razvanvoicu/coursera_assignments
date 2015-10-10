import java.util.Scanner
import scala.io.Source

object NeighborJoining {

  type Arc = (Int,Int)
  type Adj = Map[Arc,Double]
  type AgeMap = Map[Int,Double]

  def main(args: Array[String]) : Unit = {
    val in = Source.stdin.getLines()
    println(neighborJoining(in).
      map{ case ((i,j),v) => i + "->" + j + f":$v%.3f" }.
      mkString(System.getProperty("line.separator")))
  }

  def closest(n:Int,d:Adj) : Arc = {
    star(n,d).
      toList.filter{case ((i,j),_) => i != j }.
      map{case ((i,j),l) => (l,(i,j))}.
      min.
      _2
  }

  def removed(d:Adj,p:Arc) = {
    d.filter { case ((i,j),_) => i == p._1 || i == p._2 || j == p._1 || j == p._2 }
  }

  def remove(d:Adj,p:Arc) = {
    d.filter { case ((i,j),_) => i != p._1 && i != p._2 && j != p._1 && j != p._2 }
  }

  def newGraphEdges(q:Adj, k:Int, c:Arc) : Adj = {
    val a = for {
      ((n1,n2),ln) <- q
      ((m1,m2),lm) <- q
      if n1 == c._1 && m1 == c._2 && m2 == n2 && m2 != c._1 && n2 != c._2
      d = (ln+lm-q((n1,m1)))/2
      x <- List(((k,n2),d),((n2,k),d))
    } yield x
    a + ((k,k) -> 0.0)
  }

  def treeEdges(n:Int, d:Adj, k:Int, c:Arc) : Adj = {
    if (n==2)
      List((c,d(c)),((c._2,c._1),d(c))).toMap
    else {
      val td = totalDist(d)
      val delta = (td(c._1) - td(c._2)) / (n - 2)
      List(
        ((k, c._1), (d(c) + delta) / 2),
        ((c._1, k), (d(c) + delta) / 2),
        ((k, c._2), (d(c) - delta) / 2),
        ((c._2, k), (d(c) - delta) / 2)
      ).toMap
    }
  }

  def nodesFromAdj(d:Adj) : Set[Int] = {
    d.map{case ((i,_),_) => i}.toSet
  }

  def totalDist(d:Adj) : AgeMap = {
    val l = nodesFromAdj(d)
    l.map{i => (i,d.filter{case ((j,_),_) => i == j}.map(_._2).sum)}.toMap
  }

  def star(n:Int,d:Adj) : Adj = {
    val td = totalDist(d)
    d.map {
      case ((i, j), v) if i == j => ((i, j), 0.0)
      case ((i, j), v) => ((i, j), (n - 2) * v - td(i) - td(j))
    }
  }

  def neighborJoining(in:Iterator[String]): Adj = {
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
        val c = closest(2*n-k,d)
        val p = remove(d,c)
        val q = removed(d,c)
        val age = (k,d(c)/2)
        val size = (k -> (s(c._1) + s(c._2)))
        val edg = newGraphEdges(q,k,c)
        val te = treeEdges(2*n-k,d,k,c)
        (a + age, s+size, p ++ edg,r ++ te)
    }._4.map{
      case ((i,j),v) => ((i,j),v)
    }
  }
}

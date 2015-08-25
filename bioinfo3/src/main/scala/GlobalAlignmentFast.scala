import scala.collection._
import scala.io.Source

object GlobalAlignmentFast {

  var v = ""
  var w = ""
  var n = 0

  val cache = new mutable.HashMap[(Int,Int),(Int,List[Char],List[Char])]()

  def globAlign(i:Int,j:Int,score:Map[(Char,Char),Int]) : (Int,List[Char],List[Char]) = (i,j) match {
    case (-1,-1) => (0,Nil,Nil)
    case _ => {
      cache.get((i, j)) match {
        case Some(result) => {
          cache.remove(i-4,j-4)
          cache.remove(i-3,j-4)
          cache.remove(i-4,j-3)
          cache.remove(i-3,j-3)
          cache.remove(i-5,j-5)
          cache.remove(i-5,j-4)
          cache.remove(i-4,j-5)
          cache.remove(i-5,j-3)
          cache.remove(i-3,j-5)
          result
        }
        case _ => {
          val v1 = if (i != -1) {
              globAlign(i - 1, j, score) match {
                case (r, s1, s2) => (r + score((v.charAt(i), '-')), v.charAt(i) :: s1, '-' :: s2)
              }
            } else (Int.MinValue / 2, Nil, Nil)

          val v2 = if (j != -1) {
              globAlign(i, j-1, score) match {
                case (r, s1, s2) => (r + score('-', w.charAt(j)), '-' :: s1, w.charAt(j) :: s2)
              }
            } else (Int.MinValue/2, Nil, Nil)

          val v3 = if (i != -1 && j != -1) {
              globAlign(i-1, j-1, score) match {
                case (r, s1, s2) => (r + score(v.charAt(i), w.charAt(j)), v.charAt(i) :: s1, w.charAt(j) :: s2)
              }
            } else (Int.MinValue/2,  Nil, Nil)

          val result = { if (v1._1 > v2._1) if (v1._1 > v3._1) v1 else v3 else if (v2._1 > v3._1) v2 else v3 }
          cache += ((i,j) -> result)
          result
        }
      }
    }
  }

  def main(args:Array[String]): Unit = {
    val lines = Source.stdin.getLines.toArray
    val t1 = System.currentTimeMillis
    val scoreMatrixStream = getClass.getResourceAsStream("Blosum62.txt")
    val scoreMatrixTextLines = Source.fromInputStream(scoreMatrixStream).getLines.toArray
    val scoreMatrixRaw = scoreMatrixTextLines.tail.map(_.split("[ ]+").tail.filter(_ != "").map(_.toInt))
    val headings = scoreMatrixTextLines(0).split("[ ]+").tail
    val scoreMapRaw = headings.zipWithIndex.foldLeft(Map[(Char,Char),Int]()){
      case (m,(rowSym,i)) => headings.zipWithIndex.foldLeft(m) {
        case (m,(colSym,j)) => m + ((rowSym.charAt(0),colSym.charAt(0)) -> scoreMatrixRaw(i)(j))
      }
    }
    val scoreMap = headings.foldLeft(scoreMapRaw) {
      case (m,sym) => m + ((sym.charAt(0),'-') -> -5) + (('-',sym.charAt(0)) -> -5)
    }
    v = lines(0)
    w = lines(1)
    n = w.length / 2
    var s1 = new StringBuilder
    var s2 = new StringBuilder
    globAlign(v.length-1,w.length-1,scoreMap) match {
      case (score,res1,res2) =>
        println(score)
        s1 = res1.reverse.foldLeft(s1){case (s,c) => s+c}
        s2 = res2.reverse.foldLeft(s2){case (s,c) => s+c}
        println(s1)
        println(s2)
    }
    val edge : ((Int,Int),(Int,Int)) = s1.toString.zip(s2.toString).foldLeft(((0,0),(0,0)):((Int,Int),(Int,Int))){
      case (((i0,j0),(i1,j1)),(_,_)) if j0 > n => ((i0,j0),(i1,j1))
      case (((i0,j0),(i1,j1)),(c1,c2)) if j0 == n => ((i0,j0),(c1,c2) match {
        case _ if c1 == '-' => (i0,j0+1)
        case _ if c2 == '-' => (i0+1,j0)
        case _ => (i0+1,j0+1)
      })
      case (((i0,j0),(i1,j1)),(c1,c2)) => ((c1,c2) match {
        case _ if c1 == '-' => (i0,j0+1)
        case _ if c2 == '-' => (i0+1,j0)
        case _ => (i0+1,j0+1)
      },(0,0))
    }
    val t2 = System.currentTimeMillis
    println("Time: " + (t2-t1))
    println("n = " + n)
    println("v length " + v.length)
    println("w length " + w.length)
    println("s1 length " + s1.length)
    println("s2 length " + s2.length)
    println(edge._1 + " " + edge._2)
  }
}

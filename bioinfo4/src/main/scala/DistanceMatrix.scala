
import java.util.Scanner

import scala.io.Source

object DistanceMatrix {

  def matMult(n:Int,a:Map[(Int,Int),Int],b:Map[(Int,Int),Int]): Map[(Int,Int),Int] = {
    (0 to n).foldLeft(a){
      case (m,i) => (0 to n).foldLeft(m){
        case (m,j) =>
          (0 to n).foldLeft(m){
            case (m,k) => (m.get((i,j)),a.get((i,k)),b.get((k,j))) match {
              case (Some(old),Some(new1),Some(new2)) => m + ((i,j) -> Math.min(old,new1 + new2))
              case (None,Some(new1),Some(new2)) => m + ((i,j) -> (new1 + new2))
              case _ => m
            }
        }
      }
    }
  }

  def main(args: Array[String]) : Unit = {
    val inp = Source.stdin.getLines().toArray
    val n = inp(0).toInt
    val arcs = inp.tail.flatMap {
      s =>
        val scanner = new Scanner(s)
        scanner.useDelimiter("->|:")
        val src = scanner.nextInt()
        val dst = scanner.nextInt()
        val dist = scanner.nextInt()
        List(((src,dst),dist),((dst,src),dist))
    }.toMap[(Int,Int),Int]
    val m = arcs.flatMap {
      case ((src,dst),lgth) => List(src,dst)
    }.max
    val distMat = (0 to m).foldLeft(arcs){
      case (mat,_) => matMult(m,mat,arcs)
    } ++ (0 to m).map{case i => ((i,i),0)}.toMap
    val out = (0 until n).map{
      case i => (0 until n).map{
        case j => distMat((i,j))
      }.mkString(" ")
    }.mkString("\n")
    println(out)
  }
}

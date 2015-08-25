import java.util.Scanner

import scala.io.Source

object GreedySortByReversals {
  def reversal(a:Array[Int], low: Int, high: Int) = {
    val r = a.clone()
    var l = low
    var h = high
    if (l < h)
      while (l <= h) {
        val tmp = r(l)
        r(l) = - r(h)
        r(h) = - tmp
        l += 1
        h -= 1
      }
    r
  }

  def main(args: Array[String]): Unit = {
    val line = Source.stdin.getLines.toArray.apply(0)
    val inp = new Scanner(line.substring(1,line.length-1))
    val perm = Stream.iterate((inp.nextInt,true,inp.hasNextInt)) {
      p => if (p._3) (inp.nextInt,p._3,inp.hasNextInt) else (0,false,false)
    }.takeWhile(_._2).map(_._1).toArray
    lazy val arrStr : Stream[(Int,Boolean,Array[Int])] = (0,false,perm) #:: arrStr.map{
      case (i,false,p) =>
        val optIdx = p.indexOf(i+1)
        val idx = if (optIdx != -1) optIdx else p.indexOf(-(i+1))
        (i,true,reversal(p,i,idx))
      case (i,true,p) =>
        val c = p.clone
        c(i) = Math.abs(c(i))
        (i+1,false,c)
    }
    val permList = arrStr.takeWhile(p => p._1 < perm.length || (p._1 == perm.length && ! p._2))
    val permListUniq = permList.tail.foldLeft(List(permList.head)){
      case (l,e) => if (l.head._3.sameElements(e._3)) l else e::l
    }.reverse.tail

    permListUniq.foreach{
      case (_,_,p) => println("(" + p.map{ e => if (e > 0) "+" + e else e.toString }.mkString(" ") + ")")
    }
  }
}

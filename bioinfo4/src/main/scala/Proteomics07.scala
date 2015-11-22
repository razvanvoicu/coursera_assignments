import java.util.Scanner

import scala.io.Source

object Proteomics07 {
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

  def prefixOfSum(sum: Int, s: String) : Option[String] = {
    Stream.iterate((0,"",s)) {
      case (acc,r,"") => (Int.MaxValue,r,"")
      case (acc,r,s) =>
        (acc + masses(s.charAt(0)),r+s.charAt(0),s.tail)
    }.takeWhile(_._1 <= sum).last match {
      case (s,r,_) if s == sum => Option(r)
      case _ => None
    }
  }

  def score(s: String, specVec: Array[Int]) = {
    val x = s.foldLeft((0,0)) {
      case ((acc,idx),c) => (acc+specVec(idx),idx+masses(c))
    }
    x._1 + specVec(x._2)
  }

  def main(args: Array[String]) : Unit = {
    val lines = Source.stdin.getLines.toSeq
    println(s"count lines: ${lines.size}")
    val (vecs, proteomeOpt, thresholdOpt) = lines.foldLeft((Seq[Array[Int]](),None:Option[String],None:Option[Int])) {
      case ((as,p,t),line) ⇒
        println(line)
        val sc = new Scanner(line)
        if (sc.hasNextInt && p.isEmpty) {
          val vec = 0 #:: Stream.continually(if (sc.hasNextInt) sc.nextInt else Int.MinValue).
            takeWhile(_ != Int.MinValue)
          println(s"vec length = ${vec.length}")
          (as :+ vec.toArray, None, None)
        } else if (p.isEmpty) {
          (as, Some(line), None)
        } else (as,p,Some(line.toInt))
    }
    val proteome = proteomeOpt.get
    val threshold = thresholdOpt.get
    println(s"vecs size: ${vecs.size}")
    println(s"proteome: $proteome")
    println(s"threshold: $threshold")
    println
    vecs.foreach {
      v ⇒
        val result = proteome.tails.map(prefixOfSum(v.length - 1, _)).filter(_.isDefined).map {
          case Some(s) => (score(s, v), s)
          case None => (0, "")
        }.max
        if (result._1 >= threshold ) println(s"${result._2}")
    }
  }
}

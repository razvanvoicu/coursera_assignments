import java.util.Scanner
import java.util.concurrent.ConcurrentLinkedQueue

import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global

object Proteomics06 {
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
    s.foldLeft((0,0)) {
      case ((acc,idx),c) => (acc+specVec(idx),idx+masses(c))
    }._1
  }

  def main(args: Array[String]) : Unit = {
   val lines = Source.stdin.getLines
   val spectrumLine = lines.next
   val proteome = lines.next
   val sc = new Scanner(spectrumLine)
   val vec = 0 #:: Stream.continually(if(sc.hasNextInt) sc.nextInt else Int.MinValue)
     .takeWhile(_ != Int.MinValue)
   val specVec = vec.toArray
   val result = proteome.tails.map(prefixOfSum(specVec.length-1,_)).filter(_.isDefined).map {
     case Some(s) => (score(s,specVec),s)
     case None => (0,"")
   }.max
   println(s"Result: ${result._2}, score: ${result._1}")
   val expected = "KLEAARSCFSTRNE"
   println(s"Expected: $expected, score: ${score(expected,specVec)}")
  }
}

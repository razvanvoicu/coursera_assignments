import java.util.Scanner

import scala.collection.mutable
import scala.io.Source

object Proteomics09 {

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
    /*'X' → 4,
    'Z' → 5*/
  )

  val cache = mutable.Map[(Int,Int),Double]()

  def prob(spectrum: Array[Int], maxScore: Int, idx: Int, t: Int): Double = {
    (idx,t) match {
      case (0,0) ⇒ 1.0
      case (0,_) ⇒ 0.0
      case (idx,_) if idx < 0 ⇒ 0.0
      case (_,t) if t < 0 ⇒ 0.0
      case (idx,t) ⇒
        val pSum = masses.values.toList.map {
          v:Int ⇒ cache.get((idx-v,t-spectrum(idx))) match {
            case None ⇒
              val p = prob(spectrum, maxScore, idx-v, t-spectrum(idx))
              cache += (idx-v,t-spectrum(idx)) -> p
              p
            case Some(sz) ⇒ sz
          }
        }.sum
        pSum / 20.0
    }
  }

  def main(args: Array[String]) : Unit = {
    val lines = Source.stdin.getLines.toArray
    val spectrumLine = lines(0)
    val sc = new Scanner(spectrumLine)
    val spectrum = (0 #:: Stream.continually(if(sc.hasNextInt) sc.nextInt else Int.MinValue)
      .takeWhile(_ != Int.MinValue)).toArray
    val threshold = lines(1).toInt
    val maxScore = 2 * lines(2).toInt
    println("Spectrum:")
    println(spectrum.mkString(" "))
    println(s"Spectrum size: ${spectrum.size}")
    println(s"Threshold: $threshold")
    println(s"MaxScore: $maxScore")
    println
    val result = (threshold to maxScore).map(t ⇒ prob(spectrum, maxScore, spectrum.size-1,t)).sum
    println(result)
  }
}

import java.util.Scanner

import scala.io.Source
import scala.math.Ordering.Implicits._

object Proteomics10 {

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

  def indices(pep: String): IndexedSeq[Int] = pep.scanLeft(0)(_ + masses(_))

  def calcScore(spectrum: Seq[Int], gap: Int, indices: Seq[Int], subst: Seq[(Int,Int)]): Int = {
    val is = indices.toArray
    val (i1,d1) = subst(0)
    val (i2,d2) = subst(1)
    (i1 until i2).foreach{ k ⇒ is(k) -= (indices(i1)-d1) }
    (i2 until is.size).foreach{ k ⇒ is(k) += gap }
    println(s"Modified peptide indices: ${is.mkString(" ")}")
    is.map(spectrum(_)).sum
  }

  def main(args: Array[String]) : Unit = {
    val lines = Source.stdin.getLines.toArray
    val peptide = lines(0)
    val specLine = lines(1)
    val mods = lines(2).toInt
    val sc = new Scanner(specLine)
    val spectrum = (0 #:: Stream.continually(if(sc.hasNextInt) sc.nextInt else Int.MinValue)
      .takeWhile(_ != Int.MinValue)).toArray
    val gap = spectrum.length - peptide.map(masses(_)).sum - 1
    println("Got the following input:")
    println(s"Peptide: $peptide")
    println(s"Spectrum: ${spectrum.mkString(" ")}")
    println(s"Spectrum length: ${spectrum.length - 1}")
    println(s"Modification limit: $mods")
    println(s"Gap: $gap")
    println(s"Peptide indices: ${indices(peptide)}")
    println
    val candidates = for {
      comb ← (1 to peptide.length).toSeq.combinations(mods)
      i1 = comb(0)
      i2 = comb(1)
      d1 ← (indices(peptide)(i1) - masses(peptide(i1-1)) + 1) to (indices(peptide)(i1) + masses(peptide(i2-1)) - gap)
      d2 = i2 + (i1-d1) + gap
      if d1 != i1
      subst = Seq((i1,d1),(i2,d2))
      _ = println(s"Subst: $subst")
      score = calcScore(spectrum,gap,indices(peptide),subst)
      _ = println(s"Score: $score")
    } yield (score,Seq((i1,d1-indices(peptide)(i1)),(i2,gap+(d1-indices(peptide)(i1)))))
    val result = candidates.max
    println(result)
  }

}

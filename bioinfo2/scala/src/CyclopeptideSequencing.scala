import scala.io.Source
import java.util.Scanner

object CyclopeptideSequencing {
  val massLookup : Map[Char,Int] = Const.mass.toMap
  def times(xs:Set[List[Int]], ys:Set[Int]) : Set[List[Int]] = xs.flatMap { x => ys.map { _ :: x }}
  def main(args: Array[String]) : Unit = {
    val line = Source.stdin.getLines.toArray.apply(0)
    val inp = new Scanner(line)
    val spectrum = Stream.iterate((inp.nextInt, true, inp.hasNextInt)) {
      p => if (p._3) (inp.nextInt, p._3, inp.hasNextInt) else (0, false, false)
    }.takeWhile(_._2).map(_._1).toArray
    val spectrumSet = spectrum.toSet
    val protMasses = massLookup.values.toSet
    val curProt = spectrumSet intersect protMasses
    val curProtL = curProt.map(List(_))
    val bigMass = spectrum.last
    val multiSpectrum = spectrum.foldLeft(Map[Int,Int]()) {
      case (m,k) => m.get(k) match {
        case None => m + ((k,1))
        case Some(v) => m + ((k,v+1))
      }
    }
    val multiProt = multiSpectrum.filterKeys(curProt.contains(_))
    val multiProtSz = multiProt.values.sum
    val candidates = (1 until multiProtSz).foldLeft(curProtL) {
      case (a,_) =>
        val xs = times(a,curProt)
        xs.filter(x => spectrumSet contains(x sum))
    } map(_ reverse) filter {
      l =>
        val m = l.foldLeft(Map[Int,Int]()) {
          case (m, k) => m.get(k) match {
            case None => m + ((k, 1))
            case Some(v) => m + ((k, v + 1))
          }
        }
        multiProt == m
    } filter {
      l => l.tails.flatMap(t => t.reverse.tails).map(_.sum).toSet.subsetOf(spectrumSet)
    }
    println(candidates.map(_.mkString("-")).mkString(" "))

  }
}

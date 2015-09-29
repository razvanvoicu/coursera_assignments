import scala.io.Source

object GenerateTheoreticalSpectrum {
  val massLookup : Map[Char,Int] = Const.mass.toMap
  def main(args: Array[String]) : Unit = {
    val pep = Source.stdin.getLines.toArray.apply(0).toStream
    val w = pep //++ pep.take(pep.length-1)
    val sp = w.tails.flatMap(s => s.reverse.tails.map(_.reverse)).filter(_.length < pep.length).toSet + pep.toStream
    val masses = sp.toStream.map(_.map(massLookup(_)).sum).toArray
    println(masses.sorted.mkString(" "))
  }
}

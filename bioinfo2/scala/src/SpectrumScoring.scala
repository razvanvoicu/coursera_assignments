import scala.io.Source
import java.util.Scanner

object SpectrumScoring {
  def score(a:Array[Int], b:Array[Int]) : Int = {
    var i = 0;
    var j = 0;
    var score = 0;
    while (i < a.length && j < b.length) {
      if ( a(i) == b(j) ) {
        score += 1
        i += 1
        j += 1
      } else if ( a(i) < b(j) ) {
        i += 1
      } else {
        j += 1
      }
    }
    score
  }
  val massLookup : Map[Char,Int] = Const.mass.toMap
  def main(args: Array[String]) : Unit = {
    val input = Source.stdin.getLines.toArray
    val pep : Stream[Char] = input(0).toStream
    val w = pep ++ pep.take(pep.length-2)
    def pref(s:Stream[Char]) = s.reverse.tails.map(_.reverse)
    def tailFilt(s:Stream[Char]) = s.length>pep.length-2
    def tailTrim(s:Stream[Char]) = s.take(pep.length-1)
    val sp = Stream() #:: pep #:: w.tails.filter(tailFilt).map(tailTrim).flatMap(pref).filter(_.length>0).toStream
    val masses = sp.toStream.map(_.map(massLookup(_)).sum).toArray
    println(masses.sorted.mkString(" "))
    val scanner = new Scanner(input(1))
    val experimental = Stream.iterate((scanner.nextInt, true, scanner.hasNextInt)) {
      p => if (p._3) (scanner.nextInt, p._3, scanner.hasNextInt) else (0, false, false)
    }.takeWhile(_._2).map(_._1).toArray
    println(score(masses.sorted,experimental))
  }
}

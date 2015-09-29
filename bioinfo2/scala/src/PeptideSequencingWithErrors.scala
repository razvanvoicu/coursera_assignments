import scala.io.Source
import java.util.Scanner

object PeptideSequencingWithErrors {
  val prot = Const.mass.map(_._2).toSet
  def expand(prot:Set[Int],leaderBoard:List[List[Int]]) : List[List[Int]] = {
    for ( p <- prot.toList; l <- leaderBoard ) yield (l ++ List(p))
  }
  def trim(leaderBoard:List[List[Int]],spectrum:List[Int],n:Int) : List[List[Int]]  = {
    val scores = leaderBoard.map{
      l => (score(l.toArray,spectrum.toArray),l)
    }.sortBy(_._1)
    val zipped = scores.take(scores.length-1).zip(scores.tail).zipWithIndex
    val lim = zipped.takeWhile{ case (((a,x),(b,y)),i) => i <= n || a == b }
    lim.map{case (((_,x),_),_) => x}
  }
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
    val n = (new Scanner(input(0))).nextInt
    val scanner = new Scanner(input(1))
    val experimental = Stream.iterate((scanner.nextInt, true, scanner.hasNextInt)) {
      p => if (p._3) (scanner.nextInt, p._3, scanner.hasNextInt) else (0, false, false)
    }.takeWhile(_._2).map(_._1).toList
    val expProt = prot.intersect(experimental.toSet)
    var leaderScore = -1
    var leaderPep = List[Int]()
    var leaderBoard = List(List[Int]())
    val parentMass = experimental.last
    while(! leaderBoard.isEmpty ) {
      leaderBoard = expand(expProt,leaderBoard)
      leaderBoard = leaderBoard.filter{
        pep => 
          val sc = score(pep.toArray,experimental.toArray)
          val sm = pep.sum
          if (sm == parentMass && sc > leaderScore) {
            leaderPep = pep
            leaderScore = sc
          }
          sm <= parentMass
      }
      leaderBoard = trim(leaderBoard,experimental,n)
    }
    println(leaderPep.mkString("-"))
  }
}

import scala.io.Source
import java.util.Scanner

object ConvolutionCyclopeptideSequencing {
  def spectrum(pep:List[Int]) : List[Int] = {
    val pepc = pep ++ pep.take(pep.length-1)
    val sp : List[List[Int]] = List() :: 
              pepc.tails.map(_.take(pep.length-1)).toList.flatMap(_.inits).filter(_.length > 0)
    sp.map(_.sum).sorted
  }

  def expand(prot:Set[Int],leaderBoard:List[List[Int]]) : List[List[Int]] = {
    for ( p <- prot.toList; l <- leaderBoard ) yield (p::l)
  }

  def trim(leaderBoard:List[List[Int]],pep:List[Int],n:Int) : List[List[Int]]  = {
    val scores = leaderBoard.map{
      l =>
        (score(spectrum(l).toArray,pep.toArray),l)
    }.sortWith{case ((a,_),(b,_)) => a > b}
    val zipped = scores.take(scores.length-1).zip(scores.tail).zipWithIndex
    val lim = zipped.takeWhile{ case (((a,x),(b,y)),i) => i < n || a == b }
    zipped.head._1._1._2 :: lim.map{case ((_,(_,x)),_) => x}
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

	def main(args: Array[String]) : Unit = {
    val input = Source.stdin.getLines.toArray
    val m = input(0).toInt
    val n = input(1).toInt
    val scanner = new Scanner(input(2))
    val experimental = Stream.iterate((scanner.nextInt, true, scanner.hasNextInt)) {
      p => if (p._3) (scanner.nextInt, p._3, scanner.hasNextInt) else (0, false, false)
    }.takeWhile(_._2).map(_._1).toList.sorted
    val conv = { for ( x <- experimental ; y <- experimental) yield (x-y) } filter (_ > 0)
    val convMap = conv.foldLeft(Map[Int,Int]()) {
    	(m,k) =>
    		m.get(k) match {
    			case None => m + ((k,1))
          case Some(v) => m + ((k,v+1))
    		}
    }
    val mersRaw = convMap.toList.map(p => (p._2,p._1)).sorted.reverse.filter(x => 57 <= x._2 && x._2 <= 200)
    println(mersRaw.mkString("_"))
    val zipped = mersRaw.take(mersRaw.length-1).zip(mersRaw.tail).zipWithIndex
    val lim = mersRaw.head._2 :: 
      zipped.takeWhile{ case (((a,x),(b,y)),i) => i < m-1 || a == b }.map{case (((a,x),(b,y)),i) => y}
    val prot = lim.toSet
    println(lim.sorted.mkString(" ~ "))

    var leaderScore = -1
    var leaderPep = List[Int]()
    var leaderBoard = List(List[Int]())
    val parentMass = experimental.last

    while(! leaderBoard.isEmpty ) {
      leaderBoard = expand(prot,leaderBoard)
      leaderBoard = leaderBoard.filter{
        pep => 
          val sc = score(spectrum(pep).toArray,experimental.toArray)
          val sm = pep.sum
          if (sm == parentMass && sc > leaderScore) {
            leaderPep = pep
            leaderScore = sc
          }
          //if ( sm == parentMass && sc >= 60 ) println(pep.mkString("-") + " " + sc)
          sm <= parentMass
      }
      if (leaderBoard.length > n) {
        leaderBoard = trim(leaderBoard,experimental,n)
      }
    }

    println(leaderPep.reverse.mkString("-"))
  }
}
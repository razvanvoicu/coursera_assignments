
object Quiz4 {
  def spectrum(pep:List[Int]) : List[Int] = {
    val pepc = pep ++ pep.take(pep.length-2)
    val sp : List[List[Int]] = List() :: pepc.tails.filter(_.length > pep.length-2).map(_.take(pep.length-1)).toList.flatMap(_.inits).filter(_.length > 0)
    println(sp)
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
    val experimental = List(0, 86, 160, 234, 308, 320, 382)
    val conv = { for ( x <- experimental ; y <- experimental) yield (x-y) } filter (_ > 0)
    val convMap = conv.foldLeft(Map[Int,Int]()) {
    	(m,k) =>
    		m.get(k) match {
    			case None => m + ((k,1))
          case Some(v) => m + ((k,v+1))
    		}
    }
    /*
    val mersRaw = convMap.toList.map(p => (p._2,p._1)).sorted.reverse.filter(x => 57 <= x._2 && x._2 <= 200)
    val zipped = mersRaw.take(mersRaw.length-1).zip(mersRaw.tail).zipWithIndex
    val lim = mersRaw.head._2 :: 
      zipped.takeWhile{ case (((a,x),(b,y)),i) => i < m-1 || a == b }.map{case (((a,x),(b,y)),i) => y}
    val prot = lim.toSet
    println(lim.sorted.mkString(" ~ "))
    */          
    println("MAMA")
    println(spectrum(List(131,71,131,71)).mkString(" "))
    println(Array(0, 71, 98, 99, 131, 202, 202, 202, 202, 202, 299, 333, 333, 333, 503).mkString(" "))
    println(score(spectrum(List(131,71,131,71)).toArray,Array(0, 71, 98, 99, 131, 202, 202, 202, 202, 202, 299, 333, 333, 333, 503)))
    println(spectrum(List(97,129,129,97)).mkString(" "))
    println(Array(0, 97, 97, 97, 100, 129, 194, 226, 226, 226, 258, 323, 323, 355, 393, 452).mkString(" "))
    println(score(spectrum(List(97,129,129,97)).toArray,Array(0, 97, 97, 97, 100, 129, 194, 226, 226, 226, 258, 323, 323, 355, 393, 452)))
    println(convMap)
  }
}
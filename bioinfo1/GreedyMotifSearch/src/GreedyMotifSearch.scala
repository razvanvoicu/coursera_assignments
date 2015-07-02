import java.util.Scanner

object GreedyMotifSearch {

  val in = new Scanner(System.in)

  def idx : Char => Int = {
    case 'A' => 0
    case 'C' => 1
    case 'G' => 2
    case 'T' => 3
    case _ => sys.error("Unknown symbol")
  }

  val nts = Array('A','C','G','T');

  def hamming(s1:String,s2:String) : Int = s1.zip(s2).map(x => if(x._1 == x._2) 0 else 1).sum

  def freq(m : Array[String]) : Array[Array[Int]] = {
    (0 to 3).map{
      i => m.foldLeft(Iterator.fill(m(0).length)(0).toArray){
        (c:Array[Int],s:String) => s.map(x => if (x == nts(i)) 1 else 0).zip(c).map(x => x._1 + x._2).toArray
      }
    }.toArray
  }

  def profile(m:Array[String]) : Array[Array[Double]] = {
    val f = freq(m)
    f.map{_.map{
      x => x.toDouble / m.length
    }}
  }

  def consensus(m : Array[String]) : String = {
    val f = freq(m)
    (0 to (m(0).length-1)).foldLeft(""){
      (s,i) => s + nts(Array((f(0)(i),0),(f(1)(i),1),(f(2)(i),2),(f(3)(i),3)).fold((-100000,0)){
        (a,x) => (a,x) match {
          case ((ad,as),(xd,xs)) => if (ad >= xd) (ad,as) else (xd,xs)
        }
      }._2)
    }
  }

  def score(m:Array[String]) : Int = {
    val c = consensus(m)
    m.map(hamming(_,c)).sum
  }

  def prob(s:String,i:Int,l:Int,profile:Array[Array[Double]]) : Double =
    s.substring(i,i+l).zipWithIndex.foldLeft(1.0){
      (x,y) => y match {
        case (c,i) => {
          x * profile(idx(c))(i)
        }
      }
    }

  def probs(s:String,l:Int,profile:Array[Array[Double]]) : Array[(Double,String)] = {
    s.tails.take(s.length-l+1).map(x => (prob(x,0,l,profile),x.take(l))).toArray
  }

  def maxProb(s:String,l:Int,profile:Array[Array[Double]]) : String =  probs(s,l,profile).fold((-100000.0,"")){
    (a:(Double,String),x:(Double,String)) => (a,x) match {
      case ((ad,as),(xd,xs)) => if (ad >= xd) (ad,as) else (xd,xs)
    }
  }._2

  def motifSearch(k:Int,lines:Array[String]) : Array[String] = {
    val best : Array[String] = lines.map(_ take k)
    val motifOnes : Array[String] = lines(0).tails.take(lines(0).length-k+1).map(_.take(k)).toArray
    motifOnes.foldLeft(best) {
      (b:Array[String],m1:String) => {
        val motifs = lines.tail.foldLeft(Array(m1)){
          (ms:Array[String],s:String) => ms :+ maxProb(s,k,profile(ms))
        }
        if (score(b) <= score(motifs)) b else motifs
      }
    }
  }

  def main(args : Array[String]) = {
    val k = in.nextInt()
    val n = in.nextInt()
    in.nextLine()
    val lines : Array[String] = Iterator.fill(n)(in.nextLine).toArray
    motifSearch(k,lines).map(println(_))
    ()
  }
}

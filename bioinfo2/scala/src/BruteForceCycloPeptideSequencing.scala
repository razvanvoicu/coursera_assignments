import scala.io.Source

object BruteForceCycloPeptideSequencing {
  val massLookup : Map[Char,Int] = Const.mass.toMap

  def fact(n:Int): BigInt = {
    var a = BigInt(1)
    var i = n
    while(i>0) { a *= i; i -= 1 }
    a
  }
  def multiSize(m:Map[Char,Int]) : Int = {
    m.foldLeft(0) {
      case (a,(_,k)) => a + k
    }
  }
  def permCount(m:Map[Char,Int]) : BigInt = {
    val p : BigInt = m.foldLeft(BigInt(1)) {
      case (a,(_,k)) => a * fact(k)
    }
    fact(multiSize(m))/p
  }
  def calcMass(m:Map[Char,Int]) : Int = {
    m.foldLeft(0) {
      case (a,(x,k)) => a + massLookup(x)*k
    }
  }
  def addToMass(m:Map[Char,Int],k:Char) : Map[Char,Int] = {
    m.get(k) match {
      case None => m + ((k,1))
      case Some(v) => m + ((k,v+1))
    }
  }
  val t0 = System.currentTimeMillis()
  var t1 = t0
  var cnt = BigInt(0)
  def count(masses: List[(Char,Int)], massLeft: Int, accum: Map[Char,Int]) : Unit = {
    () match {
      case _ if massLeft == 0 =>
        cnt += permCount(accum)
        val u = System.currentTimeMillis()
        if (u-t1 > 10000) {
          println(((u-t0)/1000) + " seconds, solutions found: " + cnt)
          t1 = u
        }
      case _ if massLeft < 0 || masses.isEmpty =>
      case _ =>
        count(masses.tail, massLeft, accum)
        count(masses, massLeft - masses.head._2, addToMass(accum,masses.head._1) )
    }
  }
  def main(args: Array[String]) : Unit = {
    val mass: Int = Source.stdin.getLines.toArray.apply(0).toInt
    count(Const.mass,mass,Map[Char,Int]())
    println(cnt)
  }
}

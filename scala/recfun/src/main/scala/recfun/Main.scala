package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c,r) match {
    case (c,r) if c==0 || c==r => 1
    case _ => pascal(c-1,r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper (chars:List[Char], openSoFar:Int): Boolean = (chars,openSoFar) match {
      case (_,n) if n < 0 => false
      case (Nil,0) => true
      case ('('::rest,n) => helper(rest,n+1)
      case (')'::rest,n) => helper(rest,n-1)
      case (_::rest,n) => helper(rest,n)
      case _ => false
    }
    helper(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def helper (money:Int, coins: List[Int]) : Int = (money,coins) match {
      case (n,_) if n < 0 => 0
      case (0,_) => 1
      case (n,Nil) if n > 0 => 0
      case (n,head::tail) => countChange(n,tail) + countChange(n-head,coins)
    }
    helper(money,coins.distinct)
  }
}

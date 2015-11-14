import java.util.Scanner

import scala.collection.mutable
import scala.io.Source

object Proteomics05 {

   val masses = Map(
     128 -> 'Q',
     97  -> 'P',
     99  -> 'V',
     131 -> 'M',
     101 -> 'T',
     71  -> 'A',
     129 -> 'E',
     137 -> 'H',
     103 -> 'C',
     113 -> 'L',
     114 -> 'N',
     115 -> 'D',
     147 -> 'F',
     87  -> 'S',
     57  -> 'G',
     163 -> 'Y',
     156 -> 'R',
     186 -> 'W'
   )

   def main(args: Array[String]) : Unit = {
     val line = Source.stdin.getLines.next
     val sc = new Scanner(line)
     val vec = 0 #:: Stream.continually(if(sc.hasNextInt) sc.nextInt else Int.MinValue)
       .takeWhile(_ != Int.MinValue)
     val specVec = vec.toArray
     val dag = mutable.Map[Int,List[Int]]()
     for {
       i <- 0 until specVec.size
       j <- (i+1) until specVec.size
       if masses.keySet contains (j-i)
     } dag += i -> (j :: dag.getOrElse(i,List()))
     var maxPath = ""
     var maxScore = Long.MinValue
     def traverse(n: Int, sc: Long, p: String) : Unit = {
       dag.get(n) match {
         case None if n == specVec.length - 1 && sc > maxScore =>
           maxScore = sc
           maxPath = p
         case Some(l:List[Int]) => l foreach {
           k =>
             traverse(k, sc+specVec(k), p + masses(k - n))
         }
         case _ =>
       }
     }
     println("Started")
     traverse(0,0,"")
     println(maxPath)
   }
 }

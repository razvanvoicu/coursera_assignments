import java.util.Scanner

import scala.collection.SortedMap
import scala.io.Source

object DistanceBasedPhylogeny {
  def main(args: Array[String]): Unit = {
    val inp = Source.stdin.getLines().toList
    val n = inp(0).toInt
    val distLst: List[Map[Int, Int]] = inp.drop(1).map {
      case s =>
        val scanner = new Scanner(s)
        def mkElem(b: Boolean) = if (b) (scanner.nextInt(), scanner.hasNextInt, b) else (0, false, false)
        lazy val line: Stream[(Int, Boolean, Boolean)] = mkElem(true) #:: line.map { h => mkElem(h._2) }
        line.takeWhile(_._3).zipWithIndex.map { case ((v, _, _), j) => (j, v) }.toMap
    }
    var dist = distLst.zipWithIndex[Map[Int, Int], List[(Map[Int, Int], Int)]].map { case (m, i) => (i, m) }.toMap
    var limbLength = (0 until n).map {
      case j =>
        val d = {
          for {
            i <- 0 until n
            k <- 0 until n
            if i != j
            if k != j
            if i != k
          } yield ((dist(i)(j) + dist(j)(k) - dist(i)(k)) / 2)
        }.min
        (j, d)
    }.toMap
    var distM = dist.foldLeft(Map[(Int,Int),Int]()) {
      case (mp, (i, mm)) => mm.foldLeft(mp) {
        case (mp, (j, lg)) => mp + ((i, j) -> lg)
      }
    }
    var nodes = (0 until n).toSet
    var nodeToAdd = n
    var phylo = SortedMap[(Int,Int),Int]()
    while (nodes.size > 2) {
      val leaves = {
        for {
          i <- nodes
          j <- nodes
          if i != j
          if limbLength(i) + limbLength(j) == distM((i,j))
        } yield (i, j)
      }.head
      phylo = phylo + ((leaves._1,nodeToAdd) -> limbLength(leaves._1))
      phylo = phylo + ((nodeToAdd,leaves._1) -> limbLength(leaves._1))
      phylo = phylo + ((leaves._2,nodeToAdd) -> limbLength(leaves._2))
      phylo = phylo + ((nodeToAdd,leaves._2) -> limbLength(leaves._2))
      nodes = nodes.diff(Set(leaves._1,leaves._2))
      nodes.foreach {
        node =>
            distM = distM + ((nodeToAdd,node) -> (distM((node,leaves._1)) - limbLength(leaves._1)))
            distM = distM + ((node,nodeToAdd) -> distM(nodeToAdd,node))
      }
      if (nodes.size > 1) {
        nodes.foreach {
          node =>
            limbLength = limbLength + (nodeToAdd -> {
              for {
                i <- nodes
                k <- nodes
                if i != k
              } yield ((distM((i, nodeToAdd)) + distM((nodeToAdd, k)) - distM((i, k))) / 2)
            }.min)
        }
      }
      nodes += nodeToAdd
      nodeToAdd += 1
    }
    val last1 = nodes.head
    val last2 = nodes.tail.head
    phylo = phylo + ((last1,last2) -> distM((last1,last2)))
    phylo = phylo + ((last2,last1) -> distM((last2,last1)))
    val result = phylo.map{case ((i,j), l) => i + "->" + j + ":" + l}
    print(result.mkString("\n"))
  }
}

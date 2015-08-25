
import java.util.Scanner

import scala.io.Source

object TwoBreakDist {
  class Link {
    var visited = false
    var nodeIdx = -1
    var linkType = -1
    val neighbors = Array.fill[Link](2)(null)
  }

  class Node {
    val links = Array.fill[Link](2)(new Link)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines.toArray
    val pat = "[\\(][^\\(]+[\\)]".r
    val gens = lines.map {
      s => pat.findAllIn(s).map {
        s =>
          val scanner = new Scanner(s.substring(1, s.length - 1))
          def makeElem(h: Boolean) = if (h) (scanner.nextInt, scanner.hasNextInt, true) else (0, false, false)
          lazy val it: Stream[(Int, Boolean, Boolean)] = makeElem(true) #:: it.map { case (_, h, _) => makeElem(h) }
          it.takeWhile(_._3).map(_._1).toArray
      }.toArray
    }
    def connect(g:Map[Int,Node],src:Int,dest:Int,color:Int) : Map[Int,Node] = {
      val slink = if (src > 0) 0 else 1
      val dlink = if (dest > 0) 1 else 0
      val snorm = if (src > 0) src else -src
      val dnorm = if (dest > 0) dest else -dest
      def link(s:Node,d:Node,slink:Int,dlink:Int,color:Int) = {
        s.links(slink).neighbors(color) = d.links(dlink)
        d.links(dlink).neighbors(color) = s.links(slink)
      }
      def newNode(g:Map[Int,Node],norm:Int) : (Map[Int,Node],Node) = {
        val n = new Node
        val newG = g + (norm -> n)
        n.links.zipWithIndex.foreach{case (l,i) => l.nodeIdx = norm ; l.linkType = i }
        (newG,n)
      }
      (g.get(snorm),g.get(dnorm)) match {
        case (Some(s),Some(d)) =>
          link(s,d,slink,dlink,color)
          g
        case (Some(s),None) =>
          val (newG,d) = newNode(g,dnorm)
          link(s,d,slink,dlink,color)
          newG
        case (None,Some(d)) =>
          val (newG,s) = newNode(g,snorm)
          link(s,d,slink,dlink,color)
          newG
        case (None,None) =>
          val (auxG,d) = newNode(g,dnorm)
          val (newG,s) = newNode(auxG,snorm)
          link(s,d,slink,dlink,color)
          newG
      }
    }
    val graphZero = Map[Int,Node]()
    val graph = (0 to 1).foldLeft(graphZero) {
      (g,i) => gens(i).foldLeft(g) {
        case (g, a) =>
          val firstPass = a.tail.foldLeft((g, a.head)) {
            case ((g, prev), e) => (connect(g, prev, e, i), e)
          }
          connect(firstPass._1, a.last, a.head, i)
      }
    }
    val links = graph.flatMap{ case (_,n) => n.links }.toArray
    def countCycles(g:Array[Link]) = {
      var cycles = 0
      var linkType = 0
      var n = g.head
      var done = false
      while ( ! done) {
        if (n.visited) {
          cycles += 1
          n = g.find(! _.visited) match {
            case Some(v) => v
            case None => done = true ; null
          }
        } else {
          n.visited = true
          n = n.neighbors(linkType)
          linkType = 1 - linkType
        }
      }
      cycles
    }
    println(graph.size - countCycles(links))
  }
}

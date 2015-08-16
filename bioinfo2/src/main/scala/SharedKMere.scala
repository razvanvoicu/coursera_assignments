package rosalind

import scala.io.Source

object SharedKMere {

  case class Node(var children:Array[Node] = Array.fill(4)(null)) {
    var occursCount = 0
  }

  def translate : Char => Char = {
    case 'A' => 'T'
    case 'C' => 'G'
    case 'G' => 'C'
    case 'T' => 'A'
    case _ => throw new IllegalArgumentException
  }

  def idx : Char => Int = {
    case 'A' => 0
    case 'C' => 1
    case 'G' => 2
    case 'T' => 3
  }

  def charOf : Int => Char = "ACGT".charAt(_)

  def commonSubstring(n:Node,lg:Int,size:Int) : Array[String] = {
    val x = n.children.zipWithIndex.filter(p => p._1 != null && p._1.occursCount == size)
    if (lg == 0) {
      Array("")
    } else if (x.isEmpty) {
      Array()
    } else {
      x.flatMap {
        case (child,k) => commonSubstring(child,lg-1,size).map(charOf(k) + _)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines.toArray
    val k = lines(0).toInt
    val dna = lines.tail
    var root = Node()
    var nodeReused = 0
    dna.zipWithIndex.foreach {
      case (s,i) => {
        s.foldLeft(List(root)) {
          case (l, c) => root :: l.take(2*k).map {
            n =>
              if (n.children(idx(c)) == null)
                n.children(idx(c)) = Node()
              if (n.children(idx(c)).occursCount == i) n.children(idx(c)).occursCount += 1
              n.children(idx(c))
          }
        }
      }
    }
    commonSubstring(root,k,dna.size).foreach{
      e:String =>
        val a = e.r.findAllMatchIn(dna(0))
        val b = e.r.findAllMatchIn(dna(1))
        for {
          x <- a
          y <- b
        } println ("(" + x.start + ", " + y.start + ")")
    }
    root = Node()
    nodeReused = 0
    dna(1) = dna(1).reverse.map(translate(_))
    dna.zipWithIndex.foreach {
      case (s,i) => {
        s.foldLeft(List(root)) {
          case (l, c) => root :: l.take(2*k).map {
            n =>
              if (n.children(idx(c)) == null)
                n.children(idx(c)) = Node()
              if (n.children(idx(c)).occursCount == i) n.children(idx(c)).occursCount += 1
              n.children(idx(c))
          }
        }
      }
    }
    commonSubstring(root,k,dna.size).foreach{
      e:String =>
        val a = e.r.findAllMatchIn(dna(0))
        val b = e.r.findAllMatchIn(dna(1))
        for {
          x <- a
          y <- b
        } println ("(" + x.start + ", " + (dna(1).length - y.start - k) + ")")
    }
  }
}

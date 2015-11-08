import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.collection.mutable

object LargeParsimony {
  def main(args: Array[String]) : Unit = {
    val lines = Source.stdin.getLines.toArray
    var par = unrooted(lines)
    var score = Int.MaxValue
    var newScore = par.split("\n")(0).toInt
    var uTree = toUnrooted(par)
    println("\n\n\n"+par+"\n")
    while (newScore < score) {
      score = newScore
      innerEdges(uTree) map {
        s =>
          val tok = s.split("->")
          (tok(0),tok(1))
      } filter { case (s,d) => s > d } flatMap {
        case (s,d) =>
          val nn = nearestNeighbor(((s + " " + d) :: uTree.split("\n").toList.tail).toArray)
          nn
      } foreach {
        case t:String =>
          val parc = unrooted(("5\n" + t).split("\n"))
          val pars = parc.split("\n")(0).toInt
          if (pars < newScore) {
            par = parc
            newScore = pars
            uTree = toUnrooted(par)
          }
      }
      if (newScore < score) {
        println(par + "\n")
      }
    }
  }

  val nucl = Map(
    'A' -> Long.MaxValue/2,
    'C' -> Long.MaxValue/2,
    'G' -> Long.MaxValue/2,
    'T' -> Long.MaxValue/2)

  def innerEdges(tree:String) : Array[String] = {
    val lines = tree.split("\n")
    lines.filter{
      s =>
        val tok = s.split("->")
        val t1 = Try(tok(0).toInt)
        val t2 = Try(tok(1).toInt)
        t1.isSuccess && t2.isSuccess
    }
  }

  def unrooted(lines: Array[String]) : String = {
    val n = (lines.size-1) / 2
    val a = lines.filter(_.startsWith(n + "->")).last.split("->")(1)
    val l = lines.filter(s => s != (n + "->" + a) && s != (a + "->" + n)) ++
      Array((n+1)+"->"+a,(n+1)+"->"+n)
    val sp = smallParsimony(l).split("\n")
    val t = sp.drop(sp.size - 4)
    val p = t.map(s => s.split(":")(1).toInt).sum / 2
    val d = t.flatMap(s => s.split("(->)|:").take(2)).groupBy(identity)
    val rootRemoved = () match {
      case _ if d.size == 3 =>
        val nodes = d.filter{case (k,v) => v.size != 4}.keys.iterator
        val n0 = nodes.next
        val n1 = nodes.next
        Array(n0 + "->" + n1 + ":" + p, n1 + "->" + n0 + ":" + p)
      case _ if d.size == 2 =>
        val nodes = d.keys.iterator
        val n0 = nodes.next
        val n1 = nodes.next
        Array(n0 + "->" + n1 + ":" + p, n1 + "->" + n0 + ":" + p)
      case _ =>
        Array[String]()
    }
    (sp.take(sp.size - 4) ++ rootRemoved).mkString("\n")
  }

  def smallParsimony(lines: Array[String]) : String = {
    val leaves = lines(0).toInt
    val (dnaLgth,_,tree) = lines.tail.foldLeft((0,0,Map[Int,List[(Int,Array[Map[Char,Long]])]]())) {
      case ((dnaLgth,nodeCnt,m),s) =>
        val edge = s.split("->")
        val srcTry = Try(edge(0).toInt)
        val destTry = Try(edge(1).toInt)
        srcTry match {
          case Success(src) =>
            val destInt = destTry.getOrElse (nodeCnt)
            val destMap = destTry.transform (
              _ => Success (Array.fill (100) (nucl) ),
              _ => Success (edge (1).toCharArray.map {
                c => nucl + (c -> 0L)
              })
            ).get
            val add = if (destTry.isFailure) 1 else 0
            val k = edge (0).toInt
            val existing = m.getOrElse (k, List () )
            (Math.max (edge (1).length, dnaLgth), nodeCnt + add, m + ((k -> (existing :+ ((destInt, destMap) ) ) ) ) )
          case _ => (dnaLgth,nodeCnt,m)
        }
    }
    val traversable = tree + ((-1 -> List((tree.keys.max,Array.fill(dnaLgth)(nucl)))))
    traverse(dnaLgth, traversable, tree.keys.max,-1,0,Set())
    val parsimony = traversable(-1)(0)._2.map{_.map{case (k,v) => v}.min}.sum
    parsimony + "\n" + adjacency(traversable,tree.keys.max,-1,0,dnaAsString(traversable(-1)(0)._2, " " * dnaLgth), Set())
  }

  def dnaAsString(a: Array[Map[Char,Long]], parent: String) =
    a.zip(parent).map{ case (m,c) => m.map{case (k,v) => (v + {if(k==c) 0 else 1},k)}.min._2}.mkString("")
  def hamming(s1: String, s2: String) =
    s1.zip(s2).map{case (c1,c2) => if (c1==c2) 0 else 1}.sum

  def traverse(dnaLgth: Int, tree: Map[Int,List[(Int,Array[Map[Char,Long]])]], node: Int, parent: Int, child: Int, hist: Set[Int]) : Unit = {
    if (tree.isDefinedAt(node)) {
      val children = tree(node).zipWithIndex.filter(p => ! hist.contains(p._1._1))
      val left = children(0)
      val leftIdx = left._1._1
      val leftDna = left._1._2
      traverse(dnaLgth,tree,leftIdx,node,left._2,hist + leftIdx)
      val right = children(1)
      val rightIdx = right._1._1
      val rightDna = right._1._2
      traverse(dnaLgth,tree,rightIdx,node,right._2,hist + rightIdx)
      (0 until dnaLgth).foreach {
        i =>
          tree(parent)(child)._2(i) = computeDna(leftDna(i),rightDna(i))
      }
    }
  }

  def adjacency(tree: Map[Int,List[(Int,Array[Map[Char,Long]])]], node: Int, parent: Int, child: Int, ps: String, hist: Set[Int]) : String = {
    if (tree.isDefinedAt(node)) {
      val children = tree(node).zipWithIndex.filter(p => ! hist.contains(p._1._1))
      val left = children(0)
      val leftIdx = left._1._1
      val leftDna = dnaAsString(left._1._2,ps)
      val right = children(1)
      val rightIdx = right._1._1
      val rightDna = dnaAsString(right._1._2,ps)
      val parentDna = ps
      val hl = hamming(leftDna,parentDna)
      val hr = hamming(rightDna,parentDna)
      adjacency(tree, leftIdx, node, left._2, leftDna, hist+node) + adjacency(tree, rightIdx, node, right._2, rightDna, hist+node) +
        s"$parentDna->$leftDna:$hl" + "\n" + s"$parentDna->$rightDna:$hr" + "\n" +
        s"$leftDna->$parentDna:$hl" + "\n" + s"$rightDna->$parentDna:$hr" + "\n"
    } else ""
  }

  def computeDna(left: Map[Char,Long], right: Map[Char,Long]) : Map[Char,Long] = {
    def pick(k: Char) = {
      def delta(p : (Char,Long)) = { if (k == p._1) 0 else 1 } + p._2
      left.map(delta).min + right.map(delta).min
    }
    left.map{ case (k,_) => (k, pick(k)) }
  }

  def nearestNeighbor(lines: Array[String]) : Array[String] = {
    val nodes = lines(0).split(" ")
    val a = nodes(0)
    val b = nodes(1)
    val aTo = lines.filter(_.startsWith(a+"->")).filter(_ != (a + "->" + b))
    val w = aTo(0).split("->").apply(1)
    val x = aTo(1).split("->").apply(1)
    val bTo = lines.filter(_.startsWith(b+"->")).filter(_ != (b + "->" + a))
    val y = bTo(0).split("->").apply(1)
    val z = bTo(1).split("->").apply(1)
    val lines1 = Array.fill(lines.size)("")
    lines.copyToArray(lines1)
    val r1 = lines.tail.map{
      case l if l == a+"->"+x => a+"->"+y
      case l if l == x+"->"+a => y+"->"+a
      case l if l == b+"->"+y => b+"->"+x
      case l if l == y+"->"+b => x+"->"+b
      case l => l
    }
    val r2 = lines1.tail.map{
      case l if l == a+"->"+x => a+"->"+z
      case l if l == x+"->"+a => z+"->"+a
      case l if l == b+"->"+z => b+"->"+x
      case l if l == z+"->"+b => x+"->"+b
      case l => l
    }
    Array(r2.mkString("\n"), r1.mkString("\n"))
  }

  def toUnrooted(tree: String) : String = {
    val lines = tree.split("\n").tail
    val nodes = lines.flatMap{_.split("(->)|:").take(2)}.groupBy(identity)
    val leafs = nodes.filter{case (k,v) => v.size < 6}.keySet
    var inner = leafs.size - 1
    val innerMap = mutable.Map[String,Int]()
    leafs.size + "\n" + lines.map{
      s =>
        val srcDest = s.split("(->)|:").take(2)
        val src = srcDest(0)
        val dest = srcDest(1)
        val srcOut =
          if (leafs.contains(src))
            src
          else {
            innerMap.get(src) match {
              case None =>
                inner += 1
                innerMap(src) = inner
                inner.toString
              case Some(k) =>
                k.toString
            }
          }
        val destOut =
          if (leafs.contains(dest))
            dest
          else {
            innerMap.get(dest) match {
              case None =>
                inner += 1
                innerMap(src) = inner
                inner.toString
              case Some(k) => k.toString
            }
          }
        srcOut + "->" + destOut
    }.mkString("\n")
  }
}

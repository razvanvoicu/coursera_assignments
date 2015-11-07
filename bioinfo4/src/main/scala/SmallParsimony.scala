import scala.util.{Success, Try}

object SmallParsimony {
  val nucl = Map(
    'A' -> Long.MaxValue/2,
    'C' -> Long.MaxValue/2,
    'G' -> Long.MaxValue/2,
    'T' -> Long.MaxValue/2)
  def smallParsimony(lines: Array[String]) : String = {
    val leaves = lines(0).toInt
    val (dnaLgth,_,tree) = lines.tail.foldLeft((0,0,Map[Int,List[(Int,Array[Map[Char,Long]],Array[Char])]]())) {
      case ((dnaLgth,nodeCnt,m),s) =>
        val edge = s.split("->")
        val dl = Math.max(edge(1).length,dnaLgth)
        val destTry = Try(edge(1).toInt)
        val destInt = destTry.getOrElse(nodeCnt)
        val destMap = destTry.transform(
          _ => Success(Array.fill(dnaLgth)(nucl)),
          _ => Success(edge(1).toCharArray.map { c => nucl + (c -> 0L) })
        ).get
        val add = if (destTry.isFailure) 1 else 0
        val k = edge(0).toInt
        val existing = m.getOrElse(k,List())
        (dl, nodeCnt + add, m + ((k -> (existing :+ ((destInt,destMap,Array.fill(dl)(' ')))))))
    }
    val traversable = tree + ((-1 -> List((tree.keys.max,Array.fill(dnaLgth)(nucl)))))
    traverse(dnaLgth, traversable, tree.keys.max,-1,0)
    val parsimony = traversable(-1)(0)._2.map{_.map{case (k,v) => v}.min}.sum
    parsimony + "\n" + adjacency(traversable,tree.keys.max,-1,0)
  }

  def dnaAsString(a: Array[Map[Char,Long]]) =
    a.map{ _.map{case (k,v) => (v,k)}.min._2}.mkString("")
  def hamming(s1: String, s2: String) =
    s1.zip(s2).map{case (c1,c2) => if (c1==c2) 0 else 1}.sum

  def traverse(dnaLgth: Int, tree: Map[Int,List[(Int,Array[Map[Char,Long]],Array[Char])]], node: Int, parent: Int, child: Int) : Unit = {
    if (tree.isDefinedAt(node)) {
      val left = tree(node)(0)
      val leftIdx = left._1
      val leftDna = left._2
      traverse(dnaLgth,tree,leftIdx,node,0)
      val right = tree(node)(1)
      val rightIdx = right._1
      val rightDna = right._2
      traverse(dnaLgth,tree,rightIdx,node,1)
      (0 until dnaLgth).foreach {
        i =>
          tree(parent)(child)._2(i) = computeDna(leftDna(i),rightDna(i))
      }
    }
  }

  def adjacency(tree: Map[Int,List[(Int,Array[Map[Char,Long]])]], node: Int, parent: Int, child: Int) : String = {
    if (tree.isDefinedAt(node)) {
      val left = tree(node)(0)
      val leftIdx = left._1
      val leftDna = dnaAsString(left._2)
      val right = tree(node)(1)
      val rightIdx = right._1
      val rightDna = dnaAsString(right._2)
      val parentDna = dnaAsString(tree(parent)(child)._2)
      val hl = hamming(leftDna,parentDna)
      val hr = hamming(rightDna,parentDna)
      adjacency(tree, leftIdx, node, 0) + adjacency(tree, rightIdx, node, 1) +
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
}
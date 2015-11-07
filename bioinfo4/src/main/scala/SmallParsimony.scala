import scala.util.{Success, Try}

object SmallParsimony {
  val nucl = Map(
    'A' -> Long.MaxValue,
    'C' -> Long.MaxValue,
    'G' -> Long.MaxValue,
    'T' -> Long.MaxValue)
  def smallParsimony(lines: Array[String]) : String = {
    val leaves = lines(0).toInt
    val (dnaLgth,_,tree) = lines.tail.foldLeft((0,0,Map[Int,List[(Int,Array[Map[Char,Long]])]]())) {
      case ((dnaLgth,nodeCnt,m),s) =>
        val edge = s.split("->")
        val destTry = Try(edge(1).toInt)
        val destInt = destTry.getOrElse(nodeCnt)
        val destMap = destTry.transform(
          _ => Success(Array.fill(dnaLgth)(nucl)),
          _ => Success(edge(1).toCharArray.map { c => nucl + (c -> 0L) })
        ).get
        val add = if (destTry.isFailure) 1 else 0
        val k = edge(0).toInt
        val existing = m.getOrElse(k,List())
        (Math.max(edge(1).length,dnaLgth), nodeCnt + add, m + ((k -> (existing :+ ((destInt,destMap))))))
    }
    val traversable = tree + ((-1 -> List((tree.keys.max,Array.fill(dnaLgth)(nucl)))))
    traverse(dnaLgth, traversable, tree.keys.max,-1,0)
    tree.foreach{
      case (k,a) =>
        println(s"k=$k")
        a.foreach{
          l =>
            println(s"[[[${l._1}")
            l._2.foreach{ println(_)}
            println("]]]")
        }
    }
    ""
  }

  def traverse(dnaLgth: Int, tree: Map[Int,List[(Int,Array[Map[Char,Long]])]], node: Int, parent: Int, child: Int) : Unit = {
    if (tree.isDefinedAt(node)) {
      val left = tree(node)(0)
      val leftIdx = left._1
      val leftDna = left._2
      traverse(dnaLgth,tree,leftIdx,node,0)
      val right = tree(node)(1)
      val rightIdx = left._1
      val rightDna = right._2
      traverse(dnaLgth,tree,rightIdx,node,1)
      (0 until dnaLgth).foreach {
        i =>
          tree(parent)(child)._2(i) = computeDna(leftDna(i),rightDna(i))
      }
    }
  }

  def computeDna(left: Map[Char,Long], right: Map[Char,Long]) : Map[Char,Long] = {
    val (minLeftV,minLeftK) = left.map{case (k,v) => (v,k)}.min
    val (minRightV,minRightK) = right.map{case (k,v) => (v,k)}.min
    def pick(k: Char) = Math.min(
      { if (k == minLeftK) minLeftV else minLeftV+1 },
      { if (k == minRightK) minRightV else minRightV+1 }
    )
    left.map{ case (k,_) => (k, pick(k)) }
  }
}
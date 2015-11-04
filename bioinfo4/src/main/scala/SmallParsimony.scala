import scala.util.{Success, Try}

object SmallParsimony {
  val nucl = Map(
    'A' -> Long.MaxValue,
    'C' -> Long.MaxValue,
    'G' -> Long.MaxValue,
    'T' -> Long.MaxValue)
  def smallParsimony(lines: Array[String]) : String = {
    val n = lines(0).toInt
    val (_,tree) = lines.tail.foldLeft((0,Map[Int,List[(Int,Array[Map[Char,Long]])]]())) {
      case ((cnt,m),s) =>
        val edge = s.split("->")
        val destTry = Try(edge(1).toInt)
        val destInt = destTry.getOrElse(cnt)
        val destStr = destTry.transform(_ => Success(""), _ => Success(edge(1))).get
        val add = if (destTry.isFailure) 1 else 0
        val k = edge(0).toInt
        val existing = m.getOrElse(k,List())
        (cnt + add, m + ((k,existing :+ ((destInt,destStr.toCharArray.map { c => nucl + (c -> 0L) })))))
    }
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
}
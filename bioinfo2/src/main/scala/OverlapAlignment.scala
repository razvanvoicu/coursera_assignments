import scala.collection.{Map, mutable}
import scala.io.Source

object OverlapAlignment {
  val cache = new mutable.HashMap[(String, String), (Int, String, String)]()

  var best = (Int.MinValue, "", "")

  def overlapAlign(v: String, w: String, fittedOn: String, score: Map[(String, String), Int]): (Int, String, String) = (v, w) match {
    case ("", "") => (0, "", "")
    case _ => {
      cache.get((v, w)) match {
        case Some(result) => result
        case _ => {
          val v1 = if (v != "") {
            overlapAlign(v.substring(0, v.length - 1), w, fittedOn, score) match {
              case (r, s1, s2) => (r + score((v.charAt(v.length - 1).toString, "-")), s1 + v.charAt(v.length - 1), s2 + "-")
            }
          } else (Int.MinValue, "", "")
          val v2 = if (w != "") {
            overlapAlign(v, w.substring(0, w.length - 1), fittedOn, score) match {
              case (r, s1, s2) => (r + score("-", w.charAt(w.length - 1).toString), s1 + "-", s2 + w.charAt(w.length - 1))
            }
          } else (Int.MinValue, "", "")
          val v3 = if (v != "" && w != "") {
            overlapAlign(v.substring(0, v.length - 1), w.substring(0, w.length - 1), fittedOn, score) match {
              case (r, s1, s2) => (r + score(v.charAt(v.length - 1).toString, w.charAt(w.length - 1).toString), s1 + v.charAt(v.length - 1), s2 + w.charAt(w.length - 1))
            }
          } else (Int.MinValue, "", "")
          val v4 = if (w == "") (0,"","") else (Int.MinValue,"","")
          val result = Array(v1, v2, v3, v4).max
          cache += ((v, w) -> result)
          if ( v == fittedOn )
            best = Array(best, result).max
          result
        }
      }
    }
  }


  def main(args: Array[String]): Unit = {
    val headings = ('A' to 'Z').map(_.toString).toArray
    val scoreMapRaw = headings.foldLeft(Map[(String, String), Int]()) {
      case (m, rowSym) => headings.foldLeft(m) {
        case (m:Map[(String, String), Int], colSym) => m + ((rowSym, colSym) -> (if (rowSym == colSym) 1 else -2))
      }
    }
    val scoreMap = headings.foldLeft(scoreMapRaw) {
      case (m, sym) => m + ((sym, "-") -> -2) + (("-", sym) -> -2)
    }
    val lines = Source.stdin.getLines.toArray
    overlapAlign(lines(0), lines(1), lines(0), scoreMap)
    best match {
      case (score, res1, res2) =>
        println(score)
        println(res1)
        println(res2)
    }

  }
}
import scala.collection.{Map, mutable}
import scala.io.Source

object LocalAlignment {
  val cache = new mutable.HashMap[(String,String),(Int,String,String)]()

  var best = (Int.MinValue,"","")

  def locAlign(v:String,w:String,score:Map[(String,String),Int]) : (Int,String,String) = (v,w) match {
    case ("","") => (0,"","")
    case _ => {
      cache.get((v, w)) match {
        case Some(result) => result
        case _ => {
          val v1 = if (v != "") {
            locAlign(v.substring(0, v.length - 1), w, score) match {
              case (r, s1, s2) => (r + score((v.charAt(v.length - 1).toString, "-")), s1 + v.charAt(v.length - 1), s2 + "-")
            }
          } else (Int.MinValue, "", "")
          val v2 = if (w != "") {
            locAlign(v, w.substring(0, w.length - 1), score) match {
              case (r, s1, s2) => (r + score("-", w.charAt(w.length - 1).toString), s1 + "-", s2 + w.charAt(w.length - 1))
            }
          } else (Int.MinValue, "", "")
          val v3 = if (v != "" && w != "") {
            locAlign(v.substring(0, v.length - 1), w.substring(0, w.length - 1), score) match {
              case (r, s1, s2) => (r + score(v.charAt(v.length - 1).toString, w.charAt(w.length - 1).toString), s1 + v.charAt(v.length - 1), s2 + w.charAt(w.length - 1))
            }
          } else (Int.MinValue, "", "")
          val result = Array(v1,v2,v3,(0,"","")).max
          cache += ((v,w) -> result)
          best = Array(best,result).max
          result
        }
      }
    }
  }


  def main(args: Array[String]) : Unit = {
    val scoreMatrixStream = getClass.getResourceAsStream("PAM250.txt")
    val scoreMatrixTextLines = Source.fromInputStream(scoreMatrixStream).getLines.toArray
    val scoreMatrixRaw = scoreMatrixTextLines.tail.map(_.split("[ ]+").tail.filter(_ != "").map(_.toInt))
    val headings = scoreMatrixTextLines(0).split("[ ]+").tail
    val scoreMapRaw = headings.zipWithIndex.foldLeft(Map[(String,String),Int]()){
      case (m,(rowSym,i)) => headings.zipWithIndex.foldLeft(m) {
        case (m,(colSym,j)) => m + ((rowSym,colSym) -> scoreMatrixRaw(i)(j))
      }
    }
    val scoreMap = headings.foldLeft(scoreMapRaw) {
      case (m,sym) => m + ((sym,"-") -> -5) + (("-",sym) -> -5)
    }
    val lines = Source.stdin.getLines.toArray
    locAlign(lines(0),lines(1),scoreMap)
    best match {
      case (score,res1,res2) =>
        println(score)
        println(res1)
        println(res2)
    }

  }
}

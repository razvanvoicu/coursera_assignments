import scala.collection._
import scala.io.Source

object GlobalAlignmentAffine {

  val sigma = 11
  val epsilon = 1
  val cacheLower = new mutable.HashMap[(String,String),(Int,String,String)]()
  val cacheMiddle = new mutable.HashMap[(String,String),(Int,String,String)]()
  val cacheUpper = new mutable.HashMap[(String,String),(Int,String,String)]()

  def lower(v:String, w:String, scoreMap:Map[(String,String),Int]) : (Int,String,String) = {
    if (v == "" && w == "")
      (0,"","")
    else if (v == "" || w == "")
      (Int.MinValue/2,"","")
    else {
      cacheLower.get((v, w)) match {
        case Some(r) => r
        case None => {
          val l = lower(v.substring(0, v.length - 1), w, scoreMap) match {
            case (r, sv, sw) => (r - epsilon, sv + v.charAt(v.length - 1), sw + "-")
          }
          val m = middle(v.substring(0, v.length - 1), w, scoreMap) match {
            case (r, sv, sw) => (r - sigma, sv + v.charAt(v.length - 1), sw + "-")
          }
          val result = Array(l, m).max
          cacheLower.put((v, w), result)
          result
        }
      }
    }
  }

  def upper(v:String, w:String, scoreMap:Map[(String,String),Int]) : (Int,String,String) = {
    if (v == "" && w == "")
      (0,"","")
    else if (v == "" || w == "")
      (Int.MinValue/2,"","")
    else {
      cacheUpper.get((v, w)) match {
        case Some(r) => r
        case None => {
          val u = upper(v, w.substring(0, w.length - 1), scoreMap) match {
            case (r, sv, sw) => (r - epsilon, sv + "-", sw + w.charAt(w.length - 1))
          }
          val m = middle(v, w.substring(0, w.length - 1), scoreMap) match {
            case (r, sv, sw) => (r - sigma, sv + "-", sw + w.charAt((w.length - 1)))
          }
          val result = Array(u, m).max
          cacheUpper.put((v, w), result)
          result
        }
      }
    }
  }

  def middle(v:String, w:String, scoreMap:Map[(String,String),Int]) : (Int,String,String) = {
    if (v == "" && w == "")
      (0,"","")
    else if (v == "" || w == "")
      (Int.MinValue/2,"","")
    else {
      cacheMiddle.get((v, w)) match {
        case Some(r) => r
        case None => {
          val l = lower(v, w, scoreMap)
          val u = upper(v, w, scoreMap)
          val m = middle(v.substring(0, v.length - 1), w.substring(0, w.length - 1), scoreMap) match {
            case (r, sv, sw) => (r + scoreMap((v.charAt(v.length - 1).toString, w.charAt(w.length - 1).toString)), sv + v.charAt(v.length - 1), sw + w.charAt(w.length - 1))
          }
          val result = Array(l, u, m).max
          cacheMiddle.put((v, w), result)
          result
        }
      }
    }
  }

  def main(args:Array[String]): Unit = {
    val scoreMatrixStream = getClass.getResourceAsStream("Blosum62.txt")
    val scoreMatrixTextLines = Source.fromInputStream(scoreMatrixStream).getLines.toArray
    val scoreMatrixRaw = scoreMatrixTextLines.tail.map(_.split("[ ]+").tail.filter(_ != "").map(_.toInt))
    val headings = scoreMatrixTextLines(0).split("[ ]+").tail
    val scoreMap = headings.zipWithIndex.foldLeft(Map[(String,String),Int]()){
      case (m,(rowSym,i)) => headings.zipWithIndex.foldLeft(m) {
        case (m,(colSym,j)) => m + ((rowSym,colSym) -> scoreMatrixRaw(i)(j))
      }
    }
    val lines = Source.stdin.getLines.toArray
    middle(lines(0),lines(1),scoreMap) match {
      case (score,res1,res2) =>
        println(score)
        println(res1)
        println(res2)
    }
  }
}

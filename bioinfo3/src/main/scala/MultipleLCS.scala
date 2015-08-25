import scala.collection.mutable
import scala.io.Source

object MultipleLCS {
  def last(s:String) = s.charAt(s.length-1)

  def same(s1:String, s2:String, s3:String) =
    if (Set(last(s1),last(s2),last(s3)).size == 1) 1 else 0

  def short(s:String) = s.substring(0,s.length-1)

  var cache = mutable.HashMap[(String,String,String),(Int,String,String,String)]()

  def globAlign(v:String, u:String, w:String) : (Int,String,String,String) = {
    (v, u, w) match {
      case ("", "", "") => (0, "", "", "")
      case _ => {
        cache.get((v, u, w)) match {
          case Some(r) => r
          case None => {
            val v1 = if (v != "") {
              globAlign(short(v), u, w) match {
                case (r, s1, s2, s3) => (r, s1 + last(v), s2 + '-', s3 + '-')
              }
            } else (Int.MinValue, "", "", "")
            val v2 = if (u != "") {
              globAlign(v, short(u), w) match {
                case (r, s1, s2, s3) => (r, s1 + '-', s2 + last(u), s3 + '-')
              }
            } else (Int.MinValue, "", "", "")
            val v3 = if (w != "") {
              globAlign(v, u, short(w)) match {
                case (r, s1, s2, s3) => (r, s1 + '-', s2 + '-', s3 + last(w))
              }
            } else (Int.MinValue, "", "", "")
            val v4 = if (v != "" && u != "") {
              globAlign(short(v), short(u), w) match {
                case (r, s1, s2, s3) => (r, s1 + last(v), s2 + last(u), s3 + '-')
              }
            } else (Int.MinValue, "", "", "")
            val v5 = if (v != "" && w != "") {
              globAlign(short(v), u, short(w)) match {
                case (r, s1, s2, s3) => (r, s1 + last(v), s2 + '-', s3 + last(w))
              }
            } else (Int.MinValue, "", "", "")
            val v6 = if (u != "" && w != "") {
              globAlign(v, short(u), short(w)) match {
                case (r, s1, s2, s3) => (r, s1 + '-', s2 + last(u), s3 + last(w))
              }
            } else (Int.MinValue, "", "", "")
            val v7 = if (v != "" && u != "" && w != "") {
              globAlign(short(v), short(u), short(w)) match {
                case (r, s1, s2, s3) => (r + same(v, u, w), s1 + last(v), s2 + last(u), s3 + last(w))
              }
            } else (Int.MinValue, "", "", "")
            val result = Array(v1, v2, v3, v4, v5, v6, v7).max
            cache += ((v, u, w) -> result)
            result
          }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines.toArray
    globAlign(lines(0),lines(1),lines(2)) match {
      case (score,res1,res2,res3) =>
        println(score)
        println(res1)
        println(res2)
        println(res3)
    }
  }
}

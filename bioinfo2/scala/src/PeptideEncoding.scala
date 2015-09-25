import scala.io.Source
import scala.util.matching._

object PeptideEncoding {
  def main(args:Array[String]) : Unit = {
    val lines = Source.stdin.getLines()
    val text = lines.next()
    val rna = text.map {
      case 'T' => 'U'
      case c => c
    }
    val compl : Char => Char = {
      case 'A' => 'U'
      case 'C' => 'G'
      case 'G' => 'C'
      case 'U' => 'A'
    }
    val conv : Char => Char = {
      case 'U' => 'T'
      case c => c
    }
    val rc = rna.reverse.map(compl)
    val pep = lines.next()
    val pepr = new Regex(pep)
    val g3 = new Regex("[A-Z]{3}")
    val cod : Regex.MatchData => String = c => Const.codon(c.toString)
    def trans(s:String,i:Int) = g3.findAllMatchIn(s.substring(i)).toArray.map(cod).mkString("")
    val c = pepr.findAllMatchIn(trans(rna,0)).map(c => c.start).map(i => text.substring(i*3,3*(i+pep.length))) ++
      pepr.findAllMatchIn(trans(rna,1)).map(_.start).map(i => text.substring(i*3+1,3*(i+pep.length)+1)) ++
      pepr.findAllMatchIn(trans(rna,2)).map(_.start).map(i => text.substring(i*3+2,3*(i+pep.length)+2)) ++
      pepr.findAllMatchIn(trans(rc,0)).map(_.start).map(i => rc.substring(i*3,3*(i+pep.length)).reverse.map(compl).map(conv)) ++
      pepr.findAllMatchIn(trans(rc,1)).map(_.start).map(i => rc.substring(i*3+1,3*(i+pep.length)+1).reverse.map(compl).map(conv)) ++
      pepr.findAllMatchIn(trans(rc,2)).map(_.start).map(i => rc.substring(i*3+2,3*(pep.length+i)+2).reverse.map(compl).map(conv))
    println(c.mkString("\n"))
  }
}

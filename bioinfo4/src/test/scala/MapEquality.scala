
trait MapEquality {
  def mapEq(a:Map[(Int,Int),Double],b:Map[(Int,Int),Double]) : Boolean = {
    (a.keys.toSet == b.keys.toSet) &&
      (a.forall { case ((i,j),v) => Math.abs(v - b((i,j))) < 0.001 })
  }
}

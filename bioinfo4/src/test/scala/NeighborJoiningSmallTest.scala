import org.scalatest._

class NeighborJoiningSmallTest extends FlatSpec with Matchers with MapEquality {

  it should "pass the test" in {
    val inp =
      """4
        |0	23	27	20
        |23	0	30	28
        |27	30	0	30
        |20	28	30	0""".stripMargin

    val resStr =
      """0->4:8.000
        |1->5:13.500
        |2->5:16.500
        |3->4:12.000
        |4->5:2.000
        |4->0:8.000
        |4->3:12.000
        |5->1:13.500
        |5->2:16.500
        |5->4:2.000""".stripMargin.lines

    val res =
      ( resStr
          .map(_.split("->|:") match {case Array(s1,s2,s3) => ((s1.toInt,s2.toInt) -> s3.toDouble)})
          .toMap )
    val r = mapEq(NeighborJoining.neighborJoining(inp.lines), res)
    r should be (true)
  }

}

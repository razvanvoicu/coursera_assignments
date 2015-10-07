import org.scalatest._

class UPGMASmallTest extends FlatSpec with Matchers {

  it should "pass the test" in {
    val inp =
      """4
        |0	20	17	11
        |20	0	20	13
        |17	20	0	10
        |11	13	10	0""".stripMargin

    val resStr =
      """0->5:7.000
        |1->6:8.833
        |2->4:5.000
        |3->4:5.000
        |4->2:5.000
        |4->3:5.000
        |4->5:2.000
        |5->0:7.000
        |5->4:2.000
        |5->6:1.833
        |6->5:1.833
        |6->1:8.833""".stripMargin.lines

    val res =
      ( resStr
          .map(_.split("->|:") match {case Array(s1,s2,s3) => ((s1.toInt,s2.toInt) -> s3.toDouble)})
          .toMap )
    UPGMA.upgma(inp.lines) should be (res)
  }

}

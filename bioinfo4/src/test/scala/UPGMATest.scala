import org.scalatest._

class UPGMATest extends FlatSpec with Matchers {

  it should "pass 1" in {
    val inp =
      """4
        |0	20	17	11
        |20	0	20	13
        |17	20	0	10
        |11	13	10	0""".stripMargin

    UPGMA.upgma(inp.lines) should be (inp)
  }
}

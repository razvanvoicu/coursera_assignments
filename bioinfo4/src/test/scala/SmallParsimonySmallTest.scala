import org.scalatest.{Matchers, FlatSpec}
import SmallParsimony._

class SmallParsimonySmallTest extends FlatSpec with Matchers {

  val testInput =
  """4
    |4->CAAATCCC
    |4->ATTGCGAC
    |5->CTGCGCTG
    |5->ATGGACGA
    |6->4
    |6->5""".stripMargin

  val testExpectedOutput =
  """16
    |ATTGCGAC->ATAGCCAC:2
    |ATAGACAA->ATAGCCAC:2
    |ATAGACAA->ATGGACTA:2
    |ATGGACGA->ATGGACTA:1
    |CTGCGCTG->ATGGACTA:4
    |ATGGACTA->CTGCGCTG:4
    |ATGGACTA->ATGGACGA:1
    |ATGGACTA->ATAGACAA:2
    |ATAGCCAC->CAAATCCC:5
    |ATAGCCAC->ATTGCGAC:2
    |ATAGCCAC->ATAGACAA:2
    |CAAATCCC->ATAGCCAC:5""".stripMargin

  it should "pass the test" in {
    println(smallParsimony(testInput.split(System.getProperty("line.separator"))))
    smallParsimony(testInput.split(System.getProperty("line.separator"))) shouldBe testExpectedOutput
  }
}

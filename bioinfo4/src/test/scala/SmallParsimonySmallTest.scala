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
    |CTTGTCTC->CAAATCCC:4
    |CTTGTCTC->ATTGCGAC:4
    |CAAATCCC->CTTGTCTC:4
    |ATTGCGAC->CTTGTCTC:4
    |CTGGTCTG->CTGCGCTG:2
    |CTGGTCTG->ATGGACGA:4
    |CTGCGCTG->CTGGTCTG:2
    |ATGGACGA->CTGGTCTG:4
    |CTTGTCTG->CTTGTCTC:1
    |CTTGTCTG->CTGGTCTG:1
    |CTTGTCTC->CTTGTCTG:1
    |CTGGTCTG->CTTGTCTG:1
    |""".stripMargin

  it should "pass the test" in {
    println("===" + smallParsimony(testInput.split(System.getProperty("line.separator"))) + "===")
    smallParsimony(testInput.split(System.getProperty("line.separator"))) shouldBe testExpectedOutput
  }
}

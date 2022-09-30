package sequences

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

abstract class PropertyTester extends Module {
  val a = IO(Input(Bool()))
  val b = IO(Input(Bool()))
  def prop: Property
  def desc: String = "???"
  AssertPropModule(SequenceFsms.compile(prop), desc)
}

class SimpleAssertionTests extends AnyFreeSpec with ChiselScalatestTester {
  "simple boolean property assert should pass" in {
    test(new PropertyTester {
      override def prop = PropSeq(SeqExpr(a))
    }) { dut =>
      dut.a.poke(true) // make assertion pass
      dut.clock.step()
      dut.a.poke(false) // should not matter anymore since we do not _always_ assert
      dut.clock.step()
    }
  }

  "simple boolean property assert should fail" in {
    val e = intercept[ChiselAssertionError] {
      test(new PropertyTester {
        override def prop = PropSeq(SeqExpr(a))
      }) { dut =>
        dut.a.poke(false) // make assertion fail
        dut.clock.step()
      }
    }
    assert(e.getMessage.contains("assertion"))
  }

}

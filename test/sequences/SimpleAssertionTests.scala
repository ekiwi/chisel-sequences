package sequences

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

abstract class PropertyAssertOnceTester extends Module {
  val a = IO(Input(Bool()))
  val b = IO(Input(Bool()))
  def prop: Property
  def desc: String = "???"
  AssertPropModule(SequenceFsms.compile(prop), desc)
}

abstract class PropertyAlwaysAssertTester extends Module {
  val a = IO(Input(Bool()))
  val b = IO(Input(Bool()))
  def prop: Property
  def desc: String = "???"
  SequenceFsms.assertAlways(prop, desc)
}

class SimpleAssertionTests extends AnyFreeSpec with ChiselScalatestTester {
  "simple boolean property assert should pass" in {
    test(new PropertyAssertOnceTester {
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
      test(new PropertyAssertOnceTester {
        override def prop = PropSeq(SeqExpr(a))
      }) { dut =>
        dut.a.poke(false) // make assertion fail
        dut.clock.step()
      }
    }
    assert(e.getMessage.contains("assertion"))
  }

  "simple concat sequence should pass" in {
    test(new PropertyAssertOnceTester {
      override def prop = PropSeq(SeqConcat(SeqExpr(a), SeqExpr(b)))
    }) { dut =>
      dut.a.poke(true) // make assertion pass
      dut.b.poke(false) // does not matter
      dut.clock.step()
      dut.a.poke(false) // should not matter anymore since we do not _always_ assert
      dut.b.poke(true) // make assertion pass
      dut.clock.step()
      dut.b.poke(false) // should not matter anymore since we do not _always_ assert
      dut.clock.step()
    }
  }

  "simple concat sequence should fail always assert" in {
    val e = intercept[ChiselAssertionError] {
      test(new PropertyAlwaysAssertTester {
        override def prop = PropSeq(SeqConcat(SeqExpr(a), SeqExpr(b)))
      }) { dut =>
        dut.a.poke(true) // make assertion pass
        dut.b.poke(false) // does not matter
        dut.clock.step()
        dut.a.poke(false) // actually matters in this case!
        dut.b.poke(true) // make assertion pass
        dut.clock.step()
      }
    }
    assert(e.getMessage.contains("assertion"))
  }

  "simple concat sequence should pass always assert" in {
    test(new PropertyAlwaysAssertTester {
      override def prop = PropSeq(SeqConcat(SeqExpr(a), SeqExpr(b)))
    }) { dut =>
      dut.a.poke(true) // make assertion pass
      dut.b.poke(false) // does not matter
      dut.clock.step()
      dut.a.poke(true) // actually matters in this case!
      dut.b.poke(true) // make assertion pass
      dut.clock.step()
    }
  }

  "simple concat sequence should fail" in {
    val e = intercept[ChiselAssertionError] {
      test(new PropertyAssertOnceTester {
        override def prop = PropSeq(SeqConcat(SeqExpr(a), SeqExpr(b)))
      }) { dut =>
        dut.a.poke(true) // make assertion pass for now
        dut.b.poke(false) // does not matter
        dut.clock.step()
        dut.b.poke(false) // make assertion fail
        dut.clock.step()
      }
    }
    assert(e.getMessage.contains("assertion"))
  }
}

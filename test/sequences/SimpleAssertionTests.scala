package sequences

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

abstract class PropertyAlwaysAssertTester extends Module {
  val a = IO(Input(Bool()))
  val b = IO(Input(Bool()))
  def prop: Property
  def desc: String = "???"
  SequenceFsms.assertAlways(prop)
}

case class PropertyTest(prop: (Bool, Bool) => Property, a: String = "", b: String = "", failAt: Option[Int] = None) {
  require(failAt.isEmpty || failAt.get >= 0)
}
class PropertyTestModule(prop: (Bool, Bool) => Property) extends Module {
  val a = IO(Input(Bool()))
  val b = IO(Input(Bool()))
  val fail = IO(Output(Bool()))
  val m = SequenceFsms.assertAlways(prop(a, b))
  m.advance := !reset.asBool
  fail := m.fail
}



class SimpleAssertionTests extends AnyFreeSpec with ChiselScalatestTester {

  def shouldPass(a: String = "", b: String = "", prop: (Bool, Bool) => Property): Unit = {
    run(PropertyTest(a=a, b=b, prop=prop, failAt = None))
  }
  def shouldFail(failAt: Int, a: String = "", b: String = "", prop: (Bool, Bool) => Property): Unit = {
    run(PropertyTest(a=a, b=b, prop=prop, failAt = Some(failAt)))
  }

  def run(tst: PropertyTest): Unit = {
    var cycle = 0
    test(new PropertyTestModule(tst)) { dut =>
      while(!dut.done.peekBoolean()) {
        tst.failAt match {
          case Some(value) if value == cycle =>
            dut.fail.expect(true)
          case None => dut.fail.expect(false)
        }
        dut.clock.step()
        cycle += 1
      }
    }
  }

    "simple boolean property assert should pass" in {
      shouldPass(
        prop = (a, _) => PropSeq(SeqExpr(a)),
        a = "10",
      )
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



}

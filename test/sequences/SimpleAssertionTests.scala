// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>
package sequences

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import backend.Backend

abstract class PropertyAlwaysAssertTester(backend: Backend) extends Module {
  val a = IO(Input(Bool()))
  val b = IO(Input(Bool()))
  def prop: Property
  def desc: String = "???"
  assertAlways(prop, desc, backend = backend)
}

// https://www.scalatest.org/scaladoc/3.2.5/org/scalatest/freespec/AnyFreeSpec.html
trait SimpleAssertionTestsContainer { this: AnyFreeSpec with ChiselScalatestTester =>
  def tests(backend: Backend) = {
    "simple concat sequence should fail always assert" in {
      val e = intercept[ChiselAssertionError] {
        test(new PropertyAlwaysAssertTester(backend) {
          override def prop = PropSeq(SeqConcat(SeqExpr(a), SeqExpr(b)))
        }).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
          dut.a.poke(true) // make assertion pass
          dut.b.poke(false) // b does not matter in the first cycle
          dut.clock.step()
          dut.a.poke(false) // a can never be false
          dut.b.poke(true) // property fails whenever a is false, b does not matter here
          dut.clock.step()
        }
      }
      assert(e.getMessage.contains("assertion"))
    }

    "simple concat sequence should pass always assert" in {
      test(new PropertyAlwaysAssertTester(backend) {
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
        test(new PropertyAlwaysAssertTester(backend) {
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
}

class SimpleAssertionTests extends AnyFreeSpec with SimpleAssertionTestsContainer with ChiselScalatestTester {
  "Sequence FSM backend should work" - {
    tests(backend.SequenceFsms)
  }
  "Spot backend should work" - {
    tests(backend.Spot)
  }
}

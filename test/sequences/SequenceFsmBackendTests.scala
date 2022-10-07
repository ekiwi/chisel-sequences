// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>
package sequences

import chisel3._
import chiseltest._

import org.scalatest.freespec.AnyFreeSpec

class TestFindFirstInactive(n: Int) extends Module {
  val in = IO(Input(UInt(n.W)))
  val res = IO(Output(UInt(n.W)))
  res := findFirstInactive(in)
}

class SequenceFsmBackendTests extends AnyFreeSpec with ChiselScalatestTester {
  "the find first inactive method should work" in {
    test(new TestFindFirstInactive(6)) { dut =>
      dut.in.poke("b101110".U)
      dut.res.expect("b010000".U)
      dut.clock.step()
    }
  }

}

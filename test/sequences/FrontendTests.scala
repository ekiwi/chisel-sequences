// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import sequences.frontend.frontend._

abstract class BasicTestModule extends Module {
  val a = IO(Input(Bool()))
  val b = IO(Input(Bool()))
}

class FrontendTests extends AnyFreeSpec with ChiselScalatestTester {
  "parse a #1 b" in {
    class Test extends BasicTestModule {
      a.###(1)(b)
    }
    test(new Test) { _ => }
  }

  "parse a |-> b" in {
    class Test extends BasicTestModule {
      a |-> b
    }
    test(new Test) { _ => }
  }

  "parse a |=> b" in {
    class Test extends BasicTestModule {
      a |=> b
    }
    test(new Test) { _ => }
  }
}

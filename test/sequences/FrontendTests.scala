// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import sequences._

abstract class BasicTestModule extends Module {
  val a = IO(Input(Bool()))
  val b = IO(Input(Bool()))
}

class FrontendTests extends AnyFreeSpec with ChiselScalatestTester {
  private def propString(prop: Property): String = backend.serialize(ToIRConverter.toIR(prop)._1.prop)

  "parse a ##1 b" in {
    class Test extends BasicTestModule {
      val prop = a.###(1)(b)
      assert(propString(prop) == "p0 ##1 p1")
    }
    test(new Test) { _ => }
  }

  "parse a |-> b" in {
    class Test extends BasicTestModule {
      val prop = a |-> b
      assert(propString(prop) == "p0 |-> p1")
    }
    test(new Test) { _ => }
  }

  "parse a |=> b" in {
    class Test extends BasicTestModule {
      val prop = a |=> b
      assert(propString(prop) == "p0 |=> p1")
    }
    test(new Test) { _ => }
  }

  "parse a |=> a" in {
    class Test extends BasicTestModule {
      val prop = a |=> a
      // it is important that the converter recognizes that the two occurrences of a are the same signal
      assert(propString(prop) == "p0 |=> p0")
    }
    test(new Test) { _ => }
  }

  "parse a ##1 !a" ignore { // TODO: this does not work since we cannot introspect the chisel...
    class Test extends BasicTestModule {
      val prop = a.###(1)(!a)
      // it is important that the converter recognizes that the two occurrences of a are the same signal and
      // includes the inversion since this will lead to a more efficient automaton
      assert(propString(prop) == "p0 ##1 !p0")
    }
    test(new Test) { _ => }
  }
}

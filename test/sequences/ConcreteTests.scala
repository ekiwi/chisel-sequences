// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ConcreteSequenceFsmTests extends ConcreteTests(backend.SequenceFsms, true)
class ConcreteSpotFsmTests extends ConcreteTests(backend.Spot)

/** Tests all our properties using chiseltest and treadle */
abstract class ConcreteTests(back: backend.Backend, debug: Boolean = false)
    extends AnyFlatSpec
    with ChiselScalatestTester {

  val annos = if (debug) Seq(WriteVcdAnnotation) else Seq()

  private def runTest(prop: TestProperty): Unit = {
    val name = prop.name
    behavior.of(s"${back.name}: $name")

    prop.testInputs.zipWithIndex.foreach { case (in, ii) =>
      val status = if (in.failAt >= 0) s"fail at ${in.failAt}" else "pass"
      it should s"$status input #$ii" in {
        test(new PropertyModule(prop.prop, back)).withAnnotations(annos)(runTest(_, in))
      }
    }
  }

  private def runTest(dut: PropertyModule, ii: TestInput): Unit = {
    val dutInputs = Seq(dut.io.a, dut.io.b, dut.io.c, dut.io.d)
    def step(): Unit = dut.clock.step()
    val inputValues = TestInput.getPaddedInputs(ii).transpose
    val rnd = new scala.util.Random()
    inputValues.zipWithIndex.foreach { case (stepValues, stepId) =>
      // apply inputs for step
      dutInputs.zip(stepValues).foreach {
        case (io, Some(value)) => io.poke(value)
        case (io, None)        => io.poke(rnd.nextBoolean())
      }
      // check status
      if (ii.expectSuccess) {
        dut.io.fail.expect(false)
      } else {
        val expectFail = stepId == ii.failAt
        val hasFailed = stepId > ii.failAt
        if (!hasFailed) {
          dut.io.fail.expect(expectFail)
        }
      }
      // advance the clock
      step()
    }
  }

  // test all properties
  TestProperties.properties.foreach(runTest)
}

private class PropertyIO extends Bundle {
  val a = Input(Bool())
  val b = Input(Bool())
  val c = Input(Bool())
  val d = Input(Bool())
  val fail = Output(Bool())
}

private class PropertyModule(prop: TestProperty.Prop, back: backend.Backend) extends Module {
  val io = IO(new PropertyIO)
  val p = prop(io.a, io.b, io.c, io.d)
  val propertyIR = ToIRConverter.toIR(p)
  io.fail := toAutomaton(p, back)
}

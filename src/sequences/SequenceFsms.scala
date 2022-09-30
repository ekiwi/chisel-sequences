package sequences

import chisel3._
import chisel3.experimental.ChiselEnum

object SequenceFsms {
  def compile(p: Property): PropertyIO = {
    p match {
      case PropSeq(s) => PropSeqModule(compile(s))
    }
  }

  def compile(s: Sequence): SequenceIO = {
    s match {
      case SeqExpr(predicate) => SeqExprModule(predicate)
    }
  }
}

object SeqRes extends ChiselEnum {
  val SeqFail, SeqPending, SeqHold, SeqHoldStrong = Value
}

class SequenceIO extends Bundle {

  /** is the FSM active this cycle? */
  val advance = Input(Bool())

  /** indicates that the FSM has not finished yet */
  val running = Output(Bool())

  /** current result (only valid if advance is true) */
  val status = Output(SeqRes())
}

object PropRes extends ChiselEnum {
  val PropTrue, PropUndetermined, PropFalse, PropVacuous = Value
}

class PropertyIO extends Bundle {

  /** is the FSM active this cycle? */
  val advance = Input(Bool())

  /** only valid if advance is true */
  val status = Output(PropRes())
}

/** converts a boolean signal to the sequence I/O */
class SeqExprModule extends Module {
  val io = IO(new SequenceIO)
  val predicate = IO(Input(Bool()))
  // holds iff the predicate is true
  io.status := Mux(predicate, SeqRes.SeqHoldStrong, SeqRes.SeqFail)
  // no FSM state, so never running
  io.running := false.B
}

object SeqExprModule {
  def apply(predicate: Bool): SequenceIO = {
    val mod = Module(new SeqExprModule).suggestName("seq_expr")
    mod.predicate := predicate
    mod.io
  }
}

/** converts a sequence I/O into a property I/O */
class PropSeqModule extends Module {
  val seq = IO(Flipped(new SequenceIO))
  val io = IO(new PropertyIO)
  // advance is just connected
  seq.advance := io.advance

  when(seq.status.isOneOf(SeqRes.SeqHold, SeqRes.SeqHoldStrong)) {
    io.status := PropRes.PropTrue
  }.elsewhen(seq.status.isOneOf(SeqRes.SeqPending)) {
    io.status := PropRes.PropUndetermined
  }.elsewhen(seq.status.isOneOf(SeqRes.SeqFail)) {
    io.status := PropRes.PropFalse
  }.otherwise {
    // assert(false.B, "should not get here")
    io.status := DontCare
  }
}

object PropSeqModule {
  def apply(s: SequenceIO): PropertyIO = {
    val mod = Module(new PropSeqModule).suggestName("prop_seq")
    mod.seq <> s
    mod.io
  }
}

/** assert a property from the start of reset
  *  @note: this is not an always assert, it will only check the property once!
  */
class AssertPropModule(desc: String) extends Module {
  val propertyIO = IO(Flipped(new PropertyIO))

  // the assertion is active starting at reset
  val going = RegInit(true.B)
  propertyIO.advance := false.B

  // only advance when reset is false and we are still going
  when(going && !reset.asBool) {
    propertyIO.advance := true.B
    // continue advancing the property while the result is still undetermined
    going := propertyIO.status === PropRes.PropUndetermined
    // if the property returns false, this assertion fails
    assert(propertyIO.status =/= PropRes.PropFalse, desc)
  }
}

object AssertPropModule {
  def apply(p: PropertyIO, desc: String): Unit = {
    val mod = Module(new AssertPropModule(desc)).suggestName("assert_prop")
    mod.propertyIO <> p
  }
}

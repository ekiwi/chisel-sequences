// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Vighnesh Iyer <vighnesh.iyer@berkeley.edu>

package sequences.backend

import chisel3._
import chisel3.util._
import sequences.backend.HOAParser.Condition

import HOAParser._

object Spot extends Backend {
  override val name: String = "Spot"

  override def compile(prop: PropertyInfo): PropertyAutomatonModule = {
    val preds = prop.predicates
    val seq = prop.prop match {
      case PropSeq(s) => s
    }
    val psl = s"G(${sequenceToPSL(seq)})"
    val hoaString = callSpot(psl)
    val hoa = HOAParser.parseHOA(hoaString)
    Module(new SpotPropertyAutomaton(preds, hoa))
  }

  def boolExprtoPSL(e: BooleanExpr): String = e match {
    case SymbolExpr(name) => name
    case NotExpr(e)       => s"!(${boolExprtoPSL(e)})"
    case AndExpr(a, b)    => ???
    case OrExpr(a, b)     => ???
  }

  def sequenceToPSL(s: Sequence): String = {
    s match {
      case SeqPred(predicate) =>
        s"(${boolExprtoPSL(predicate)})"
      case SeqConcat(s1, s2) =>
        s"(${sequenceToPSL(s1)} & X(${sequenceToPSL(s2)}))"
      case SeqOr(s1, s2)          => ???
      case SeqIntersect(s1, s2)   => ???
      case SeqNot(s1)             => s"!${sequenceToPSL(s1)}"
      case SeqImplies(s1, p1)     => ???
      case SeqImpliesNext(s1, p1) => ???
      case SeqFuse(s1, s2)        => ???
    }
  }

  def callSpot(psl: String): String = {
    val cmd = Seq("ltl2tgba", "-B", "-D", "-f", psl)
    val result = os.proc(cmd).call()
    assert(result.exitCode == 0)
    result.out.toString
  }

  def conditionToChisel(cond: Condition, apMap: Map[HOAParser.AP, String]): PredicateBundle => Bool = {
    predBundle: PredicateBundle =>
      cond match {
        case HOAParser.True            => true.B
        case HOAParser.Predicate(ap)   => predBundle.elements(apMap(ap))
        case HOAParser.Not(p)          => !predBundle.elements(apMap(p.ap))
        case HOAParser.And(conds @ _*) => conds.map(conditionToChisel(_, apMap)(predBundle)).reduce(_ && _)
        case HOAParser.Or(conds @ _*)  => conds.map(conditionToChisel(_, apMap)(predBundle)).reduce(_ || _)
      }
  }

  /** Assuming we're in some state with the given transitions, construct a circuit that tells us the next state we're moving to and
    * raise the output Bool flag if there exists no transition (i.e. the automata has failed)
    */
  def nextStateCircuit(
    predicateBundle: PredicateBundle,
    transitions:     Map[Condition, StateId],
    apMap:           Map[AP, String]
  ): (UInt, Bool) = {
    val nextState = WireDefault(0.U)
    val fail = WireDefault(1.B)
    for ((condition, nextStateId) <- transitions) {
      when(conditionToChisel(condition, apMap)(predicateBundle)) {
        nextState := nextStateId.U
        fail := 0.B
      }
    }
    (nextState, fail)
  }

  class SpotPropertyAutomaton(preds: Seq[String], hoa: HOAParser.HOA) extends PropertyAutomatonModule {
    val io = IO(new PropertyAutomatonIO(preds))

    val automataState = RegInit(UInt(log2Ceil(hoa.nStates).W), hoa.initialState.U)
    val failed = RegInit(Bool(), false.B)
    val willFail = WireDefault(false.B)

    for ((stateId, state) <- hoa.states) {
      when(automataState === stateId.U) {
        val (nextState, fail) = nextStateCircuit(io.predicates, state.transitions, hoa.aps)
        automataState := nextState
        willFail := fail
      }
    }

    failed := Mux(failed, failed, willFail)
    io.fail := failed || willFail
  }
}

// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences

import chisel3.Bool
import backend.{Backend, PropertyInfo, SeqPred}

import scala.collection.mutable

object toAutomaton {
  def apply(prop: Property, backend: Backend): Bool = {
    val (info, pred) = new ToIRConverter().toIR(prop)
    val mod = backend.compile(info)
    // connect predicates as inputs
    mod.io.predicates.elements.foreach { case (name, input) => input := pred(name) }
    // return fail signal
    mod.io.fail
  }
}

private class ToIRConverter {
  private val pred = mutable.LinkedHashMap[Bool, String]()

  def toIR(prop: Property): (PropertyInfo, Map[String, Bool]) = {
    pred.clear()
    val propertyIR = convert(prop)
    val nameToPred = pred.toSeq.map { case (a, b) => (b, a) }
    (backend.PropertyInfo(propertyIR, nameToPred.map(_._1)), nameToPred.toMap)
  }

  private def convert(prop: Property): backend.Property = prop match {
    case PropSeq(s) => backend.PropSeq(convert(s))
  }

  private def convert(seq: Sequence): backend.Sequence = seq match {
    case SeqExpr(predicate) =>
      val name = pred.getOrElseUpdate(predicate, f"p${pred.size}")
      SeqPred(name)
    case SeqOr(s1, s2)          => backend.SeqOr(convert(s1), convert(s2))
    case SeqConcat(s1, s2)      => backend.SeqConcat(convert(s1), convert(s2))
    case SeqIntersect(s1, s2)   => backend.SeqIntersect(convert(s1), convert(s2))
    case SeqNot(s1)             => backend.SeqNot(convert(s1))
    case SeqImplies(s1, p1)     => backend.SeqImplies(convert(s1), convert(p1))
    case SeqImpliesNext(s1, p1) => backend.SeqImpliesNext(convert(s1), convert(p1))
    case SeqFuse(s1, s2)        => backend.SeqFuse(convert(s1), convert(s2))
  }
}

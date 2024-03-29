// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences

import chisel3.Bool

import scala.collection.mutable

object toAutomaton {
  def apply(prop: Property, backendImpl: backend.Backend): Bool = {
    val (info, pred) = ToIRConverter.toIR(prop)
    val mod = backendImpl.compile(info)
    // connect predicates as inputs
    mod.io.predicates.elements.foreach { case (name, input) => input := pred(name) }
    // return fail signal
    mod.io.fail
  }
}

private object ToIRConverter {
  def toIR(prop: Property): (backend.PropertyInfo, Map[String, Bool]) = new ToIRConverter().toIR(prop)
}

private class ToIRConverter private () {
  private val pred = mutable.LinkedHashMap[Bool, String]()

  def toIR(prop: Property): (backend.PropertyInfo, Map[String, Bool]) = {
    pred.clear()
    val propertyIR = convert(prop)
    val nameToPred = pred.toSeq.map { case (a, b) => (b, a) }
    (backend.PropertyInfo(propertyIR, nameToPred.map(_._1)), nameToPred.toMap)
  }

  private def convert(prop: Property): backend.Property = prop match {
    case PropSeq(s) => backend.PropSeq(convert(s))
  }

  private def convert(seq: Sequence): backend.Sequence = seq match {
    case SeqExpr(predicate)     => backend.SeqPred(convert(predicate))
    case SeqOr(s1, s2)          => backend.SeqOr(convert(s1), convert(s2))
    case SeqConcat(s1, s2)      => backend.SeqConcat(convert(s1), convert(s2))
    case SeqIntersect(s1, s2)   => backend.SeqIntersect(convert(s1), convert(s2))
    case SeqNot(s1)             => backend.SeqNot(convert(s1))
    case SeqImplies(s1, p1)     => backend.SeqImplies(convert(s1), convert(p1))
    case SeqImpliesNext(s1, p1) => backend.SeqImpliesNext(convert(s1), convert(p1))
    case SeqFuse(s1, s2)        => backend.SeqFuse(convert(s1), convert(s2))
  }

  private def convert(e: chisel3.Bool): backend.BooleanExpr = {
    // TODO: is there a way to introspect the chisel to find when an expression is a and/or/not expression?
    val name = pred.getOrElseUpdate(e, f"p${pred.size}")
    backend.SymbolExpr(name)
  }
}

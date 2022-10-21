// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences.backend

import chisel3._

import scala.collection.immutable.{SeqMap, VectorMap}

trait Backend {
  def name: String
  def compile(prop: PropertyInfo): PropertyAutomatonModule
}

/** Contains a converted property and the name of all predicates used in it. */
case class PropertyInfo(prop: Property, predicates: Seq[String])

class PredicateBundle(predicates: Seq[String]) extends Record {
  override val elements:  SeqMap[String, Bool] = VectorMap[String, Bool](predicates.map(p => p -> Input(Bool())): _*)
  override def cloneType: PredicateBundle.this.type = new PredicateBundle(predicates).asInstanceOf[this.type]
}

class PropertyAutomatonIO(preds: Seq[String]) extends Bundle {
  val predicates = new PredicateBundle(preds)
  val fail = Output(Bool())
}

trait PropertyAutomatonModule { this: Module =>
  val io: PropertyAutomatonIO
}

sealed trait BooleanExpr {}
case class SymbolExpr(name: String) extends BooleanExpr
case class NotExpr(e: BooleanExpr) extends BooleanExpr
case class AndExpr(a: BooleanExpr, b: BooleanExpr) extends BooleanExpr
case class OrExpr(a: BooleanExpr, b: BooleanExpr) extends BooleanExpr

sealed trait Sequence {}

case class SeqPred(predicate: BooleanExpr) extends Sequence
case class SeqOr(s1: Sequence, s2: Sequence) extends Sequence
case class SeqConcat(s1: Sequence, s2: Sequence) extends Sequence
case class SeqIntersect(s1: Sequence, s2: Sequence) extends Sequence
case class SeqNot(s1: Sequence) extends Sequence
case class SeqImplies(s1: Sequence, p1: Property) extends Sequence
case class SeqImpliesNext(s1: Sequence, p1: Property) extends Sequence
case class SeqFuse(s1: Sequence, s2: Sequence) extends Sequence

sealed trait Property {}

case class PropSeq(s: Sequence) extends Property

object serialize {
  def apply(p: Property): String = {
    p match {
      case PropSeq(s) => apply(s)
    }
  }

  def apply(s: Sequence): String = {
    s match {
      case SeqPred(predicate)     => apply(predicate)
      case SeqOr(s1, s2)          => apply(s1) + " or " + apply(s2)
      case SeqConcat(s1, s2)      => apply(s1) + " ##1 " + apply(s2)
      case SeqIntersect(s1, s2)   => apply(s1) + " and " + apply(s2)
      case SeqNot(s1)             => "not" + apply(s1)
      case SeqImplies(s1, p1)     => apply(s1) + " |-> " + apply(p1)
      case SeqImpliesNext(s1, p1) => apply(s1) + " |=> " + apply(p1)
      case SeqFuse(s1, s2)        => apply(s1) + " ##0 " + apply(s2)
    }
  }

  def apply(e: BooleanExpr): String = e match {
    case SymbolExpr(name) => name
    case NotExpr(e)       => s"!${apply(e)}"
    case AndExpr(a, b)    => s"(${apply(a)} && ${apply(b)})"
    case OrExpr(a, b)     => s"(${apply(a)} || ${apply(b)})"
  }
}

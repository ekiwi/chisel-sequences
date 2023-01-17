// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences

import scala.language.implicitConversions

package object sequences {
  implicit def chiselBoolToSequence(b: chisel3.Bool): Sequence = SeqExpr(b)
  implicit def chiselBoolToProperty(b: chisel3.Bool): Property = PropSeq(SeqExpr(b))
  implicit def sequenceToProperty(s:   Sequence):     Property = PropSeq(s)
}

sealed trait Sequence {
  def ###(delay: Int = 1)(other: Sequence): SeqConcat = {
    require(delay >= 0, "negative delay is not supported")
    require(delay == 1, "TODO")
    SeqConcat(this, other)
  }
  def |->(p: Property): SeqImplies = SeqImplies(this, p)
  def |=>(p: Property): SeqImpliesNext = SeqImpliesNext(this, p)
}

case class SeqExpr(predicate: chisel3.Bool) extends Sequence
case class SeqOr(s1: Sequence, s2: Sequence) extends Sequence
case class SeqConcat(s1: Sequence, s2: Sequence) extends Sequence
case class SeqIntersect(s1: Sequence, s2: Sequence) extends Sequence
case class SeqNot(s1: Sequence) extends Sequence
case class SeqImplies(s1: Sequence, p1: Property) extends Sequence
case class SeqImpliesNext(s1: Sequence, p1: Property) extends Sequence
case class SeqFuse(s1: Sequence, s2: Sequence) extends Sequence

sealed trait Property {}

case class PropSeq(s: Sequence) extends Property

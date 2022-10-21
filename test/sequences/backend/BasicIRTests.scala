// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences.backend

import org.scalatest.freespec.AnyFreeSpec

class BasicIRTests extends AnyFreeSpec {

  "case class IR should serialize to a human readable format" in {
    val (a, b) = (SymbolExpr("a"), SymbolExpr("b"))
    assert(serialize(SeqPred(a)) == "a")
    assert(serialize(PropSeq(SeqPred(a))) == "a")
    assert(serialize(SeqOr(SeqPred(a), SeqPred(b))) == "a or b")
  }

}

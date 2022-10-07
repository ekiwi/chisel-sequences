// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences

import chisel3._
import org.scalatest.freespec.AnyFreeSpec

class BasicIRTests extends AnyFreeSpec {

  "case class IR should serialize to a human readable format" in {
    assert(serialize(SeqExpr(false.B)) == "Bool(false)")
    assert(serialize(PropSeq(SeqExpr(false.B))) == "Bool(false)")
    assert(serialize(SeqOr(SeqExpr(false.B), SeqExpr(true.B))) == "Bool(false) or Bool(true)")
  }

}

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

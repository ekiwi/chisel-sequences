package sequences.backend

import chisel3._
import org.scalatest.freespec.AnyFreeSpec

class SpotTest extends AnyFreeSpec {
  "Spot PSL serializer should emit legal PSL" in {
    assert(Spot.toPSL(SeqPred("a")) == "(a)")
    val expr = SeqConcat(SeqPred("a"), SeqPred("b"))
    assert(Spot.toPSL(expr) == "((a) & X((b)))")
    println(Spot.callSpot(Spot.toPSL(expr)))
  }
}
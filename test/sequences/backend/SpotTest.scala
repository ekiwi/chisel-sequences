package sequences.backend

import org.scalatest.freespec.AnyFreeSpec

class SpotTest extends AnyFreeSpec {
  "Spot PSL serializer should emit legal PSL" in {
    val (a, b) = (SymbolExpr("a"), SymbolExpr("b"))
    assert(Spot.toPSL(SeqPred(a)) == "(a)")
    val expr = SeqConcat(SeqPred(a), SeqPred(b))
    assert(Spot.toPSL(expr) == "((a) & X((b)))")
    //println(Spot.callSpot(Spot.toPSL(expr)))
  }
}

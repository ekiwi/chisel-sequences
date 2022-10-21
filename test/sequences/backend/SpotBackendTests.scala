package sequences.backend

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chisel3._

class SpotBackendTests extends AnyFreeSpec with ChiselScalatestTester {

  "Spot PSL serializer should emit legal PSL" in {
    val (a, b) = (SymbolExpr("a"), SymbolExpr("b"))
    assert(Spot.sequenceToPSL(SeqPred(a)) == "(a)")
    val expr = SeqConcat(SeqPred(a), SeqPred(b))
    assert(Spot.sequenceToPSL(expr) == "((a) & X((b)))")
  }

  "Spot backend should emit monitor circuit for G(a & X !b)" in {
    val a = SeqPred(SymbolExpr("a"))
    val b = SeqPred(SymbolExpr("b"))
    val notB = SeqNot(b)

    def checker(): PropertyAutomatonModule = {
      Spot.compile(PropertyInfo(PropSeq(SeqConcat(a, notB)), Seq("a", "b")))
    }

    test(checker()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.predicates.elements("a").poke(1.B) // a must always be true
      c.io.predicates.elements("b").poke(1.B) // b must be false after a (b must always be false after the first cycle)
      c.io.fail.expect(0.B)
      c.clock.step()
      c.io.predicates.elements("b").poke(0.B)
      c.io.fail.expect(0.B)
      c.clock.step(10)
      c.io.fail.expect(0.B)
    }

    test(checker()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.predicates.elements("a").poke(1.B)
      c.io.predicates.elements("b").poke(1.B)
      c.io.fail.expect(0.B)
      c.clock.step()
      c.io.predicates.elements("b").poke(1.B)
      c.io.fail.expect(0.B)
      c.clock.step(1)
      c.io.fail.expect(1.B)
      c.clock.step(10)
      c.io.fail.expect(1.B)
    }
  }
}

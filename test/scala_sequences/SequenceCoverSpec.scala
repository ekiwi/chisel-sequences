package scala_sequences

import org.scalatest.freespec.AnyFreeSpec
import scala_sequences.Evaluator.CoverResult

class SequenceCoverSpec extends AnyFreeSpec {
  val isTrue = AtmProp[Boolean](i => i)
  val isFalse = AtmProp[Boolean](i => !i)
  val isTwo = AtmProp[Int](i => i == 2)
  val isThree = AtmProp[Int](i => i == 3)
  val delay = Delay(1)

  "cover should capture start and stop points for a sequence" in {
    val trueThanFalse = Concat(isTrue, isFalse) // isTrue ##1 isFalse
    val trace = Seq(1, 0, 0, 1, 0, 0, 1).map(_ == 1)
    val result = Evaluator.cover(trueThanFalse, trace)
    assert(
      result == CoverResult(
        completed = Seq((0, 1), (3, 4)),
        pending = Seq(6)
      )
    )
  }

  "cover 2 (nested concat)" in {
    val twoTrueOneFalse = Concat(isTrue, Concat(isTrue, isFalse)) // isTrue ##1 isTrue ##1 isFalse
    val trace = Seq(1, 1, 1, 0, 1, 1, 0, 1, 1).map(_ == 1)
    val result = Evaluator.cover(twoTrueOneFalse, trace)
    assert(
      result == CoverResult(
        completed = Seq((1, 3), (4, 6)),
        pending = Seq(7, 8)
      )
    )
  }

  "cover 3 (nested concat with Int)" in {
    val twoWaitThanThree: ScalaSeq[Int] = Concat(isTwo, isThree) // isTwo ##1 isThree
    val trace = Seq(1, 2, 3, 2, 4, 3, 2, 2, 3, 2)
    val result = Evaluator.cover(twoWaitThanThree, trace)
    assert(
      result == CoverResult(
        completed = Seq((1, 2), (7, 8)),
        pending = Seq(9)
      )
    )
  }

  "cover 4 (repeated sequence)" in {
    val repeatTrueThrice = Fuse(Repeated(Fuse(isTrue, Delay(1)), 2), isTrue) // isTrue[*3]
    val trace = Seq(1, 1, 1, 0, 1, 1, 1, 1, 0, 1).map(_ == 1)
    val result = Evaluator.cover(repeatTrueThrice, trace)
    assert(
      result == CoverResult(
        completed = Seq((0, 2), (4, 6), (5, 7)),
        pending = Seq(9)
      )
    )
  }
}

package sequences.backend

import HOAParser._
import fastparse._
import org.scalatest.freespec.AnyFreeSpec

class HOAParserTests extends AnyFreeSpec {
  "groupStates should aggregate state declarations and their transitions" in {
    val input = Seq(
      "State: 0 {0}",
      "[0] 0",
      "[1] 1",
      "State: 1",
      "[t] 1"
    )
    val groupedStates = HOAParser.groupStates(input)
    assert(groupedStates == Seq(input.slice(0, 3), input.slice(3, 5)))
  }

  "transition condition parser should handle !, &, and |" in {
    assert(parse("t", conditionParser(_)).get.value == True)
    assert(parse("0", conditionParser(_)).get.value == Predicate(0))
    assert(parse("100", conditionParser(_)).get.value == Predicate(100))
    assert(
      parse("23&45", conditionParser(_)).get.value ==
        And(Predicate(23), Predicate(45))
    )
    assert(
      parse("1&!3", conditionParser(_)).get.value ==
        And(Predicate(1), Not(Predicate(3)))
    )
    assert(
      parse("1&!3&!5", conditionParser(_)).get.value ==
        And(Predicate(1), Not(Predicate(3)), Not(Predicate(5)))
    )
    assert(
      parse("!1&2&!3&!5&6", conditionParser(_)).get.value ==
        And(Not(Predicate(1)), Predicate(2), Not(Predicate(3)), Not(Predicate(5)), Predicate(6))
    )
    assert(
      parse("1 | 2", conditionParser(_)).get.value ==
        Or(And(Predicate(1)), And(Predicate(2)))
    )
    assert(
      parse("!0 | 1&!3 | 2&3&!4", conditionParser(_)).get.value == Or(
        And(Not(Predicate(0))),
        And(Predicate(1), Not(Predicate(3))),
        And(Predicate(2), Predicate(3), Not(Predicate(4)))
      )
    )
  }

  "parse trivial formula 'a'" in {
    val hoaString =
      """
        |HOA: v1
        |name: "a"
        |States: 2
        |Start: 1
        |AP: 1 "a"
        |acc-name: Buchi
        |Acceptance: 1 Inf(0)
        |properties: trans-labels explicit-labels state-acc deterministic
        |properties: stutter-invariant terminal
        |--BODY--
        |State: 0 {0}
        |[t] 0
        |State: 1
        |[0] 0
        |--END--
        |
        |""".stripMargin
    val hoa = HOAParser.parseHOA(hoaString)
    assert(
      hoa == HOA(
        name = "a",
        nStates = 2,
        initialState = 1,
        aps = Map(0 -> "a"),
        acceptingState = 1,
        states = Map(
          0 -> State(0, accepting = true, Map(True -> 0)),
          1 -> State(1, accepting = false, Map(Predicate(0) -> 0))
        )
      )
    )
  }

  "parse complex formula 'G(a -> X !b)'" in {
    val hoaString =
      """
        |HOA: v1
        |name: "G(a & X!b)"
        |States: 2
        |Start: 0
        |AP: 2 "a" "b"
        |acc-name: Buchi
        |Acceptance: 1 Inf(0)
        |properties: trans-labels explicit-labels state-acc deterministic
        |properties: very-weak
        |--BODY--
        |State: 0
        |[0] 1
        |State: 1 {0}
        |[0&!1] 1
        |--END--
        |""".stripMargin
    val hoa = HOAParser.parseHOA(hoaString)
    assert(
      hoa == HOA(
        name = "G(a & X!b)",
        nStates = 2,
        initialState = 0,
        aps = Map(0 -> "a", 1 -> "b"),
        acceptingState = 1,
        states = Map(
          0 -> State(0, accepting = false, Map(Predicate(0) -> 1)),
          1 -> State(1, accepting = true, Map(And(Predicate(0), Not(Predicate(1))) -> 1))
        )
      )
    )
  }
}

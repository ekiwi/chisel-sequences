package sequences.backend

import org.scalatest.freespec.AnyFreeSpec

class HOAParserTests extends AnyFreeSpec {
  "parse 'a'" in {
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
    val hoa = HOAParser.parse(hoaString)
    assert(hoa.name == "a")
    assert(hoa.nStates == 2)
    assert(hoa.acceptingState == 1)
    assert(hoa.aps == Map(0 -> "a"))
  }
}

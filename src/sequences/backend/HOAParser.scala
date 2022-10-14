package sequences.backend

/** Example HOA file:
  * HOA: v1
  * name: "f"
  * States: 2
  * Start: 1
  * AP: 1 "f"
  * acc-name: Buchi
  * Acceptance: 1 Inf(0)
  * properties: trans-labels explicit-labels state-acc deterministic
  * properties: stutter-invariant terminal
  * --BODY--
  * State: 0 {0}
  * [t] 0
  * State: 1
  * [0] 0
  * --END--
  */

object HOAParser {
  type AP = Int

  sealed trait Condition

  case class AtomicPredicate(ap: AP) extends Condition

  case class And(c1: Condition, c2: Condition) extends Condition

  case class Not(c1: Condition) extends Condition

  case object True extends Condition

  case class State(n: Int, accepting: Boolean, transitions: Map[Condition, Int])

  case class HOA(
    name:           String,
    nStates:        Int,
    startState:     Int,
    aps:            Map[AP, String],
    acceptingState: Int,
    states:         Set[State])

  def parse(s: String): HOA = {
    val lines = s.split('\n')
    lines.foldLeft(HOA("", 0, 0, Map.empty, 0, Set.empty)) { (hoa, line) =>
      if (line.startsWith("name: ")) {
        hoa.copy(name = line.drop("name: ".length + 1).dropRight(1))
      } else if (line.startsWith("States: ")) {
        hoa.copy(nStates = line.drop("States: ".length).toInt)
      } else if (line.startsWith("Start: ")) {
        hoa.copy(startState = line.drop("Start: ".length).toInt)
      } else if (line.startsWith("Acceptance: ")) {
        val accStates = line.drop("Acceptance: ".length).split(' ')
        assert(accStates.length == 2) // TODO: parse acceptance sets vs single state + capture acceptance conditions
        hoa.copy(acceptingState = accStates(0).toInt)
      } else if (line.startsWith("AP: ")) {
        val APline = line.drop("AP: ".length).split(' ').drop(1) // first element is the number of APs
        val aps = Map(APline.zipWithIndex.map { case (ap, apIndex) => (apIndex, ap.drop(1).dropRight(1)) }.toSeq: _*)
        hoa.copy(aps = aps)
      } else {
        hoa
      }
    }
  }
}

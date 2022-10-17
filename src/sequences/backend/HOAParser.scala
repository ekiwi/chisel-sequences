package sequences.backend

import fastparse._
import fastparse.NoWhitespace._

/** A parser for a subset of the Hanoi Omega Automata (HOA) Format
  */
object HOAParser {
  type AP = Int

  sealed trait Condition

  case class Predicate(ap: AP) extends Condition

  case class Not(p: Predicate) extends Condition

  case class And(conds: Condition*) extends Condition

  case class Or(conds: Condition*) extends Condition

  case object True extends Condition

  case class State(n: Int, accepting: Boolean, transitions: Map[Condition, Int])

  case class HOA(
    name:           String,
    nStates:        Int,
    initialState:   Int,
    aps:            Map[AP, String],
    acceptingState: Int,
    states:         Set[State])

  def groupStates(s: Seq[String]): Seq[Seq[String]] = {
    val stateLines = s.zipWithIndex.filter { case (line, idx) => line.startsWith("State: ") }.map { case (line, idx) =>
      idx
    }
    (stateLines :+ s.length)
      .sliding(2)
      .map { idxRange => s.slice(idxRange(0), idxRange(1)) }
      .toSeq
  }

  def trueLit[_: P]: P[Condition] = P(CharIn("t").map(_ => True))
  def predId[_:  P]: P[Predicate] = P(CharIn("0-9").rep(1).!.map(id => Predicate(id.toInt)))
  def not[_:     P]: P[Condition] = P("!" ~ predId).map(p => Not(p))
  def atom[_:    P]: P[Condition] = P(not | predId)
  def and[_:     P]: P[Condition] = P(atom ~ ("&" ~ atom).rep(1)).map { case (p1: Condition, andSeq: Seq[Condition]) =>
    And(Seq(p1) ++ andSeq: _*)
  }
  def atomOrAnd[_: P]: P[Condition] = P(and | atom)
  def or[_:        P]: P[Condition] = P(atomOrAnd ~ (" | " ~ atomOrAnd).rep(1)).map {
    case (p1: Condition, orSeq: Seq[Condition]) => Or(Seq(p1) ++ orSeq: _*)
  }
  def conditionParser[_: P]: P[Condition] = P((trueLit | or | and | atom) ~ End)

  def parseTransition(s: String): (Condition, Int) = {
    // s is of the form: "[0&1] 1"
    val split = s.split(' ')
    assert(split.length == 2)

    val condition = split(0).drop(1).dropRight(1) // first element is the condition, remove the brackets
    val nextState = split(1).toInt // second element is the target state

    val parsedCond = parse(condition, conditionParser(_))
    (parsedCond.get.value, nextState)
  }

  def parseHOA(s: String): HOA = {
    val lines = s.split('\n').toSeq
    val headerLimit = lines.indexWhere(s => s == "--BODY--")
    val endLimit = lines.indexWhere(s => s == "--END--")
    val header = lines.slice(1, headerLimit)

    // Parse the header
    val hoaHeader = header.foldLeft(HOA("", 0, 0, Map.empty, 0, Set.empty)) { (hoa, line) =>
      if (line.startsWith("name: ")) {
        hoa.copy(name = line.drop("name: ".length + 1).dropRight(1))
      } else if (line.startsWith("States: ")) {
        hoa.copy(nStates = line.drop("States: ".length).toInt)
      } else if (line.startsWith("Start: ")) {
        hoa.copy(initialState = line.drop("Start: ".length).toInt)
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

    // Group every state and all its transitions together so they can be parsed as one block
    val body = groupStates(lines.slice(headerLimit + 1, endLimit))

    // Parse the body
    body.foldLeft(hoaHeader) { (hoa: HOA, lines: Seq[String]) =>
      assert(lines.head.startsWith("State: "))
      val stateLine = lines.head.drop("State: ".length).split(' ')
      assert(stateLine.length == 1 || stateLine.length == 2)
      val stateIdx = stateLine(0).toInt
      val isAccepting = stateLine.length > 1

      val transitionLines = lines.drop(1)
      val transitions = transitionLines.map(parseTransition)

      hoa.copy(states = hoa.states + State(stateIdx, isAccepting, Map(transitions: _*)))
    }
  }
}

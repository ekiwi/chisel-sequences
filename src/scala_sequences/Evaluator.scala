// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Vighnesh Iyer <vighnesh.iyer@berkeley.edu>

package scala_sequences

object Evaluator {
  type Time = Int
  case class CoverResult(completed: Seq[(Time, Time)], pending: Seq[Time])
  case class CoverState[T](seqsInFlight: Set[(Time, SequenceStatus[T])], time: Time, completed: Seq[(Time, Time)])

  sealed trait SequenceStatus[+T]
  case object TerminatedFailed extends SequenceStatus[Nothing]
  case object TerminatedDone extends SequenceStatus[Nothing]
  case class Running[T](next: ScalaSeq[T]) extends SequenceStatus[T]

  // Given this value, and a running sequence, does it terminate as successful or failed on this cycle,
  //   or is it still going to be running (with something to check later in time)?
  private def step[T](value: T, status: SequenceStatus[T]): SequenceStatus[T] = {
    status match {
      case Running(next) =>
        next match {
          case ap: AtmProp[T] =>
            if (ap.pred(value)) {
              TerminatedDone
            } else {
              TerminatedFailed
            }
          case fuse: Fuse[T] =>
            val seq1Step = step(value, Running(fuse.seq1))
            seq1Step match {
              case TerminatedFailed => TerminatedFailed
              case TerminatedDone =>
                val seq2Step = step(value, Running(fuse.seq2))
                seq2Step match {
                  case TerminatedFailed => TerminatedFailed
                  case TerminatedDone   => TerminatedDone
                  case r @ Running(_)   => r
                }
              case r: Running[T] => Running(Fuse(r.next, fuse.seq2))
            }
          case Delay(n) =>
            if (n <= 0) {
              TerminatedDone
            } else {
              Running(Delay(n - 1))
            }
          case Repeated(seq, repeats) =>
            if (repeats == 0) {
              TerminatedDone
            } else if (repeats == 1) {
              step(value, Running(seq))
            } else {
              val seqStep = step(value, Running(seq))
              seqStep match {
                case TerminatedFailed => TerminatedFailed
                case TerminatedDone   => step(value, Running(Repeated(seq, repeats - 1)))
                case Running(next)    => Running(Fuse(next, Repeated(seq, repeats - 1)))
              }
            }
          case Or(seq1, seq2)      => ???
          case Implies(seq1, seq2) => ???
        }
      case TerminatedDone   => TerminatedDone
      case TerminatedFailed => TerminatedFailed
    }
  }

  // Given this value, does this sequence start?
  private def start[T](value: T, seq: ScalaSeq[T]): Boolean = {
    seq match {
      case ap: AtmProp[T] => ap.pred(value)
      case Fuse(seq1, seq2)       => start(value, seq1)
      case Delay(n)               => n > 0
      case Repeated(seq, repeats) => start(value, seq)
      case Or(seq1, seq2)         => start(value, seq1) || start(value, seq2)
      case Implies(seq1, seq2)    => start(value, seq1)
    }
  }

  def cover[T](sequence: ScalaSeq[T], trace: Seq[T]): CoverResult = {
    val finalState = trace.foldLeft(CoverState[T](Set.empty, 0, Seq.empty)) { (state, value) =>
      // If a new sequence instance launches now, then add it to the sequences in flight
      val seqsInFlight: Set[(Time, SequenceStatus[T])] = if (start(value, sequence)) {
        state.seqsInFlight.incl((state.time, Running(sequence))) // record when this sequence started
      } else {
        state.seqsInFlight
      }

      // Step each sequence instance given this value
      val seqsAfterStep = seqsInFlight.map { case (startTime, seq) => (startTime, step(value, seq)) }

      // If any sequence instances have failed, remove then from the in-flight set
      val seqsNonFailures = seqsAfterStep.filter { case (startTime, seqStatus) =>
        seqStatus match {
          case TerminatedFailed =>
            false // TODO: we probably want to track failures in "assert" mode, but this is "cover" mode
          case TerminatedDone => true
          case Running(_)     => true
        }
      }

      // If any sequence instances have completed, remove them from the in-flight set, and turn them into (startTime, endTime) pairs
      val seqsCompleted = seqsNonFailures.filter { case (startTime, seqStatus) =>
        seqStatus match {
          case TerminatedFailed => ??? // should never happen
          case TerminatedDone   => true
          case Running(next)    => false
        }
      }.map { case (startTime, seqStatus) => (startTime, state.time) }

      state.copy(
        // only keep the sequences still in the 'Running' state
        seqsInFlight = seqsAfterStep.filter { case (startTime, seqStatus) =>
          seqStatus match { case r: Running[T] => true; case _ => false }
        },
        time = state.time + 1,
        completed = state.completed ++ seqsCompleted
      )
    }
    CoverResult(finalState.completed, finalState.seqsInFlight.map { case (startTime, _) => startTime }.toSeq)
  }
}

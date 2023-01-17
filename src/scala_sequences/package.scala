// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Vighnesh Iyer <vighnesh.iyer@berkeley.edu>

package object scala_sequences {
  sealed trait ScalaSeq[+T]

  case class AtmProp[T](pred: T => Boolean) extends ScalaSeq[T]
  case class Fuse[T](seq1: ScalaSeq[T], seq2: ScalaSeq[T]) extends ScalaSeq[T]
  case class Delay(n: Int) extends ScalaSeq[Nothing] {
    require(n >= 0)
  }
  case class Repeated[T](seq: ScalaSeq[T], repeats: Int) extends ScalaSeq[T]
  // case class UnboundedRepeat[T](seq: ScalaSeq[T], min_repeat: Int) extends ScalaSeq[T]
  case class Or[T](seq1: ScalaSeq[T], seq2: ScalaSeq[T]) extends ScalaSeq[T]
  case class Implies[T](seq1: ScalaSeq[T], seq2: ScalaSeq[T]) extends ScalaSeq[T]

  def Concat[T](seq1: ScalaSeq[T], seq2: ScalaSeq[T]): ScalaSeq[T] = {
    Fuse(Fuse(seq1, Delay(1)), seq2)
  }

  sealed trait Property[+T] {}

  case class PropSeq[T](s: ScalaSeq[T]) extends Property[T]
}

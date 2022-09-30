//

package sequences

import chisel3._

sealed trait Sequence {}

case class SeqExpr(predicate: Bool) extends Sequence
case class SeqOr(s1: Sequence, s2: Sequence) extends Sequence
case class SeqConcat(s1: Sequence, s2: Sequence) extends Sequence
case class SeqIntersect(s1: Sequence, s2: Sequence) extends Sequence
case class SeqNot(s1: Sequence) extends Sequence
case class SeqImplies(s1: Sequence, p1: Property) extends Sequence
case class SeqImpliesNext(s1: Sequence, p1: Property) extends Sequence
case class SeqFuse(s1: Sequence, s2: Sequence) extends Sequence

sealed trait Property {}

case class PropSeq(s: Sequence) extends Property

object serialize {
  def apply(p: Property): String = {
    p match {
      case PropSeq(s) => apply(s)
    }
  }

  def apply(s: Sequence): String = {
    s match {
      case SeqExpr(predicate)     => predicate.toString
      case SeqOr(s1, s2)          => apply(s1) + " or " + apply(s2)
      case SeqConcat(s1, s2)      => apply(s1) + " ##1 " + apply(s2)
      case SeqIntersect(s1, s2)   => apply(s1) + " and " + apply(s2)
      case SeqNot(s1)             => "not" + apply(s1)
      case SeqImplies(s1, p1)     => apply(s1) + "implies" + apply(p1)
      case SeqImpliesNext(s1, p1) => apply(s1) + "implies next" + apply(p1)
      case SeqFuse(s1, s2)        => apply(s1) + " ##0 " + apply(s2)
    }
  }
}

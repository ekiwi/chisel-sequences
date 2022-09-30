package sequences

import chisel3._

sealed trait Sequence {

}


case class SeqExpr(predicate: Bool) extends Sequence
case class SeqOr(s1: Sequence, s2: Sequence) extends Sequence
case class SeqConcat(s1: Sequence, s2: Sequence) extends Sequence
case class SeqIntersect(s1: Sequence, s2: Sequence) extends Sequence
case class SeqNot(s1: Sequence) extends Sequence
case class SeqImplies(s1: Sequence, p1: Property) extends Sequence
case class SeqImpliesNext(s1: Sequence, p1: Property) extends Sequence
case class SeqFuse(s1: Sequence, s2: Sequence) extends Sequence

sealed trait Property {

}

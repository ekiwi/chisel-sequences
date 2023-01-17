// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences

import chisel3._

case class TestProperty(name: String, prop: (Bool, Bool, Bool, Bool) => Property, testInputs: Seq[TestInput])
trait TestInput {
  def a: String; def b: String; def c: String; def d: String; def failAt: Int
  require(failAt == -1 || failAt >= 0)
  def expectSuccess: Boolean = failAt == -1
}
object TestInput {
  def getPaddedInputs(input: TestInput): Seq[Seq[Option[Boolean]]] = {
    val inputStrings = Seq(input.a, input.b, input.c, input.d)
    val length = inputStrings.map(_.length).max
    val paddedInputs: Seq[Array[Char]] = inputStrings.map(str => str.toCharArray.padTo(length, 'X'))
    paddedInputs.map { seq =>
      seq.toSeq.map {
        case 'X' => None
        case '1' => Some(true)
        case '0' => Some(false)
        case other =>
          throw new RuntimeException(s"Unknown input character: $other in ${seq.mkString("Array(", ", ", ")")}")
      }
    }
  }
}
case class Pass(a: String, b: String = "", c: String = "", d: String = "") extends TestInput {
  override def failAt: Int = -1
}
case class Fail(at: Int, a: String, b: String = "", c: String = "", d: String = "") extends TestInput {
  override def failAt: Int = at
}

object TestProperty {
  type Prop = (Bool, Bool, Bool, Bool) => Property
  def apply(name: String, prop: Bool => Property, testInputs: Seq[TestInput]): TestProperty =
    new TestProperty(name, (a, _, _, _) => prop(a), testInputs)
  def apply(name: String, prop: (Bool, Bool) => Property, testInputs: Seq[TestInput]): TestProperty =
    new TestProperty(name, (a, b, _, _) => prop(a, b), testInputs)
  def apply(name: String, prop: (Bool, Bool, Bool) => Property, testInputs: Seq[TestInput]): TestProperty =
    new TestProperty(name, (a, b, c, _) => prop(a, b, c), testInputs)
  def apply(name: String, prop: (Bool, Bool, Bool, Bool) => Property, testInputs: Seq[TestInput]): TestProperty =
    new TestProperty(name, (a, b, c, d) => prop(a, b, c, d), testInputs)
}

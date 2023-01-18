// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences

import sequences._

object TestProperties {
  val properties = Seq(
    TestProperty(
      "a ##1 a",
      a => a.###(1)(a),
      Seq(
        Pass(a = "1"),
        Pass(a = "11"),
        Fail(at = 1, a = "10"),
        Fail(at = 0, a = "0X")
      )
    ),
    TestProperty(
      "a ##1 !a",
      a => a.###(1)(!a),
      Seq(
        Pass(a = "1"),
        // there is no two cycle trace that will make this pass
        Fail(at = 1, a = "10"),
        Fail(at = 1, a = "11"),
        Fail(at = 0, a = "0")
      )
    ),
    TestProperty(
      "a ##1 b",
      (a, b) => a.###(1)(b),
      Seq(
        Pass(a = "11", b = "X1"),
        Fail(at = 1, a = "10", b = "XX"),
        Fail(at = 1, a = "1X", b = "X0")
      )
    ),
    TestProperty(
      "a |=> b",
      (a, b) => a |=> b,
      Seq(
        Pass(a = "1x", b = "x1"),
        Pass(a = "0x", b = "x0"),
        Pass(a = "010x", b = "xx1x"),
        Fail(at = 1, a = "1x", b = "X0")
      )
    ).markSkip
  )
}

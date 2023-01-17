// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License

package sequences

import chisel3.assert
import backend.{Backend, SequenceFsms}

object assertAlways {
  def apply(prop: Property, desc: String = "", backend: Backend = SequenceFsms): Unit = {
    val fail = toAutomaton(prop, backend)
    assert(!fail, desc)
  }
}

package sequences

import chisel3.assert
import backend.{Backend, SequenceFsms}

object assertAlways {
  def apply(prop: Property, desc: String = "", backend: Backend = SequenceFsms): Unit = {
    val fail = toAutomaton(prop, backend)
    assert(!fail, desc)
  }

}

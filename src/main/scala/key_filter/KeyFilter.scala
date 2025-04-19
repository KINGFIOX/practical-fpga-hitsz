// See README.md for license details.

package key_filter

import chisel3._
import chisel3.util._

class KeyFilter(delay: Int) extends Module {
  val io = IO(new Bundle {
    val key_in = Input(Bool())
    val key_out = Output(Bool())
  })

  val valid = io.key_in

  val s_idle :: s_wait :: s_done :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val cnt = Counter(delay)

  // output, counter
  val wrap = WireInit(false.B)
  switch(state) {
    is(s_idle) {
      cnt.reset() // reset the counter when idle
    }
    is(s_wait) {
      wrap := cnt.inc()
    }
    is(s_done) {
      // do nothing
    }
  }

  // output, io.key_out
  io.key_out := state === s_idle && RegNext(state) === s_done

  // state transition table
  switch(state) {
    is(s_idle) {
      when(valid) {
        state := s_wait
      }
    }
    is(s_wait) {
      when(wrap) {
        state := s_done
      }
      when(!valid) {
        state := s_idle
      }
    }
    is(s_done) {
      when(!valid) {
        state := s_idle
      }
    }
  }

}

// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage

/** Generate Verilog sources and save it in file GCD.v
  */
object KeyFilter extends App {
  ChiselStage.emitSystemVerilogFile(
    new KeyFilter(10),
    Array("--target-dir", "generated"),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
}

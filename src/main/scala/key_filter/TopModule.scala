package key_filter

import chisel3._

class TopModule extends RawModule {
  override def desiredName = "key_filter"

  val io = FlatIO(new Bundle {
    val Clk = Input(Clock())
    val Reset_n = Input(Bool())
    val Key = Input(Bool())
    val Key_P_Flag = Output(Bool())
  })

  val reset = !io.Reset_n
  val clock = io.Clk

  // Initialize outputs with default values
  val impl = withClockAndReset(clock, reset) { Module(new KeyFilter(15000)) }
  impl.io.key_in := io.Key
  io.Key_P_Flag := impl.io.key_out

}

// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage

/** Generate Verilog sources and save it in file GCD.v
  */
object TopModule extends App {
  ChiselStage.emitSystemVerilogFile(
    new TopModule,
    Array("--target-dir", "generated"),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
}

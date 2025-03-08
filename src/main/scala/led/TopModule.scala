package led

import chisel3._

class TopModule extends RawModule {
  val io = IO(new Bundle {
    val pll_inst1_CLKOUT0 = Input(Clock())
    val pll_inst1_LOCKED = Input(Bool())
    val led_data = Output(UInt(8.W))
  })

  val led =
    withClockAndReset(io.pll_inst1_CLKOUT0, !io.pll_inst1_LOCKED) {
      Module(new TrafficLight(100_000_000))
    }

  io.led_data := false.B ## false.B ## !led.io.ns_red ## !led.io.ns_yellow ## !led.io.ns_green ## !led.io.we_red ## !led.io.we_yellow ## !led.io.we_green

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

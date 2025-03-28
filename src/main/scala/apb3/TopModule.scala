package apb3

import chisel3._

class TopModuleIO extends Bundle {
  val io_apbSlave_0_PADDR = Input(UInt(16.W))
  val io_apbSlave_0_PSEL = Input(Bool())
  val io_apbSlave_0_PENABLE = Input(Bool())
  val io_apbSlave_0_PWRITE = Input(Bool())
  val io_apbSlave_0_PWDATA = Input(UInt(32.W))
  val io_apbSlave_0_PREADY = Output(Bool())
  val io_apbSlave_0_PRDATA = Output(UInt(32.W))
  val io_apbSlave_0_PSLVERROR = Output(Bool())
}

class TopModule extends RawModule {
  override def desiredName = "apb_slave_memory"

  val io = FlatIO(new Bundle {
    val clk = Input(Clock())
    val reset_n = Input(Bool())
    val bits = new TopModuleIO
  })

  val rst = !io.reset_n
  val clk = io.clk

  // Initialize outputs with default values
  val impl = withClockAndReset(clk, rst) {
    Module(new ApbSlaveMemory)
  }

  impl.io.addr := io.bits.io_apbSlave_0_PADDR
  impl.io.sel := io.bits.io_apbSlave_0_PSEL
  impl.io.enable := io.bits.io_apbSlave_0_PENABLE
  impl.io.write := io.bits.io_apbSlave_0_PWRITE
  impl.io.wdata := io.bits.io_apbSlave_0_PWDATA
  io.bits.io_apbSlave_0_PREADY := impl.io.ready
  io.bits.io_apbSlave_0_PRDATA := impl.io.rdata
  io.bits.io_apbSlave_0_PSLVERROR := impl.io.error

  val bram_test = withClockAndReset(clk, rst) {
    Module(new Bram)
  }

  bram_test.io.reset := rst
  bram_test.io.clk := clk
  impl.io.bram <> bram_test.io.bits

}

// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage

/** Generate Verilog sources and save it in file GCD.v
  */
object apb_slave_memory extends App {
  ChiselStage.emitSystemVerilogFile(
    new TopModule,
    Array("--target-dir", "generated"),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
}

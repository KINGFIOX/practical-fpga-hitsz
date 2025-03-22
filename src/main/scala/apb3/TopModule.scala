package apb3

import chisel3._

class apb_slave_memory extends RawModule {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reset_n = Input(Bool())

    val io_apbSlave_0_PADDR = Input(UInt(16.W))
    val io_apbSlave_0_PSEL = Input(Bool())
    val io_apbSlave_0_PENABLE = Input(Bool())
    val io_apbSlave_0_PWRITE = Input(Bool())
    val io_apbSlave_0_PWDATA = Input(UInt(32.W))
    val io_apbSlave_0_PREADY = Output(Bool())
    val io_apbSlave_0_PRDATA = Output(UInt(32.W))
    val io_apbSlave_0_PSLVERROR = Output(Bool())
  })

  val rst = !io.reset_n
  val clk = io.clk

  // Initialize outputs with default values
  val impl = withClockAndReset(clk, rst) {
    Module(new ApbSlaveMemory)
  }

  impl.io.addr := io.io_apbSlave_0_PADDR
  impl.io.sel := io.io_apbSlave_0_PSEL
  impl.io.enable := io.io_apbSlave_0_PENABLE
  impl.io.write := io.io_apbSlave_0_PWRITE
  impl.io.wdata := io.io_apbSlave_0_PWDATA
  io.io_apbSlave_0_PREADY := impl.io.ready
  io.io_apbSlave_0_PRDATA := impl.io.rdata
  io.io_apbSlave_0_PSLVERROR := impl.io.error

  val bram = withClockAndReset(clk, rst) {
    Module(new Bram)
  }

  bram.io.reset := rst
  bram.io.clk := clk
  impl.io.bram <> bram.io.bits

}

// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage

/** Generate Verilog sources and save it in file GCD.v
  */
object apb_slave_memory extends App {
  ChiselStage.emitSystemVerilogFile(
    new apb_slave_memory,
    Array("--target-dir", "generated"),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
}

// See README.md for license details.

package apb3

import chisel3._
import chisel3.util._
import os.write
import apb3.ApbSlaveMemoryEnum.IDLE

object ApbSlaveMemoryEnum extends ChiselEnum {
  val IDLE, SETUP, ACCESS = Value
}

class ApbSlaveMemory extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(16.W))
    val sel = Input(Bool())
    val enable = Input(Bool())
    val write = Input(Bool())
    val wdata = Input(UInt(32.W))
    val ready = Output(Bool())
    val rdata = Output(UInt(32.W))
    val error = Output(Bool())

  })

  // hold the value of addr, write, wdata, and rdata

  val state = RegInit(ApbSlaveMemoryEnum.IDLE)

  // the next logic
  val state_next = MuxCase(
    ApbSlaveMemoryEnum.IDLE,
    Seq(
      (state === ApbSlaveMemoryEnum.IDLE) -> Mux(
        io.sel && !io.enable,
        ApbSlaveMemoryEnum.SETUP,
        ApbSlaveMemoryEnum.IDLE
      ),
      (state === ApbSlaveMemoryEnum.SETUP) -> ApbSlaveMemoryEnum.ACCESS,
      (state === ApbSlaveMemoryEnum.ACCESS) -> Mux(
        io.sel,
        Mux(io.enable, ApbSlaveMemoryEnum.ACCESS, ApbSlaveMemoryEnum.SETUP),
        ApbSlaveMemoryEnum.IDLE
      )
    )
  )
  state := state_next

  // ready logic
  io.ready := Mux(
    !io.write,
    true.B, // read always ready
    MuxCase(
      true.B,
      Seq(
        (state === ApbSlaveMemoryEnum.ACCESS && state_next === ApbSlaveMemoryEnum.IDLE) -> true.B,
        (state === ApbSlaveMemoryEnum.ACCESS && state_next === ApbSlaveMemoryEnum.ACCESS) -> false.B,
        (state === ApbSlaveMemoryEnum.ACCESS && state_next === ApbSlaveMemoryEnum.SETUP) -> true.B
      )
    )
  )

  // error logic (ignore)
  io.error := false.B

  val addr = io.addr(6, 0)

  // resource
  val reg = Reg(Vec(128, UInt(32.W)))
  val addr_r = RegInit(0.U(7.W))
  io.rdata := reg(addr_r)

  // lock
  when(state === ApbSlaveMemoryEnum.SETUP) {
    addr_r := addr
    when(io.sel) {
      when(io.write) {
        reg(addr) := io.wdata
      }
    }
  }

  // memory
}

// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage

/** Generate Verilog sources and save it in file GCD.v
  */
object ApbSlaveMemory extends App {
  ChiselStage.emitSystemVerilogFile(
    new ApbSlaveMemory,
    Array("--target-dir", "generated"),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
}

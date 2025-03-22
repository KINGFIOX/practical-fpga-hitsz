// See README.md for license details.

package apb3

import chisel3._
import chisel3.util._

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
    val bram = Flipped(new BramBundle)
  })

  io.ready := true.B
  io.rdata := false.B
  io.error := false.B

  // read through bram
  io.bram.re := io.enable
  io.bram.raddr := io.addr(5, 0)
  io.rdata := io.bram.rdata_b

  // write with the state machine
  io.bram.we := false.B
  io.bram.waddr := 0.U
  io.bram.wdata_a := 0.U

  val state = RegInit(ApbSlaveMemoryEnum.IDLE)

  switch(state) {
    is(ApbSlaveMemoryEnum.IDLE) {
      when(io.sel) { // 当需要传输时, 总线进入 setup 状态, 此时 sel 置位
        state := ApbSlaveMemoryEnum.SETUP
      }
    }
    is(ApbSlaveMemoryEnum.SETUP) { // 总线只在一个时钟周期内保持在 setup 状态, 并且总是在时钟的下一个上升沿移动到 Access 状态
      state := ApbSlaveMemoryEnum.ACCESS
      // addr, write, sel, wdata 在 setup -> access 转换期间必须保持稳定
      io.bram.waddr := io.addr(5, 0)
      io.bram.wdata_a := io.wdata
      io.bram.raddr := io.addr(5, 0)
      io.bram.we := io.write
      // jump to access state, ready is low
    }
    is(ApbSlaveMemoryEnum.ACCESS) {
      io.ready := false.B // 从设备保持为低电平, 则外围总线保持 access 状态
      when(!io.enable) { // assert enable 在 access 状态下有效
        state := ApbSlaveMemoryEnum.IDLE
        io.error := true.B
      }
      when(!io.sel) {
        state := ApbSlaveMemoryEnum.IDLE
      }.otherwise {
        state := ApbSlaveMemoryEnum.SETUP
      }
    }
  }

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

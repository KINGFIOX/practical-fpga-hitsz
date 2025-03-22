package apb3

import chisel3._

class BramBundle extends Bundle {
  val re = Input(Bool()) // read enable
  val raddr = Input(UInt(7.W))
  val rdata_b = Output(UInt(32.W))
  val we = Input(Bool()) // write enable
  val waddr = Input(UInt(7.W))
  val wdata_a = Input(UInt(32.W))
}

class Bram extends BlackBox {
  val io = IO(new Bundle {
    val bits = new BramBundle
    val clk = Input(Clock())
    val reset = Input(Bool())
  })
}

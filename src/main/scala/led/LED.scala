// See README.md for license details.

package led

import chisel3._
import chisel3.util._

// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage

// sw == 1 && k == 1 -> 两边向中间
// sw == 0 && k == 1 -> 中间向两边
// sw == 1 && k == 0 -> 循环左移
// sw == 0 && k == 0 -> 循环右移
class TopModule extends Module {
  val io = IO(new Bundle {
    val sw = Input(Bool())
    val k = Input(Bool())
    val led = Output(UInt(8.W))
  })

  def shiftLeftRotate(value: UInt, width: Int): UInt = {
    val head = value(width - 1)
    val tail = value(width - 2, 0)
    tail ## head
  }

  def shiftRightRotate(value: UInt, width: Int): UInt = {
    val init = value(width - 1, 1)
    val last = value(0)
    last ## init
  }

  val led = RegInit(0.U(8.W))
  val cnt = Counter(50_000_000) // 100M * 0.5 = 50M

  val last = RegInit(0.U(2.W))
  when(cnt.inc()) {
    last := io.sw ## io.k
  }

  val fire = last =/= (io.sw ## io.k)

  when(cnt.inc()) {
    when(led === 0.U || fire) {
      when(io.k === 1.U && io.sw === 1.U) {
        led := "b1000_0001".U
      }
      when(io.k === 1.U && io.sw === 0.U) {
        led := "b0001_1000".U
      }
      when(io.k === 0.U && io.sw === 1.U) {
        led := "b0000_0001".U
      }
      when(io.k === 0.U && io.sw === 0.U) {
        led := "b1000_0000".U
      }
    }.otherwise {
      val high = led(7, 4)
      val low = led(3, 0)
      when(io.k === 1.U && io.sw === 1.U) {
        led := shiftRightRotate(high, 4) ## shiftLeftRotate(low, 4)
      }
      when(io.k === 1.U && io.sw === 0.U) {
        led := shiftLeftRotate(high, 4) ## shiftRightRotate(low, 4)
      }
      when(io.k === 0.U && io.sw === 1.U) {
        led := shiftLeftRotate(led, 8)
      }
      when(io.k === 0.U && io.sw === 0.U) {
        led := shiftRightRotate(led, 8)
      }
    }
  }

  io.led := led
}

/** Generate Verilog sources and save it in file GCD.v
  */
object TopModule extends App {
  ChiselStage.emitSystemVerilogFile(
    new TopModule,
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
}

// See README.md for license details.

package led

import chisel3._
import chisel3.util._

object TrafficLightEnum extends ChiselEnum {
  val ns_red_we_green, ns_red_we_yellow, ns_green_we_red, ns_yellow_we_red =
    Value
}


class TrafficLight extends Module {
  val io = IO(new Bundle {
    val ns_red = Output(Bool())
    val ns_yellow = Output(Bool())
    val ns_green = Output(Bool())
    val we_red = Output(Bool())
    val we_yellow = Output(Bool())
    val we_green = Output(Bool())
  })

  val cnt = Counter(100_000_000) // 100M * 1s

  // 用于指示持续的时间
  val last = Counter(3)

  val state = Reg(TrafficLightEnum())

  // 状态切换
  when(cnt.inc()) {
    switch(state) {
      is(TrafficLightEnum.ns_red_we_green) {
        when(last.inc()) {
          state := TrafficLightEnum.ns_red_we_yellow
        }
      }
      is(TrafficLightEnum.ns_red_we_yellow) {
        state := TrafficLightEnum.ns_green_we_red
      }
      is(TrafficLightEnum.ns_green_we_red) {
        when(last.inc()) {
          state := TrafficLightEnum.ns_yellow_we_red
        }
      }
      is(TrafficLightEnum.ns_yellow_we_red) {
        state := TrafficLightEnum.ns_red_we_green
      }
    }
  }

  // 状态输出
  io.ns_red := state === TrafficLightEnum.ns_red_we_green || state === TrafficLightEnum.ns_red_we_yellow
  io.ns_yellow := state === TrafficLightEnum.ns_yellow_we_red
  io.ns_green := state === TrafficLightEnum.ns_green_we_red
  io.we_red := state === TrafficLightEnum.ns_green_we_red || state === TrafficLightEnum.ns_yellow_we_red
  io.we_yellow := state === TrafficLightEnum.ns_red_we_yellow
  io.we_green := state === TrafficLightEnum.ns_red_we_green

}

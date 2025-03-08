#include "VTrafficLight.h"
#include "verilatedos.h"
#include <cstdlib>
#include <stdlib.h>
#include <verilated.h>
#include <verilated_vcd_c.h>

vluint64_t sim_time = 0;

int main(int argc, char **argv, char **env) {
  VTrafficLight *dut = new VTrafficLight;

  Verilated::traceEverOn(true);
  VerilatedVcdC *m_trace = new VerilatedVcdC;
  dut->trace(m_trace, 5);
  m_trace->open("waveform.vcd");

  for (int i = 0; i < 100; i++) {
    dut->clock ^= 1;
    dut->eval();
    m_trace->dump(sim_time);
    sim_time++;
  }
  m_trace->close();
  delete dut;
  exit(EXIT_SUCCESS);
}

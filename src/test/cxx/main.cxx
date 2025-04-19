#include "VKeyFilter.h"
#include "verilatedos.h"
#include <cstdlib>
#include <verilated.h>
#include <verilated_vcd_c.h>

#define CYCLES 200

vluint64_t sim_time = 0;
vluint64_t clk_time = 0;

// ✅
void drive(VKeyFilter *dut) {
  if (clk_time == 0) {
    dut->reset = 1;
  }
  if (clk_time == 1) {
    dut->reset = 0;
  }
  if (clk_time == 2) {
    dut->io_key_in = 1;
  }
  if (clk_time == 100) {
    dut->io_key_in = 0;
  }
}

void combinational(VKeyFilter *dut) {
  static bool once = false;
  static vluint32_t data[1 << 7]; // (1 << 7) * 4
  if (!once) {
    for (int i = 0; i < 1 << 7; i++) {
      data[i] = random();
    }
    once = true;
  }
}

void simulate(VKeyFilter *dut, VerilatedVcdC *m_trace,
              void (*drive)(VKeyFilter *)) {
  for (int i = 0; i < CYCLES; i++) {
    for (int j = 0; j < 2; j++) {
      dut->clock ^= 1;
      if (j == 0) {         // posedge
        drive(dut);         // drive signals
        combinational(dut); // combinational logic
      }
      dut->eval();
      m_trace->dump(sim_time);
      sim_time++;
    }
    clk_time++;
  }
}

int main(int argc, char **argv, char **env) {
  srand(time(NULL));
  VKeyFilter *dut = new VKeyFilter;

  Verilated::traceEverOn(true);
  VerilatedVcdC *m_trace = new VerilatedVcdC;
  dut->trace(m_trace, -1);
  m_trace->open("waveform.vcd");
  simulate(dut, m_trace, drive);
  m_trace->close();
  delete dut;
  exit(EXIT_SUCCESS);
}

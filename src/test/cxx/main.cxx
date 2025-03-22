#include "VApbSlaveMemory.h"
#include "verilatedos.h"
#include <cstdlib>
#include <verilated.h>
#include <verilated_vcd_c.h>

vluint64_t sim_time = 0;
vluint64_t clk_time = 0;

// ✅
void write_without_wait(VApbSlaveMemory *dut) {
  if (clk_time == 1) {
    dut->io_sel = 1;
    dut->io_write = 1;
    dut->io_addr = random();
    dut->io_wdata = random();
  }
  if (clk_time == 2) {
    dut->io_enable = 1;
  }
  if (clk_time == 3) {
    dut->io_sel = 0;
    dut->io_enable = 0;
    dut->io_wdata = 0;
  }
}

void write_with_wait(VApbSlaveMemory *dut) {
  if (clk_time == 1) {
    dut->io_addr = random();
    dut->io_write = 1;
    dut->io_sel = 1;
    dut->io_wdata = random();
  }
  if (clk_time == 2) {
    dut->io_enable = 1;
  }
  if (clk_time == 5) {
    dut->io_sel = 0;
    dut->io_enable = 0;
    dut->io_wdata = 0;
  }
}

void simulate(VApbSlaveMemory *dut, VerilatedVcdC *m_trace,
              void (*func)(VApbSlaveMemory *)) {
  for (int i = 0; i < 100; i++) {
    for (int j = 0; j < 2; j++) {
      dut->clock ^= 1;
      if (j == 0) {
        func(dut);
      }
      dut->eval();
      m_trace->dump(sim_time);
      sim_time++;
    }
    clk_time++;
  }
}

int main(int argc, char **argv, char **env) {
  VApbSlaveMemory *dut = new VApbSlaveMemory;

  Verilated::traceEverOn(true);
  VerilatedVcdC *m_trace = new VerilatedVcdC;
  dut->trace(m_trace, -1);
  m_trace->open("waveform.vcd");
  simulate(dut, m_trace, write_with_wait);
  m_trace->close();
  delete dut;
  exit(EXIT_SUCCESS);
}

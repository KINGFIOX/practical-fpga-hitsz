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

// ✅
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

// 不确定
void read_without_wait(VApbSlaveMemory *dut) {
  if (clk_time == 0) {
    dut->io_write = 1;
  }
  if (clk_time == 1) {
    dut->io_addr = random();
    dut->io_write = 0;
    dut->io_sel = 1;
  }
  if (clk_time == 2) {
    dut->io_enable = 1;
  }
  if (clk_time == 3) {
    dut->io_sel = 0;
    dut->io_enable = 0;
  }
}

void read_with_wait1(VApbSlaveMemory *dut) {
  if (clk_time == 0) {
    dut->io_write = 1;
  }
  if (clk_time == 1) {
    dut->io_addr = random();
    dut->io_write = 0;
    dut->io_sel = 1;
  }
  if (clk_time == 2) {
    dut->io_enable = 1;
  }
  if (clk_time == 5) {
    dut->io_sel = 0;
    dut->io_enable = 0;
  }
}

void read_with_wait2(VApbSlaveMemory *dut) {
  if (clk_time == 0) {
    dut->io_write = 1;
  }
  if (clk_time == 1) {
    dut->io_addr = random();
    dut->io_write = 0;
    dut->io_sel = 1;
  }
  if (clk_time == 2) {
    dut->io_enable = 1;
  }
  if (clk_time == 5) {
    dut->io_sel = 0;
    dut->io_enable = 0;
  }
}

#define WIDTH ((1 << 7) - 1)

#define MASK(x) ((x) & WIDTH)

void combinational(VApbSlaveMemory *dut) {
  static bool once = false;
  static vluint32_t data[1 << 7]; // (1 << 7) * 4
  if (!once) {
    for (int i = 0; i < 1 << 7; i++) {
      data[i] = random();
    }
    once = true;
  }
  if (dut->io_bram_re) {
    dut->io_bram_rdata_b = data[MASK(dut->io_bram_raddr)];
    printf("%2d: read %d from %d\n", clk_time, dut->io_rdata,
           dut->io_bram_raddr);
  }
  if (dut->io_bram_we) {
    data[MASK(dut->io_bram_waddr)] = dut->io_bram_wdata_a;
  }
}

void simulate(VApbSlaveMemory *dut, VerilatedVcdC *m_trace,
              void (*drive)(VApbSlaveMemory *)) {
  for (int i = 0; i < 100; i++) {
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
  VApbSlaveMemory *dut = new VApbSlaveMemory;

  Verilated::traceEverOn(true);
  VerilatedVcdC *m_trace = new VerilatedVcdC;
  dut->trace(m_trace, -1);
  m_trace->open("waveform.vcd");
  simulate(dut, m_trace, read_with_wait1);
  m_trace->close();
  delete dut;
  exit(EXIT_SUCCESS);
}

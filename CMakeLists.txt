cmake_minimum_required(VERSION 3.12)
cmake_policy(SET CMP0144 NEW)

project(sim)

set(VERILATOR_ROOT "/usr/share/verilator" CACHE STRING "Verilator root directory")

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_C_COMPILER "/usr/bin/gcc")
set(CMAKE_CXX_COMPILER "/usr/bin/g++")
set(CMAKE_CXX_STANDARD 20)

find_package(verilator HINTS ${VERILATOR_ROOT})
if (NOT verilator_FOUND)
  message(FATAL_ERROR "Verilator was not found. Either install it, or set the VERILATOR_ROOT environment variable")
endif()

add_executable(sim "src/test/cxx/main.cxx")

# Add the Verilated circuit to the target
verilate(sim COVERAGE TRACE OPT_FAST
  VERILATOR_ARGS -Wall -Wno-UNUSEDSIGNAL
  SOURCES "generated/ApbSlaveMemory.sv")

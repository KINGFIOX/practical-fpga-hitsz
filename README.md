# README

## sbt run

`sbt run` 将 .scala 编译成 .sv

## cmake

`cmake -S . -B build` 用 cmake 生成 Makefile, 并将 .sv 编译成等价的 cxx class

`cmake --build build` 编译 main.cxx 并生成可执行文件

## run executable

`./build/Traffic` 进行仿真, 生成 wave.vcd

## view wave

`gtkwave ./waveform.vcd` 查看波形

chisel: src/main/scala/key_filter/KeyFilter.scala
	sbt 'runMain key_filter.KeyFilter'

build: generated/KeyFilter.sv src/test/cxx/main.cxx
	cmake -S . -B build
	cmake --build build

sim: build
	./build/sim

top:
	sbt 'runMain key_filter.TopModule'

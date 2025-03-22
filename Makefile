sbt: src/main/scala/apb3/ApbSlaveMemory.scala
	sbt 'runMain apb3.ApbSlaveMemory'

build: generated/ApbSlaveMemory.sv src/test/cxx/main.cxx
	cmake -S . -B build
	cmake --build build

sim: build
	./build/sim

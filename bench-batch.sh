#!/bin/bash

for t in asm-less-opt asm llvm 
do
	stack clean
	if [ $t == "llvm" ]; then
		stack build --ghc-options="-O2 -fllvm"
	elif [ $t == "asm-less-opt" ]; then
		stack build
	else
		stack build --ghc-options="-O2"
	fi

	for i in 1 2 4 8 16 32 64 128 256
	do
		taskset -c 1 stack run 0000:01:00.0 0000:03:00.0 $i > ./measurements/$t/measurement-batch-$i-16.log &
		sleep 60
		kill $(pidof forwarder)
	done
done

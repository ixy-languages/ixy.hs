#!/bin/bash

declare -a CCARGS
CCARGS[llvm]="-O2 -fllvm"
CCARGS[asm]="-O2"

for t in asm llvm
do
	stack clean
	stack build --ghc-options=${CCARGS[$t]}

	for i in 1 2 4 8 16 32 64 128 256
	do
		taskset -c 1 stack run 0000:01:00.0 0000:03:00.0 $i > ./measurements/$t/measurement-batch-$i.log &
		sleep 60
		kill $(pidof forwarder)
	done
done

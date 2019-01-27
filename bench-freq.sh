#!/bin/bash

MIN=/sys/devices/system/cpu/intel_pstate/min_perf_pct
MAX=/sys/devices/system/cpu/intel_pstate/max_perf_pct
TURBO=/sys/devices/system/cpu/intel_pstate/no_turbo

set -x

for t in asm-less-opt llvm asm 
do
	echo "Now benchmarking: $t"
	stack clean
	if [ $t == "llvm" ]; then
		stack build --ghc-options="-O2 -fllvm"
	elif [ $t == "asm-less-opt" ]; then
		stack build
	else
		stack build --ghc-options="-O2"
	fi

	echo 1 > $TURBO
	echo 49 > $MIN
	echo 49 > $MAX

	for i in 49 55 64 70 79 82 91 100
	do
		echo $i > $MAX
		echo $i > $MIN
		sleep 1

		taskset -c 1 stack run 0000:01:00.0 0000:03:00.0 128 > measurements/$t/measurement-freq-$i.log &
		sleep 60
		kill $(pidof forwarder)
	done

	echo 0 > $TURBO
	taskset -c 1 stack run 0000:01:00.0 0000:03:00.0 128 > measurements/$t/measurement-freq-110.log &
	sleep 60
	kill $(pidof forwarder)
done

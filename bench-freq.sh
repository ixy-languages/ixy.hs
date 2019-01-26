#!/bin/bash

MIN=/sys/devices/system/cpu/intel_pstate/min_perf_pct
MAX=/sys/devices/system/cpu/intel_pstate/max_perf_pct
TURBO=/sys/devices/system/cpu/intel_pstate/no_turbo

declare -a CCARGS
CCARGS[llvm]="-O2 -fllvm"
CCARGS[asm]="-O2"

for t in asm llvm
do
	stack clean
	stack build --ghc-options=${CCARGS[$t]}
	echo 1 > $TURBO
	echo 49 > $MIN
	echo 49 > $MAX

	for i in 49 55 64 70 79 82 91 100
	do
		echo $i > $MAX
		echo $i > $MIN
		sleep 1

		taskset -c 1 stack run 0000:01:00.0 0000:03:00.0 128 > measurements/$t/measurment-freq-$i.log &
		sleep 60
		kill $(pidof forwarder)
	done

	echo 0 > $TURBO
	taskset -c 1 stack run 0000:01:00.0 0000:03:00.0 128 > measurements/$t/measurment-freq-110.log &
	sleep 60
	kill $(pidof forwarder)
done

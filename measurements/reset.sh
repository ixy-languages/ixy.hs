#!/bin/bash

echo 49 > /sys/devices/system/cpu/intel_pstate/min_perf_pct
echo 100 > /sys/devices/system/cpu/intel_pstate/max_perf_pct
echo 1 > /sys/devices/system/cpu/intel_pstate/no_turbo

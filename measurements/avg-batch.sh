#!/bin/bash

for t in asm llvm
do
	F=./$t/raw-batch.csv
	rm $F
	for i in 1 2 4 8 16 32 64 128 256
	do
		echo -n "$i, " >> $F
		./sanitize.sh ./$t/measurement-batch-$i.log | awk '{sum += $1; num++} END {print sum/num}' >> $F
	done
done

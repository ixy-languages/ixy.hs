#!/bin/bash

for t in asm llvm asm-less-opt
do
	F=./$t/raw-freq.csv
	rm $F
	for i in 49 55 64 70 79 82 91 100 110
	do
		echo -n "$i, " >> $F
		./sanitize.sh ./$t/measurement-freq-$i.log | awk '{sum += $1; num++} END {print sum/num}' >> $F
	done
done

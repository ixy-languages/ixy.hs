
#!/bin/bash

for t in asm llvm asm-less-opt
do
	F=./$t/raw-gc.csv
	rm $F
	for i in 1 2 4 8 16 32 64 128 256
	do
		echo -n "$i, " >> $F
		tail -n 6 ./$t/measurement-gc-$i.log | head -n 1 | awk '{print $3}' | sed -e "s/%//g" >> $F
	done
done

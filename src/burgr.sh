#!/bin/bash
# usage: "./burgr.sh file-name"
# do not append with .bun

echo "------compiling object file------"
gcc -c -Wall println.c
echo "------running burger.native------"
./burger.native < tests/$1.bun > $1.ll
echo "------llc command------"
llc $1.ll
echo "------linking files------"
gcc -o $1 $1.s println.o
echo "------running executable------"
./$1

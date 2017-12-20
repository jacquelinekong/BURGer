#!/bin/bash
# usage: "./burgr.sh file-name"
# do not append with .bun
# Author: Jordan Lee

echo "------compiling object file------"
gcc -c -Wall stdlib.c
echo "------running burger.native------"
./burger.native < $1.bun > $1.ll
echo "------llc command------"
llc $1.ll

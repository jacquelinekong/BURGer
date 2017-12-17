#!/bin/bash
# usage: "./burgr.sh file-name"
# do not append with .bun

./burger.native < tests/$1.bun > $1.ll
clang $1.ll
echo "--------------------"
echo "Output for .ll test:"
echo "--------------------"
cat $1.ll

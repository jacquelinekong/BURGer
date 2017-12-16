#!/bin/bash
# usage: "./burgr.sh file-name"
# do not append with .bun

./burger.native < $1.bun > $1.ll
clang $1.ll

echo "------compiling object file------"
gcc -c -Wall println.c
echo "------running burger.native------"
./burger.native < test-println.bun > test-println.ll
echo "------llc command------"
llc test-println.ll
echo "------linking files------"
gcc -o test-println test-println.s println.o
echo "------running executable------"
./test-println

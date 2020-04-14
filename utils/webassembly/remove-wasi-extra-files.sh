#!/bin/bash
basepath="compiler/wasi-sdk"
filestoremove="bin/clang*
bin/llvm*
bin/llc"
for i in $filestoremove
do
	echo $basepath/$i
	rm $basepath/$i
done

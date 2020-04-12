#!/bin/bash
basepath="compiler/opt/swiftwasm-sdk"
filestoremove="bin/sil-* 
bin/lldb*
bin/sourcekitd-*
bin/swift-api-digester
bin/swift-autolink-extract
bin/swift-demangle
bin/swift-demangle-yamldump
bin/swift-format
bin/swift-llvm-opt
bin/swift-refactor
bin/swift-reflection-dump
bin/swift-*-test
lib/libsourcekitdInProc.so
lib/swift/clang/lib/linux/*
lib/swift_static/linux/*
lib/swift/linux/x86_64/*
lib/swift/linux/*"
for i in $filestoremove
do
	echo $basepath/$i
	rm $basepath/$i
done
# Mac only
rm -r $basepath/lib/swift/macosx $basepath/lib/sourcekitd.framework

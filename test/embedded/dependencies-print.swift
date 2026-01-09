// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -no-allocations %t/test.swift -c -o %t/a.o

// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.o | sort | tee %t/actual-dependencies.txt

// RUN: %if OS=linux-gnu %{ comm -13 %t/allowed-dependencies_linux.txt %t/actual-dependencies.txt > %t/extra.txt %} %else %{ comm -13 %t/allowed-dependencies_macos.txt %t/actual-dependencies.txt > %t/extra.txt %}
// RUN: test ! -s %t/extra.txt

//--- allowed-dependencies_macos.txt
___divti3
___modti3
___stack_chk_fail
___stack_chk_guard
_memmove
_memset
_putchar

//--- allowed-dependencies_linux.txt
__divti3
__modti3
__stack_chk_fail
__stack_chk_guard
memmove
memset
putchar
//--- test.swift
// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-clang %t/a.o %t/print.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// UNSUPPORTED: OS=linux-gnu && CPU=aarch64

print("Hello Embedded Swift!") // CHECK: Hello Embedded Swift!
print(42) // CHECK: 42
print(false) // CHECK: false

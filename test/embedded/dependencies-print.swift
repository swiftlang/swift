// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -no-allocations %s -c -o %t/a.o

// RUN: grep DEP\: %s | sed 's#// DEP\: ##' | sort > %t/allowed-dependencies.txt

// Linux/ELF doesn't use the "_" prefix in symbol mangling.
// RUN: if [ %target-os == "linux-gnu" ]; then sed -E -i -e 's/^_(.*)$/\1/' %t/allowed-dependencies.txt; fi

// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.o | sort | tee %t/actual-dependencies.txt

// Fail if there is any entry in actual-dependencies.txt that's not in allowed-dependencies.txt
// RUN: test -z "`comm -13 %t/allowed-dependencies.txt %t/actual-dependencies.txt`"

// DEP: ___stack_chk_fail
// DEP: ___stack_chk_guard
// DEP: ___divti3
// DEP: ___modti3
// DEP: _memmove
// DEP: _memset
// DEP: _putchar

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

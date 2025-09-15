// RUN: %empty-directory(%t)
// RUN: %target-clang -x c -c %S/Inputs/unbuffered-putchar.c -o %t/unbuffered-putchar.o

// RUN: %target-build-swift -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none %s -Xlinker %t/unbuffered-putchar.o -o %t/a.out
// RUN: not --crash %t/a.out 2>&1 | %FileCheck %s --check-prefix=CHECK-MESSAGE

// RUN: %target-build-swift -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none %s -Xlinker %t/unbuffered-putchar.o -o %t/a.out -O
// RUN: not --crash %t/a.out 2>&1 | %FileCheck %s --check-prefix=CHECK-NOMESSAGE

// RUN: %target-build-swift -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none %s -Xlinker %t/unbuffered-putchar.o -o %t/a.out -Osize
// RUN: not --crash %t/a.out 2>&1 | %FileCheck %s --check-prefix=CHECK-NOMESSAGE

// RUN: %target-build-swift -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none %s -Xlinker %t/unbuffered-putchar.o -o %t/a.out -O     -assert-config Debug
// RUN: not --crash %t/a.out 2>&1 | %FileCheck %s --check-prefix=CHECK-MESSAGE

// RUN: %target-build-swift -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none %s -Xlinker %t/unbuffered-putchar.o -o %t/a.out -Osize -assert-config Debug
// RUN: not --crash %t/a.out 2>&1 | %FileCheck %s --check-prefix=CHECK-MESSAGE

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: swift_feature_Embedded

func test() {
     preconditionFailure("task failed successfully")
     // CHECK-MESSAGE: {{.*}}/traps-preconditionfailure-exec.swift:[[@LINE-1]]: Fatal error: task failed successfully
     // CHECK-NOMESSAGE-NOT: Fatal error
     // CHECK-NOMESSAGE-NOT: task failed successfully
}

test()

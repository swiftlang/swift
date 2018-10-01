// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground-high-performance -Xfrontend -playground -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift %S/../PlaygroundTransform/Inputs/PlaygroundsRuntime.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

#sourceLocation(file: "code.exe", line: 15)
func foo(_ x: Int) -> Bool {
  return x == 1
}

foo(1)
[1,2,3].map(foo)

// CHECK: [19:1-19:7] pc before
// CHECK-NEXT: [15:1-15:27] pc before
// CHECK-NEXT: [15:1-15:27] pc after
// CHECK-NEXT: [16:3-16:16] pc before
// CHECK-NEXT: [16:3-16:16] pc after
// CHECK-NEXT: [16:10-16:11] __builtin_log[='true']
// this next result is unexpected...
// CHECK-NEXT: [19:1-19:7] __builtin_log[='true']
// CHECK-NEXT: [19:1-19:7] pc after
// now for the array
// CHECK-NEXT: [20:1-20:17] pc before
// CHECK-NEXT: [15:1-15:27] pc before
// CHECK-NEXT: [15:1-15:27] pc after
// CHECK-NEXT: [16:3-16:16] pc before
// CHECK-NEXT: [16:3-16:16] pc after
// this next result is unexpected...
// CHECK-NEXT: [16:10-16:11] __builtin_log[='true']
// CHECK-NEXT: [15:1-15:27] pc before
// CHECK-NEXT: [15:1-15:27] pc after
// CHECK-NEXT: [16:3-16:16] pc before
// CHECK-NEXT: [16:3-16:16] pc after
// this next result is unexpected...
// CHECK-NEXT: [16:10-16:11] __builtin_log[='false']
// CHECK-NEXT: [15:1-15:27] pc before
// CHECK-NEXT: [15:1-15:27] pc after
// CHECK-NEXT: [16:3-16:16] pc before
// CHECK-NEXT: [16:3-16:16] pc after
// this next result is unexpected...
// CHECK-NEXT: [16:10-16:11] __builtin_log[='false']
// CHECK-NEXT: [20:1-20:17] __builtin_log[='[true, false, false]']
// CHECK-NEXT: [20:1-20:17] pc after

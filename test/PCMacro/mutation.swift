// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main2 %S/Inputs/PCMacroRuntime.swift %t/main.swift %S/Inputs/SilentPlaygroundsRuntime.swift
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

#sourceLocation(file: "main.swift", line: 10)
class A {
  func access() -> Void {
  }
}

class B {
  var a : A = A()
  init() {
    a.access()
  }
  func mutateIvar() -> Void {
    a.access()
  }
}

var b = B()
b.mutateIvar()

// CHECK: [25:1-25:12] pc before
// this should be logging the init, this is tracked in the init.swift test.
// Once fixed update this test to include it.
// MISSING [17:3-17:9] pc before
// MISSING [17:3-17:9] pc after
// MISSING [18:5-18:15] pc before
// CHECK-NEXT: [11:3-11:24] pc before
// CHECK-NEXT: [11:3-11:24] pc after
// MISSING [18:5-18:15] pc after
// CHECK-NEXT: [25:1-25:12] pc after

// CHECK-NEXT: [26:1-26:15] pc before
// CHECK-NEXT: [20:3-20:28] pc before
// CHECK-NEXT: [20:3-20:28] pc after
// CHECK-NEXT: [21:5-21:15] pc before
// CHECK-NEXT: [11:3-11:24] pc before
// CHECK-NEXT: [11:3-11:24] pc after
// CHECK-NEXT: [21:5-21:15] pc after
// CHECK-NEXT: [26:1-26:15] pc after

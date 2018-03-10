// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift %S/Inputs/SilentPlaygroundsRuntime.swift
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

#sourceLocation(file: "main.swift", line: 8)
func foo() {
  func bar() {
    2
  }
  
  let baz: () -> Void = {
    3
  }
  
  1
  bar()
  baz()
}

foo()

// CHECK: [22:1-22:6] pc before
// CHECK-NEXT: [8:1-8:11] pc before
// CHECK-NEXT: [8:1-8:11] pc after
// CHECK-NEXT: [13:3-15:4] pc before
// CHECK-NEXT: [13:3-15:4] pc after
// CHECK-NEXT: [17:3-17:4] pc before
// CHECK-NEXT: [17:3-17:4] pc after
// CHECK-NEXT: [18:3-18:8] pc before
// CHECK-NEXT: [9:3-9:13] pc before
// CHECK-NEXT: [9:3-9:13] pc after
// CHECK-NEXT: [10:5-10:6] pc before
// CHECK-NEXT: [10:5-10:6] pc after
// CHECK-NEXT: [18:3-18:8] pc after
// CHECK-NEXT: [19:3-19:8] pc before
// CHECK-NEXT: [14:5-14:6] pc before
// CHECK-NEXT: [14:5-14:6] pc after
// CHECK-NEXT: [14:5-14:6] pc before
// CHECK-NEXT: [14:5-14:6] pc after
// CHECK-NEXT: [19:3-19:8] pc after
// CHECK-NEXT: [22:1-22:6] pc after

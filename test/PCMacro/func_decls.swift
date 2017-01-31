// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift %S/Inputs/SilentPlaygroundsRuntime.swift
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

// Lets check that the source ranges are correct on all different kinds of func
// decls.
#sourceLocation(file: "main.swift", line: 8)
func function1(_ x: Int) -> Bool {
  return x == 1
}

_ = function1(0)

// CHECK: [12:1-12:17] pc before
// CHECK-NEXT: [8:1-8:33] pc before
// CHECK-NEXT: [8:1-8:33] pc after
// CHECK-NEXT: [9:3-9:16] pc before
// CHECK-NEXT: [9:3-9:16] pc after
// CHECK-NEXT: [12:1-12:17] pc after

func function2(_ x: Int) {
  
}
_ = function2(0)

// CHECK-NEXT: [24:1-24:17] pc before
// CHECK-NEXT: [21:1-21:25] pc before
// CHECK-NEXT: [21:1-21:25] pc after
// CHECK-NEXT: [24:1-24:17] pc after

func function3(_ x: Int) throws {
  
}
_ = try! function3(0)
// this test is XFAIL-ed in func_throw_notype and should be updated to 31:32 instead of 31:25 once fixed.
// CHECK-NEXT: [34:1-34:22] pc before
// CHECK-NEXT: [31:1-31:25] pc before
// CHECK-NEXT: [31:1-31:25] pc after
// CHECK-NEXT: [34:1-34:22] pc after

func function4(_ x: Int) throws -> Bool {
  return x == 1
}
_ = try! function4(0)

// CHECK-NEXT: [44:1-44:22] pc before
// CHECK-NEXT: [41:1-41:40] pc before
// CHECK-NEXT: [41:1-41:40] pc after
// CHECK-NEXT: [42:3-42:16] pc before
// CHECK-NEXT: [42:3-42:16] pc after
// CHECK-NEXT: [44:1-44:22] pc after

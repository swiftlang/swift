// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift %S/Inputs/SilentPlaygroundsRuntime.swift
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test
// XFAIL: *

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

#sourceLocation(file: "main.swift", line: 31)
func function3(_ x: Int) throws {
  
}
_ = try! function3(0)
// this test is XFAIL-ed due to the range not including throws
// CHECK: [34:1-34:22] pc before
// CHECK-NEXT: [31:1-31:32] pc before
// CHECK-NEXT: [31:1-31:32] pc after
// CHECK-NEXT: [34:1-34:22] pc after

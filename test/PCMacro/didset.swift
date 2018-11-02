// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift %S/Inputs/SilentPlaygroundsRuntime.swift
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

#sourceLocation(file: "main.swift", line: 10)
struct S {
    var a : [Int] = [] {
        didSet {
            print("Set")
        }
    }
}

var s = S()
s.a = [3,2]
s.a.append(300)

// CHECK: [18:1-18:12] pc before
// CHECK-NEXT: [18:1-18:12] pc after
// CHECK-NEXT: [19:1-19:12] pc before
// CHECK-NEXT: [12:9-12:15] pc before
// CHECK-NEXT: [12:9-12:15] pc after
// CHECK-NEXT: [13:13-13:25] pc before
// CHECK-NEXT: Set
// CHECK-NEXT: [13:13-13:25] pc after
// CHECK-NEXT: [19:1-19:12] pc after

// CHECK-NEXT: [20:1-20:16] pc before
// CHECK-NEXT: [12:9-12:15] pc before
// CHECK-NEXT: [12:9-12:15] pc after
// CHECK-NEXT: [13:13-13:25] pc before
// CHECK-NEXT: Set
// CHECK-NEXT: [13:13-13:25] pc after
// CHECK-NEXT: [20:1-20:16] pc after

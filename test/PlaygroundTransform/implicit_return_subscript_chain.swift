// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground -Xfrontend -debugger-support) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground -Xfrontend -debugger-support) | %FileCheck %s
//
// -pc-macro -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support) | %FileCheck %s
//
// REQUIRES: executable_test

import PlaygroundSupport

struct S {
    var dict: [Int : Int?]
    subscript(_ int: Int) -> Int? {
        get {
            dict[int, default: Int(String(int))]
        }
        set {
            dict[int] = newValue
        }
    }
    init() {
        dict = [:]
    }
}
var s = S()
s[13] = 33
s[14]
s[13]
// CHECK: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

// CHECK: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[newValue='Optional(33)']
// CHECK-NEXT: {{.*}} __builtin_log[int='13']
// CHECK-NEXT: {{.*}} __builtin_log[='Optional(Optional(33))']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

// CHECK: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[int='14']
// CHECK-NEXT: {{.*}} __builtin_log[='Optional(14)']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='Optional(14)']

// CHECK: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[int='13']
// CHECK-NEXT: {{.*}} __builtin_log[='Optional(33)']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='Optional(33)']

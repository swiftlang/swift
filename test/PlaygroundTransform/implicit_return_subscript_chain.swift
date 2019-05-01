// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support -o %t/main2 %S/Inputs/PlaygroundsRuntime.swift %S/Inputs/SilentPCMacroRuntime.swift %t/main.swift
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s
// REQUIRES: executable_test
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
// CHECK-NEXT: {{.*}} __builtin_log[='Optional(Optional(33))']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

// CHECK: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[='Optional(14)']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='Optional(14)']

// CHECK: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[='Optional(33)']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='Optional(33)']

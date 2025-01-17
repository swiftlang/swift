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

// Labeled, unlabeled, and multi-value parameters are logged.
func f(x: Int, _ y: Float = 7.62, z: String...) -> Int {
    func g(w: (Int, Int)) -> Int {
        func h(v: Int) -> Int {
            return v + 1
        }
        return h(v: w.0) + h(v: w.1) + 1
    }
    return x + z.count + g(w: (1, 2))
}
let foo = f(x: 42, z: "hello", "world")

// Unnamed parameters do are not logged.
func f(_: Int) -> Int {
    return 42
}
let bar = f(21)

// CHECK:      {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[x='42']
// CHECK-NEXT: {{.*}} __builtin_log[y='7.62']
// CHECK-NEXT: {{.*}} __builtin_log[z='["hello", "world"]']
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[w='(1, 2)']
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[v='1']
// CHECK-NEXT: {{.*}} __builtin_log[='2']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[v='2']
// CHECK-NEXT: {{.*}} __builtin_log[='3']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='6']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='50']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[foo='50']

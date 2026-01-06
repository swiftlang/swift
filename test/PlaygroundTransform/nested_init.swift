// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground) | %FileCheck %s
//
// -pc-macro -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground) | %FileCheck %s
//
// REQUIRES: executable_test

import PlaygroundSupport

// First make sure we get results in constructors, destructors, and regular functions for an unwrapped class.
class MyClass {
    init() {
        let x = 1
    }
    func f() {
        let y = 2
    }
    deinit {
        let z = 3
    }
}
do {
    let c = MyClass()
    c.f()
}

// CHECK:      [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[x='1']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[y='2']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[z='3']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit

// Now make sure we get results in constructors, destructors, and regular functions for a wrapped class too.
struct Playground {
    static func doit() {
        class MyClass {
            init() {
                let x = 1
            }
            func f() {
                let y = 2
            }
            deinit {
                let z = 3
            }
        }
        do {
            let c = MyClass()
            c.f()
        }
    }
}
Playground.doit()

// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[x='1']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[y='2']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[z='3']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit

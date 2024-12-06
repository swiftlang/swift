// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// Build PlaygroundSupport module
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift

// -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -playground -o %t/main5a -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -playground -o %t/main6a -I=%t %t/PlaygroundSupport.o %t/main.swift

// -pc-macro -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground -o %t/main5b -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground -o %t/main6b -I=%t %t/PlaygroundSupport.o %t/main.swift

// RUN: %target-codesign %t/main5a
// RUN: %target-codesign %t/main5b
// RUN: %target-codesign %t/main6a
// RUN: %target-codesign %t/main6b

// RUN: %target-run %t/main5a | %FileCheck %s
// RUN: %target-run %t/main5b | %FileCheck %s
// RUN: %target-run %t/main6a | %FileCheck %s
// RUN: %target-run %t/main6b | %FileCheck %s

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

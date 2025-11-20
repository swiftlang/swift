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

// First make sure we get results in accessors and regular functions for an unwrapped class.
class MyClass {
    func f() {
        let x = 1
    }
    var v1: Int {
        return 2
    }
    var v2: Int {
        get {
            return 3
        }
        set {
            let y = 4
        }
    }
    var v3: Int = 5 {
        didSet {
            let z = 6
        }
    }
    subscript(index: Int) -> Int {
        get {
            return 7
        }
        set {
            let y = 8
        }
    }
}
do {
    let c = MyClass()
    c.f()
    c.v1
    c.v2 = 7
    c.v2
    c.v3 = 8
    c[9]
    c[9] = 10
}

// CHECK:      [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[x='1']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='2']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[='2']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[newValue='7']
// CHECK-NEXT: [{{.*}}] __builtin_log[y='4']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='3']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[='3']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[z='6']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[index='9']
// CHECK-NEXT: [{{.*}}] __builtin_log[='7']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[='7']
// CHECK-NEXT: [{{.*}}] __builtin_log[='10']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[newValue='10']
// CHECK-NEXT: [{{.*}}] __builtin_log[index='9']
// CHECK-NEXT: [{{.*}}] __builtin_log[y='8']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit

// Now make sure we get results in accessors and regular functions for a wrapped class too.
struct Playground {
    static func doit() {
        class MyClass {
            func f() {
                let x = 1
            }
            var v1: Int {
                return 2
            }
            var v2: Int {
                get {
                    return 3
                }
                set {
                    let y = 4
                }
            }
            var v3: Int = 5 {
                didSet {
                    let z = 6
                }
            }
            subscript(index: Int) -> Int {
                get {
                    return 7
                }
                set {
                    let y = 8
                }
            }
        }
        do {
            let c = MyClass()
            c.f()
            c.v1
            c.v2 = 7
            c.v2
            c.v3 = 8
            c[9]
            c[9] = 10
        }
    }
}
Playground.doit()

// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[x='1']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='2']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[='2']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[newValue='7']
// CHECK-NEXT: [{{.*}}] __builtin_log[y='4']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='3']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[='3']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[z='6']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main{{.*}}.MyClass']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[index='9']
// CHECK-NEXT: [{{.*}}] __builtin_log[='7']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[='7']
// CHECK-NEXT: [{{.*}}] __builtin_log[='10']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[newValue='10']
// CHECK-NEXT: [{{.*}}] __builtin_log[index='9']
// CHECK-NEXT: [{{.*}}] __builtin_log[y='8']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit

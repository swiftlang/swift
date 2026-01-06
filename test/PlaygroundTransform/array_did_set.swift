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

// CHECK: [{{.*}}] __builtin_log[s='S(a: [])']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: Set
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[s='S(a: [3, 2])']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: Set
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[a='[3, 2, 300]']

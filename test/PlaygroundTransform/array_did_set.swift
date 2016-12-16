// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %S/Inputs/SilentPCMacroRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test
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

// CHECK: [{{.*}}] $builtin_log[s='S(a: [])']
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_entry
// CHECK-NEXT: Set
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] $builtin_log[s='S(a: [3, 2])']
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_entry
// CHECK-NEXT: Set
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] $builtin_log[a='[3, 2, 300]']

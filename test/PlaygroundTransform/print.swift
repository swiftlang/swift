// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | FileCheck %s
// REQUIRES: executable_test

var a = 2
print(a) // with newline
print(a, appendNewline: true) // with newline
print(a, appendNewline: false) // no newline

var b = 3
debugPrint(b+1) // with newline
debugPrint(b+1, appendNewline: true) // with newline
debugPrint(b+1, appendNewline: false) // no newline

debugPrint(b+1, appendNewline: { false }())

// CHECK: [{{.*}}] $builtin_log[a='2']
// CHECK-NEXT: [{{.*}}] $builtin_print<appendNewline=true>['2']
// CHECK-NEXT: [{{.*}}] $builtin_print<appendNewline=true>['2']
// CHECK-NEXT: [{{.*}}] $builtin_print<appendNewline=false>['2']
// CHECK-NEXT: [{{.*}}] $builtin_log[b='3']
// CHECK-NEXT: [{{.*}}] $builtin_debugPrint<appendNewline=true>['4']
// CHECK-NEXT: [{{.*}}] $builtin_debugPrint<appendNewline=true>['4']
// CHECK-NEXT: [{{.*}}] $builtin_debugPrint<appendNewline=false>['4']
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] $builtin_log[='false']
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] $builtin_debugPrint<appendNewline=false>['4']


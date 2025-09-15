// Tests that `-playground-option` can turn on various options individually,
// including those that are off by default for `-playground-high-performance`.
// Also test that unknown options are gracefully ignored.
//
// REQUIRES: executable_test
//
// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground -Xfrontend -playground-high-performance -Xfrontend -playground-option -Xfrontend FunctionParameters -Xfrontend -playground-option -Xfrontend ScopeEvents -Xfrontend -playground-option -Xfrontend ThisOptionShouldBeGracefullyIgnored) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground -Xfrontend -playground-high-performance -Xfrontend -playground-option -Xfrontend FunctionParameters -Xfrontend -playground-option -Xfrontend ScopeEvents -Xfrontend -playground-option -Xfrontend ThisOptionShouldBeGracefullyIgnored) | %FileCheck %s
//
// -pc-macro -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground -Xfrontend -pc-macro -Xfrontend -playground-high-performance -Xfrontend -playground-option -Xfrontend FunctionParameters -Xfrontend -playground-option -Xfrontend ScopeEvents -Xfrontend -playground-option -Xfrontend ThisOptionShouldBeGracefullyIgnored) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground -Xfrontend -pc-macro -Xfrontend -playground-high-performance -Xfrontend -playground-option -Xfrontend FunctionParameters -Xfrontend -playground-option -Xfrontend ScopeEvents -Xfrontend -playground-option -Xfrontend ThisOptionShouldBeGracefullyIgnored) | %FileCheck %s

import PlaygroundSupport

func foo(x: Int, y: Double) -> Bool {
    return true
}

let a = foo(x: 42, y: 3.14)
if (a) {
  5
} else {
  7
}
// CHECK: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[x='42']
// CHECK-NEXT: [{{.*}}] __builtin_log[y='3.14']
// CHECK-NEXT: [{{.*}}] __builtin_log[='true']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[a='true']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='5']

var b = true
for i in 0..<3 {
  i
  continue
}
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[b='true']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='0']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='1']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='2']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit

var c = true
for i in 0..<3 {
  i
  break
}
// CHECK-NEXT: [{{.*}}] __builtin_log[c='true']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='0']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit

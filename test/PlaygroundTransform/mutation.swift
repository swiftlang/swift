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

class A {
  func access() -> Void {
  }
}

class B {
  var a : A = A()
  init() {
    a.access()
  }
  func mutateIvar() -> Void {
    a.access()
  }
}

var b = B()
b.mutateIvar()
// CHECK: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// note: b.a should not be reported here because we are in init()
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[b='main.B']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[a='main.A']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit 
// CHECK-NEXT: [{{.*}}] __builtin_log[b='main.B']

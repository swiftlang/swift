// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | FileCheck %s

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
// CHECK: [{{.*}}] $builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_exit
// note: b.a should not be reported here because we are in init()
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] $builtin_log[b='main.B']
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] $builtin_log[a='main.A']
// CHECK-NEXT: [{{.*}}] $builtin_log_scope_exit 
// CHECK-NEXT: [{{.*}}] $builtin_log[b='main.B']

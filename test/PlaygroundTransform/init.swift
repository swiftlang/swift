// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main %S/Inputs/PlaygroundsRuntime.swift %S/Inputs/SilentPCMacroRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

class B {
  init() {
  }
}

class C : B {
  var i : Int
  var j : Int
  override init() {
    i = 3
    j = 5
    i + j
  }
}
let c = C()
// CHECK: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='8']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[c='main.C']

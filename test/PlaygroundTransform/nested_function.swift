// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %S/Inputs/SilentPCMacroRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

func returnSum() -> Int {
  var y = 10
  func add() {
    y += 5
  }
  add()
  let addAgain = {
    y += 5
  }
  addAgain()
  let addMulti = {
    y += 5
    _ = 0 // force a multi-statement closure
  }
  addMulti()
  return y
}

returnSum()

// CHECK-NOT: $builtin
// CHECK: [9:{{.*}}] $builtin_log_scope_entry
// CHECK-NEXT: [10:{{.*}}] $builtin_log[y='10']
// CHECK-NEXT: [11:{{.*}}] $builtin_log_scope_entry
// CHECK-NEXT: [12:{{.*}}] $builtin_log[y='15']
// CHECK-NEXT: [11:{{.*}}] $builtin_log_scope_exit
// CHECK-NEXT: [15:{{.*}}] $builtin_log[addAgain='{{.*}}']
// CHECK-NEXT: [15:{{.*}}] $builtin_log_scope_entry

// FIXME: We drop the log for the addition here.
// CHECK-NEXT: [16:{{.*}}] $builtin_log[='()']

// CHECK-NEXT: [15:{{.*}}] $builtin_log_scope_exit

// FIXME: There's an extra, unbalanced scope exit here.
// CHECK-NEXT: [9:{{.*}}] $builtin_log_scope_exit

// CHECK-NEXT: [19:{{.*}}] $builtin_log[addMulti='{{.*}}']
// CHECK-NEXT: [19:{{.*}}] $builtin_log_scope_entry
// CHECK-NEXT: [20:{{.*}}] $builtin_log[y='25']
// CHECK-NEXT: [21:{{.*}}] $builtin_log[='0']
// CHECK-NEXT: [19:{{.*}}] $builtin_log_scope_exit
// CHECK-NEXT: [24:{{.*}}] $builtin_log[='25']
// CHECK-NEXT: [9:{{.*}}] $builtin_log_scope_exit
// CHECK-NEXT: [27:{{.*}}] $builtin_log[='25']
// CHECK-NOT: $builtin

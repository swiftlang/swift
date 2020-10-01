// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
// RUN: %target-build-swift -Xfrontend -playground -o %t/main -I=%t %t/PlaygroundSupport.o %t/main.swift && %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main2 -I=%t %t/PlaygroundSupport.o %t/main.swift && %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s
// REQUIRES: executable_test

import PlaygroundSupport

#sourceLocation(file: "main.swift", line: 9)
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

// CHECK-NOT: __builtin
// CHECK: [9:{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [10:{{.*}}] __builtin_log[y='10']
// CHECK-NEXT: [11:{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [12:{{.*}}] __builtin_log[y='15']
// CHECK-NEXT: [11:{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [15:{{.*}}] __builtin_log[addAgain='{{.*}}']
// CHECK-NEXT: [15:{{.*}}] __builtin_log_scope_entry

// FIXME: We drop the log for the addition here.
// CHECK-NEXT: [16:{{.*}}] __builtin_log[='()']

// CHECK-NEXT: [15:{{.*}}] __builtin_log_scope_exit

// FIXME: There's an extra, unbalanced scope exit here.
// CHECK-NEXT: [9:{{.*}}] __builtin_log_scope_exit

// CHECK-NEXT: [19:{{.*}}] __builtin_log[addMulti='{{.*}}']
// CHECK-NEXT: [19:{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [20:{{.*}}] __builtin_log[y='25']
// CHECK-NEXT: [21:{{.*}}] __builtin_log[='0']
// CHECK-NEXT: [19:{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [24:{{.*}}] __builtin_log[='25']
// CHECK-NEXT: [9:{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [27:{{.*}}] __builtin_log[='25']
// CHECK-NOT: __builtin

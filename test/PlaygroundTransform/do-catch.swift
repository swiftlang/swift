// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main %S/Inputs/PlaygroundsRuntime.swift %S/Inputs/SilentPCMacroRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

func doSomething() throws -> Int { return 5 }

do {
  try doSomething()
} catch {
  print(error)
}

// CHECK: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='5']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[='5']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit

1
try doSomething()
// CHECK-LABEL: [{{.*}}] __builtin_log[='1']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='5']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[='5']

2
try! doSomething()
// CHECK-LABEL: [{{.*}}] __builtin_log[='2']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='5']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[='5']

3
try? doSomething()
// CHECK-LABEL: [{{.*}}] __builtin_log[='3']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_entry
// CHECK-NEXT: [{{.*}}] __builtin_log[='5']
// CHECK-NEXT: [{{.*}}] __builtin_log_scope_exit
// CHECK-NEXT: [{{.*}}] __builtin_log[='Optional(5)']

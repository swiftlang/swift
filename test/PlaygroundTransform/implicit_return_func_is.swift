// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support -o %t/main2 %S/Inputs/PlaygroundsRuntime.swift %S/Inputs/SilentPCMacroRuntime.swift %t/main.swift
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s
// REQUIRES: executable_test
func iss<T>(_ instance: Any, anInstanceOf type: T.Type) -> Bool {
  instance is T
}
iss("hello", anInstanceOf: String.self)
iss(57, anInstanceOf: String.self)
// CHECK: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[='true']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='true']
// CHECK: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[='false']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='false']

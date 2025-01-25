// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground -Xfrontend -debugger-support) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground -Xfrontend -debugger-support) | %FileCheck %s
//
// -pc-macro -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support) | %FileCheck %s
//
// REQUIRES: executable_test

import PlaygroundSupport

func iss<T>(_ instance: Any, anInstanceOf type: T.Type) -> Bool {
  instance is T
}
iss("hello", anInstanceOf: String.self)
iss(57, anInstanceOf: String.self)
// CHECK: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[instance='hello']
// CHECK-NEXT: {{.*}} __builtin_log[type='String']
// CHECK-NEXT: {{.*}} __builtin_log[='true']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='true']
// CHECK: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[instance='57']
// CHECK-NEXT: {{.*}} __builtin_log[type='String']
// CHECK-NEXT: {{.*}} __builtin_log[='false']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='false']

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

func add<I : SignedNumeric>(_ lhs: I, _ rhs: I) -> I {
  lhs + rhs
}
add(3, 4)
// CHECK: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[lhs='3']
// CHECK-NEXT: {{.*}} __builtin_log[rhs='4']
// CHECK-NEXT: {{.*}} __builtin_log[='7']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='7']


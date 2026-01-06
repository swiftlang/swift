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

func id<T>(_ t: T) -> T {
  return t
}

for i in 0..<3 {
  _ = id(i)
}

// CHECK:      __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log[t='0']
// CHECK-NEXT: __builtin_log[='0']
// CHECK-NEXT: __builtin_log_scope_exit
// CHECK-NEXT: __builtin_log[='0']
// CHECK-NEXT: __builtin_log_scope_exit
// CHECK-NEXT: __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log[t='1']
// CHECK-NEXT: __builtin_log[='1']
// CHECK-NEXT: __builtin_log_scope_exit
// CHECK-NEXT: __builtin_log[='1']
// CHECK-NEXT: __builtin_log_scope_exit
// CHECK-NEXT: __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log[t='2']
// CHECK-NEXT: __builtin_log[='2']
// CHECK-NEXT: __builtin_log_scope_exit
// CHECK-NEXT: __builtin_log[='2']
// CHECK-NEXT: __builtin_log_scope_exit

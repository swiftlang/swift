// Tests that `-playground-high-performance` turns off expensive logging, and
// that turning off the corresponding options using `-playground-option` with
// a `No` prefix does the same thing.
//
// REQUIRES: executable_test
//
// -playground -playground-high-performance
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground -Xfrontend -playground-high-performance) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground -Xfrontend -playground-high-performance) | %FileCheck %s
//
// -pc-macro -playground  -playground-high-performance
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -playground-high-performance) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -playground-high-performance) | %FileCheck %s
//
// -playground -playground-option NoScopeEvents -playground-option NoFunctionParameters
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground -Xfrontend -playground-option -Xfrontend NoScopeEvents -Xfrontend -playground-option -Xfrontend NoFunctionParameters) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground -Xfrontend -playground-option -Xfrontend NoScopeEvents -Xfrontend -playground-option -Xfrontend NoFunctionParameters) | %FileCheck %s

import PlaygroundSupport

var a = true
if (a) {
  5
} else {
  7
}

for i in 0..<3 {
  i
}
// CHECK: [{{.*}}] __builtin_log[a='true']
// CHECK-NEXT: [{{.*}}] __builtin_log[='5']
// CHECK-NEXT: [{{.*}}] __builtin_log[='0']
// CHECK-NEXT: [{{.*}}] __builtin_log[='1']
// CHECK-NEXT: [{{.*}}] __builtin_log[='2']

var b = true
for i in 0..<3 {
  i
  continue
}
// CHECK-NEXT: [{{.*}}] __builtin_log[b='true']
// CHECK-NEXT: [{{.*}}] __builtin_log[='0']
// CHECK-NEXT: [{{.*}}] __builtin_log[='1']
// CHECK-NEXT: [{{.*}}] __builtin_log[='2']

var c = true
for i in 0..<3 {
  i
  break
}
// CHECK-NEXT: [{{.*}}] __builtin_log[c='true']
// CHECK-NEXT: [{{.*}}] __builtin_log[='0']

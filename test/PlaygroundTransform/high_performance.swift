// Tests that `-playground-high-performance` turns off expensive logging, and
// that turning off the corresponding options using `-playground-option` with
// a `No` prefix does the same thing.
//
// REQUIRES: executable_test
//
// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
//
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -playground-high-performance -o %t/main -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
//
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -playground-high-performance -o %t/main2 -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s
//
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -playground-option -Xfrontend NoScopeEvents -Xfrontend -playground-option -Xfrontend NoFunctionParameters -o %t/main3 -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main3
// RUN: %target-run %t/main3 | %FileCheck %s

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

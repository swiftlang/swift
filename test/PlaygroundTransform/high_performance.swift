// Tests that `-playground-high-performance` turns off expensive logging, and
// that turning off the corresponding options using `-playground-option` with
// a `No` prefix does the same thing.
//
// REQUIRES: executable_test
//
// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// Build PlaygroundSupport module
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift

// -playground -playground-high-performance
// RUN: %target-build-swift -swift-version 5 -Xfrontend -playground -Xfrontend -playground-high-performance -o %t/main5a -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -playground -Xfrontend -playground-high-performance -o %t/main6a -I=%t %t/PlaygroundSupport.o %t/main.swift

// -pc-macro -playground  -playground-high-performance
// RUN: %target-build-swift -swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -playground-high-performance -o %t/main5b -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -playground-high-performance -o %t/main6b -I=%t %t/PlaygroundSupport.o %t/main.swift

// -playground -playground-option NoScopeEvents -playground-option NoFunctionParameters
// RUN: %target-build-swift -swift-version 5 -Xfrontend -playground -Xfrontend -playground-option -Xfrontend NoScopeEvents -Xfrontend -playground-option -Xfrontend NoFunctionParameters -o %t/main5c -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -playground -Xfrontend -playground-option -Xfrontend NoScopeEvents -Xfrontend -playground-option -Xfrontend NoFunctionParameters -o %t/main6c -I=%t %t/PlaygroundSupport.o %t/main.swift

// RUN: %target-codesign %t/main5a
// RUN: %target-codesign %t/main5b
// RUN: %target-codesign %t/main5c
// RUN: %target-codesign %t/main6a
// RUN: %target-codesign %t/main6b
// RUN: %target-codesign %t/main6c

// RUN: %target-run %t/main5a | %FileCheck %s
// RUN: %target-run %t/main5b | %FileCheck %s
// RUN: %target-run %t/main5c | %FileCheck %s
// RUN: %target-run %t/main6a | %FileCheck %s
// RUN: %target-run %t/main6b | %FileCheck %s
// RUN: %target-run %t/main6c | %FileCheck %s

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

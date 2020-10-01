// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/PCMacroRuntime.swift %S/Inputs/SilentPlaygroundsRuntime.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -o %t/main -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support -o %t/main2 -I=%t %t/PlaygroundSupport.o %t/main.swift 
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

import PlaygroundSupport

#sourceLocation(file: "main.swift", line: 8)
var a = false
if (a) {
  5
} else {
  7
}

// CHECK: [8:1-8:14] pc before
// CHECK-NEXT: [8:1-8:14] pc after
// CHECK-NEXT: [9:1-9:7] pc before
// CHECK-NEXT: [9:1-9:7] pc after
// CHECK-NEXT: [11:3-11:7] pc before
// CHECK-NEXT: [11:3-11:7] pc after
// CHECK-NEXT: [12:3-12:4] pc before
// CHECK-NEXT: [12:3-12:4] pc after

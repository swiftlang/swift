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
func foo() {
  defer {
    2
  }
  1
}

foo()

// CHECK: [15:1-15:6] pc before
// CHECK-NEXT: [8:1-8:11] pc before
// CHECK-NEXT: [8:1-8:11] pc after
// CHECK-NEXT: [12:3-12:4] pc before
// CHECK-NEXT: [12:3-12:4] pc after
// CHECK-NEXT: [10:5-10:6] pc before
// CHECK-NEXT: [10:5-10:6] pc after
// CHECK-NEXT: [15:1-15:6] pc after

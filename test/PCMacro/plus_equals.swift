// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main2 %S/Inputs/PCMacroRuntime.swift %t/main.swift %S/Inputs/SilentPlaygroundsRuntime.swift
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

#sourceLocation(file: "main.swift", line: 10)
var a = "a"
var b = "b" 
a += b 
// CHECK: [10:1-10:12] pc before
// CHECK-NEXT: [10:1-10:12] pc after
// CHECK-NEXT: [11:1-11:12] pc before
// CHECK-NEXT: [11:1-11:12] pc after
// CHECK-NEXT: [12:1-12:7] pc before
// CHECK-NEXT: [12:1-12:7] pc after

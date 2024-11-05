// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// Build PlaygroundSupport module
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift

// -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -playground -o %t/main5a -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -playground -o %t/main6a -I=%t %t/PlaygroundSupport.o %t/main.swift

// -pc-macro -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground -o %t/main5b -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground -o %t/main6b -I=%t %t/PlaygroundSupport.o %t/main.swift

// RUN: %target-codesign %t/main5a
// RUN: %target-codesign %t/main5b
// RUN: %target-codesign %t/main6a
// RUN: %target-codesign %t/main6b

// RUN: %target-run %t/main5a | %FileCheck %s
// RUN: %target-run %t/main5b | %FileCheck %s
// RUN: %target-run %t/main6a | %FileCheck %s
// RUN: %target-run %t/main6b | %FileCheck %s

// REQUIRES: executable_test

import PlaygroundSupport

var str : String = ""

print(("One", 2))
print("One", to: &str)
print("One", terminator: "\n")
print("One", terminator: "")
print("One", terminator: "\n", to: &str)
print("One", terminator: "", to: &str)

debugPrint(("One", 2))
debugPrint("One", to: &str)
debugPrint("One", terminator: "\n")
debugPrint("One", terminator: "")
debugPrint("One", terminator: "\n", to: &str)
debugPrint("One", terminator: "", to: &str)

// CHECK: [{{.*}}] __builtin_log[str='']
// CHECK-NEXT: ("One", 2)
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: One
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: One[{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: ("One", 2)
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: "One"
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: "One"[{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint


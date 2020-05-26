// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
// RUN: %target-build-swift -Xfrontend -playground -o %t/main -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main2 -I=%t %t/PlaygroundSupport.o %t/main.swift 
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s
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


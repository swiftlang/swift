// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | FileCheck %s
// REQUIRES: executable_test

var str : String = ""

print(("One", 2))
print("One", &str)
print("One", appendNewline:true)
print("One", appendNewline:false)
print("One", &str, appendNewline:true)
print("One", &str, appendNewline:false)

debugPrint(("One", 2))
debugPrint("One", &str)
debugPrint("One", appendNewline:true)
debugPrint("One", appendNewline:false)
debugPrint("One", &str, appendNewline:true)
debugPrint("One", &str, appendNewline:false)

// CHECK: [{{.*}}] $builtin_log[str='']
// CHECK-NEXT: ("One", 2)
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: One
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: One[{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: ("One", 2)
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: "One"
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: "One"[{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint


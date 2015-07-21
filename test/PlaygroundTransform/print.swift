// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | FileCheck %s
// REQUIRES: executable_test

var str : String = ""

print("One")
print("One", 2)
print("One", &str)
print("One", appendNewline:true)
print("One", appendNewline:false)
print("One", &str, appendNewline:true)
print("One", &str, appendNewline:false)

debugPrint("One")
debugPrint("One", 2)
debugPrint("One", &str)
debugPrint("One", appendNewline:true)
debugPrint("One", appendNewline:false)
debugPrint("One", &str, appendNewline:true)
debugPrint("One", &str, appendNewline:false)

// CHECK: [{{.*}}] $builtin_print<>['One']
// CHECK-NEXT: [{{.*}}] $builtin_print<>['("One", 2)']
// CHECK-NEXT: [{{.*}}] $builtin_print<stream>['One']
// CHECK-NEXT: [{{.*}}] $builtin_print<appendNewline=true>['One']
// CHECK-NEXT: [{{.*}}] $builtin_print<appendNewline=false>['One']
// CHECK-NEXT: [{{.*}}] $builtin_print<stream, appendNewline=true>['One']
// CHECK-NEXT: [{{.*}}] $builtin_print<stream, appendNewline=false>['One']
// CHECK-NEXT: [{{.*}}] $builtin_debugPrint<>['One']
// CHECK-NEXT: [{{.*}}] $builtin_debugPrint<>['("One", 2)']
// CHECK-NEXT: [{{.*}}] $builtin_debugPrint<stream>['One']
// CHECK-NEXT: [{{.*}}] $builtin_debugPrint<appendNewline=true>['One']
// CHECK-NEXT: [{{.*}}] $builtin_debugPrint<appendNewline=false>['One']
// CHECK-NEXT: [{{.*}}] $builtin_debugPrint<stream, appendNewline=true>['One']
// CHECK-NEXT: [{{.*}}] $builtin_debugPrint<stream, appendNewline=false>['One']


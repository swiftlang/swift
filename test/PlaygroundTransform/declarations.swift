// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | FileCheck %s
// REQUIRES: executable_test

var a = 2
var b = 3
a + b 
// CHECK: [{{.*}}] $builtin_log[a='2']
// CHECK-NEXT: [{{.*}}] $builtin_log[b='3']
// CHECK-NEXT: [{{.*}}] $builtin_log[='5']

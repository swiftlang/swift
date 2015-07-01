// RUN: rm -rf %t && mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | FileCheck %s
// REQUIRES: executable_test

// FIXME: deal with inout arguments to print
// XFAIL: *

var s = "a"
print("b", &s)
// CHECK: [{{.*}}] $builtin_log[s='a']
// CHECK-NEXT: [{{.*}}] $builtin_print<appendNewline=true>['a']

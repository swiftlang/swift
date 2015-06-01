// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | FileCheck %s
// REQUIRES: executable_test

var foo = [true, false]
foo.append(true)
// CHECK: [{{.*}}] $builtin_log[foo='[true, false]']
// CHECK-NEXT: [{{.*}}] $builtin_log[foo='[true, false, true]']

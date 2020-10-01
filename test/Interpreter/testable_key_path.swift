// RUN: %empty-directory(%t)
// RUN: %target-build-swift -whole-module-optimization -c -o %t/Module.o -enable-testing -parse-as-library -emit-module -emit-module-path %t/Module.swiftmodule -module-name Module %S/Inputs/testable_key_path_2.swift
// RUN: %target-build-swift -o %t/a.out -I %t %s %t/Module.o
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

@testable import Module

let c = TestClass()

print("You say \(c.field)")

let kp = \TestClass.field

c[keyPath: kp] = "hello"

print("I say \(c.field)")

// CHECK: I say hello

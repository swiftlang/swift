// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/../require-layout-generic-arg.swift %S/../Inputs/require-layout-generic-class.swift %s -o %t/test
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test | %FileCheck %s

// REQUIRES: executable_test

func test() {
  requestType(Sub(1))
}

// CHECK: Int
test()

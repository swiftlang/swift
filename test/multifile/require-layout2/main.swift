// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/../require-layout-generic-arg-closure.swift %S/../Inputs/require-layout-generic-class.swift %s -o %t/test
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test | %FileCheck %s

// REQUIRES: executable_test

func test() {
  requestType2(x: 1)
}

// CHECK: test.Sub<Swift.Int>
test()

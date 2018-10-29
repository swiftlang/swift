// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -o %t/a.out %t/main.swift %S/Inputs/KeyPathMultiFile_b.swift
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test

import StdlibUnittest

var keyPathMultiFile = TestSuite("key paths across multiple files")

keyPathMultiFile.test("identity across multiple files") {
  expectEqual(A_x_keypath(), \A.x)
  expectEqual(A_subscript_0_keypath(), \A.[0])
}

runAllTests()

// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

let BoolTests = TestSuite("Bool")

BoolTests.test("Init with NSNumber") {
  expectFalse(Bool(NSNumber(integerLiteral: 0)))
  expectTrue(Bool(NSNumber(integerLiteral: 1)))
  expectTrue(Bool(NSNumber(integerLiteral: 2)))
}

runAllTests()

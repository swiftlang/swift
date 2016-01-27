// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

let BoolTests = TestSuite("Bool")

BoolTests.test("Init with NSNumber") {
  expectFalse(Bool(NSNumber(integerLiteral: 0)))
  expectTrue(Bool(NSNumber(integerLiteral: 1)))
  expectTrue(Bool(NSNumber(integerLiteral: 2)))
}

runAllTests()

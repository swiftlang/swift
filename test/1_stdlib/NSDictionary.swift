// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

import Foundation

var tests = TestSuite("NSDictionary")

tests.test("copy construction") {
  let expected = ["A":1, "B":2, "C":3, "D":4]
  let x = NSDictionary(dictionary: expected as NSDictionary)
  expectEqual(expected, x as! Dictionary)
  let y = NSMutableDictionary(dictionary: expected as NSDictionary)
  expectEqual(expected, y as NSDictionary as! Dictionary)
}

runAllTests()

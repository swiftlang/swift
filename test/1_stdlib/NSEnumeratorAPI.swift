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

var NSEnumeratorAPI = TestSuite("NSEnumeratorAPI")

NSEnumeratorAPI.test("SequenceType") {
  let result = NSDictionary().keyEnumerator()
  expectSequenceType(result)
}

NSEnumeratorAPI.test("keyEnumerator") {
  let result = [1: "one", 2: "two"]
  expectEqualsUnordered(
    [1, 2], NSDictionary(dictionary: result).keyEnumerator()) {
      switch ($0 as! Int, $1 as! Int) {
      case let (x, y) where x == y: return .EQ
      case let (x, y) where x < y: return .LT
      case _: return .GT
      }
    }
}

runAllTests()

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest


import Foundation

var NSEnumeratorAPI = TestSuite("NSEnumeratorAPI")

NSEnumeratorAPI.test("Sequence") {
  let result = NSDictionary().keyEnumerator()
  expectSequenceType(result)
}

NSEnumeratorAPI.test("keyEnumerator") {
  let result = [1 as NSNumber: "one", 2 as NSNumber: "two"]
  expectEqualsUnordered(
    [1, 2], NSDictionary(dictionary: result as [AnyHashable: Any]).keyEnumerator()) {
      switch ($0 as! Int, $1 as! Int) {
      case let (x, y) where x == y: return .eq
      case let (x, y) where x < y: return .lt
      case _: return .gt
      }
    }
}

runAllTests()

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

var NSEnumeratorAPI = TestSuite("NSEnumeratorAPI")

NSEnumeratorAPI.test("SequenceType") {
  let result = NSDictionary().keyEnumerator()
  isSequenceType(result)
}

NSEnumeratorAPI.test("keyEnumerator") {
  let result = [1: "one", 2: "two"]
  expectEqualsUnordered(
    [1, 2], NSDictionary(dictionary: result).keyEnumerator(),
    {
      switch ($0 as! Int, $1 as! Int) {
      case let (x, y) where x == y: return .EQ
      case let (x, y) where x < y: return .LT
      case _: return .GT
      }
    })
}

runAllTests()

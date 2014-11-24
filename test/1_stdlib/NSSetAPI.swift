// RUN: %target-run-simple-swift
// XFAIL: linux

import StdlibUnittest
import Foundation

var NSSetAPI = TestSuite("NSSetAPI")

NSSetAPI.test("SequenceType") {
  let result = NSSet()
  isSequenceType(result)
}

NSSetAPI.test("initWithObjects") {
  let result = NSSet(objects: 1, "two")
  // using the descriptions of 1 and "two" are fine for this test.
  expectEqualsUnordered([1, "two"], result) {
    switch ($0.description < $1.description, $0.description == $1.description) {
    case (true, _): return .LT
    case (_, true): return .EQ
    case _: return .GT
    }
  }
}

NSSetAPI.test("Printable") {
  let result = toString(NSSet(objects:"a", "b", "c", "42"))
  let expect = "{(\n    b,\n    42,\n    c,\n    a\n)}"
  expectEqual(expect, result)
}

var NSOrderedSetAPI = TestSuite("NSOrderedSetAPI")

runAllTests()

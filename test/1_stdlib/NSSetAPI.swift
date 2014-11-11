// RUN: %target-run-simple-swift

import StdlibUnittest
import Foundation

var NSSetAPI = TestSuite("NSSetAPI")

NSSetAPI.test("SequenceType") {
  let result = NSSet()
  isSequenceType(result)
}

NSSetAPI.test("initWithObjects") {
  let result = NSSet(objects: 1, "two")
  expectEqualsUnordered([1, "two"], result) {
    ($0 as NSObject) == ($1 as NSObject)
  }
}

NSSetAPI.test("Printable") {
  let result = toString(NSSet(objects:"a", "b", "c", "42"))
  let expect = "{(\n    b,\n    42,\n    c,\n    a\n)}"
  expectEqual(expect, result)
}

var NSOrderedSetAPI = TestSuite("NSOrderedSetAPI")

runAllTests()

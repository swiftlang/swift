// RUN: %target-run-simple-swift

import StdlibUnittest
import Foundation

var NSSetAPI = TestSuite("NSSetAPI")

NSSetAPI.test("SequenceType") {
  let result = NSSet()
  isSequenceType(result)
}

private func compareAnythingAtAll(x: AnyObject, y: AnyObject)
  -> ExpectedComparisonResult {
  switch (x.description < y.description, x.description == y.description) {
  case (true, _): return .LT
  case (_, true): return .EQ
  default: return .GT
  }
}

NSSetAPI.test("initWithObjects") {
  let result = NSSet(objects: 1, "two")
  // using the descriptions of 1 and "two" are fine for these tests.
  expectEqualsUnordered([1, "two"], result, compareAnythingAtAll)
}

NSSetAPI.test("ArrayLiteralConvertible") {
  let result: NSSet = [1, "two"]
  expectEqualsUnordered([1, "two"], result, compareAnythingAtAll)
}

NSSetAPI.test("Printable") {
  let result = toString(NSSet(objects:"a", "b", "c", "42"))
  let expect = "{(\n    b,\n    42,\n    c,\n    a\n)}"
  expectEqual(expect, result)
}

var NSOrderedSetAPI = TestSuite("NSOrderedSetAPI")

NSOrderedSetAPI.test("SequenceType") {
  let result = NSOrderedSet()
  isSequenceType(result)
}

NSOrderedSetAPI.test("initWithObjects") {
  let result = NSOrderedSet(objects: 1, "two")
  expectEqualsUnordered([1, "two"], result, compareAnythingAtAll)
}

NSOrderedSetAPI.test("ArrayLiteralConvertible") {
  let result: NSOrderedSet = [1, "two"]
  expectEqualsUnordered([1, "two"], result, compareAnythingAtAll)
}

NSOrderedSetAPI.test("Printable") {
  let result = toString(NSOrderedSet(objects:"a", "b", "c", "42"))
  let expect = "{(\n    a,\n    b,\n    c,\n    42\n)}"
  expectEqual(expect, result)
}

var NSIndexSetAPI = TestSuite("NSIndexSetAPI")

NSIndexSetAPI.test("SequenceType") {
  let result = NSIndexSet()
  isSequenceType(result)
  let s = NSIndexSet(indexesInRange: NSMakeRange(1, 1))
  var g = s.generate()
  // FIXME: Compiler doesn't accept these terms.
  // expectEqual(Optional<Int>.Some(1), g.next())
  // expectEqual(Optional<Int>.None, g.next())
  expectOptionalEqual(1, g.next())
  expectEmpty(g.next())
}

runAllTests()

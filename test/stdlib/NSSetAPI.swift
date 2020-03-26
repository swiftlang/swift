// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest


import Foundation

var NSSetAPI = TestSuite("NSSetAPI")

NSSetAPI.test("Sequence") {
  let result = NSSet()
  expectSequenceType(result)
}

private func compareAnythingAtAll(_ x: Any, y: Any)
  -> ExpectedComparisonResult {
  let xDescription = "\(x)"
  let yDescription = "\(y)"
  switch (xDescription < yDescription, xDescription == yDescription) {
  case (true, _): return .lt
  case (_, true): return .eq
  default: return .gt
  }
}

NSSetAPI.test("initWithObjects") {
  let result = NSSet(objects: 1, "two")
  // using the descriptions of 1 and "two" are fine for these tests.
  expectEqualsUnordered([1, "two"] as [Any], result, compare: compareAnythingAtAll)
}

NSSetAPI.test("ExpressibleByArrayLiteral") {
  let result: NSSet = [1, "two"]
  expectEqualsUnordered([1, "two"] as [Any], result, compare: compareAnythingAtAll)
}

NSSetAPI.test("CustomStringConvertible") {
  let result = String(describing: NSSet(objects:"a", "b", "c", "42"))
  let expect = "{(\n    b,\n    42,\n    c,\n    a\n)}"
  expectEqual(expect, result)
}

NSSetAPI.test("AnyHashable containing NSSet") {
  let values: [NSSet] = [
    NSSet(),
    NSSet(objects: 1, 2, 3),
    NSSet(objects: 1, 2, 3),
  ]
  let anyHashables = values.map(AnyHashable.init)
  expectEqual(Set<AnyHashable>.self, type(of: anyHashables[0].base))
  expectEqual(Set<AnyHashable>.self, type(of: anyHashables[1].base))
  expectEqual(Set<AnyHashable>.self, type(of: anyHashables[2].base))
  expectNotEqual(anyHashables[0], anyHashables[1])
  expectEqual(anyHashables[1], anyHashables[2])
}

NSSetAPI.test("AnyHashable containing NSSet that contains an NSSet") {
  let anyHashable = AnyHashable(NSSet(objects: NSSet(objects: 1,2,3)))
  expectEqual(Set<AnyHashable>.self, type(of: anyHashable.base))

  if let firstNested
    = expectNotNil((anyHashable.base as! Set<AnyHashable>).first!) {
    expectEqual(Set<AnyHashable>.self, type(of: firstNested.base))
  }
}

NSSetAPI.test("Incorrectly constructed Set for backwards compatibility") {
  let array:NSArray = [NSObject()] as NSArray
  let wrongSet = Set<NSObject>(_immutableCocoaSet: array)
  print(wrongSet.startIndex)
}

var NSOrderedSetAPI = TestSuite("NSOrderedSetAPI")

NSOrderedSetAPI.test("Sequence") {
  let result = NSOrderedSet()
  expectSequenceType(result)
}

NSOrderedSetAPI.test("initWithObjects") {
  let result = NSOrderedSet(objects: 1, "two")
  expectEqualsUnordered([1, "two"] as [Any], result, compare: compareAnythingAtAll)
}

NSOrderedSetAPI.test("ExpressibleByArrayLiteral") {
  let result: NSOrderedSet = [1, "two"]
  expectEqualsUnordered([1, "two"] as [Any], result, compare: compareAnythingAtAll)
}

NSOrderedSetAPI.test("CustomStringConvertible") {
  let result = String(describing: NSOrderedSet(objects:"a", "b", "c", "42"))
  let expect = "{(\n    a,\n    b,\n    c,\n    42\n)}"
  expectEqual(expect, result)
}

NSSetAPI.test("copy construction") {
  let expected: Set = ["A", "B", "C", "D"]
  let x = NSSet(set: expected as NSSet)
  expectEqual(expected, x as! Set)
  let y = NSMutableSet(set: expected as NSSet)
  expectEqual(expected, y as NSSet as! Set)
}

var NSIndexSetAPI = TestSuite("NSIndexSetAPI")

NSIndexSetAPI.test("Sequence") {
  let result = NSIndexSet()
  let _ = expectSequenceType(result)
  let s = NSIndexSet(indexesIn: NSMakeRange(1, 1))
  var iter = s.makeIterator()
  // FIXME: Compiler doesn't accept these terms.
  // expectEqual(Optional<Int>.some(1), iter.next())
  // expectEqual(Optional<Int>.none, iter.next())
  expectEqual(1, iter.next())
  expectNil(iter.next())
  let empty = NSIndexSet(indexesIn: NSMakeRange(1, 0))
  var emptyGen = empty.makeIterator()
  expectNil(emptyGen.next())
}

runAllTests()

// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

// XFAIL: rdar45749460

import StdlibUnittest

var FilterTestSuite = TestSuite("HashedCollectionFilter")

FilterTestSuite.test("Dictionary.filter(_:) -> [Key: Value]")
  .xfail(.always("Not actually running under Swift 4")).code
{
  let d = [10: 1010, 20: 1020, 30: 1030, 40: 1040]
  // filter(_:) should produce a dictionary in Swift 4
  let f: Any = d.filter { $0.key > 20 }
  expectTrue(f is [Int: Int])
}

FilterTestSuite.test("Dictionary.filter(_:) -> [(Key, Value)] available") {
  let d = [10: 1010, 20: 1020, 30: 1030, 40: 1040]
  // The Array-returning version from Sequence should still be accessible
  let f: [(Int, Int)] = d.filter { $0.key > 20 }
  expectEqual(2, f.count)
}

FilterTestSuite.test("Set.filter(_:) -> Set<Element>")
  .xfail(.always("Not actually running under Swift 4")).code
{
  let s: Set = [10, 20, 30, 40]
  // filter(_:) should produce a set in Swift 4
  let f: Any = s.filter { $0 > 20 }
  expectTrue(f is Set<Int>)
}

FilterTestSuite.test("Set.filter(_:) -> [Element] available") {
  let s: Set = [10, 20, 30, 40]
  // The Array-returning version from Sequence should still be accessible
  let f: [Int] = s.filter { $0 > 20 }
  expectEqual(2, f.count)
}

FilterTestSuite.test("Dictionary.keys -> Keys")
  .xfail(.always("Not actually running under Swift 4")).code
{
  let d = [10: 1010, 20: 1020, 30: 1030, 40: 1040]
  // .keys should produce a Dictionary.Keys in Swift 4
  let f: Any = d.keys
  expectTrue(f is Dictionary<Int, Int>.Keys)
}

FilterTestSuite.test("Dictionary.values -> Values")
  .xfail(.always("Not actually running under Swift 4")).code
{
  let d = [10: 1010, 20: 1020, 30: 1030, 40: 1040]
  // .values should produce a Dictionary.Values in Swift 4
  let f: Any = d.values
  expectTrue(f is Dictionary<Int, Int>.Values)
}

runAllTests()


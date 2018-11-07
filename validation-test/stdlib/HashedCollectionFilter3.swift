// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import StdlibUnittest

var FilterTestSuite = TestSuite("HashedCollectionFilter")

FilterTestSuite.test("Dictionary.filter(_:) -> [(Key, Value)]") {
  let d = [10: 1010, 20: 1020, 30: 1030, 40: 1040]
  let f: Any = d.filter { (k, v) in k > 20 }
  expectTrue(f is [(Int, Int)])
}

FilterTestSuite.test("Set.filter(_:) -> [Element]") {
  let s: Set = [10, 20, 30, 40]
  let f: Any = s.filter { $0 > 20 }
  expectTrue(f is [Int])
}

FilterTestSuite.test("Dictionary.keys -> LazyMapCollection") {
  let d = [10: 1010, 20: 1020, 30: 1030, 40: 1040]
  // .keys should produce a LazyMapCollection in Swift 3
  let f: Any = d.keys
  let g = f as! LazyMapCollection<[Int: Int], Int>
  expectEqual(4, g.count)
}

FilterTestSuite.test("Dictionary.values -> LazyMapCollection") {
  let d = [10: 1010, 20: 1020, 30: 1030, 40: 1040]
  // .values should produce a LazyMapCollection in Swift 3
  let f: Any = d.values
  let g = f as! LazyMapCollection<[Int: Int], Int>
  expectEqual(4, g.count)
}

runAllTests()


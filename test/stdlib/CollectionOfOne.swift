// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let OneTests = TestSuite("CollectionOfOne")
OneTests.test("Basic tests") {
  let two = CollectionOfOne(2)
  expectEqual(1, two.count)
  for x in two {
    expectEqual(2, x)
  }

  let twentyOne = CollectionOfOne(21)
  expectEqual(1, twentyOne.count)
  for x in twentyOne.indices {
    expectEqual(42, twentyOne[x] * 2)
  }
}

runAllTests()

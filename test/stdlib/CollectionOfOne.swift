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

if #available(SwiftStdlib 6.3, *) {
  OneTests.test("Equatable") {
    checkEquatable(true, CollectionOfOne(1), CollectionOfOne(1))
    checkEquatable(false, CollectionOfOne("a"), CollectionOfOne("b"))
  }

  OneTests.test("Hashable") {
    let collections = [CollectionOfOne(1), CollectionOfOne(2)]
    checkHashable(collections, equalityOracle: { collections[$0] == collections[$1] })
  }
}

runAllTests()

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

if #available(SwiftStdlib 6.4, *) {
  OneTests.test("Equatable") {
    checkEquatable(true, CollectionOfOne(1), CollectionOfOne(1))
    checkEquatable(false, CollectionOfOne("a"), CollectionOfOne("b"))
  }

  OneTests.test("Hashable") {
    let collections = [CollectionOfOne(1), CollectionOfOne(2)]
    checkHashable(collections, equalityOracle: { collections[$0] == collections[$1] })
  }
}

OneTests.test("withContigousStorageIfAvailable")
.require(.stdlib_6_5).code {

  struct W: ~Copyable { let i: Int }

  let one = CollectionOfOne(42)
  let wrapped = one.withContiguousStorageIfAvailable({ W(i: $0.count + $0[0]) })
  if let count = expectNotNil(wrapped) {
    expectEqual(43, count.i)
  }
}

runAllTests()

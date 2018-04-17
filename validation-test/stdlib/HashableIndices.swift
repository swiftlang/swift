// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var HashTests = TestSuite("HashableIndices")

HashTests.test("ClosedRangeIndex") {
  let a = 1...10
  checkHashable(a.indices, equalityOracle: { $0 == $1 })
}

HashTests.test("FlattenIndex") {
  let a = [1...10, 11...20, 21...30].joined()
  checkHashable(a.indices, equalityOracle: { $0 == $1 })
}

HashTests.test("LazyDropWhileIndex") {
  let a = (1...10).lazy.drop(while: { $0 < 5 })
  checkHashable(a.indices, equalityOracle: { $0 == $1 })
}

HashTests.test("LazyPrefixWhileIndex") {
  let a = (1...10).lazy.prefix(while: { $0 < 5 })
  checkHashable(a.indices, equalityOracle: { $0 == $1 })
}

HashTests.test("ReversedIndex") {
  let a = (1...10).lazy.filter({ $0 > 3 }).reversed()
  checkHashable(a.indices, equalityOracle: { $0 == $1 })
}

HashTests.test("ReversedRandomAccessIndex") {
  let a = (1...10).reversed()
  checkHashable(a.indices, equalityOracle: { $0 == $1 })
}

runAllTests()

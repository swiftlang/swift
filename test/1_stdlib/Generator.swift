// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import StdlibUnittest
var tests = TestSuite("Iterator")

// Check to make sure we are actually getting Optionals out of this
// IteratorProtocol
tests.test("Range") {
  var w = (1..<2).generate()
  var maybe_one = w.next()
  expectType(Optional<Int>.self, &maybe_one)
  expectEqual(1, maybe_one)
  expectEmpty(w.next())
}

tests.test("RangeIteratorConformsToSequence") {
  for x in (1..<2).generate() { 
    expectEqual(1, x)
  }
}

// Test round-trip IteratorProtocol/IteratorProtocol adaptation
tests.test("IteratorSequence") {
  var r = 1..<7
  var x = MinimalIterator(Array(r))
  for a in IteratorSequence(x) {
    expectEqual(r.startIndex, a)
    ++r.startIndex
  }
  expectEqual(r.startIndex, r.endIndex)
}

struct MyIterator : IteratorProtocol {
  var i = 0
  mutating func next() -> Int? {
    return i < 10 ? i++ : nil
  }
}

extension MyIterator : SequenceType {}
tests.test("IteratorsModelSequenceTypeByDeclaration") {
  var n = 0
  for i in MyIterator() {
    expectEqual(n++, i)
  }
}

runAllTests()

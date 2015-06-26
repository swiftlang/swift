// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import StdlibUnittest
var tests = TestSuite("Generator")

// Check to make sure we are actually getting Optionals out of this
// GeneratorType
tests.test("Range") {
  var w = (1..<2).generate()
  var maybe_one = w.next()
  expectType(Optional<Int>.self, &maybe_one)
  expectEqual(1, maybe_one)
  expectEmpty(w.next())
}

tests.test("RangeGeneratorConformsToSequence") {
  for x in (1..<2).generate() { 
    expectEqual(1, x)
  }
}

// Test round-trip GeneratorType/GeneratorType adaptation
tests.test("GeneratorSequence") {
  var r = 1..<7
  var x = MinimalGenerator(Array(r))
  for a in GeneratorSequence(x) {
    expectEqual(r.startIndex, a)
    ++r.startIndex
  }
  expectEqual(r.startIndex, r.endIndex)
}

runAllTests()

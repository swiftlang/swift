// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

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
    r.startIndex = r.startIndex.successor()
  }
  expectEqual(r.startIndex, r.endIndex)
}

struct G : GeneratorType {
  var i = 0
  mutating func next() -> Int? {
    if i >= 10 { return nil }
    i += 1
    return i-1
  }
}

extension G : SequenceType {}
tests.test("GeneratorsModelSequenceTypeByDeclaration") {
  var n = 0
  for i in G() {
    expectEqual(n, i)
    n += 1
  }
}

runAllTests()

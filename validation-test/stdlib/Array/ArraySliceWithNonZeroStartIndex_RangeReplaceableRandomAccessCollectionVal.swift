//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Array/Inputs/ArrayConformanceTests.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest


let tests = TestSuite("ArraySliceWithNonZeroStartIndex_RangeReplaceableRandomAccessCollectionVal")


func ArraySliceWithNonZeroStartIndex<T>(_ elements: [T]) -> ArraySlice<T> {
  var r = ArraySlice<T>(_startIndex: 1000)
  r.append(contentsOf: elements)
  expectEqual(1000, r.startIndex)
  return r
}

do {
  var resiliencyChecks = CollectionMisuseResiliencyChecks.all
  resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .none


  // Test RangeReplaceableCollectionType conformance with value type elements.
  tests.addRangeReplaceableRandomAccessSliceTests(
    "ArraySliceWithNonZeroStartIndex.",
    makeCollection: { (elements: [OpaqueValue<Int>]) in
      return ArraySliceWithNonZeroStartIndex(elements)
    },
    wrapValue: identity,
    extractValue: identity,
    makeCollectionOfEquatable: { (elements: [MinimalEquatableValue]) in
      return ArraySliceWithNonZeroStartIndex(elements)
    },
    wrapValueIntoEquatable: identityEq,
    extractValueFromEquatable: identityEq,
    resiliencyChecks: resiliencyChecks)


} // do

runAllTests()


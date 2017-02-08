//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Array/Inputs/ArrayConformanceTests.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest


let tests = TestSuite("ArraySliceWithNonZeroStartIndex_MutableRandomAccessCollectionVal")


func ArraySliceWithNonZeroStartIndex<T>(_ elements: [T]) -> ArraySlice<T> {
  var r = ArraySlice<T>(_startIndex: 1000)
  r.append(contentsOf: elements)
  expectEqual(1000, r.startIndex)
  return r
}

do {
  var resiliencyChecks = CollectionMisuseResiliencyChecks.all
  resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .none


  // Test MutableCollectionType conformance with value type elements.
  tests.addMutableRandomAccessCollectionTests(
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
    makeCollectionOfComparable: { (elements: [MinimalComparableValue]) in
      return ArraySliceWithNonZeroStartIndex(elements)
    },
    wrapValueIntoComparable: identityComp,
    extractValueFromComparable: identityComp,
    resiliencyChecks: resiliencyChecks,
    withUnsafeMutableBufferPointerIsSupported: true,
    isFixedLengthCollection: false)


} // do

runAllTests()


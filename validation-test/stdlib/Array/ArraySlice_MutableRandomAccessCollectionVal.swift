//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Array/Inputs/ArrayConformanceTests.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest


let tests = TestSuite("ArraySlice_MutableRandomAccessCollectionVal")



do {
  var resiliencyChecks = CollectionMisuseResiliencyChecks.all
  resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .none


  // Test MutableCollectionType conformance with value type elements.
  tests.addMutableRandomAccessCollectionTests(
    "ArraySlice.",
    makeCollection: { (elements: [OpaqueValue<Int>]) in
      return ArraySlice(elements)
    },
    wrapValue: identity,
    extractValue: identity,
    makeCollectionOfEquatable: { (elements: [MinimalEquatableValue]) in
      return ArraySlice(elements)
    },
    wrapValueIntoEquatable: identityEq,
    extractValueFromEquatable: identityEq,
    makeCollectionOfComparable: { (elements: [MinimalComparableValue]) in
      return ArraySlice(elements)
    },
    wrapValueIntoComparable: identityComp,
    extractValueFromComparable: identityComp,
    resiliencyChecks: resiliencyChecks,
    withUnsafeMutableBufferPointerIsSupported: true,
    isFixedLengthCollection: false)


} // do

runAllTests()


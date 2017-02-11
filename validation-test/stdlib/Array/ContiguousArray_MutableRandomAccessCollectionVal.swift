//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Array/Inputs/ArrayConformanceTests.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest


let tests = TestSuite("ContiguousArray_MutableRandomAccessCollectionVal")



do {
  var resiliencyChecks = CollectionMisuseResiliencyChecks.all
  resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .none


  // Test MutableCollectionType conformance with value type elements.
  tests.addMutableRandomAccessCollectionTests(
    "ContiguousArray.",
    makeCollection: { (elements: [OpaqueValue<Int>]) in
      return ContiguousArray(elements)
    },
    wrapValue: identity,
    extractValue: identity,
    makeCollectionOfEquatable: { (elements: [MinimalEquatableValue]) in
      return ContiguousArray(elements)
    },
    wrapValueIntoEquatable: identityEq,
    extractValueFromEquatable: identityEq,
    makeCollectionOfComparable: { (elements: [MinimalComparableValue]) in
      return ContiguousArray(elements)
    },
    wrapValueIntoComparable: identityComp,
    extractValueFromComparable: identityComp,
    resiliencyChecks: resiliencyChecks,
    withUnsafeMutableBufferPointerIsSupported: true,
    isFixedLengthCollection: false)


} // do

runAllTests()


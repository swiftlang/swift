//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Array/Inputs/ArrayConformanceTests.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest


let tests = TestSuite("ArraySlice_RangeReplaceableRandomAccessCollectionVal")



do {
  var resiliencyChecks = CollectionMisuseResiliencyChecks.all
  resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .none


  // Test RangeReplaceableCollectionType conformance with value type elements.
  tests.addRangeReplaceableRandomAccessSliceTests(
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
    resiliencyChecks: resiliencyChecks)


} // do

runAllTests()


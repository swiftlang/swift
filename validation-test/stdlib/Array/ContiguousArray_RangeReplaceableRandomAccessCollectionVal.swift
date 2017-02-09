//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Array/Inputs/ArrayConformanceTests.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest


let tests = TestSuite("ContiguousArray_RangeReplaceableRandomAccessCollectionVal")



do {
  var resiliencyChecks = CollectionMisuseResiliencyChecks.all
  resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .none


  // Test RangeReplaceableCollectionType conformance with value type elements.
  tests.addRangeReplaceableRandomAccessCollectionTests(
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
    resiliencyChecks: resiliencyChecks)


} // do

runAllTests()


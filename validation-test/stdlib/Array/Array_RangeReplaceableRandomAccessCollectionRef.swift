//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Array/Inputs/ArrayConformanceTests.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest


let tests = TestSuite("Array_RangeReplaceableRandomAccessCollectionRef")



do {
  var resiliencyChecks = CollectionMisuseResiliencyChecks.all
  resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .none


  // Test RangeReplaceableCollectionType conformance with reference type elements.
  tests.addRangeReplaceableRandomAccessCollectionTests(
    "Array.",
    makeCollection: { (elements: [LifetimeTracked]) in
      return Array(elements)
    },
    wrapValue: { (element: OpaqueValue<Int>) in LifetimeTracked(element.value) },
    extractValue: { (element: LifetimeTracked) in OpaqueValue(element.value) },
    makeCollectionOfEquatable: { (elements: [MinimalEquatableValue]) in
      // FIXME: use LifetimeTracked.
      return Array(elements)
    },
    wrapValueIntoEquatable: identityEq,
    extractValueFromEquatable: identityEq,
    resiliencyChecks: resiliencyChecks)


} // do

runAllTests()


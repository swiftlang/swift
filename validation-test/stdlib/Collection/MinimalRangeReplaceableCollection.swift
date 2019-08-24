// -*- swift -*-

//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Collection/Inputs/Template.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// FIXME: the test is too slow when the standard library is not optimized.
// rdar://problem/46878013
// REQUIRES: optimized_stdlib

import StdlibUnittest
import StdlibCollectionUnittest

var CollectionTests = TestSuite("Collection")

// Test collections using value types as elements.
do {
  var resiliencyChecks = CollectionMisuseResiliencyChecks.all
  resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .trap

  CollectionTests.addRangeReplaceableCollectionTests(
    makeCollection: { (elements: [OpaqueValue<Int>]) in
      return MinimalRangeReplaceableCollection(elements: elements)
    },
    wrapValue: identity,
    extractValue: identity,
    makeCollectionOfEquatable: { (elements: [MinimalEquatableValue]) in
      return MinimalRangeReplaceableCollection(elements: elements)
    },
    wrapValueIntoEquatable: identityEq,
    extractValueFromEquatable: identityEq,
    resiliencyChecks: resiliencyChecks
  )
}

runAllTests()



// -*- swift -*-

//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Collection/Inputs/LazyMapTemplate.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

var CollectionTests = TestSuite("Collection")

// Test collections using value types as elements.
CollectionTests.addSequenceTests(
  makeSequence: { (elements: [OpaqueValue<Int>]) -> LazyMapSequence<MinimalSequence<OpaqueValue<Int>>, OpaqueValue<Int>> in
    MinimalSequence(elements: elements).lazy.map(identity)
  },
  wrapValue: identity,
  extractValue: identity,
  makeSequenceOfEquatable: { (elements: [MinimalEquatableValue]) -> LazyMapSequence<MinimalSequence<MinimalEquatableValue>, MinimalEquatableValue> in
    MinimalSequence(elements: elements).lazy.map(identityEq)
  },
  wrapValueIntoEquatable: identityEq,
  extractValueFromEquatable: identityEq
)

// Test collections using reference types as elements.
CollectionTests.addSequenceTests(
  makeSequence: { (elements: [LifetimeTracked]) -> LazyMapSequence<MinimalSequence<LifetimeTracked>, LifetimeTracked> in
    MinimalSequence(elements: elements).lazy.map { $0 }
  },
  wrapValue: { (element: OpaqueValue<Int>) in
    LifetimeTracked(element.value, identity: element.identity)
  },
  extractValue: { (element: LifetimeTracked) in
    OpaqueValue(element.value, identity: element.identity)
  },
  makeSequenceOfEquatable: { (elements: [LifetimeTracked]) -> LazyMapSequence<MinimalSequence<LifetimeTracked>, LifetimeTracked> in
    MinimalSequence(elements: elements).lazy.map { $0 }
  },
  wrapValueIntoEquatable: { (element: MinimalEquatableValue) in
    LifetimeTracked(element.value, identity: element.identity)
  },
  extractValueFromEquatable: { (element: LifetimeTracked) in
    MinimalEquatableValue(element.value, identity: element.identity)
  }
)

// Test sequence instances and iterators.
CollectionTests.test("LazyMapCollection instances (Sequence)") {
  do {
    let expected = ["convent", "conform", "constrict", "condone"]
    let base = ["vent", "form", "strict", "done"]
    checkSequence(
      expected,
      MinimalSequence(elements: base).lazy.map { "con" + $0 })
  }
  do {
    let expected = [1, 4, 9, 16, 25, 36, 49, 64]
    let base = [1, 2, 3, 4, 5, 6, 7, 8]
    checkSequence(
      expected,
      MinimalSequence(elements: base).lazy.map { $0 * $0 })
  }
}

runAllTests()


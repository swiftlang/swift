// -*- swift -*-

//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Collection/Inputs/Template.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %S/../../../utils/gyb %s -o %t/main.swift
// RUN: %S/../../../utils/line-directive %t/main.swift -- %target-build-swift %t/main.swift -o %t/DefaultedMutableRangeReplaceableBidirectionalCollection.swift.a.out
// RUN: %S/../../../utils/line-directive %t/main.swift -- %target-run %t/DefaultedMutableRangeReplaceableBidirectionalCollection.swift.a.out
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

var CollectionTests = TestSuite("Collection")

// Test collections using value types as elements.
do {
  var resiliencyChecks = CollectionMisuseResiliencyChecks.all
  resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .trap

  CollectionTests.addRangeReplaceableBidirectionalCollectionTests(
    makeCollection: { (elements: [OpaqueValue<Int>]) in
      return DefaultedMutableRangeReplaceableBidirectionalCollection(elements: elements)
    },
    wrapValue: identity,
    extractValue: identity,
    makeCollectionOfEquatable: { (elements: [MinimalEquatableValue]) in
      return DefaultedMutableRangeReplaceableBidirectionalCollection(elements: elements)
    },
    wrapValueIntoEquatable: identityEq,
    extractValueFromEquatable: identityEq,
    resiliencyChecks: resiliencyChecks
  )
  CollectionTests.addMutableBidirectionalCollectionTests(
    makeCollection: { (elements: [OpaqueValue<Int>]) in
      return DefaultedMutableRangeReplaceableBidirectionalCollection(elements: elements)
    },
    wrapValue: identity,
    extractValue: identity,
    makeCollectionOfEquatable: { (elements: [MinimalEquatableValue]) in
      return DefaultedMutableRangeReplaceableBidirectionalCollection(elements: elements)
    },
    wrapValueIntoEquatable: identityEq,
    extractValueFromEquatable: identityEq,
    makeCollectionOfComparable: { (elements: [MinimalComparableValue]) in
      return DefaultedMutableRangeReplaceableBidirectionalCollection(elements: elements)
    },
    wrapValueIntoComparable: identityComp,
    extractValueFromComparable: identityComp,
    resiliencyChecks: resiliencyChecks
    , withUnsafeMutableBufferPointerIsSupported: false,
    isFixedLengthCollection: true
  )
}

// Test collections using a reference type as element.
do {
  var resiliencyChecks = CollectionMisuseResiliencyChecks.all
  resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .trap

  CollectionTests.addRangeReplaceableBidirectionalCollectionTests(
    makeCollection: { (elements: [LifetimeTracked]) in
      return DefaultedMutableRangeReplaceableBidirectionalCollection(elements: elements)
    },
    wrapValue: { (element: OpaqueValue<Int>) in
      LifetimeTracked(element.value, identity: element.identity)
    },
    extractValue: { (element: LifetimeTracked) in
      OpaqueValue(element.value, identity: element.identity)
    },
    makeCollectionOfEquatable: { (elements: [MinimalEquatableValue]) in
      // FIXME: use LifetimeTracked.
      return DefaultedMutableRangeReplaceableBidirectionalCollection(elements: elements)
    },
    wrapValueIntoEquatable: identityEq,
    extractValueFromEquatable: identityEq,
    resiliencyChecks: resiliencyChecks
  )
  CollectionTests.addMutableBidirectionalCollectionTests(
    makeCollection: { (elements: [LifetimeTracked]) in
      return DefaultedMutableRangeReplaceableBidirectionalCollection(elements: elements)
    },
    wrapValue: { (element: OpaqueValue<Int>) in
      LifetimeTracked(element.value, identity: element.identity)
    },
    extractValue: { (element: LifetimeTracked) in
      OpaqueValue(element.value, identity: element.identity)
    },
    makeCollectionOfEquatable: { (elements: [MinimalEquatableValue]) in
      // FIXME: use LifetimeTracked.
      return DefaultedMutableRangeReplaceableBidirectionalCollection(elements: elements)
    },
    wrapValueIntoEquatable: identityEq,
    extractValueFromEquatable: identityEq,
    makeCollectionOfComparable: { (elements: [MinimalComparableValue]) in
      // FIXME: use LifetimeTracked.
      return DefaultedMutableRangeReplaceableBidirectionalCollection(elements: elements)
    },
    wrapValueIntoComparable: identityComp,
    extractValueFromComparable: identityComp,
    resiliencyChecks: resiliencyChecks
    , withUnsafeMutableBufferPointerIsSupported: false,
    isFixedLengthCollection: true
  )
}

runAllTests()

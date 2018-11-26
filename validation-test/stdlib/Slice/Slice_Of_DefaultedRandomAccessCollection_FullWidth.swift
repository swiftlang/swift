// -*- swift -*-

//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Slice/Inputs/Template.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// FIXME: the test is too slow when the standard library is not optimized.
// REQUIRES: optimized_stdlib

import StdlibUnittest
import StdlibCollectionUnittest

var SliceTests = TestSuite("Collection")

let prefix: [Int] = []
let suffix: [Int] = []

func makeCollection(elements: [OpaqueValue<Int>])
  -> Slice<DefaultedRandomAccessCollection<OpaqueValue<Int>>> {
  var baseElements = prefix.map(OpaqueValue.init)
  baseElements.append(contentsOf: elements)
  baseElements.append(contentsOf: suffix.map(OpaqueValue.init))
  let base = DefaultedRandomAccessCollection(elements: baseElements)
  let startIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count))
  let endIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count + elements.count))
  return Slice(base: base, bounds: startIndex..<endIndex)
}

func makeCollectionOfEquatable(elements: [MinimalEquatableValue])
  -> Slice<DefaultedRandomAccessCollection<MinimalEquatableValue>> {
  var baseElements = prefix.map(MinimalEquatableValue.init)
  baseElements.append(contentsOf: elements)
  baseElements.append(contentsOf: suffix.map(MinimalEquatableValue.init))
  let base = DefaultedRandomAccessCollection(elements: baseElements)
  let startIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count))
  let endIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count + elements.count))
  return Slice(base: base, bounds: startIndex..<endIndex)
}

func makeCollectionOfComparable(elements: [MinimalComparableValue])
  -> Slice<DefaultedRandomAccessCollection<MinimalComparableValue>> {
  var baseElements = prefix.map(MinimalComparableValue.init)
  baseElements.append(contentsOf: elements)
  baseElements.append(contentsOf: suffix.map(MinimalComparableValue.init))
  let base = DefaultedRandomAccessCollection(elements: baseElements)
  let startIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count))
  let endIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count + elements.count))
  return Slice(base: base, bounds: startIndex..<endIndex)
}

var resiliencyChecks = CollectionMisuseResiliencyChecks.all
resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .trap
resiliencyChecks.subscriptOnOutOfBoundsIndicesBehavior = .trap
resiliencyChecks.subscriptRangeOnOutOfBoundsRangesBehavior = .trap

SliceTests.addRandomAccessCollectionTests(
  "Slice_Of_DefaultedRandomAccessCollection_FullWidth.swift.",
  makeCollection: makeCollection,
  wrapValue: identity,
  extractValue: identity,
  makeCollectionOfEquatable: makeCollectionOfEquatable,
  wrapValueIntoEquatable: identityEq,
  extractValueFromEquatable: identityEq,
  resiliencyChecks: resiliencyChecks,
  outOfBoundsIndexOffset: 6
)

runAllTests()

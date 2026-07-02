// -*- swift -*-

//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Slice/Inputs/Template.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

import StdlibUnittest
import StdlibCollectionUnittest

var SliceTests = TestSuite("Collection")

let prefix: [Int] = []
let suffix: [Int] = [ -9999, -9998, -9997]

func makeCollection(elements: [OpaqueValue<Int>])
  -> Slice<MinimalMutableBidirectionalCollection<OpaqueValue<Int>>> {
  var baseElements = prefix.map(OpaqueValue.init)
  baseElements.append(contentsOf: elements)
  baseElements.append(contentsOf: suffix.map(OpaqueValue.init))
  let base = MinimalMutableBidirectionalCollection(elements: baseElements)
  let startIndex = base.index(
    base.startIndex,
    offsetBy: prefix.count)
  let endIndex = base.index(
    base.startIndex,
    offsetBy: prefix.count + elements.count)
  return Slice(base: base, bounds: startIndex..<endIndex)
}

func makeCollectionOfEquatable(elements: [MinimalEquatableValue])
  -> Slice<MinimalMutableBidirectionalCollection<MinimalEquatableValue>> {
  var baseElements = prefix.map(MinimalEquatableValue.init)
  baseElements.append(contentsOf: elements)
  baseElements.append(contentsOf: suffix.map(MinimalEquatableValue.init))
  let base = MinimalMutableBidirectionalCollection(elements: baseElements)
  let startIndex = base.index(
    base.startIndex,
    offsetBy: prefix.count)
  let endIndex = base.index(
    base.startIndex,
    offsetBy: prefix.count + elements.count)
  return Slice(base: base, bounds: startIndex..<endIndex)
}

func makeCollectionOfComparable(elements: [MinimalComparableValue])
  -> Slice<MinimalMutableBidirectionalCollection<MinimalComparableValue>> {
  var baseElements = prefix.map(MinimalComparableValue.init)
  baseElements.append(contentsOf: elements)
  baseElements.append(contentsOf: suffix.map(MinimalComparableValue.init))
  let base = MinimalMutableBidirectionalCollection(elements: baseElements)
  let startIndex = base.index(
    base.startIndex,
    offsetBy: prefix.count)
  let endIndex = base.index(
    base.startIndex,
    offsetBy: prefix.count + elements.count)
  return Slice(base: base, bounds: startIndex..<endIndex)
}

var resiliencyChecks = CollectionMisuseResiliencyChecks.all
resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .trap
resiliencyChecks.subscriptOnOutOfBoundsIndicesBehavior = .trap
resiliencyChecks.subscriptRangeOnOutOfBoundsRangesBehavior = .trap

SliceTests.addMutableBidirectionalCollectionTests(
  "Slice_Of_MinimalMutableBidirectionalCollection_WithSuffix.swift.",
  makeCollection: makeCollection,
  wrapValue: identity,
  extractValue: identity,
  makeCollectionOfEquatable: makeCollectionOfEquatable,
  wrapValueIntoEquatable: identityEq,
  extractValueFromEquatable: identityEq,
  makeCollectionOfComparable: makeCollectionOfComparable,
  wrapValueIntoComparable: identityComp,
  extractValueFromComparable: identityComp,
  resiliencyChecks: resiliencyChecks,
  outOfBoundsIndexOffset: 6
  , withUnsafeMutableBufferPointerIsSupported: false,
  isFixedLengthCollection: true
)

runAllTests()

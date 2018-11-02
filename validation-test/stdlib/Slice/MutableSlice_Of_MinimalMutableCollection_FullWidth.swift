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
  -> MutableSlice<MinimalMutableCollection<OpaqueValue<Int>>> {
  var baseElements = prefix.map(OpaqueValue.init)
  baseElements.append(contentsOf: elements)
  baseElements.append(contentsOf: suffix.map(OpaqueValue.init))
  let base = MinimalMutableCollection(elements: baseElements)
  let startIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count))
  let endIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count + elements.count))
  return MutableSlice(
    base: base,
    bounds: startIndex..<endIndex)
}

func makeCollectionOfEquatable(elements: [MinimalEquatableValue])
  -> MutableSlice<MinimalMutableCollection<MinimalEquatableValue>> {
  var baseElements = prefix.map(MinimalEquatableValue.init)
  baseElements.append(contentsOf: elements)
  baseElements.append(contentsOf: suffix.map(MinimalEquatableValue.init))
  let base = MinimalMutableCollection(elements: baseElements)
  let startIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count))
  let endIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count + elements.count))
  return MutableSlice(
    base: base,
    bounds: startIndex..<endIndex)
}

func makeCollectionOfComparable(elements: [MinimalComparableValue])
  -> MutableSlice<MinimalMutableCollection<MinimalComparableValue>> {
  var baseElements = prefix.map(MinimalComparableValue.init)
  baseElements.append(contentsOf: elements)
  baseElements.append(contentsOf: suffix.map(MinimalComparableValue.init))
  let base = MinimalMutableCollection(elements: baseElements)
  let startIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count))
  let endIndex = base.index(
    base.startIndex,
    offsetBy: numericCast(prefix.count + elements.count))
  return MutableSlice(
    base: base,
    bounds: startIndex..<endIndex)
}

var resiliencyChecks = CollectionMisuseResiliencyChecks.all
resiliencyChecks.creatingOutOfBoundsIndicesBehavior = .trap
resiliencyChecks.subscriptOnOutOfBoundsIndicesBehavior = .trap
resiliencyChecks.subscriptRangeOnOutOfBoundsRangesBehavior = .trap

SliceTests.addMutableCollectionTests(
  "MutableSlice_Of_MinimalMutableCollection_FullWidth.swift.",
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

// rdar://problem/35760754

func f<C : MutableCollection>(
  _ elements: inout C
) { }

var CrashTests = TestSuite("MutableSliceCrash")

extension TestSuite {
  public func addMyMutableCollectionTests<C : MutableCollection>(
    _ testNamePrefix: String = "",
    maker: @escaping ([C.Element]) -> C
  ) {

    // this runs just fine
    self.test("\(testNamePrefix).Direct") {
      var c = makeCollection(elements: [])
      f(&c[c.startIndex..<c.endIndex])
    }

    // this used to crash, even though the only difference is it's calling
    // makeCollection via the argument to the original function
    self.test("\(testNamePrefix).Indirect") {
      var c = maker([])
      f(&c[c.startIndex..<c.endIndex])
    }
  }
}

CrashTests.addMyMutableCollectionTests(
  "Crasher",
  maker: makeCollection
)

runAllTests()

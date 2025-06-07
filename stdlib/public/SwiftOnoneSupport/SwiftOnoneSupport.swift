//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Pre-specialization of some popular generic classes and functions.
//===----------------------------------------------------------------------===//
import Swift

// =============================================================================
// Definitions of proxy functions that mimic a generic function signature in the
// standard library and are annotated with the standard library's
// actual generic function name. The "prespecialize" annotation forces
// the actual generic function to be specialized based on the argument
// types passed to the proxy function.
// =============================================================================

extension Collection {
  // _failEarlyRangeCheck(_: A.Index, bounds: Swift.Range<A.Index>) -> ()
  @_semantics("prespecialize.$sSlsE20_failEarlyRangeCheck_6boundsy5IndexQz_SnyADGtF")
  func _prespecializeCollection(index: Index, range: Range<Index>) {}
}

extension Collection where Iterator == IndexingIterator<Self> {
  // makeIterator() -> Swift.IndexingIterator<A>
  @_semantics("prespecialize.$sSlss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyF")
  func _prespecializeIndexingIterator() {}
}

extension BidirectionalCollection {
  // reversed() -> ReversedCollection<A>
  @_semantics("prespecialize.$sSKsE8reverseds18ReversedCollectionVyxGyF")
  func _prespecializeBidirectionalCollection() {}
}

extension MutableCollection where Self: BidirectionalCollection {
  // _reverse(within: Swift.Range<A.Index>) -> ()
  @_semantics("prespecialize.$sSMsSKRzrlE8_reverse6withinySny5IndexSlQzG_tF")
  mutating func _prespecializeMutableBirectionalCollection(range: Range<Index>) {}

  // _insertionSort(within: Swift.Range<A.Index>,
  //    by: (A.Element, A.Element
  //  ) throws -> Swift.Bool) throws -> ()
  @_semantics("prespecialize.$sSMsSKRzrlE14_insertionSort6within2byySny5IndexSlQzG_Sb7ElementSTQz_AHtKXEtKF")
  mutating func _prespecializeMutableBirectionalCollection(range: Range<Index>, cmp: (Element, Element) throws -> Bool) {}

  // _insertionSort(
  //    within: Swift.Range<A.Index>,
  //    sortedEnd: A.Index,
  //    by: (A.Element, A.Element) throws -> Swift.Bool
  //  ) throws -> ()
  @_semantics("prespecialize.$sSMsSKRzrlE14_insertionSort6within9sortedEnd2byySny5IndexSlQzG_AFSb7ElementSTQz_AItKXEtKF")
  mutating func _prespecializeMutableBirectionalCollection(range: Range<Index>, end: Index, cmp: (Element, Element) throws -> Bool) {}
} // extension MutableCollection where Self: BidirectionalCollection

extension MutableCollection where Self: RandomAccessCollection {
  // sort(by: (A.Element, A.Element) throws -> Swift.Bool) throws -> ()
  @_semantics("prespecialize.$sSMsSkRzrlE4sort2byySb7ElementSTQz_ADtKXE_tKF")
  mutating func _prespecializeMutableRandomAccessCollection(cmp: (Element, Element) throws -> Bool) throws {}
}

extension RandomAccessCollection where Index : Strideable, Index.Stride == Int {
  // index(after: A.Index) -> A.Index
  @_semantics("prespecialize.$sSksSx5IndexRpzSnyABG7IndicesRtzSiAA_6StrideRTzrlE5index5afterA2B_tF")
  func _prespecializeRandomAccessCollection(after: Index) {}

  // indices.getter : Swift.Range<A.Index>
  @_semantics("prespecialize.$sSksSx5IndexRpzSnyABG7IndicesRtzSiAA_6StrideRTzrlE7indicesACvg")
  func _prespecializeRandomAccessCollection() {}
}

// _allocateUninitializedArray<A>(Builtin.Word) -> ([A], Builtin.RawPointer)
@_semantics("prespecialize.$ss27_allocateUninitializedArrayySayxG_BptBwlF")
func _prespecializeArray<T>(_ word: Builtin.Word) -> ([T], Builtin.RawPointer) {
  return ([], Builtin.inttoptr_Word(word))
}

extension Array {
  // init() -> [A]
  @_semantics("prespecialize.$sS2ayxGycfC")
  // startIndex.getter : Swift.Int
  @_semantics("prespecialize.$sSa10startIndexSivg")
  // _getCapacity() -> Swift.Int
  @_semantics("prespecialize.$sSa12_getCapacitySiyF")
  // _makeMutableAndUnique() -> ()
  @_semantics("prespecialize.$sSa21_makeMutableAndUniqueyyF")
  // _copyToContiguousArray() -> Swift.ContiguousArray<A>
  @_semantics("prespecialize.$sSa22_copyToContiguousArrays0cD0VyxGyF")
  // _hoistableIsNativeTypeChecked() -> Swift.Bool
  @_semantics("prespecialize.$sSa29_hoistableIsNativeTypeCheckedSbyF")
  // count.getter : Swift.Int
  @_semantics("prespecialize.$sSa5countSivg")
  // capacity.getter : Swift.Int
  @_semantics("prespecialize.$sSa8capacitySivg")
  // endIndex.getter : Swift.Int
  @_semantics("prespecialize.$sSa8endIndexSivg")
  // formIndex(before: inout Swift.Int) -> ()
  @_semantics("prespecialize.$sSa9formIndex6beforeySiz_tF")
  func _prespecializeArray() {}

  // _makeUniqueAndReserveCapacityIfNotUnique() -> ()
  @_semantics("prespecialize.$sSa034_makeUniqueAndReserveCapacityIfNotB0yyF")
  func _prespecializeMutableArray() {}

  // _checkSubscript(_: Swift.Int, wasNativeTypeChecked: Swift.Bool) -> Swift._DependenceToken
  @_semantics("prespecialize.$sSa15_checkSubscript_20wasNativeTypeCheckeds16_DependenceTokenVSi_SbtF")
  func _prespecializeArray(index: Int, flag: Bool) {}

  // _getElement(_: Swift.Int, wasNativeTypeChecked: Swift.Bool, matchingSubscriptCheck: Swift._DependenceToken) -> A
  @_semantics("prespecialize.$sSa11_getElement_20wasNativeTypeChecked22matchingSubscriptCheckxSi_Sbs16_DependenceTokenVtF")
  func _prespecializeArray(index: Int, flag: Bool, token: _DependenceToken) {}

  // init(arrayLiteral: A...) -> [A]
  @_semantics("prespecialize.$sSa12arrayLiteralSayxGxd_tcfC")
  func _prespecializeArray(arrayLiteral: Element...) {}

  // init(_unsafeUninitializedCapacity: Swift.Int, initializingWith: (inout Swift.UnsafeMutableBufferPointer<A>, inout Swift.Int) throws -> ()) throws -> [A]
  @_semantics("prespecialize.$sSa28_unsafeUninitializedCapacity16initializingWithSayxGSi_ySryxGz_SiztKXEtKcfC")
  func _prespecializeArray(capacity: Int, generator: (inout UnsafeMutableBufferPointer<Element>, inout Int) throws -> ()) {}

  // removeAll(keepingCapacity: Swift.Bool) -> ()
  @_semantics("prespecialize.$sSa9removeAll15keepingCapacityySb_tF")
  // default argument 0 of Swift.Array.removeAll(keepingCapacity: Swift.Bool) -> ()
  @_semantics("prespecialize.$sSa9removeAll15keepingCapacityySb_tFfA_")
  func _prespecializeArray(flag: Bool) {}

  // init(_uninitializedCount: Swift.Int) -> [A]
  @_semantics("prespecialize.$sSa19_uninitializedCountSayxGSi_tcfC")
  // _reserveCapacityAssumingUniqueBuffer(oldCount: Swift.Int) -> ()
  @_semantics("prespecialize.$sSa36_reserveCapacityAssumingUniqueBuffer8oldCountySi_tF")
  // reserveCapacity(Swift.Int) -> ()
  @_semantics("prespecialize.$sSa15reserveCapacityyySiF")
  // _copyToNewBuffer(oldCount: Swift.Int) -> ()
  @_semantics("prespecialize.$sSa16_copyToNewBuffer8oldCountySi_tF")
  // _getCount() -> Swift.Int
  @_semantics("prespecialize.$sSa9_getCountSiyF")
  // formIndex(after: inout Swift.Int) -> ()
  @_semantics("prespecialize.$sSa9formIndex5afterySiz_tF")
  // subscript.modify : (Swift.Int) -> A
  @_semantics("prespecialize.$sSayxSiciM")
  // subscript.getter : (Swift.Int) -> A
  @_semantics("prespecialize.$sSayxSicig")
  // subscript.read : (Swift.Int) -> A
  @_semantics("prespecialize.$sSayxSicir")
  func _prespecializeArray(index: Int) {}

  // _appendElementAssumeUniqueAndCapacity(_: Swift.Int, newElement: __owned A) -> ()
  @_semantics("prespecialize.$sSa37_appendElementAssumeUniqueAndCapacity_03newB0ySi_xntF")
  func _prespecializeArray(index: Int, element: Element) {}

  // append(__owned A) -> ()
  @_semantics("prespecialize.$sSa6appendyyxnF")
  // init(repeating: A, count: Swift.Int) -> [A]
  @_semantics("prespecialize.$sSa9repeating5countSayxGx_SitcfC")
  func _prespecializeArray(element: Element, index: Int) {}

  // replaceSubrange<A where A == A1.Element, A1: Swift.Collection>(
  //   _: Swift.Range<Swift.Int>, with: __owned A1
  // ) -> ()
  @_semantics("prespecialize.$sSa15replaceSubrange_4withySnySiG_qd__nt7ElementQyd__RszSlRd__lF")
  func _prespecializeArray<C: Collection>(range: Range<C.Index>, collection: C) where Element == C.Element {}

  // _withUnsafeMutableBufferPointerIfSupported<A>(
  //   (inout Swift.UnsafeMutableBufferPointer<A>) throws -> A1
  // ) throws -> A1?
  @_semantics("prespecialize.$sSa42_withUnsafeMutableBufferPointerIfSupportedyqd__Sgqd__SryxGzKXEKlF")
  func _prespecializeArray<R>(with: (inout UnsafeMutableBufferPointer<Element>) throws -> R) {}
} // extension Array

extension _ContiguousArrayBuffer {
  // startIndex.getter : Swift.Int
  @_semantics("prespecialize.$ss22_ContiguousArrayBufferV10startIndexSivg")
  // firstElementAddress.getter : Swift.UnsafeMutablePointer<A>
  @_semantics("prespecialize.$ss22_ContiguousArrayBufferV19firstElementAddressSpyxGvg")
  // count.getter : Swift.Int
  @_semantics("prespecialize.$ss22_ContiguousArrayBufferV7_buffer19shiftedToStartIndexAByxGAE_SitcfC")
  // endIndex.getter : Swift.Int
  @_semantics("prespecialize.$ss22_ContiguousArrayBufferV8endIndexSivg")
  // init() -> Swift._ContiguousArrayBuffer<A>
  @_semantics("prespecialize.$ss22_ContiguousArrayBufferVAByxGycfC")
  func _prespecializeContiguousArrayBuffer() {}

  // _copyContents(subRange: Swift.Range<Swift.Int>, initializing: Swift.UnsafeMutablePointer<A>) -> Swift.UnsafeMutablePointer<A>
  @_semantics("prespecialize.$ss22_ContiguousArrayBufferV13_copyContents8subRange12initializingSpyxGSnySiG_AFtF")
  func _prespecializeContiguousArrayBuffer(range: Range<Int>, pointer: UnsafeMutablePointer<Element>) {}

  // _initStorageHeader(count: Swift.Int, capacity: Swift.Int) -> ()
  @_semantics("prespecialize.$ss22_ContiguousArrayBufferV18_initStorageHeader5count8capacityySi_SitF")
  func _prespecializeContiguousArrayBuffer(count: Int, capacity: Int) {}

  @_semantics("prespecialize.$ss22_ContiguousArrayBufferV5countSivg")
  // init(_buffer: Swift._ContiguousArrayBuffer<A>, shiftedToStartIndex: Swift.Int) -> Swift._ContiguousArrayBuffer<A>
  func _prespecializeContiguousArrayBuffer(buffer: _ContiguousArrayBuffer<Element>, index: Int) {}
}

#if _runtime(_ObjC)
extension _ArrayBuffer {
  // requestNativeBuffer() -> Swift._ContiguousArrayBuffer<A>?
  @_semantics("prespecialize.$ss12_ArrayBufferV013requestNativeB0s011_ContiguousaB0VyxGSgyF")
  // _nonNative.getter : Swift._CocoaArrayWrapper
  @_semantics("prespecialize.$ss12_ArrayBufferV10_nonNatives06_CocoaA7WrapperVvg")
  // startIndex.getter : Swift.Int
  @_semantics("prespecialize.$ss12_ArrayBufferV10startIndexSivg")
  // firstElementAddress.getter : Swift.UnsafeMutablePointer<A>
  @_semantics("prespecialize.$ss12_ArrayBufferV19firstElementAddressSpyxGvg")
  // isUniquelyReferenced() -> Swift.Bool
  @_semantics("prespecialize.$ss12_ArrayBufferV20isUniquelyReferencedSbyF")
  // count.setter : Swift.Int
  @_semantics("prespecialize.$ss12_ArrayBufferV5countSivs")
  // _native.getter : Swift._ContiguousArrayBuffer<A>
  @_semantics("prespecialize.$ss12_ArrayBufferV7_natives011_ContiguousaB0VyxGvg")
  // _isNative.getter : Swift.Bool
  @_semantics("prespecialize.$ss12_ArrayBufferV9_isNativeSbvg")
  // capacity.getter : Swift.Int
  @_semantics("prespecialize.$ss12_ArrayBufferV8capacitySivg")
  // endIndex.getter : Swift.Int
  @_semantics("prespecialize.$ss12_ArrayBufferV8endIndexSivg")
  func _prespecializeArrayBuffer() {}

  // requestUniqueMutableBackingBuffer(minimumCapacity: Swift.Int) -> Swift._ContiguousArrayBuffer<A>?
  @_semantics("prespecialize.$ss12_ArrayBufferV027requestUniqueMutableBackingB015minimumCapacitys011_ContiguousaB0VyxGSgSi_tF")
  // _getElementSlowPath(Swift.Int) -> Swift.AnyObject
  @_semantics("prespecialize.$ss12_ArrayBufferV19_getElementSlowPathyyXlSiF")
  // subscript.getter : (Swift.Int) -> A
  @_semantics("prespecialize.$ss12_ArrayBufferVyxSicig")
  // subscript.read : (Swift.Int) -> A
  @_semantics("prespecialize.$ss12_ArrayBufferVyxSicir")
  func _prespecializeArrayBuffer(index: Int) {}

  // _typeCheck(Swift.Range<Swift.Int>) -> ()
  @_semantics("prespecialize.$ss12_ArrayBufferV10_typeCheckyySnySiGF")
  func _prespecializeArrayBuffer(range: Range<Int>) {}

  // _copyContents(subRange: Swift.Range<Swift.Int>, initializing: Swift.UnsafeMutablePointer<A>) -> Swift.UnsafeMutablePointer<A>
  @_semantics("prespecialize.$ss12_ArrayBufferV13_copyContents8subRange12initializingSpyxGSnySiG_AFtF")
  func _prespecializeArrayBuffer(range: Range<Int>, pointer: UnsafeMutablePointer<Element>) {}

  // _checkInoutAndNativeTypeCheckedBounds(_: Swift.Int, wasNativeTypeChecked: Swift.Bool) -> ()
  @_semantics("prespecialize.$ss12_ArrayBufferV37_checkInoutAndNativeTypeCheckedBounds_03wasfgH0ySi_SbtF")
  func _prespecializeArrayBuffer(index: Int, flag: Bool) {}

  // init(_buffer: Swift._ContiguousArrayBuffer<A>, shiftedToStartIndex: Swift.Int) -> Swift._ArrayBuffer<A>
  @_semantics("prespecialize.$ss12_ArrayBufferV7_buffer19shiftedToStartIndexAByxGs011_ContiguousaB0VyxG_SitcfC")
  func _prespecializeArrayBuffer(buffer: _ContiguousArrayBuffer<Element>, index: Int) {}
}
#endif // ObjC

extension Range {
  // contains(A) -> Swift.Bool
  @_semantics("prespecialize.$sSn8containsySbxF")
  func _prespecializeRange(bound: Bound) {}

  // init(uncheckedBounds: (lower: A, upper: A)) -> Swift.Range<A>
  @_semantics("prespecialize.$sSn15uncheckedBoundsSnyxGx5lower_x5uppert_tcfC")
  func _prespecializeRange(bounds: (lower: Bound, upper: Bound)) {}
}

extension Range where Bound: Strideable, Bound.Stride : SignedInteger {
  // startIndex.getter
  @_semantics("prespecialize.$sSnsSxRzSZ6StrideRpzrlE10startIndexxvg")
  // endIndex.getter
  @_semantics("prespecialize.$sSnsSxRzSZ6StrideRpzrlE8endIndexxvg")
  // index(after: A) -> A
  @_semantics("prespecialize.$sSnsSxRzSZ6StrideRpzrlE5index5afterxx_tF")
  // subscript.read
  @_semantics("prespecialize.$sSnsSxRzSZ6StrideRpzrlEyxxcir")
  func _prespecializeIntegerRange(bound: Bound) {}
}

extension ClosedRange {
  // init(uncheckedBounds: (lower: A, upper: A)) -> Swift.ClosedRange<A>
  @_semantics("prespecialize.$sSN15uncheckedBoundsSNyxGx5lower_x5uppert_tcfC")
  func _prespecializeClosedRange() {}
}

extension ClosedRange where Bound: Strideable, Bound.Stride : SignedInteger {
  // startIndex.getter
  @_semantics("prespecialize.$sSNsSxRzSZ6StrideRpzrlE10startIndexSNsSxRzSZABRQrlE0C0Oyx_Gvg")
  // endIndex.getter
  @_semantics("prespecialize.$sSNsSxRzSZ6StrideRpzrlE8endIndexSNsSxRzSZABRQrlE0C0Oyx_Gvg")
  // subscript.read
  @_semantics("prespecialize.$sSNsSxRzSZ6StrideRpzrlEyxSNsSxRzSZABRQrlE5IndexOyx_Gcir")
  func _prespecializeIntegerClosedRange() {}

  // index(after: ClosedRange<A>< where A: Swift.Strideable, A.Stride: Swift.SignedInteger>.Index)
  // -> ClosedRange<A>< where A: Swift.Strideable, A.Stride: Swift.SignedInteger>.Index
  @_semantics("prespecialize.$sSNsSxRzSZ6StrideRpzrlE5index5afterSNsSxRzSZABRQrlE5IndexOyx_GAG_tF")
  func _prespecializeIntegerClosedRange(range: Self) {}
}

// IndexingIterator.next() -> A.Element?
@_semantics("prespecialize.$ss16IndexingIteratorV4next7ElementQzSgyF")
func _prespecializeIndexingIterator<Elements>(_ x: IndexingIterator<Elements>) where Elements : Collection {}

// =============================================================================
// Helpers that construct arguments of the necessary specialized types,
// passing them to the above generic proxy functions.
// =============================================================================

func prespecializeCollections<T>(_ element: T) {
  var umbp = UnsafeMutableBufferPointer<T>.allocate(capacity: 1)
  let cmp = { (_: T, _: T) in return false }
  unsafe umbp._prespecializeMutableBirectionalCollection(range: 0..<0)
  unsafe umbp._prespecializeMutableBirectionalCollection(range: 0..<0, cmp: cmp)
  unsafe umbp._prespecializeMutableBirectionalCollection(range: 0..<0, end: 0, cmp: cmp)
  try! unsafe umbp._prespecializeMutableRandomAccessCollection(cmp: cmp)

  let _: (Array<T>, Builtin.RawPointer) = _prespecializeArray(0._builtinWordValue)

  var array = Array<T>()
  array._prespecializeArray()
  array._prespecializeMutableArray()
  array._prespecializeArray(index: 0, flag: false)
  array._prespecializeArray(index: 0, flag: false, token: _DependenceToken())
  array._prespecializeArray(arrayLiteral: element)
  unsafe array._prespecializeArray(capacity: 0) { (_: inout UnsafeMutableBufferPointer<T>, _: inout Int) in return }
  array._prespecializeArray(flag: false)
  array._prespecializeArray(index: 0)
  array._prespecializeArray(index: 0, element: element)
  array._prespecializeArray(element: element, index: 0)
  array._prespecializeArray(range: 0..<0, collection: EmptyCollection())
  unsafe array._prespecializeArray(with: { (_: inout UnsafeMutableBufferPointer<T>) -> Optional<()> in return () })
  array._prespecializeBidirectionalCollection()
  array._prespecializeRandomAccessCollection()
  try! array._prespecializeMutableRandomAccessCollection(cmp: cmp)

  let cab = _ContiguousArrayBuffer<T>()
  cab._prespecializeContiguousArrayBuffer()
  unsafe cab._prespecializeContiguousArrayBuffer(range: (0..<0), pointer: umbp.baseAddress!)
  cab._prespecializeContiguousArrayBuffer(count: 0, capacity: 0)
  cab._prespecializeContiguousArrayBuffer(buffer: cab, index: 0)

#if _runtime(_ObjC)
  let ab = _ArrayBuffer<T>()
  ab._prespecializeArrayBuffer()
  ab._prespecializeArrayBuffer(index: 0)
  ab._prespecializeArrayBuffer(range: (0..<0))
  unsafe ab._prespecializeArrayBuffer(range: (0..<0), pointer: umbp.baseAddress!)
  ab._prespecializeArrayBuffer(index: 0, flag: false)
  ab._prespecializeArrayBuffer(buffer: cab, index: 0)
  ab._prespecializeRandomAccessCollection(after: 0)
  ab._prespecializeRandomAccessCollection()
  ab._prespecializeCollection(index: 0, range: (0..<0))
#endif // ObjC

  var ca = ContiguousArray<T>()
  ca._prespecializeRandomAccessCollection()
  try! ca._prespecializeMutableRandomAccessCollection(cmp: cmp)

  let cb = _ContiguousArrayBuffer<T>()
  cb._prespecializeRandomAccessCollection()
}

func prespecializeRanges() {
  // Range<Int>
  (0..<0)._prespecializeCollection(index: 0, range: (0..<0))
  (0..<0)._prespecializeRange(bound: 0)
  (0..<0)._prespecializeRange(bounds: (0, 0))
  (0..<0)._prespecializeIntegerRange(bound: 0)
  (0..<0)._prespecializeIndexingIterator()
  _prespecializeIndexingIterator((0..<0).makeIterator())
  // ClosedRange<Int>
  (0...0)._prespecializeClosedRange()
  (0...0)._prespecializeIntegerClosedRange()
  (0...0)._prespecializeIntegerClosedRange(range: (0...0))
  (0...0)._prespecializeIndexingIterator()
  _prespecializeIndexingIterator((0...0).makeIterator())
}

// =============================================================================
// Top-level function that statically calls all generic entry points
// that require prespecialization.
// =============================================================================

// Allow optimization here so that specialization occurs.
func prespecializeAll() {
  prespecializeCollections(() as Any)
  prespecializeCollections("a" as Character)
  prespecializeCollections("a" as Unicode.Scalar)
  prespecializeCollections("a".utf8)
  prespecializeCollections("a".utf16)
  prespecializeCollections("a".unicodeScalars)
  prespecializeCollections("a" as String)
  prespecializeCollections(1.5 as Double)
  prespecializeCollections(1.5 as Float)
  prespecializeCollections(1 as Int)
  prespecializeCollections(1 as UInt)
  prespecializeCollections(1 as Int8)
  prespecializeCollections(1 as Int16)
  prespecializeCollections(1 as Int32)
  prespecializeCollections(1 as Int64)
  prespecializeCollections(1 as UInt8)
  prespecializeCollections(1 as UInt16)
  prespecializeCollections(1 as UInt32)
  prespecializeCollections(1 as UInt64)

  prespecializeRanges()
}

// Mark with optimize(none) to make sure its not get
// rid of by dead function elimination. 
@_optimize(none)
internal func _swift_forcePrespecializations() {
  prespecializeAll()
}

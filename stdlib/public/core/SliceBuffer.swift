//===--- SliceBuffer.swift - Backing storage for ArraySlice<Element> ------===//
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

/// Buffer type for `ArraySlice<Element>`.
@_versioned
internal struct _SliceBuffer<Element>
  : _ArrayBufferProtocol,
    RandomAccessCollection
{
  internal typealias NativeStorage = _ContiguousArrayStorage<Element>
  internal typealias NativeBuffer = _ContiguousArrayBuffer<Element>

  internal init(
    owner: AnyObject, subscriptBaseAddress: UnsafeMutablePointer<Element>,
    indices: Range<Int>, hasNativeBuffer: Bool
  ) {
    self.owner = owner
    self.subscriptBaseAddress = subscriptBaseAddress
    self.startIndex = indices.lowerBound
    let bufferFlag = UInt(hasNativeBuffer ? 1 : 0)
    self.endIndexAndFlags = (UInt(indices.upperBound) << 1) | bufferFlag
    _invariantCheck()
  }

  internal init() {
    let empty = _ContiguousArrayBuffer<Element>()
    self.owner = empty.owner
    self.subscriptBaseAddress = empty.firstElementAddress
    self.startIndex = empty.startIndex
    self.endIndexAndFlags = 1
    _invariantCheck()
  }

  internal init(_buffer buffer: NativeBuffer, shiftedToStartIndex: Int) {
    let shift = buffer.startIndex - shiftedToStartIndex
    self.init(
      owner: buffer.owner,
      subscriptBaseAddress: buffer.subscriptBaseAddress + shift,
      indices: shiftedToStartIndex..<shiftedToStartIndex + buffer.count,
      hasNativeBuffer: true)
  }

  internal func _invariantCheck() {
    let isNative = _hasNativeBuffer
    let isNativeStorage: Bool = owner is _ContiguousArrayStorageBase
    _sanityCheck(isNativeStorage == isNative)
    if isNative {
      _sanityCheck(count <= nativeBuffer.count)
    }
  }

  internal var _hasNativeBuffer: Bool {
    return (endIndexAndFlags & 1) != 0
  }

  internal var nativeBuffer: NativeBuffer {
    _sanityCheck(_hasNativeBuffer)
    return NativeBuffer(
      owner as? _ContiguousArrayStorageBase ?? _emptyArrayStorage)
  }

  internal var nativeOwner: AnyObject {
    _sanityCheck(_hasNativeBuffer, "Expect a native array")
    return owner
  }

  /// Replace the given subRange with the first newCount elements of
  /// the given collection.
  ///
  /// - Precondition: This buffer is backed by a uniquely-referenced
  ///   `_ContiguousArrayBuffer` and
  ///   `insertCount <= numericCast(newValues.count)`.
  internal mutating func replaceSubrange<C>(
    _ subrange: Range<Int>,
    with insertCount: Int,
    elementsOf newValues: C
  ) where C : Collection, C.Iterator.Element == Element {

    _invariantCheck()
    _sanityCheck(insertCount <= numericCast(newValues.count))

    _sanityCheck(_hasNativeBuffer && isUniquelyReferenced())

    let eraseCount = subrange.count
    let growth = insertCount - eraseCount
    let oldCount = count

    var native = nativeBuffer
    let hiddenElementCount = firstElementAddress - native.firstElementAddress

    _sanityCheck(native.count + growth <= native.capacity)

    let start = subrange.lowerBound - startIndex + hiddenElementCount
    let end = subrange.upperBound - startIndex + hiddenElementCount
    native.replaceSubrange(
      start..<end,
      with: insertCount,
      elementsOf: newValues)

    self.endIndex = self.startIndex + oldCount + growth

    _invariantCheck()
  }

  /// A value that identifies the storage used by the buffer.  Two
  /// buffers address the same elements when they have the same
  /// identity and count.
  internal var identity: UnsafeRawPointer {
    return UnsafeRawPointer(firstElementAddress)
  }

  /// An object that keeps the elements stored in this buffer alive.
  internal var owner: AnyObject
  internal let subscriptBaseAddress: UnsafeMutablePointer<Element>

  @_versioned
  internal var firstElementAddress: UnsafeMutablePointer<Element> {
    return subscriptBaseAddress + startIndex
  }

  internal var firstElementAddressIfContiguous: UnsafeMutablePointer<Element>? {
    return firstElementAddress
  }

  /// [63:1: 63-bit index][0: has a native buffer]
  internal var endIndexAndFlags: UInt

  //===--- Non-essential bits ---------------------------------------------===//

  internal mutating func requestUniqueMutableBackingBuffer(
    minimumCapacity: Int
  ) -> NativeBuffer? {
    _invariantCheck()
    if _fastPath(_hasNativeBuffer && isUniquelyReferenced()) {
      if capacity >= minimumCapacity {
        // Since we have the last reference, drop any inaccessible
        // trailing elements in the underlying storage.  That will
        // tend to reduce shuffling of later elements.  Since this
        // function isn't called for subscripting, this won't slow
        // down that case.
        var native = nativeBuffer
        let offset = self.firstElementAddress - native.firstElementAddress
        let backingCount = native.count
        let myCount = count

        if _slowPath(backingCount > myCount + offset) {
          native.replaceSubrange(
            (myCount+offset)..<backingCount,
            with: 0,
            elementsOf: EmptyCollection())
        }
        _invariantCheck()
        return native
      }
    }
    return nil
  }

  internal mutating func isMutableAndUniquelyReferenced() -> Bool {
    return _hasNativeBuffer && isUniquelyReferenced()
  }

  internal mutating func isMutableAndUniquelyReferencedOrPinned() -> Bool {
    return _hasNativeBuffer && isUniquelyReferencedOrPinned()
  }

  /// If this buffer is backed by a `_ContiguousArrayBuffer`
  /// containing the same number of elements as `self`, return it.
  /// Otherwise, return `nil`.
  internal func requestNativeBuffer() -> _ContiguousArrayBuffer<Element>? {
    _invariantCheck()
    if _fastPath(_hasNativeBuffer && nativeBuffer.count == count) {
      return nativeBuffer
    }
    return nil
  }

  @discardableResult
  internal func _copyContents(
    subRange bounds: Range<Int>,
    initializing target: UnsafeMutablePointer<Element>
  ) -> UnsafeMutablePointer<Element> {
    _invariantCheck()
    _sanityCheck(bounds.lowerBound >= startIndex)
    _sanityCheck(bounds.upperBound >= bounds.lowerBound)
    _sanityCheck(bounds.upperBound <= endIndex)
    let c = bounds.count
    target.initialize(from: subscriptBaseAddress + bounds.lowerBound, count: c)
    return target + c
  }

  /// True, if the array is native and does not need a deferred type check.
  internal var arrayPropertyIsNativeTypeChecked: Bool {
    return _hasNativeBuffer
  }

  internal var count: Int {
    get {
      return endIndex - startIndex
    }
    set {
      let growth = newValue - count
      if growth != 0 {
        nativeBuffer.count += growth
        self.endIndex += growth
      }
      _invariantCheck()
    }
  }

  /// Traps unless the given `index` is valid for subscripting, i.e.
  /// `startIndex ≤ index < endIndex`
  internal func _checkValidSubscript(_ index : Int) {
    _precondition(
      index >= startIndex && index < endIndex, "Index out of bounds")
  }

  internal var capacity: Int {
    let count = self.count
    if _slowPath(!_hasNativeBuffer) {
      return count
    }
    let n = nativeBuffer
    let nativeEnd = n.firstElementAddress + n.count
    if (firstElementAddress + count) == nativeEnd {
      return count + (n.capacity - n.count)
    }
    return count
  }

  internal mutating func isUniquelyReferenced() -> Bool {
    return isKnownUniquelyReferenced(&owner)
  }

  internal mutating func isUniquelyReferencedOrPinned() -> Bool {
    return _isKnownUniquelyReferencedOrPinned(&owner)
  }

  @_versioned
  internal func getElement(_ i: Int) -> Element {
    _sanityCheck(i >= startIndex, "slice index is out of range (before startIndex)")
    _sanityCheck(i < endIndex, "slice index is out of range")
    return subscriptBaseAddress[i]
  }

  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  internal subscript(position: Int) -> Element {
    get {
      return getElement(position)
    }
    nonmutating set {
      _sanityCheck(position >= startIndex, "slice index is out of range (before startIndex)")
      _sanityCheck(position < endIndex, "slice index is out of range")
      subscriptBaseAddress[position] = newValue
    }
  }

  internal subscript(bounds: Range<Int>) -> _SliceBuffer {
    get {
      _sanityCheck(bounds.lowerBound >= startIndex)
      _sanityCheck(bounds.upperBound >= bounds.lowerBound)
      _sanityCheck(bounds.upperBound <= endIndex)
      return _SliceBuffer(
        owner: owner,
        subscriptBaseAddress: subscriptBaseAddress,
        indices: bounds,
        hasNativeBuffer: _hasNativeBuffer)
    }
    set {
      fatalError("not implemented")
    }
  }

  //===--- Collection conformance -------------------------------------===//
  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  internal var startIndex: Int

  /// The collection's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// `endIndex` is always reachable from `startIndex` by zero or more
  /// applications of `index(after:)`.
  internal var endIndex: Int {
    get {
      return Int(endIndexAndFlags >> 1)
    }
    set {
      endIndexAndFlags = (UInt(newValue) << 1) | (_hasNativeBuffer ? 1 : 0)
    }
  }

  internal typealias Indices = CountableRange<Int>

  //===--- misc -----------------------------------------------------------===//
  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.
  internal func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(UnsafeBufferPointer(start: firstElementAddress,
      count: count))
  }

  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.
  internal mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(
      UnsafeMutableBufferPointer(start: firstElementAddress, count: count))
  }
}

extension _SliceBuffer {
  internal func _copyToContiguousArray() -> ContiguousArray<Element> {
    if _hasNativeBuffer {
      let n = nativeBuffer
      if count == n.count {
        return ContiguousArray(_buffer: n)
      }
    }

    let result = _ContiguousArrayBuffer<Element>(
      _uninitializedCount: count,
      minimumCapacity: 0)
    result.firstElementAddress.initialize(
      from: firstElementAddress, count: count)
    return ContiguousArray(_buffer: result)
  }
}

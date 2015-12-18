//===--- SliceBuffer.swift - Backing storage for ArraySlice<Element> ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Buffer type for `ArraySlice<Element>`.
public // @testable
struct _SliceBuffer<Element> : _ArrayBufferProtocol {
  internal typealias NativeStorage = _ContiguousArrayStorage<Element>
  public typealias NativeBuffer = _ContiguousArrayBuffer<Element>

  init(
    owner: AnyObject, subscriptBaseAddress: UnsafeMutablePointer<Element>,
    indices: Range<Int>, hasNativeBuffer: Bool
  ) {
    self.owner = owner
    self.subscriptBaseAddress = subscriptBaseAddress
    self.startIndex = indices.startIndex
    let bufferFlag = UInt(hasNativeBuffer ? 1 : 0)
    self.endIndexAndFlags = (UInt(indices.endIndex) << 1) | bufferFlag
    _invariantCheck()
  }

  public init() {
    let empty = _ContiguousArrayBuffer<Element>()
    self.owner = empty.owner
    self.subscriptBaseAddress = empty.firstElementAddress
    self.startIndex = empty.startIndex
    self.endIndexAndFlags = 1
    _invariantCheck()
  }

  public init(_ buffer: NativeBuffer, shiftedToStartIndex: Int) {
    let shift = buffer.startIndex - shiftedToStartIndex
    self.init(
      owner: buffer.owner,
      subscriptBaseAddress: buffer.subscriptBaseAddress + shift,
      indices: shiftedToStartIndex..<shiftedToStartIndex + buffer.length,
      hasNativeBuffer: true)
  }

  func _invariantCheck() {
    let isNative = _hasNativeBuffer
    let isNativeStorage: Bool = (owner as? _ContiguousArrayStorageBase) != nil
    _sanityCheck(isNativeStorage == isNative)
    if isNative {
      _sanityCheck(length <= nativeBuffer.length)
    }
  }

  var _hasNativeBuffer: Bool {
    return (endIndexAndFlags & 1) != 0
  }

  var nativeBuffer: NativeBuffer {
    _sanityCheck(_hasNativeBuffer)
    return NativeBuffer(
      owner as? _ContiguousArrayStorageBase ?? _emptyArrayStorage)
  }

  public var nativeOwner: AnyObject {
    _sanityCheck(_hasNativeBuffer, "Expect a native array")
    return owner
  }

  /// Replace the given subRange with the first subRangeNewLength elements of
  /// the given collection.
  ///
  /// - Requires: This buffer is backed by a uniquely-referenced
  ///   `_ContiguousArrayBuffer` and
  ///   `subRangeNewLength <= numericCast(newValues.length)`.
  public mutating func replace<
    C : Collection where C.Iterator.Element == Element
  >(
    subRange subRange: Range<Int>,
    with subRangeNewLength: Int,
    elementsOf newValues: C
  ) {
    _invariantCheck()
    _sanityCheck(subRangeNewLength <= numericCast(newValues.length))

    _sanityCheck(_hasNativeBuffer && isUniquelyReferenced())

    let eraseCount = subRange.length
    let growth = subRangeNewLength - eraseCount
    let oldCount = length

    var native = nativeBuffer
    let hiddenElementCount = firstElementAddress - native.firstElementAddress

    _sanityCheck(native.length + growth <= native.capacity)

    let start = subRange.startIndex - startIndex + hiddenElementCount
    let end = subRange.endIndex - startIndex + hiddenElementCount
    native.replace(
      subRange: start..<end,
      with: subRangeNewLength,
      elementsOf: newValues)

    self.endIndex = self.startIndex + oldCount + growth

    _invariantCheck()
  }

  /// A value that identifies the storage used by the buffer.  Two
  /// buffers address the same elements when they have the same
  /// identity and length.
  public var identity: UnsafePointer<Void> {
    return UnsafePointer(firstElementAddress)
  }

  /// An object that keeps the elements stored in this buffer alive.
  public var owner: AnyObject
  public let subscriptBaseAddress: UnsafeMutablePointer<Element>

  public var firstElementAddress: UnsafeMutablePointer<Element> {
    return subscriptBaseAddress + startIndex
  }

  /// [63:1: 63-bit index][0: has a native buffer]
  var endIndexAndFlags: UInt

  //===--- Non-essential bits ---------------------------------------------===//

  @warn_unused_result
  public mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
    -> NativeBuffer?
  {
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
        let nativeLength = native.length
        let myLength = length

        if _slowPath(nativeLength > myLength + offset) {
          native.replace(
            subRange: (myLength+offset)..<nativeLength,
            with: 0,
            elementsOf: EmptyCollection())
        }
        _invariantCheck()
        return native
      }
    }
    return nil
  }

  @warn_unused_result
  public mutating func isMutableAndUniquelyReferenced() -> Bool {
    return _hasNativeBuffer && isUniquelyReferenced()
  }

  @warn_unused_result
  public mutating func isMutableAndUniquelyReferencedOrPinned() -> Bool {
    return _hasNativeBuffer && isUniquelyReferencedOrPinned()
  }

  /// If this buffer is backed by a `_ContiguousArrayBuffer`
  /// containing the same number of elements as `self`, return it.
  /// Otherwise, return `nil`.
  @warn_unused_result
  public func requestNativeBuffer() -> _ContiguousArrayBuffer<Element>? {
    _invariantCheck()
    if _fastPath(_hasNativeBuffer && nativeBuffer.length == length) {
      return nativeBuffer
    }
    return nil
  }

  public func _uninitializedCopy(
    bounds: Range<Int>, target: UnsafeMutablePointer<Element>
  ) -> UnsafeMutablePointer<Element> {
    _invariantCheck()
    _sanityCheck(bounds.startIndex >= startIndex)
    _sanityCheck(bounds.endIndex >= bounds.startIndex)
    _sanityCheck(bounds.endIndex <= endIndex)
    let length = bounds.length
    target.initializeFrom(
      subscriptBaseAddress + bounds.startIndex, count: length)
    return target + length
  }

  /// True, if the array is native and does not need a deferred type check.
  var arrayPropertyIsNativeTypeChecked : Bool {
    return _hasNativeBuffer
  }

  public var length: Int {
    get {
      return endIndex - startIndex
    }
    set {
      let growth = newValue - length
      if growth != 0 {
        nativeBuffer.length += growth
        self.endIndex += growth
      }
      _invariantCheck()
    }
  }

  /// Traps unless the given `index` is valid for subscripting, i.e.
  /// `startIndex ≤ index < endIndex`
  internal func _checkValidSubscript(index : Int) {
    _require(
      index >= startIndex && index < endIndex, "Index out of bounds")
  }

  public var capacity: Int {
    let length = self.length
    if _slowPath(!_hasNativeBuffer) {
      return length
    }
    let n = nativeBuffer
    let nativeEnd = n.firstElementAddress + n.length
    if (firstElementAddress + length) == nativeEnd {
      return length + (n.capacity - n.length)
    }
    return length
  }

  @warn_unused_result
  mutating func isUniquelyReferenced() -> Bool {
    return isUniquelyReferencedNonObjC(&owner)
  }

  @warn_unused_result
  mutating func isUniquelyReferencedOrPinned() -> Bool {
    return isUniquelyReferencedOrPinnedNonObjC(&owner)
  }

  @warn_unused_result
  func getElement(i: Int) -> Element {
    _sanityCheck(i >= startIndex, "negative slice index is out of range")
    _sanityCheck(i < endIndex, "slice index out of range")
    return subscriptBaseAddress[i]
  }

  /// Access the element at `position`.
  ///
  /// - Requires: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  public subscript(position: Int) -> Element {
    get {
      return getElement(position)
    }
    nonmutating set {
      _sanityCheck(position >= startIndex, "negative slice index is out of range")
      _sanityCheck(position < endIndex, "slice index out of range")
      subscriptBaseAddress[position] = newValue
    }
  }

  public subscript(bounds: Range<Int>) -> _SliceBuffer {
    get {
      _sanityCheck(bounds.startIndex >= startIndex)
      _sanityCheck(bounds.endIndex >= bounds.startIndex)
      _sanityCheck(bounds.endIndex <= endIndex)
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
  public
  var startIndex: Int

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public
  var endIndex: Int {
    get {
      return Int(endIndexAndFlags >> 1)
    }
    set {
      endIndexAndFlags = (UInt(newValue) << 1) | (_hasNativeBuffer ? 1 : 0)
    }
  }

  //===--- misc -----------------------------------------------------------===//
  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.
  public
  func withUnsafeBufferPointer<R>(
    @noescape body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(
      UnsafeBufferPointer(start: firstElementAddress, length: length))
  }

  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.
  public
  mutating func withUnsafeMutableBufferPointer<R>(
    @noescape body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(
      UnsafeMutableBufferPointer(start: firstElementAddress, length: length))
  }
}

extension _SliceBuffer {
  public func _copyToNativeArrayBuffer() -> _ContiguousArrayBuffer<Element> {
    if _hasNativeBuffer {
      let n = nativeBuffer
      if length == n.length {
        return n
      }
    }

    let result = _ContiguousArrayBuffer<Element>(
      length: length,
      minimumCapacity: 0)
    result.firstElementAddress.initializeFrom(
      firstElementAddress, count: length)
    return result
  }
}

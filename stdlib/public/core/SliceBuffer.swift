//===--- SliceBuffer.swift - Backing storage for ArraySlice<Element> ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Buffer type for `ArraySlice<Element>`.
public // @testable
struct _SliceBuffer<Element> : _ArrayBufferProtocol, RandomAccessCollection {
  internal typealias NativeStorage = _ContiguousArrayStorage<Element>
  public typealias NativeBuffer = _ContiguousArrayBuffer<Element>

  init(
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
      indices: shiftedToStartIndex..<shiftedToStartIndex + buffer.count,
      hasNativeBuffer: true)
  }

  func _invariantCheck() {
    let isNative = _hasNativeBuffer
    let isNativeStorage: Bool = (owner as? _ContiguousArrayStorageBase) != nil
    _sanityCheck(isNativeStorage == isNative)
    if isNative {
      _sanityCheck(count <= nativeBuffer.count)
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

  /// Replace the given subRange with the first newCount elements of
  /// the given collection.
  ///
  /// - Precondition: This buffer is backed by a uniquely-referenced
  ///   `_ContiguousArrayBuffer` and
  ///   `insertCount <= numericCast(newValues.count)`.
  public mutating func replace<
    C : Collection where C.Iterator.Element == Element
  >(
    subRange: Range<Int>,
    with insertCount: Int,
    elementsOf newValues: C
  ) {
    _invariantCheck()
    _sanityCheck(insertCount <= numericCast(newValues.count))

    _sanityCheck(_hasNativeBuffer && isUniquelyReferenced())

    let eraseCount = subRange.count
    let growth = insertCount - eraseCount
    let oldCount = count

    var native = nativeBuffer
    let hiddenElementCount = firstElementAddress - native.firstElementAddress

    _sanityCheck(native.count + growth <= native.capacity)

    let start = subRange.lowerBound - startIndex + hiddenElementCount
    let end = subRange.upperBound - startIndex + hiddenElementCount
    native.replace(
      subRange: start..<end,
      with: insertCount,
      elementsOf: newValues)

    self.endIndex = self.startIndex + oldCount + growth

    _invariantCheck()
  }

  /// A value that identifies the storage used by the buffer.  Two
  /// buffers address the same elements when they have the same
  /// identity and count.
  public var identity: UnsafePointer<Void> {
    return UnsafePointer(firstElementAddress)
  }

  /// An object that keeps the elements stored in this buffer alive.
  public var owner: AnyObject
  public let subscriptBaseAddress: UnsafeMutablePointer<Element>

  public var firstElementAddress: UnsafeMutablePointer<Element> {
    return subscriptBaseAddress + startIndex
  }

  public var firstElementAddressIfContiguous: UnsafeMutablePointer<Element>? {
    return firstElementAddress
  }

  /// [63:1: 63-bit index][0: has a native buffer]
  var endIndexAndFlags: UInt

  //===--- Non-essential bits ---------------------------------------------===//

  @warn_unused_result
  public mutating func requestUniqueMutableBackingBuffer(
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
          native.replace(
            subRange: (myCount+offset)..<backingCount,
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
    if _fastPath(_hasNativeBuffer && nativeBuffer.count == count) {
      return nativeBuffer
    }
    return nil
  }

  @discardableResult
  public func _copyContents(
    subRange bounds: Range<Int>,
    initializing target: UnsafeMutablePointer<Element>
  ) -> UnsafeMutablePointer<Element> {
    _invariantCheck()
    _sanityCheck(bounds.lowerBound >= startIndex)
    _sanityCheck(bounds.upperBound >= bounds.lowerBound)
    _sanityCheck(bounds.upperBound <= endIndex)
    let c = bounds.count
    target.initializeFrom(subscriptBaseAddress + bounds.lowerBound, count: c)
    return target + c
  }

  /// True, if the array is native and does not need a deferred type check.
  var arrayPropertyIsNativeTypeChecked : Bool {
    return _hasNativeBuffer
  }

  public
  var count: Int {
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

  public var capacity: Int {
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

  @warn_unused_result
  mutating func isUniquelyReferenced() -> Bool {
    return isUniquelyReferencedNonObjC(&owner)
  }

  @warn_unused_result
  mutating func isUniquelyReferencedOrPinned() -> Bool {
    return isUniquelyReferencedOrPinnedNonObjC(&owner)
  }

  @_versioned
  @warn_unused_result
  func getElement(_ i: Int) -> Element {
    _sanityCheck(i >= startIndex, "negative slice index is out of range")
    _sanityCheck(i < endIndex, "slice index out of range")
    return subscriptBaseAddress[i]
  }

  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
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
  public var startIndex: Int

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `index(after:)`.
  public var endIndex: Int {
    get {
      return Int(endIndexAndFlags >> 1)
    }
    set {
      endIndexAndFlags = (UInt(newValue) << 1) | (_hasNativeBuffer ? 1 : 0)
    }
  }

  public typealias Indices = CountableRange<Int>

  //===--- misc -----------------------------------------------------------===//
  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.
  public
  func withUnsafeBufferPointer<R>(
    _ body: @noescape (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(UnsafeBufferPointer(start: firstElementAddress,
      count: count))
  }

  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.
  public
  mutating func withUnsafeMutableBufferPointer<R>(
    _ body: @noescape (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(
      UnsafeMutableBufferPointer(start: firstElementAddress, count: count))
  }
}

extension _SliceBuffer {
  public func _copyToNativeArrayBuffer() -> _ContiguousArrayBuffer<Element> {
    if _hasNativeBuffer {
      let n = nativeBuffer
      if count == n.count {
        return n
      }
    }

    let result = _ContiguousArrayBuffer<Element>(
      uninitializedCount: count,
      minimumCapacity: 0)
    result.firstElementAddress.initializeFrom(
      firstElementAddress, count: count)
    return result
  }
}

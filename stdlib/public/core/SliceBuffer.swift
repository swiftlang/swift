//===--- SliceBuffer.swift - Backing storage for ArraySlice<Element> ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Buffer type for `ArraySlice<Element>`.
@frozen
@usableFromInline
@safe
internal struct _SliceBuffer<Element>
  : _ArrayBufferProtocol,
    RandomAccessCollection
{
  #if $Embedded
  @usableFromInline
  typealias AnyObject = Builtin.NativeObject
  #endif

  internal typealias NativeStorage = _ContiguousArrayStorage<Element>
  @usableFromInline
  internal typealias NativeBuffer = _ContiguousArrayBuffer<Element>

  /// An object that keeps the elements stored in this buffer alive.
  @usableFromInline
  internal var owner: AnyObject

  @usableFromInline
  internal let subscriptBaseAddress: UnsafeMutablePointer<Element>

  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  @usableFromInline
  internal var startIndex: Int

  /// [63:1: 63-bit index][0: has a native buffer]
  @usableFromInline
  internal var endIndexAndFlags: UInt

  @inlinable
  internal init(
    owner: AnyObject,
    subscriptBaseAddress: UnsafeMutablePointer<Element>,
    startIndex: Int,
    endIndexAndFlags: UInt
  ) {
    self.owner = owner
    unsafe self.subscriptBaseAddress = subscriptBaseAddress
    self.startIndex = startIndex
    self.endIndexAndFlags = endIndexAndFlags
  }

  @inlinable
  internal init(
    owner: AnyObject, subscriptBaseAddress: UnsafeMutablePointer<Element>,
    indices: Range<Int>, hasNativeBuffer: Bool
  ) {
    self.owner = owner
    unsafe self.subscriptBaseAddress = subscriptBaseAddress
    self.startIndex = indices.lowerBound
    let bufferFlag = UInt(hasNativeBuffer ? 1 : 0)
    self.endIndexAndFlags = (UInt(indices.upperBound) << 1) | bufferFlag
    _invariantCheck()
  }

  @inlinable
  internal init() {
    let empty = _ContiguousArrayBuffer<Element>()
    #if $Embedded
    self.owner = Builtin.castToNativeObject(_emptyArrayStorage)
    #else
    self.owner = _emptyArrayStorage
    #endif
    unsafe self.subscriptBaseAddress = empty.firstElementAddress
    self.startIndex = empty.startIndex
    self.endIndexAndFlags = 1
    _invariantCheck()
  }

  @inlinable
  internal init(_buffer buffer: NativeBuffer, shiftedToStartIndex: Int) {
    let shift = buffer.startIndex - shiftedToStartIndex
    unsafe self.init(
      owner: buffer.owner,
      subscriptBaseAddress: buffer.subscriptBaseAddress + shift,
      indices: shiftedToStartIndex..<shiftedToStartIndex + buffer.count,
      hasNativeBuffer: true)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _invariantCheck() {
    let isNative = _hasNativeBuffer
    let isNativeStorage: Bool
    #if !$Embedded
    isNativeStorage = owner is __ContiguousArrayStorageBase
    #else
    isNativeStorage = true
    #endif
    _internalInvariant(isNativeStorage == isNative)
    if isNative {
      _internalInvariant(count <= nativeBuffer.count)
    }
  }

  @inlinable
  internal var _hasNativeBuffer: Bool {
    return (endIndexAndFlags & 1) != 0
  }

  @inlinable
  internal var nativeBuffer: NativeBuffer {
    _internalInvariant(_hasNativeBuffer)
    #if !$Embedded
    return NativeBuffer(
      owner as? __ContiguousArrayStorageBase ?? _emptyArrayStorage)
    #else
    return NativeBuffer(unsafe unsafeBitCast(_nativeObject(toNative: owner),
      to: __ContiguousArrayStorageBase.self))
    #endif
  }

  @inlinable
  internal var nativeOwner: AnyObject {
    _internalInvariant(_hasNativeBuffer, "Expect a native array")
    return owner
  }

  /// Replace the given subRange with the first newCount elements of
  /// the given collection.
  ///
  /// - Precondition: This buffer is backed by a uniquely-referenced
  ///   `_ContiguousArrayBuffer` and `insertCount <= newValues.count`.
  @inlinable
  internal mutating func replaceSubrange<C>(
    _ subrange: Range<Int>,
    with insertCount: Int,
    elementsOf newValues: __owned C
  ) where C: Collection, C.Element == Element {

    _invariantCheck()
    _internalInvariant(insertCount <= newValues.count)

    _internalInvariant(_hasNativeBuffer)
    _internalInvariant(isUniquelyReferenced())

    let eraseCount = subrange.count
    let growth = insertCount - eraseCount
    let oldCount = count

    var native = nativeBuffer
    let hiddenElementCount = unsafe firstElementAddress - native.firstElementAddress

    _internalInvariant(native.count + growth <= native.capacity)

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
  @inlinable
  internal var identity: UnsafeRawPointer {
    return unsafe UnsafeRawPointer(firstElementAddress)
  }

  @inlinable
  internal var firstElementAddress: UnsafeMutablePointer<Element> {
    return unsafe subscriptBaseAddress + startIndex
  }

  @inlinable
  internal var firstElementAddressIfContiguous: UnsafeMutablePointer<Element>? {
    return unsafe firstElementAddress
  }

  //===--- Non-essential bits ---------------------------------------------===//

  @inlinable
  internal mutating func requestUniqueMutableBackingBuffer(
    minimumCapacity: Int
  ) -> NativeBuffer? {
    _invariantCheck()
    // Note: with COW support it's already guaranteed to have a uniquely
    // referenced buffer. This check is only needed for backward compatibility.
    if _fastPath(isUniquelyReferenced()) {
      if capacity >= minimumCapacity {
        // Since we have the last reference, drop any inaccessible
        // trailing elements in the underlying storage.  That will
        // tend to reduce shuffling of later elements.  Since this
        // function isn't called for subscripting, this won't slow
        // down that case.
        var native = nativeBuffer
        let offset = unsafe self.firstElementAddress - native.firstElementAddress
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

  @inlinable
  internal mutating func isMutableAndUniquelyReferenced() -> Bool {
    // This is a performance optimization that ensures that the copy of self
    // that occurs at -Onone is destroyed before we call
    // isUniquelyReferenced. This code used to be:
    //
    //   return _hasNativeBuffer && isUniquelyReferenced()
    //
    // https://github.com/apple/swift/issues/48987
    if !_hasNativeBuffer {
      return false
    }
    return isUniquelyReferenced()
  }

  /// If this buffer is backed by a `_ContiguousArrayBuffer`
  /// containing the same number of elements as `self`, return it.
  /// Otherwise, return `nil`.
  @inlinable
  internal func requestNativeBuffer() -> _ContiguousArrayBuffer<Element>? {
    _invariantCheck()
    if _fastPath(_hasNativeBuffer && nativeBuffer.count == count) {
      return nativeBuffer
    }
    return nil
  }

  @inlinable
  @discardableResult
  internal __consuming func _copyContents(
    subRange bounds: Range<Int>,
    initializing target: UnsafeMutablePointer<Element>
  ) -> UnsafeMutablePointer<Element> {
    _invariantCheck()
    _internalInvariant(bounds.lowerBound >= startIndex)
    _internalInvariant(bounds.upperBound >= bounds.lowerBound)
    _internalInvariant(bounds.upperBound <= endIndex)
    let c = bounds.count
    unsafe target.initialize(from: subscriptBaseAddress + bounds.lowerBound, count: c)
    return unsafe target + c
  }

  @inlinable
  internal __consuming func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Element>
  ) -> (Iterator, UnsafeMutableBufferPointer<Element>.Index) {
    _invariantCheck()
    guard buffer.count > 0 else { return (makeIterator(), 0) }
    let c = Swift.min(self.count, buffer.count)
    unsafe buffer.baseAddress!.initialize(
      from: firstElementAddress,
      count: c)
    _fixLifetime(owner)
    return (IndexingIterator(_elements: self, _position: startIndex + c), c)
  }

  /// True, if the array is native and does not need a deferred type check.
  @inlinable
  internal var arrayPropertyIsNativeTypeChecked: Bool {
    return _hasNativeBuffer
  }

  @inlinable
  internal var count: Int {
    get {
      return endIndex - startIndex
    }
    set {
      let growth = newValue - count
      if growth != 0 {
        nativeBuffer.mutableCount += growth
        self.endIndex += growth
      }
      _invariantCheck()
    }
  }

  /// Traps unless the given `index` is valid for subscripting, i.e.
  /// `startIndex ≤ index < endIndex`
  @inlinable
  internal func _checkValidSubscript(_ index: Int) {
    let translatedIndex = UInt(bitPattern: index &- startIndex)
    let translatedEnd = UInt(bitPattern: endIndex &- startIndex)
    _precondition(translatedIndex < translatedEnd, "Index out of bounds")
  }

  @inlinable
  internal var capacity: Int {
    let count = self.count
    if _slowPath(!_hasNativeBuffer) {
      return count
    }
    let n = nativeBuffer
    let nativeEnd = unsafe n.firstElementAddress + n.count
    if unsafe (firstElementAddress + count) == nativeEnd {
      return count + (n.capacity - n.count)
    }
    return count
  }

  /// Returns `true` if this buffer's storage is uniquely-referenced;
  /// otherwise, returns `false`.
  ///
  /// This function should only be used for internal soundness checks and for
  /// backward compatibility.
  /// To guard a buffer mutation, use `beginCOWMutation`.
  @inlinable
  internal mutating func isUniquelyReferenced() -> Bool {
    return isKnownUniquelyReferenced(&owner)
  }

  /// Returns `true` and puts the buffer in a mutable state if the buffer's
  /// storage is uniquely-referenced; otherwise, performs no action and returns
  /// `false`.
  ///
  /// - Precondition: The buffer must be immutable.
  ///
  /// - Warning: It's a requirement to call `beginCOWMutation` before the buffer
  ///   is mutated.
  @_alwaysEmitIntoClient
  internal mutating func beginCOWMutation() -> Bool {
    if !_hasNativeBuffer {
      return false
    }
    if Bool(Builtin.beginCOWMutation(&owner)) {
#if INTERNAL_CHECKS_ENABLED && COW_CHECKS_ENABLED
      nativeBuffer.isImmutable = false
#endif
      return true
    }
    return false;
  }

  /// Puts the buffer in an immutable state.
  ///
  /// - Precondition: The buffer must be mutable or the empty array singleton.
  ///
  /// - Warning: After a call to `endCOWMutation` the buffer must not be mutated
  ///   until the next call of `beginCOWMutation`.
  @_alwaysEmitIntoClient
  @inline(__always)
  internal mutating func endCOWMutation() {
#if INTERNAL_CHECKS_ENABLED && COW_CHECKS_ENABLED
    nativeBuffer.isImmutable = true
#endif
    Builtin.endCOWMutation(&owner)
  }

  @inlinable
  internal func getElement(_ i: Int) -> Element {
    _internalInvariant(i >= startIndex, "slice index is out of range (before startIndex)")
    _internalInvariant(i < endIndex, "slice index is out of range")
    return unsafe subscriptBaseAddress[i]
  }

  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  @inlinable
  internal subscript(position: Int) -> Element {
    get {
      return getElement(position)
    }
    nonmutating set {
      _internalInvariant(position >= startIndex, "slice index is out of range (before startIndex)")
      _internalInvariant(position < endIndex, "slice index is out of range")
      unsafe subscriptBaseAddress[position] = newValue
    }
  }

  @inlinable
  internal subscript(bounds: Range<Int>) -> _SliceBuffer {
    get {
      _internalInvariant(bounds.lowerBound >= startIndex)
      _internalInvariant(bounds.upperBound >= bounds.lowerBound)
      _internalInvariant(bounds.upperBound <= endIndex)
      return unsafe _SliceBuffer(
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

  /// The collection's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// `endIndex` is always reachable from `startIndex` by zero or more
  /// applications of `index(after:)`.
  @inlinable
  internal var endIndex: Int {
    get {
      return Int(endIndexAndFlags >> 1)
    }
    set {
      endIndexAndFlags = (UInt(newValue) << 1) | (_hasNativeBuffer ? 1 : 0)
    }
  }

  @usableFromInline
  internal typealias Indices = Range<Int>

  //===--- misc -----------------------------------------------------------===//
  // Superseded by the typed-throws version of this function, but retained
  // for ABI reasons.
  @usableFromInline
  @_silgen_name("$ss12_SliceBufferV010withUnsafeB7Pointeryqd__qd__SRyxGKXEKlF")
  internal func __abi_withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try unsafe body(UnsafeBufferPointer(start: firstElementAddress,
      count: count))
  }

  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.
  @_alwaysEmitIntoClient
  internal func withUnsafeBufferPointer<R, E>(
    _ body: (UnsafeBufferPointer<Element>) throws(E) -> R
  ) throws(E) -> R {
    defer { _fixLifetime(self) }
    return try unsafe body(UnsafeBufferPointer(start: firstElementAddress,
      count: count))
  }

  // Superseded by the typed-throws version of this function, but retained
  // for ABI reasons.
  @usableFromInline
  @_silgen_name("$ss12_SliceBufferV017withUnsafeMutableB7Pointeryqd__qd__SryxGKXEKlF")
  internal mutating func __abi_withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try unsafe body(
      UnsafeMutableBufferPointer(start: firstElementAddress, count: count))
  }

  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.
  @_alwaysEmitIntoClient
  internal mutating func withUnsafeMutableBufferPointer<R, E>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws(E) -> R
  ) throws(E) -> R {
    defer { _fixLifetime(self) }
    return try unsafe body(
      UnsafeMutableBufferPointer(start: firstElementAddress, count: count))
  }

  @inlinable
  internal func unsafeCastElements<T>(to type: T.Type) -> _SliceBuffer<T> {
    _internalInvariant(_isClassOrObjCExistential(T.self))
    let baseAddress = unsafe UnsafeMutableRawPointer(self.subscriptBaseAddress)
      .assumingMemoryBound(to: T.self)
    return unsafe _SliceBuffer<T>(
      owner: self.owner,
      subscriptBaseAddress: baseAddress,
      startIndex: self.startIndex,
      endIndexAndFlags: self.endIndexAndFlags)
  }
}

@available(*, unavailable)
extension _SliceBuffer: Sendable {}

extension _SliceBuffer {
  @inlinable
  internal __consuming func _copyToContiguousArray() -> ContiguousArray<Element> {
    if _hasNativeBuffer {
      let n = nativeBuffer
      if count == n.count {
        return ContiguousArray(_buffer: n)
      }
    }

    let result = _ContiguousArrayBuffer<Element>(
      _uninitializedCount: count,
      minimumCapacity: 0)
    unsafe result.firstElementAddress.initialize(
      from: firstElementAddress, count: count)
    return ContiguousArray(_buffer: result)
  }
}

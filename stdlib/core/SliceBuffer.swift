//===--- SliceBuffer.swift - Backing storage for Slice<T> -----------------===//
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

/// Buffer type for Slice<T>
public
struct _SliceBuffer<T> : _ArrayBufferType {
  typealias Element = T
  typealias NativeStorage = _ContiguousArrayStorage<T>
  typealias NativeBuffer = _ContiguousArrayBuffer<T>

  init(owner: AnyObject, start: UnsafeMutablePointer<T>, count: Int, 
       hasNativeBuffer: Bool) {
    self.owner = owner
    self.start = start
    self._countAndFlags = (UInt(count) << 1) | (hasNativeBuffer ? 1 : 0)
  }

  public
  init() {
    let empty = _ContiguousArrayBuffer<T>()
    owner = empty.owner
    start = empty.baseAddress
    _countAndFlags = 1
    _invariantCheck()
  }

  public
  init(_ buffer: NativeBuffer) {
    owner = buffer.owner
    start = buffer.baseAddress
    _countAndFlags = (UInt(buffer.count) << 1) | 1
    _invariantCheck()
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
    return (_countAndFlags & 1) != 0
  }

  var nativeBuffer: NativeBuffer {
    _sanityCheck(_hasNativeBuffer)
    return NativeBuffer(
      (owner as? _ContiguousArrayStorageBase) ?? _emptyArrayStorage)
  }

  /// Replace the given subRange with the first newCount elements of
  /// the given collection.
  ///
  /// Requires: this buffer is backed by a uniquely-referenced
  /// _ContiguousArrayBuffer,
  ///
  /// Requires: insertCount <= numericCast(count(newValues))
  ///
  public
  mutating func replace<C: CollectionType where C.Generator.Element == T>(
    #subRange: Range<Int>, with insertCount: Int, elementsOf newValues: C
  ) {
    _invariantCheck()
    // FIXME: <rdar://problem/17464946> with
    // -DSWIFT_STDLIB_ASSERTIONS=FALSE, enabling this sanityCheck
    // actually causes leaks in the stdlib/NewArray.swift.gyb test
    /* _sanityCheck(insertCount <= numericCast(count(newValues))) */
    
    _sanityCheck(_hasNativeBuffer && isUniquelyReferenced())

    var native = nativeBuffer
    let offset = start - native.baseAddress
    let eraseCount = Swift.count(subRange)
    let growth = insertCount - eraseCount
    
    let oldCount = count
    
    _sanityCheck(native.count + growth <= native.capacity)

    native.replace(
      subRange: (subRange.startIndex+offset)..<(subRange.endIndex + offset),
      with: insertCount,
      elementsOf: newValues)
    
    setLocalCount(oldCount + growth)
    _invariantCheck()
  }

  /// A value that identifies the storage used by the buffer.  Two
  /// buffers address the same elements when they have the same
  /// identity and count.
  public var identity: UnsafePointer<Void> {
    return UnsafePointer(start)
  }
  
  /// An object that keeps the elements stored in this buffer alive
  public
  var owner: AnyObject
  var start: UnsafeMutablePointer<T>
  var _countAndFlags: UInt

  //===--- Non-essential bits ---------------------------------------------===//

  public
  mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
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
        let offset = self.baseAddress - native.baseAddress
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

  public
  mutating func isMutableAndUniquelyReferenced() -> Bool {
    return _hasNativeBuffer && isUniquelyReferenced()
  }

  public
  mutating func isMutableAndUniquelyReferencedOrPinned() -> Bool {
    return _hasNativeBuffer && isUniquelyReferencedOrPinned()
  }

  /// If this buffer is backed by a `_ContiguousArrayBuffer`
  /// containing the same number of elements as `self`, return it.
  /// Otherwise, return `nil`.
  public
  func requestNativeBuffer() -> _ContiguousArrayBuffer<Element>? {
    _invariantCheck()
    if _fastPath(_hasNativeBuffer && nativeBuffer.count == count) {
      return nativeBuffer
    }
    return nil
  }

  public
  func _uninitializedCopy(
    subRange: Range<Int>, var target: UnsafeMutablePointer<T>
  ) -> UnsafeMutablePointer<T> {
    _invariantCheck()
    _sanityCheck(subRange.startIndex >= 0)
    _sanityCheck(subRange.endIndex >= subRange.startIndex)
    _sanityCheck(subRange.endIndex <= count)
    
    let c = subRange.endIndex - subRange.startIndex
    target.initializeFrom(start + subRange.startIndex, count: c)
    return target + c
  }

  public
  var baseAddress: UnsafeMutablePointer<T> {
    return start
  }

  public
  var count: Int {
    get {
      return Int(_countAndFlags >> 1)
    }
    set {
      let growth = newValue - count
      if growth != 0 {
        nativeBuffer.count += growth
        setLocalCount(newValue)
      }
      _invariantCheck()
    }
  }

  /// Return whether the given `index` is valid for subscripting, i.e. `0
  /// ≤ index < count`
  internal func _isValidSubscript(index : Int) -> Bool {
    return index >= 0 && index < count
  }

  /// Modify the count in this buffer without a corresponding change
  /// in the underlying nativeBuffer.  The implementation of replace()
  /// uses this, because it does a wholesale replace in the underlying
  /// buffer.
  mutating func setLocalCount(newValue: Int) {
    _countAndFlags = (UInt(newValue) << 1) | (_countAndFlags & 1)
  }

  public
  var capacity: Int {
    let count = self.count
    if _slowPath(!_hasNativeBuffer) {
      return count
    }
    let n = nativeBuffer
    if (count + start) == (n.count + n.baseAddress) {
      return count + (n.capacity - n.count)
    }
    return count
  }

  mutating func isUniquelyReferenced() -> Bool {
    return isUniquelyReferencedNonObjC(&owner)
  }

  mutating func isUniquelyReferencedOrPinned() -> Bool {
    return isUniquelyReferencedOrPinnedNonObjC(&owner)
  }

  /// Access the element at `position`.
  ///
  /// Requires: `position` is a valid position in `self` and
  /// `position != endIndex`.
  public subscript(position: Int) -> T {
    get {
      _sanityCheck(position >= 0, "negative slice index is out of range")
      _sanityCheck(position < count, "slice index out of range")
      return start[position]
    }
    nonmutating set {
      _sanityCheck(position >= 0, "negative slice index is out of range")
      _sanityCheck(position < count, "slice index out of range")
      start[position] = newValue
    }
  }

  public
  subscript (subRange: Range<Int>) -> _SliceBuffer {
    _sanityCheck(subRange.startIndex >= 0)
    _sanityCheck(subRange.endIndex >= subRange.startIndex)
    _sanityCheck(subRange.endIndex <= count)
    return _SliceBuffer(
      owner: owner, start: start + subRange.startIndex,
      count: subRange.endIndex - subRange.startIndex, 
      hasNativeBuffer: _hasNativeBuffer)
  }

  //===--- CollectionType conformance -------------------------------------===//
  /// The position of the first element in a non-empty collection.
  ///
  /// Identical to `endIndex` in an empty collection.
  public
  var startIndex: Int {
    return 0
  }

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public
  var endIndex: Int {
    return count
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> IndexingGenerator<_SliceBuffer> {
    return IndexingGenerator(self)
  }

  //===--- misc -----------------------------------------------------------===//
  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.
  public
  func withUnsafeBufferPointer<R>(
    body: (UnsafeBufferPointer<Element>)->R
  ) -> R {
    let ret = body(UnsafeBufferPointer(start: self.baseAddress, count: count))
    _fixLifetime(self)
    return ret
  }

  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.  
  public
  mutating func withUnsafeMutableBufferPointer<R>(
    body: (UnsafeMutableBufferPointer<T>)->R
  ) -> R {
    let ret = body(
      UnsafeMutableBufferPointer(start: baseAddress, count: count))
    _fixLifetime(self)
    return ret
  }
}

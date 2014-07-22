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

  init(owner: AnyObject?, start: UnsafeMutablePointer<T>, count: Int, 
       hasNativeBuffer: Bool) {
    self.owner = owner
    self.start = start
    self._countAndFlags = (UInt(count) << 1) | (hasNativeBuffer ? 1 : 0)
  }

  public
  init() {
    owner = .None
    start = nil
    _countAndFlags = 0
    _invariantCheck()
  }

  public
  init(_ buffer: NativeBuffer) {
    owner = buffer._storage
    start = buffer.baseAddress
    _countAndFlags = (UInt(buffer.count) << 1) | (owner ? 1 : 0)
    _invariantCheck()
  }

  func _invariantCheck() {
    let isNative = _hasNativeBuffer
    _sanityCheck(
      (owner as? NativeStorage).boolValue == isNative
    )
    if isNative {
      _sanityCheck(count <= nativeBuffer.count)
    }
  }
  
  var _hasNativeBuffer: Bool {
    _sanityCheck(
      owner || (_countAndFlags & 1) == 0,
      "Something went wrong: an unowned buffer cannot have a native buffer")
    return (_countAndFlags & 1) != 0
  }

  var nativeBuffer: NativeBuffer {
    _sanityCheck(_hasNativeBuffer)
    return NativeBuffer(owner as? NativeStorage)
  }

  /// Replace the given subRange with the first newCount elements of
  /// the given collection.
  ///
  /// Requires: this buffer is backed by a uniquely-referenced
  /// _ContiguousArrayBuffer,
  ///
  /// Requires: insertCount <= numericCast(countElements(newValues))
  ///
  public
  mutating func replace<C: CollectionType where C.Generator.Element == T>(
    #subRange: Range<Int>, with insertCount: Int, elementsOf newValues: C
  ) {
    _invariantCheck()
    // FIXME: <rdar://problem/17464946> with
    // -DSWIFT_STDLIB_INTERNAL_CHECKS=OFF, enabling this sanityCheck
    // actually causes leaks in the stdlib/NewArray.swift.gyb test
    /* _sanityCheck(insertCount <= numericCast(countElements(newValues))) */
    
    _sanityCheck(_hasNativeBuffer && isUniquelyReferenced())
    
    var native = reinterpretCast(owner) as NativeBuffer
    let offset = start - native.baseAddress
    let eraseCount = countElements(subRange)
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
  
  /// A value that identifies first mutable element, if any.  Two
  /// arrays compare === iff they are both empty, or if their buffers
  /// have the same identity and count.
  public
  var identity: Word {
    return reinterpretCast(start)
  }
  
  
  /// An object that keeps the elements stored in this buffer alive
  public
  var owner: AnyObject?
  var start: UnsafeMutablePointer<T>
  var _countAndFlags: UInt

  //===--- Non-essential bits ---------------------------------------------===//

  public
  func _asCocoaArray() -> _CocoaArrayType {
    _sanityCheck(
      _isBridgedToObjectiveC(T.self),
      "Array element type is not bridged to ObjectiveC")
    _invariantCheck()

    return _extractOrCopyToNativeArrayBuffer(self)._asCocoaArray()
  }

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
        var backing = reinterpretCast(owner) as NativeBuffer
        let offset = self.baseAddress - backing.baseAddress
        let backingCount = backing.count
        let myCount = count

        if _slowPath(backingCount > myCount + offset) {
          backing.replace(
            subRange: (myCount+offset)..<backingCount,
            with: 0,
            elementsOf: EmptyCollection())
        }
        _invariantCheck()
        return backing
      }
    }
    return nil
  }

  public
  mutating func isMutableAndUniquelyReferenced() -> Bool {
    return _hasNativeBuffer && isUniquelyReferenced()
  }

  /// If this buffer is backed by a _ContiguousArrayBuffer, return it.
  /// Otherwise, return nil.  Note: the result's baseAddress may
  /// not match ours, since we are a _SliceBuffer.
  public
  func requestNativeBuffer() -> _ContiguousArrayBuffer<Element>? {
    _invariantCheck()
    if _fastPath(_hasNativeBuffer) {
      return  reinterpretCast(owner) as NativeBuffer
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
    for i in subRange {
      target++.initialize(start[i])
    }
    return target
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
    return Swift._isUniquelyReferenced(&owner)
  }

  public
  subscript(i: Int) -> T {
    get {
      _sanityCheck(i >= 0, "negative slice index is out of range")
      _sanityCheck(i < count, "slice index out of range")
      return start[i]
    }
    nonmutating set {
      _sanityCheck(i >= 0, "negative slice index is out of range")
      _sanityCheck(i < count, "slice index out of range")
      start[i] = newValue
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
  public
  var startIndex: Int {
    return 0
  }

  public
  var endIndex: Int {
    return count
  }

  public
  func generate() -> IndexingGenerator<_SliceBuffer> {
    return IndexingGenerator(self)
  }

  //===--- misc -----------------------------------------------------------===//
  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.
  public
  func withUnsafeBufferPointer<R>(
    body: (UnsafeBufferPointer<Element>)->R
  ) -> R {
    let ret = body(UnsafeBufferPointer(start: self.baseAddress, length: count))
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
      UnsafeMutableBufferPointer(start: baseAddress, length: count))
    _fixLifetime(self)
    return ret
  }
}

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
struct SliceBuffer<T> : ArrayBufferType {
  typealias Element = T
  typealias NativeStorage = ContiguousArrayStorage<T>
  typealias NativeBuffer = ContiguousArrayBuffer<T>

  init(owner: AnyObject?, start: T*, count: Int, hasNativeBuffer: Bool) {
    self.owner = owner
    self.start = start
    self._countAndFlags = (UInt(count) << 1) | (hasNativeBuffer ? 1 : 0)
  }
  
  init() {
    owner = .None
    start = nil
    _countAndFlags = 0
    _invariantCheck()
  }
  
  init(_ buffer: NativeBuffer) {
    owner = buffer.storage
    start = buffer.elementStorage
    _countAndFlags = (UInt(buffer.count) << 1) | 1
    _invariantCheck()
  }

  func _invariantCheck() {
    let isNative = _hasNativeBuffer
    _sanityCheck(
      (owner as NativeStorage).getLogicValue() == isNative
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
    return NativeBuffer(owner as NativeStorage)
  }
  
  /// A value that identifies first mutable element, if any.  Two
  /// arrays compare === iff they are both empty, or if their buffers
  /// have the same identity and count.
  var identity: Word {
    return reinterpretCast(start)
  }
  
  
  /// An object that keeps the elements stored in this buffer alive
  var owner: AnyObject?
  var start: T*
  var _countAndFlags: UInt

  //===--- Non-essential bits ---------------------------------------------===//
  
  func asCocoaArray() -> CocoaArray {
    _sanityCheck(
      isBridgedToObjectiveC(T.self),
      "Array element type is not bridged to ObjectiveC")
    _invariantCheck()

    return _extractOrCopyToNativeArrayBuffer(self).asCocoaArray()
  }

  mutating func requestUniqueMutableBuffer(minimumCapacity: Int)
    -> NativeBuffer?
  {
    _invariantCheck()
    if _fastPath(_hasNativeBuffer && Swift.isUniquelyReferenced(&owner)) {
      if capacity >= minimumCapacity {
        return reinterpretCast(owner) as NativeBuffer
      }
    }
    return nil
  }

  /// If this buffer is backed by a ContiguousArrayBuffer, return it.
  /// Otherwise, return nil.  Note: the result's elementStorage may
  /// not match ours, since we are a SliceBuffer.
  func requestNativeBuffer() -> ContiguousArrayBuffer<Element>? {
    _invariantCheck()
    if _fastPath(_hasNativeBuffer) {
      return  reinterpretCast(owner) as NativeBuffer
    }
    return nil
  }
  
  func _uninitializedCopy(subRange: Range<Int>, var target: T*) -> T* {
    _invariantCheck()
    _sanityCheck(subRange.startIndex >= 0)
    _sanityCheck(subRange.endIndex >= subRange.startIndex)
    _sanityCheck(subRange.endIndex <= count)
    for i in subRange {
      target++.initialize(start[i])
    }
    return target
  }

  var elementStorage: T* {
    return start
  }

  var count: Int {
    get {
      return Int(_countAndFlags >> 1)
    }
    set {
      _invariantCheck()
      let growth = newValue - count
      if growth == 0 {
        return
      }
      nativeBuffer.count += growth
      _countAndFlags = (UInt(newValue) << 1) | (_countAndFlags & 1)
    }
  }
  
  var capacity: Int {
    let count = self.count
    if _slowPath(!_hasNativeBuffer) {
      return count
    }
    let n = nativeBuffer
    if (count + start) == (n.count + n.elementStorage) {
      return count + (n.capacity - n.count)
    }
    return count
  }

  mutating func isUniquelyReferenced() -> Bool {
    return Swift.isUniquelyReferenced(&owner)
  }

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

  subscript (subRange: Range<Int>) -> SliceBuffer {
    _sanityCheck(subRange.startIndex >= 0)
    _sanityCheck(subRange.endIndex >= subRange.startIndex)
    _sanityCheck(subRange.endIndex <= count)
    return SliceBuffer(
      owner: owner, start: start + subRange.startIndex,
      count: subRange.endIndex - subRange.startIndex, 
      hasNativeBuffer: _hasNativeBuffer)
  }

  //===--- Collection conformance -----------------------------------------===//
  var startIndex: Int {
    return 0
  }
  
  var endIndex: Int {
    return count
  }

  func generate() -> IndexingGenerator<SliceBuffer> {
    return IndexingGenerator(self)
  }

  //===--- misc -----------------------------------------------------------===//
  func withUnsafePointerToElements<R>(body: (UnsafePointer<T>)->R) -> R {
    let start = self.start
    return owner ? withExtendedLifetime(owner!) { body(start) } : body(start)
  }
}

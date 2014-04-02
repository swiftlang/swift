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

struct SliceBuffer<T> : ArrayBufferType {
  typealias Element = T
  typealias NativeStorage = NativeArrayStorage<T>
  typealias NativeBuffer = NativeArrayBuffer<T>

  init(
    owner: AnyObject?,
    start: UnsafePointer<T>,
    count: Int,
    hasNativeBuffer: Bool
  ) {
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
  
  init(x: NativeBuffer) {
    owner = x.storage
    start = x.elementStorage
    _countAndFlags = (UInt(x.count) << 1) | 1
    _invariantCheck()
  }

  func _invariantCheck() {
    let isNative = _hasNativeBuffer
    assert(
      (owner as NativeStorage).getLogicValue() == isNative
    )
    if isNative {
      assert(count <= nativeBuffer.count)
    }
  }
  
  var _hasNativeBuffer: Bool {
    assert(
      owner || (_countAndFlags & 1) == 0,
      "Something went wrong: an unowned buffer cannot have a native buffer")
    return (_countAndFlags & 1) != 0
  }

  var nativeBuffer: NativeBuffer {
    assert(_hasNativeBuffer)
    return NativeBuffer(owner as NativeStorage)
  }
  
  var owner: AnyObject?
  var start: UnsafePointer<T>
  var _countAndFlags: UInt

  //===--- Non-essential bits ---------------------------------------------===//
  //extension SliceBuffer : ArrayBufferType {
  
  func asCocoaArray() -> CocoaArray {
    // It's tempting to allocate a new instance of an NSArray subclass
    // that just wraps this Slice.  However, since we don't have a
    // managedByCopyOnWrite bit in the dynamically-allocated storage,
    // and we are in principle part of a mutable object, we need to
    // copy eagerly in most cases.  The one exception is when our
    // owner is nil.  We could think about doing that optimization
    // someday.
    assert(
      isBridgedToObjectiveC(T.self),
      "Array element type is not bridged to ObjectiveC")
    _invariantCheck()
    
    // FIXME: can't write this pending <rdar://problem/16397774>:
    // return asArray(UnsafeArray(start, count)) as NativeArray<T>
    var result = NativeArrayBuffer<T>()
    result += UnsafeArray(start, count)
    return result.asCocoaArray()
  }

  func _uninitializedCopy(
    subRange: Range<Int>, var target: UnsafePointer<T>
  ) -> UnsafePointer<T> {
    _invariantCheck()
    assert(subRange.startIndex >= 0)
    assert(subRange.endIndex >= subRange.startIndex)
    assert(subRange.endIndex <= count)
    for i in subRange {
      target++.initialize(start[i])
    }
    return target
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

  func isMutable() -> Bool {
    return _hasNativeBuffer
  }
  
  subscript(i: Int) -> T {
    get {
      assert(i >= 0, "negative slice index is out of range")
      assert(i < count, "slice index out of range")
      return start[i]
    }
    set {
      assert(i >= 0, "negative slice index is out of range")
      assert(i < count, "slice index out of range")
      assert(isMutable())
      start[i] = newValue
    }
  }

  subscript (subRange: Range<Int>) -> SliceBuffer {
    assert(subRange.startIndex >= 0)
    assert(subRange.endIndex >= subRange.startIndex)
    assert(subRange.endIndex <= count)
    return SliceBuffer(
      owner, start + subRange.startIndex,
      subRange.endIndex - subRange.startIndex, _hasNativeBuffer)
  }
}

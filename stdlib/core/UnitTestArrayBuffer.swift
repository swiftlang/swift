//===--- UnitTestArrayBuffer.swift ----------------------------------------===//
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

import SwiftShims

public struct _UnitTestArrayBuffer<T> : _ArrayBufferType {

  /// Make a buffer with uninitialized elements.  After using this
  /// method, you must either initialize the count elements at the
  /// result's .baseAddress or set the result's .count to zero.
  public init(count: Int, minimumCapacity: Int)
  {
    let realMinimumCapacity = max(count, minimumCapacity)
    if realMinimumCapacity == 0 {
      self = _UnitTestArrayBuffer<T>()
    }
    else {
      _base = _HeapBuffer(
        _ContiguousArrayStorage<T>.self,
        _ArrayBody(),
        realMinimumCapacity)

      var bridged = false
#if _runtime(_ObjC)
      if _canBeClass(T.self) != 0 {
        bridged = _isBridgedVerbatimToObjectiveC(T.self)
      }
#endif

      _base.value = _ArrayBody(
        count: count, capacity: _base._capacity(),
        elementTypeIsBridgedVerbatim: bridged)
    }
  }

  init(_ storage: _ContiguousArrayStorageBase?) {
    _base = unsafeBitCast(storage, _HeapBuffer<_ArrayBody, T>.self)
  }

  public var hasStorage: Bool {
    return _base.hasStorage
  }

  internal func _getBaseAddress() -> UnsafeMutablePointer<T> {
    return _base.hasStorage ? _base.baseAddress : nil
  }

  var arrayPropertyIsNative : Bool {
    return true
  }

  var arrayPropertyNeedsElementTypeCheck : Bool {
    return false
  }


  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, nil.
  public var baseAddress: UnsafeMutablePointer<T> {
    return _getBaseAddress()
  }

  /// A pointer to the first element, assuming that the elements are stored
  /// contiguously.
  var _unsafeElementStorage: UnsafeMutablePointer<T> {
    return _base.baseAddress
  }

  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.
  public func withUnsafeBufferPointer<R>(
    @noescape body: (UnsafeBufferPointer<Element>) -> R
  ) -> R {
    let ret = body(UnsafeBufferPointer(start: self.baseAddress, count: count))
    _fixLifetime(self)
    return ret
  }

  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.
  public mutating func withUnsafeMutableBufferPointer<R>(
    @noescape body: (UnsafeMutableBufferPointer<T>) -> R
  ) -> R {
    let ret = body(
      UnsafeMutableBufferPointer(start: baseAddress, count: count))
    _fixLifetime(self)
    return ret
  }

  //===--- _ArrayBufferType conformance -----------------------------------===//
  /// The type of elements stored in the buffer
  public typealias Element = T

  /// create an empty buffer
  public init() {
    _base = unsafeBitCast(_emptyArrayStorage, _HeapBuffer<_ArrayBody, T>.self)
  }

  /// Adopt the storage of x
  public init(_ buffer: _ContiguousArrayBuffer<Element>) {
    _base = _HeapBuffer(buffer.owner)
  }

  public mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
    -> _ContiguousArrayBuffer<Element>?
  {
    if _fastPath(isUniquelyReferenced() && capacity >= minimumCapacity) {
      return requestNativeBuffer()
    }
    return nil
  }

  public mutating func isMutableAndUniquelyReferenced() -> Bool {
    return isUniquelyReferenced()
  }

  public mutating func isMutableAndUniquelyReferencedOrPinned() -> Bool {
    return isUniquelyReferencedOrPinned()
  }

  /// If this buffer is backed by a `_UnitTestArrayBuffer`
  /// containing the same number of elements as `self`, return it.
  /// Otherwise, return `nil`.
  public func requestNativeBuffer() -> _ContiguousArrayBuffer<Element>? {
    return _ContiguousArrayBuffer(
      _base.storage as? _ContiguousArrayStorageBase ?? _emptyArrayStorage)
  }

  /// Replace the given subRange with the first newCount elements of
  /// the given collection.
  ///
  /// Requires: this buffer is backed by a uniquely-referenced
  /// _UnitTestArrayBuffer
  public mutating func replace<
    C: CollectionType where C.Generator.Element == Element
  >(
    #subRange: Range<Int>, with newCount: Int, elementsOf newValues: C
  ) {
    _arrayNonSliceInPlaceReplace(&self, subRange, newCount, newValues)
  }

  @inline(__always)
  func getElement(i: Int, _ isNative: Bool, _ needsElementTypeCheck: Bool) -> T {
    _sanityCheck(_isValidSubscript(i, false), "Array index out of range")
    // If the index is in bounds, we can assume we have storage.
    return _unsafeElementStorage[i]
  }

  /// Get/set the value of the ith element
  public subscript(i: Int) -> T {
    get {
      return getElement(i, true, false)
    }
    nonmutating set {
      _sanityCheck(i >= 0 && i < count, "Array index out of range")
      // If the index is in bounds, we can assume we have storage.

      // FIXME: Manually swap because it makes the ARC optimizer happy.  See
      // <rdar://problem/16831852> check retain/release order
      // _unsafeElementStorage[i] = newValue
      var nv = newValue
      let tmp = nv
      nv = _unsafeElementStorage[i]
      _unsafeElementStorage[i] = tmp
    }
  }

  /// How many elements the buffer stores
  public var count: Int {
    get {
      return _base.hasStorage ? _base.value.count : 0
    }
    nonmutating set {
      _sanityCheck(newValue >= 0)

      _sanityCheck(
        newValue <= capacity,
        "Can't grow an array buffer past its capacity")

      _sanityCheck(_base.hasStorage || newValue == 0)

      if _base.hasStorage {
        _base.value.count = newValue
      }
    }
  }

  /// Return whether the given `index` is valid for subscripting, i.e. `0
  /// ≤ index < count`
  func _isValidSubscript(index : Int, _ isNative: Bool) -> Bool {
    /// Instead of returning 0 for no storage, we explicitly check
    /// for the existance of storage.
    /// Note that this is better than folding hasStorage in to
    /// the return from this function, as this implementation generates
    /// no shortcircuiting blocks.
    _precondition(_base.hasStorage, "Cannot index empty buffer")
    return (index >= 0) && (index < _base.value.count)
  }

  /// How many elements the buffer can store without reallocation
  public var capacity: Int {
    return _base.hasStorage ? _base.value.capacity : 0
  }

  /// Copy the given subRange of this buffer into uninitialized memory
  /// starting at target.  Return a pointer past-the-end of the
  /// just-initialized memory.
  public func _uninitializedCopy(
    subRange: Range<Int>, target: UnsafeMutablePointer<T>
  ) -> UnsafeMutablePointer<T> {
    _sanityCheck(subRange.startIndex >= 0)
    _sanityCheck(subRange.endIndex >= subRange.startIndex)
    _sanityCheck(subRange.endIndex <= count)

    let c = subRange.endIndex - subRange.startIndex
    target.initializeFrom(baseAddress + subRange.startIndex, count: c)
    _fixLifetime(owner)
    return target + c
  }

  /// Return a _SliceBuffer containing the given subRange of values
  /// from this buffer.
  public subscript(subRange: Range<Int>) -> _SliceBuffer<T>
  {
    return _SliceBuffer(
      owner: _base.storage ?? _emptyArrayStorage,
      start: baseAddress + subRange.startIndex,
      count: subRange.endIndex - subRange.startIndex,
      hasNativeBuffer: true)
  }

  /// Return true iff this buffer's storage is uniquely-referenced.
  /// NOTE: this does not mean the buffer is mutable.  Other factors
  /// may need to be considered, such as whether the buffer could be
  /// some immutable Cocoa container.
  public mutating func isUniquelyReferenced() -> Bool {
    return _base.isUniquelyReferenced()
  }
  public mutating func isUniquelyReferencedOrPinned() -> Bool {
    return _base.isUniquelyReferencedOrPinned()
  }

#if _runtime(_ObjC)
  /// Convert to an NSArray.
  /// Precondition: T is bridged to Objective-C
  /// O(1).
  public func _asCocoaArray() -> _NSArrayCoreType {
    _sanityCheck(
        _isBridgedToObjectiveC(T.self),
        "Array element type is not bridged to Objective-C")
    if count == 0 {
      return _SwiftDeferredNSArray(
        _nativeStorage: _emptyArrayStorage)
    }
    return _SwiftDeferredNSArray(_nativeStorage: _storage!)
  }
#endif

  /// An object that keeps the elements stored in this buffer alive
  public var owner: AnyObject {
    return _storage ?? _emptyArrayStorage
  }

  /// A value that identifies the storage used by the buffer.  Two
  /// buffers address the same elements when they have the same
  /// identity and count.
  public var identity: UnsafePointer<Void> {
    return withUnsafeBufferPointer { UnsafePointer($0.baseAddress) }
  }
  
  /// Return true iff we have storage for elements of the given
  /// `proposedElementType`.  If not, we'll be treated as immutable.
  func canStoreElementsOfDynamicType(proposedElementType: Any.Type) -> Bool {
    if let s = _storage {
      return s.canStoreElementsOfDynamicType(proposedElementType)
    }
    return false
  }

  /// Return true if the buffer stores only elements of type `U`.
  /// Requires: `U` is a class or `@objc` existential. O(N)
  func storesOnlyElementsOfType<U>(
    _: U.Type
  ) -> Bool {
    _sanityCheck(_isClassOrObjCExistential(U.self))
    
    // Start with the base class so that optimizations based on
    // 'final' don't bypass dynamic type check.
    let s: _ContiguousArrayStorageBase? = _storage
    
    if _fastPath(s != nil){
      if _fastPath(s!.staticElementType is U.Type) {
        // Done in O(1)
        return true
      }
    }

    // Check the elements
    for x in self {
      if !(x is U) {
        return false
      }
    }
    return true
  }

  //===--- private --------------------------------------------------------===//
  var _storage: _ContiguousArrayStorageBase? {
    return unsafeBitCast(
      _base.storage, Optional<_ContiguousArrayStorageBase>.self)
  }

  typealias _Base = _HeapBuffer<_ArrayBody, T>
  var _base: _Base
}

/// Append the elements of rhs to lhs
public func += <
  T, C: CollectionType where C.Generator.Element == T
> (
  inout lhs: _UnitTestArrayBuffer<T>, rhs: C
) {
  let oldCount = lhs.count
  let newCount = oldCount + numericCast(count(rhs))

  if _fastPath(newCount <= lhs.capacity) {
    lhs.count = newCount
    (lhs.baseAddress + oldCount).initializeFrom(rhs)
  }
  else {
    let newLHS = _UnitTestArrayBuffer<T>(
      count: newCount,
      minimumCapacity: _growArrayCapacity(lhs.capacity))

    if lhs._base.hasStorage {
      newLHS.baseAddress.moveInitializeFrom(lhs.baseAddress, count: oldCount)
      lhs._base.value.count = 0
    }
    lhs._base = newLHS._base
    (lhs._base.baseAddress + oldCount).initializeFrom(rhs)
  }
}

/// Append rhs to lhs
public func += <T> (inout lhs: _UnitTestArrayBuffer<T>, rhs: T) {
  lhs += CollectionOfOne(rhs)
}

func === <T>(
  lhs: _UnitTestArrayBuffer<T>, rhs: _UnitTestArrayBuffer<T>
) -> Bool {
  return lhs._base == rhs._base
}

func !== <T>(
  lhs: _UnitTestArrayBuffer<T>, rhs: _UnitTestArrayBuffer<T>
) -> Bool {
  return lhs._base != rhs._base
}

extension _UnitTestArrayBuffer : CollectionType {
  /// The position of the first element in a non-empty collection.
  ///
  /// Identical to `endIndex` in an empty collection.
  public var startIndex: Int {
    return 0
  }
  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: Int {
    return count
  }
  
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> IndexingGenerator<_UnitTestArrayBuffer> {
    return IndexingGenerator(self)
  }
}


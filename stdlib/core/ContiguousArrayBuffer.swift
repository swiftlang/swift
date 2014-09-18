//===--- ArrayBridge.swift - Array<T> <=> NSArray bridging ----------------===//
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

// The empty array prototype.  We use the same object for all empty
// [Native]Array<T>s.
let _emptyContiguousArrayStorageBase = unsafeBitCast(
  _ContiguousArrayBuffer<Int>(count: 0, minimumCapacity: 0),
  _ContiguousArrayStorageBase.self
)

// The class that implements the storage for a ContiguousArray<T>
final class _ContiguousArrayStorage<T> : _ContiguousArrayStorageBase {
  typealias Buffer = _ContiguousArrayBuffer<T>

  deinit {
    let b = Buffer(self)
    b.baseAddress.destroy(b.count)
    b._base._value.destroy()
  }

  final func __getInstanceSizeAndAlignMask() -> (Int,Int) {
    return Buffer(self)._base._allocatedSizeAndAlignMask()
  }

  /// If `T` is bridged verbatim, returns a pointer to the array data buffer.
  /// Otherwise, returns a null pointer.
  override internal func _tryGetVerbatimBridgedUnsafeBuffer(
    dummy: Void
  ) -> UnsafeBufferPointer<AnyObject> {
    if _isBridgedVerbatimToObjectiveC(T.self) {
      let nativeBuffer = Buffer(self)
      return UnsafeBufferPointer(
        start: UnsafePointer(nativeBuffer.baseAddress),
        count: nativeBuffer.count)
    }
    return UnsafeBufferPointer(start: .null(), count: 0)
  }

  /// Returns the number of elements in the array.
  ///
  /// Precondition: `T` is bridged non-verbatim.
  override internal func _getNonVerbatimBridgedCount(dummy: Void) -> Int {
    _sanityCheck(
      !_isBridgedVerbatimToObjectiveC(T.self),
      "Verbatim bridging for should be handled separately")
    return Buffer(self).count
  }

  /// Bridge array elements and return a new buffer that owns them.
  ///
  /// Precondition: `T` is bridged non-verbatim.
  override internal func _getNonVerbatimBridgedHeapBuffer(dummy: Void) ->
    HeapBuffer<Int, AnyObject> {
    _sanityCheck(
      !_isBridgedVerbatimToObjectiveC(T.self),
      "Verbatim bridging for should be handled separately")
    let nativeBuffer = Buffer(self)
    let count = nativeBuffer.count
    let result = HeapBuffer<Int, AnyObject>(
      HeapBufferStorage<Int, AnyObject>.self, count, count)
    let resultPtr = result.baseAddress
    for i in 0..<count {
      (resultPtr + i).initialize(
        _bridgeToObjectiveCUnconditional(nativeBuffer[i]))
    }
    return result
  }

  /// Return true if the `proposedElementType` is `T` or a subclass of
  /// `T`.  We can't store anything else without violating type
  /// safety; for example, the destructor has static knowledge that
  /// all of the elements can be destroyed as `T`
  override func canStoreElementsOfDynamicType(
    proposedElementType: Any.Type
  ) -> Bool {
    return proposedElementType is T.Type
  }

  /// A type that every element in the array is.
  override var staticElementType: Any.Type {
    return T.self
  }
}

public struct _ContiguousArrayBuffer<T> : _ArrayBufferType {

  /// Make a buffer with uninitialized elements.  After using this
  /// method, you must either initialize the count elements at the
  /// result's .baseAddress or set the result's .count to zero.
  public init(count: Int, minimumCapacity: Int)
  {
    _base = HeapBuffer(
      _ContiguousArrayStorage<T>.self,
      _ArrayBody(),
      max(count, minimumCapacity))

    var bridged = false
    if _canBeClass(T.self) != 0 {
      bridged = _isBridgedVerbatimToObjectiveC(T.self)
    }

    _base.value = _ArrayBody(
      count: count, capacity: _base._capacity(),
      elementTypeIsBridgedVerbatim: bridged)
  }

  init(_ storage: _ContiguousArrayStorage<T>?) {
    _base = unsafeBitCast(storage , HeapBuffer<_ArrayBody, T>.self)
  }

  public var hasStorage: Bool {
    return _base.hasStorage
  }

  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, nil.
  public var baseAddress: UnsafeMutablePointer<T> {
    return _base.hasStorage ? _base.baseAddress : nil
  }

  /// A pointer to the first element, assuming that the elements are stored
  /// contiguously.
  var _unsafeElementStorage: UnsafeMutablePointer<T> {
    return _base.baseAddress
  }

  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.
  public func withUnsafeBufferPointer<R>(
    body: (UnsafeBufferPointer<Element>)->R
  ) -> R {
    let ret = body(UnsafeBufferPointer(start: self.baseAddress, count: count))
    _fixLifetime(self)
    return ret
  }

  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.
  public mutating func withUnsafeMutableBufferPointer<R>(
    body: (UnsafeMutableBufferPointer<T>)->R
  ) -> R {
    let ret = body(
      UnsafeMutableBufferPointer(start: baseAddress, count: count))
    _fixLifetime(self)
    return ret
  }

  public mutating func take() -> _ContiguousArrayBuffer {
    if !_base.hasStorage {
      return _ContiguousArrayBuffer()
    }
    _sanityCheck(_base.isUniquelyReferenced(), "Can't \"take\" a shared array buffer")
    let result = self
    _base = _Base()
    return result
  }

  //===--- _ArrayBufferType conformance -----------------------------------===//
  /// The type of elements stored in the buffer
  public typealias Element = T

  /// create an empty buffer
  public init() {
    _base = HeapBuffer()
  }

  /// Adopt the storage of x
  public init(_ buffer: _ContiguousArrayBuffer) {
    self = buffer
  }

  public mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
    -> _ContiguousArrayBuffer<Element>?
  {
    if _fastPath(isUniquelyReferenced() && capacity >= minimumCapacity) {
      return self
    }
    return nil
  }

  public mutating func isMutableAndUniquelyReferenced() -> Bool {
    return isUniquelyReferenced()
  }

  /// If this buffer is backed by a _ContiguousArrayBuffer, return it.
  /// Otherwise, return nil.  Note: the result's baseAddress may
  /// not match ours, if we are a _SliceBuffer.
  public func requestNativeBuffer() -> _ContiguousArrayBuffer<Element>? {
    return self
  }

  /// Replace the given subRange with the first newCount elements of
  /// the given collection.
  ///
  /// Requires: this buffer is backed by a uniquely-referenced
  /// _ContiguousArrayBuffer
  public mutating func replace<
    C: CollectionType where C.Generator.Element == Element
  >(
    #subRange: Range<Int>, with newCount: Int, elementsOf newValues: C
  ) {
    _arrayNonSliceInPlaceReplace(&self, subRange, newCount, newValues)
  }

  /// Get/set the value of the ith element
  public subscript(i: Int) -> T {
    get {
      _sanityCheck(_isValidSubscript(i), "Array index out of range")
      // If the index is in bounds, we can assume we have storage.
      return _unsafeElementStorage[i]
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
  func _isValidSubscript(index : Int) -> Bool {
    /// Instead of returning 0 for no storage, we explicitly check
    /// for the existance of storage.
    /// Note that this is better than folding hasStorage in to
    /// the return from this function, as this implementation generates
    /// no shortcircuiting blocks.
    _precondition(_base.hasStorage, "Cannot index empty buffer")
    return (index >= 0) & (index < _base.value.count)
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

    var dst = target
    var src = baseAddress + subRange.startIndex
    for i in subRange {
      dst++.initialize(src++.memory)
    }
    _fixLifetime(owner)
    return dst
  }

  /// Return a _SliceBuffer containing the given subRange of values
  /// from this buffer.
  public subscript(subRange: Range<Int>) -> _SliceBuffer<T>
  {
    return _SliceBuffer(
      owner: _base.storage,
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

  /// Returns true iff this buffer is mutable. NOTE: a true result
  /// does not mean the buffer is uniquely-referenced.
  public func isMutable() -> Bool {
    return true
  }

  /// Convert to an NSArray.
  /// Precondition: T is bridged to Objective-C
  /// O(1).
  public func _asCocoaArray() -> _SwiftNSArrayRequiredOverridesType {
    _sanityCheck(
        _isBridgedToObjectiveC(T.self),
        "Array element type is not bridged to ObjectiveC")
    if count == 0 {
      return _NSSwiftArrayImpl(
        _nativeStorage: _emptyContiguousArrayStorageBase)
    }
    return _NSSwiftArrayImpl(_nativeStorage: _storage!)
  }

  /// An object that keeps the elements stored in this buffer alive
  public var owner: AnyObject? {
    return _storage
  }

  /// A value that identifies first mutable element, if any.  Two
  /// arrays compare === iff they are both empty, or if their buffers
  /// have the same identity and count.
  public var identity: Word {
    return unsafeBitCast(baseAddress, Word.self)
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
    let s = _storage
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
  typealias _OptionalStorage = _ContiguousArrayStorage<T>?
  var _storage: _ContiguousArrayStorage<T>? {
    return unsafeBitCast(_base.storage, _OptionalStorage.self)
  }

  typealias _Base = HeapBuffer<_ArrayBody, T>
  var _base: _Base
}

/// Append the elements of rhs to lhs
public func += <
  T, C: CollectionType where C._Element == T
> (
  inout lhs: _ContiguousArrayBuffer<T>, rhs: C
) {
  let oldCount = lhs.count
  let newCount = oldCount + numericCast(countElements(rhs))

  if _fastPath(newCount <= lhs.capacity) {
    lhs.count = newCount
    (lhs.baseAddress + oldCount).initializeFrom(rhs)
  }
  else {
    let newLHS = _ContiguousArrayBuffer<T>(
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
public func += <T> (inout lhs: _ContiguousArrayBuffer<T>, rhs: T) {
  lhs += CollectionOfOne(rhs)
}

func === <T>(
  lhs: _ContiguousArrayBuffer<T>, rhs: _ContiguousArrayBuffer<T>
) -> Bool {
  return lhs._base == rhs._base
}

func !== <T>(
  lhs: _ContiguousArrayBuffer<T>, rhs: _ContiguousArrayBuffer<T>
) -> Bool {
  return lhs._base != rhs._base
}

extension _ContiguousArrayBuffer : CollectionType {
  public var startIndex: Int {
    return 0
  }
  public var endIndex: Int {
    return count
  }
  public func generate() -> IndexingGenerator<_ContiguousArrayBuffer> {
    return IndexingGenerator(self)
  }
}

public func ~> <
  S: _Sequence_Type
>(
  source: S, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<S.Generator.Element>
{
  let initialCapacity = source~>_underestimateCount()
  var result = _ContiguousArrayBuffer<S.Generator.Element>(
    count: 0, minimumCapacity: initialCapacity)

  // Using GeneratorSequence here essentially promotes the sequence to
  // a SequenceType from _Sequence_Type so we can iterate the elements
  for x in GeneratorSequence(source.generate()) {
    result += x
  }
  return result.take()
}

public func ~> <
  C: protocol<_CollectionType,_Sequence_Type>
>(
  source: C, _:(_CopyToNativeArrayBuffer, ())
) -> _ContiguousArrayBuffer<C.Generator.Element>
{
  return _copyCollectionToNativeArrayBuffer(source)
}

func _copyCollectionToNativeArrayBuffer<
  C: protocol<_CollectionType,_Sequence_Type>
>(source: C) -> _ContiguousArrayBuffer<C.Generator.Element>
{
  let count = countElements(source)
  if count == 0 {
    return _ContiguousArrayBuffer()
  }

  var result = _ContiguousArrayBuffer<C.Generator.Element>(
    count: numericCast(count),
    minimumCapacity: 0
  )

  var p = result.baseAddress
  for x in GeneratorSequence(source.generate()) {
    (p++).initialize(x)
  }

  return result
}

protocol _ArrayType : CollectionType {
  var count: Int {get}

  typealias _Buffer : _ArrayBufferType
  var _buffer: _Buffer {get}
}

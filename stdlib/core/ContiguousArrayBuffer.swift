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
let emptyNSSwiftArray : _NSSwiftArray
  = reinterpretCast(ContiguousArrayBuffer<Int>(count: 0, minimumCapacity: 0))

// The class that implements the storage for a ContiguousArray<T>
@final class ContiguousArrayStorage<T> : _NSSwiftArray {
  typealias Buffer = ContiguousArrayBuffer<T>
  
  deinit {
    let b = Buffer(self)
    b.elementStorage.destroy(b.count)
    b._base._value.destroy()
  }

  @final func __getInstanceSizeAndAlignMask() -> (Int,Int) {
    return Buffer(self)._base._allocatedSizeAndAlignMask()
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

struct ContiguousArrayBuffer<T> : ArrayBufferType, LogicValue {
  
  /// Make a buffer with uninitialized elements.  After using this
  /// method, you must either initialize the count elements at the
  /// result's .elementStorage or set the result's .count to zero.
  init(count: Int, minimumCapacity: Int)
  {
    _base = HeapBuffer(
      ContiguousArrayStorage<T>.self,
      _ArrayBody(),
      max2(count, minimumCapacity))

    var bridged = false
    if _canBeClass(T.self) {
      bridged = isBridgedVerbatimToObjectiveC(T.self)
    }

    _base.value = _ArrayBody(count: count, capacity: _base._capacity(),  
                            elementTypeIsBridgedVerbatim: bridged)
  }

  init(_ storage: ContiguousArrayStorage<T>?) {
    _base = reinterpretCast(storage)
  }
  
  func getLogicValue() -> Bool {
    return _base.getLogicValue()
  }

  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, nil.
  var elementStorage: UnsafePointer<T> {
    return _base ? _base.elementStorage : nil
  }

  /// A pointer to the first element, assuming that the elements are stored
  /// contiguously.
  var _unsafeElementStorage: UnsafePointer<T> {
    return _base.elementStorage
  }

  func withUnsafePointerToElements<R>(body: (UnsafePointer<T>)->R) -> R {
    let p = _base.elementStorage
    return withExtendedLifetime(_base) { body(p) }
  }

  mutating func take() -> ContiguousArrayBuffer {
    if !_base {
      return ContiguousArrayBuffer()
    }
    _sanityCheck(_base.isUniquelyReferenced(), "Can't \"take\" a shared array buffer")
    let result = self
    _base = _Base()
    return result
  }

  //===--- ArrayBufferType conformance ------------------------------------===//
  /// The type of elements stored in the buffer
  typealias Element = T

  /// create an empty buffer
  init() {
    _base = HeapBuffer()
  }

  /// Adopt the storage of x
  init(_ buffer: ContiguousArrayBuffer) {
    self = buffer
  }
  
  mutating func requestUniqueMutableBuffer(minimumCapacity: Int)
    -> ContiguousArrayBuffer<Element>?
  {
    return isUniquelyReferenced() && capacity >= minimumCapacity ? self : nil
  }

  /// If this buffer is backed by a ContiguousArrayBuffer, return it.
  /// Otherwise, return nil.  Note: the result's elementStorage may
  /// not match ours, if we are a SliceBuffer.
  func requestNativeBuffer() -> ContiguousArrayBuffer<Element>? {
    return self
  }
  
  /// Get/set the value of the ith element
  subscript(i: Int) -> T {
    get {
      _sanityCheck(i >= 0 && i < count, "Array index out of range")
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
  var count: Int {
    get {
      return _base ? _base.value.count : 0
    }
    nonmutating set {
      _sanityCheck(newValue >= 0)
      
      _sanityCheck(
        newValue <= capacity,
        "Can't grow an array buffer past its capacity")

      _sanityCheck(_base || newValue == 0)
      
      if _base {
        _base.value.count = newValue
      }
    }
  }

  /// How many elements the buffer can store without reallocation
  var capacity: Int {
    return _base ? _base.value.capacity : 0
  }

  /// Copy the given subRange of this buffer into uninitialized memory
  /// starting at target.  Return a pointer past-the-end of the
  /// just-initialized memory.
  func _uninitializedCopy(
    subRange: Range<Int>, target: UnsafePointer<T>
  ) -> UnsafePointer<T> {
    _sanityCheck(subRange.startIndex >= 0)
    _sanityCheck(subRange.endIndex >= subRange.startIndex)
    _sanityCheck(subRange.endIndex <= count)
    
    var dst = target
    var src = elementStorage + subRange.startIndex
    for i in subRange {
      dst++.initialize(src++.memory)
    }
    _fixLifetime(owner)
    return dst
  }
  
  /// Return a SliceBuffer containing the given subRange of values
  /// from this buffer.
  subscript(subRange: Range<Int>) -> SliceBuffer<T>
  {
    return SliceBuffer(
      owner: _base.storage,
      start: elementStorage + subRange.startIndex,
      count: subRange.endIndex - subRange.startIndex,
      hasNativeBuffer: true)
  }
  
  /// Return true iff this buffer's storage is uniquely-referenced.
  /// NOTE: this does not mean the buffer is mutable.  Other factors
  /// may need to be considered, such as whether the buffer could be
  /// some immutable Cocoa container.
  mutating func isUniquelyReferenced() -> Bool {
    return _base.isUniquelyReferenced()
  }

  /// Returns true iff this buffer is mutable. NOTE: a true result
  /// does not mean the buffer is uniquely-referenced.
  func isMutable() -> Bool {
    return true
  }

  /// Convert to an NSArray.
  /// Precondition: T is bridged to Objective-C
  /// O(1) if T is bridged verbatim, O(N) otherwise
  func _asCocoaArray() -> _CocoaArray {
    _sanityCheck(
      isBridgedToObjectiveC(T.self),
      "Array element type is not bridged to ObjectiveC")
    if count == 0 {
      return emptyNSSwiftArray
    }
    if _fastPath(_base.value.elementTypeIsBridgedVerbatim) {
      return reinterpretCast(_base.storage)
    }
    return ContiguousArray(self).map { bridgeToObjectiveC($0)! }._buffer._storage!
  }
  
  /// An object that keeps the elements stored in this buffer alive
  var owner: AnyObject? {
    return _storage
  }

  /// A value that identifies first mutable element, if any.  Two
  /// arrays compare === iff they are both empty, or if their buffers
  /// have the same identity and count.
  var identity: Word {
    return reinterpretCast(elementStorage)
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
    if _fastPath(s) {
      if _fastPath(s!.staticElementType is U.Type) {
        // Done in O(1)
        return true
      }
    }
    
    // Check the elements
    for x in self {
      // FIXME: reinterpretCast works around <rdar://problem/16953026>
      if !(reinterpretCast(x) as AnyObject is U) {
        return false
      }
    }
    return true
  }
  
  //===--- private --------------------------------------------------------===//
  var _storage: ContiguousArrayStorage<T>? {
    return reinterpretCast(_base.storage)
  }
  
  typealias _Base = HeapBuffer<_ArrayBody, T>
  var _base: _Base
}

/// Append the elements of rhs to lhs
func += <
  T, C: Collection where C._Element == T
> (
  inout lhs: ContiguousArrayBuffer<T>, rhs: C
) {
  let oldCount = lhs.count
  let newCount = oldCount + numericCast(countElements(rhs))

  if _fastPath(newCount <= lhs.capacity) {
    lhs.count = newCount
    (lhs.elementStorage + oldCount).initializeFrom(rhs)
  }
  else {
    let newLHS = ContiguousArrayBuffer<T>(count: newCount, 
                                      minimumCapacity: lhs.capacity * 2)
    if lhs._base {
      newLHS.elementStorage.moveInitializeFrom(lhs.elementStorage, 
                                               count: oldCount)
      lhs._base.value.count = 0
    }
    lhs._base = newLHS._base
    (lhs._base.elementStorage + oldCount).initializeFrom(rhs)
  }
}

/// Append rhs to lhs
func += <T> (inout lhs: ContiguousArrayBuffer<T>, rhs: T) {
  lhs += CollectionOfOne(rhs)
}

func === <T>(
  lhs: ContiguousArrayBuffer<T>, rhs: ContiguousArrayBuffer<T>
) -> Bool {
  return lhs._base == rhs._base
}

func !== <T>(
  lhs: ContiguousArrayBuffer<T>, rhs: ContiguousArrayBuffer<T>
) -> Bool {
  return lhs._base != rhs._base
}

extension ContiguousArrayBuffer : Collection {
  var startIndex: Int {
    return 0
  }
  var endIndex: Int {
    return count
  }
  func generate() -> IndexingGenerator<ContiguousArrayBuffer> {
    return IndexingGenerator(self)
  }
}

func ~> <
  S: _Sequence_
>(
  source: S, _: (_CopyToNativeArrayBuffer,())
) -> ContiguousArrayBuffer<S.GeneratorType.Element>
{
  var result = ContiguousArrayBuffer<S.GeneratorType.Element>()

  // Using GeneratorSequence here essentially promotes the sequence to
  // a Sequence from _Sequence_ so we can iterate the elements
  for x in GeneratorSequence(source.generate()) {
    result += x
  }
  return result.take()
}

func ~> <  
  C: protocol<_Collection,_Sequence_>
>(
  source: C, _:(_CopyToNativeArrayBuffer, ())
) -> ContiguousArrayBuffer<C.GeneratorType.Element>
{
  return _copyCollectionToNativeArrayBuffer(source)
}

func _copyCollectionToNativeArrayBuffer<C: protocol<_Collection,_Sequence_>>(
  source: C
) -> ContiguousArrayBuffer<C.GeneratorType.Element>
{
  let count = countElements(source)
  if count == 0 {
    return ContiguousArrayBuffer()
  }
  
  var result = ContiguousArrayBuffer<C.GeneratorType.Element>(
    count: numericCast(count),
    minimumCapacity: 0
  )

  var p = result.elementStorage
  for x in GeneratorSequence(source.generate()) {
    (p++).initialize(x)
  }
  
  return result
}

protocol _ArrayType : Collection {
  var count: Int {get}

  typealias _Buffer : ArrayBufferType
  var _buffer: _Buffer {get}
}

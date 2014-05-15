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
let emptyNSSwiftArray : NSSwiftArray
  = reinterpretCast(NativeArrayBuffer<Int>(count: 0, minimumCapacity: 0))

// The class that implements the storage for a NativeArray<T>
@final class NativeArrayStorage<T> : NSSwiftArray {
  typealias Buffer = NativeArrayBuffer<T>
  
  deinit {
    let b = Buffer(self)
    b.elementStorage.destroy(b.count)
    b.base._value.destroy()
  }

  override var dynamicElementType: Any.Type {
    return T.self
  }
}

struct NativeArrayBuffer<T> : ArrayBufferType, LogicValue {
  
  /// Make a buffer with uninitialized elements.  After using this
  /// method, you must either initialize the count elements at the
  /// result's .elementStorage or set the result's .count to zero.
  init(count: Int, minimumCapacity: Int)
  {
    base = HeapBuffer(
      NativeArrayStorage<T>.self,
      _ArrayBody(),
      max(count, minimumCapacity))

    var bridged = false
    if _canBeClass(T.self) {
      bridged = isBridgedVerbatimToObjectiveC(T.self)
    }

    base.value = _ArrayBody(count: count, capacity: base._capacity(),  
                            elementTypeIsBridgedVerbatim: bridged)
  }

  init(_ storage: NativeArrayStorage<T>?) {
    base = reinterpretCast(storage)
  }
  
  func getLogicValue() -> Bool {
    return base.getLogicValue()
  }

  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, nil.
  var elementStorage: UnsafePointer<T> {
    return base ? base.elementStorage : nil
  }

  /// A pointer to the first element, assuming that the elements are stored
  /// contiguously.
  var _unsafeElementStorage: UnsafePointer<T> {
    return base.elementStorage
  }

  func withUnsafePointerToElements<R>(body: (UnsafePointer<T>)->R) -> R {
    let p = base.elementStorage
    return withExtendedLifetime(base) { body(p) }
  }

  mutating func take() -> NativeArrayBuffer {
    if !base {
      return NativeArrayBuffer()
    }
    assert(base.isUniquelyReferenced(), "Can't \"take\" a shared array buffer")
    let result = self
    base = Base()
    return result
  }

  //===--- ArrayBufferType conformance ------------------------------------===//
  /// The type of elements stored in the buffer
  typealias Element = T

  /// create an empty buffer
  init() {
    base = HeapBuffer()
  }

  /// Adopt the storage of x
  init(_ buffer: NativeArrayBuffer) {
    self = buffer
  }
  
  mutating func requestUniqueMutableBuffer(minimumCapacity: Int)
    -> NativeArrayBuffer<Element>?
  {
    return isUniquelyReferenced() && capacity >= minimumCapacity ? self : nil
  }

  /// If this buffer is backed by a NativeArrayBuffer, return it.
  /// Otherwise, return nil.  Note: the result's elementStorage may
  /// not match ours, if we are a SliceBuffer.
  func requestNativeBuffer() -> NativeArrayBuffer<Element>? {
    return self
  }
  
  /// Get/set the value of the ith element
  subscript(i: Int) -> T {
    get {
      assert(i >= 0 && i < count, "Array index out of range")
      // If the index is in bounds, we can assume we have storage.
      return _unsafeElementStorage[i]
    }
    nonmutating set {
      assert(i >= 0 && i < count, "Array index out of range")
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
      return base ? base.value.count : 0
    }
    nonmutating set {
      assert(newValue >= 0)
      
      assert(
        newValue <= capacity,
        "Can't grow an array buffer past its capacity")

      assert(base || newValue == 0)
      
      if base {
        base.value.count = newValue
      }
    }
  }

  /// How many elements the buffer can store without reallocation
  var capacity: Int {
    return base ? base.value.capacity : 0
  }

  /// Copy the given subRange of this buffer into uninitialized memory
  /// starting at target.  Return a pointer past-the-end of the
  /// just-initialized memory.
  func _uninitializedCopy(
    subRange: Range<Int>, target: UnsafePointer<T>
  ) -> UnsafePointer<T> {
    assert(subRange.startIndex >= 0)
    assert(subRange.endIndex >= subRange.startIndex)
    assert(subRange.endIndex <= count)
    
    var dst = target
    var src = elementStorage + subRange.startIndex
    for i in subRange {
      dst++.initialize(src++.get())
    }
    _fixLifetime(owner)
    return dst
  }
  
  /// Return a SliceBuffer containing the given subRange of values
  /// from this buffer.
  subscript(subRange: Range<Int>) -> SliceBuffer<T>
  {
    return SliceBuffer(
      owner: base.storage,
      start: elementStorage + subRange.startIndex,
      count: subRange.endIndex - subRange.startIndex,
      hasNativeBuffer: true)
  }
  
  /// Return true iff this buffer's storage is uniquely-referenced.
  /// NOTE: this does not mean the buffer is mutable.  Other factors
  /// may need to be considered, such as whether the buffer could be
  /// some immutable Cocoa container.
  mutating func isUniquelyReferenced() -> Bool {
    return base.isUniquelyReferenced()
  }

  /// Returns true iff this buffer is mutable. NOTE: a true result
  /// does not mean the buffer is uniquely-referenced.
  func isMutable() -> Bool {
    return true
  }

  /// Convert to an NSArray.
  /// Precondition: T is bridged to Objective-C
  /// O(1) if T is bridged verbatim, O(N) otherwise
  func asCocoaArray() -> CocoaArray {
    assert(
      isBridgedToObjectiveC(T.self),
      "Array element type is not bridged to ObjectiveC")
    if count == 0 {
      return emptyNSSwiftArray
    }
    if _fastPath(base.value.elementTypeIsBridgedVerbatim) {
      return reinterpretCast(base.storage)
    }
    return NativeArray(self).map { bridgeToObjectiveC($0)! }.buffer.storage!
  }
  
  /// An object that keeps the elements stored in this buffer alive
  var owner: AnyObject? {
    return storage
  }

  /// A value that identifies first mutable element, if any.  Two
  /// arrays compare === iff they are both empty, or if their buffers
  /// have the same identity and count.
  var identity: Word {
    return reinterpretCast(elementStorage)
  }
  
  func asBufferOf<U>(_: U.Type) -> NativeArrayBuffer<U>? {
    if !(dynamicElementType is U.Type) {
     return nil
    }
    return NativeArrayBuffer<U>(
      reinterpretCast(storage) as NativeArrayStorage<U>)
  }
   
  var dynamicElementType: Any.Type {
    return storage ? storage!.dynamicElementType : T.self
  }
  
  //===--- private --------------------------------------------------------===//
  var storage: NativeArrayStorage<T>? {
    return reinterpretCast(base.storage)
  }
  
  typealias Base = HeapBuffer<_ArrayBody, T>
  var base: Base
}

/// Append the elements of rhs to lhs
func += <
  T, C: Collection where C._Element == T
> (
  inout lhs: NativeArrayBuffer<T>, rhs: C
) {
  let oldCount = lhs.count
  let newCount = oldCount + numericCast(countElements(rhs))

  if _fastPath(newCount <= lhs.capacity) {
    lhs.count = newCount
    (lhs.elementStorage + oldCount).initializeFrom(rhs)
  }
  else {
    let newLHS = NativeArrayBuffer<T>(count: newCount, 
                                      minimumCapacity: lhs.capacity * 2)
    if lhs.base {
      newLHS.elementStorage.moveInitializeFrom(lhs.elementStorage, 
                                               count: oldCount)
      lhs.base.value.count = 0
    }
    lhs.base = newLHS.base
    (lhs.base.elementStorage + oldCount).initializeFrom(rhs)
  }
}

/// Append rhs to lhs
func += <T> (inout lhs: NativeArrayBuffer<T>, rhs: T) {
  lhs += CollectionOfOne(rhs)
}

func === <T>(
  lhs: NativeArrayBuffer<T>, rhs: NativeArrayBuffer<T>
) -> Bool {
  return lhs.base == rhs.base
}

func !== <T>(
  lhs: NativeArrayBuffer<T>, rhs: NativeArrayBuffer<T>
) -> Bool {
  return lhs.base != rhs.base
}

extension NativeArrayBuffer : Collection {
  var startIndex: Int {
    return 0
  }
  var endIndex: Int {
    return count
  }
  func generate() -> IndexingGenerator<NativeArrayBuffer> {
    return IndexingGenerator(self)
  }
}

func ~> <
  S: _Sequence_
>(
  source: S, _: (_CopyToNativeArrayBuffer,())
) -> NativeArrayBuffer<S.GeneratorType.Element>
{
  var result = NativeArrayBuffer<S.GeneratorType.Element>()

  // Using GeneratorSequence here essentially promotes the sequence to
  // a Sequence from _Sequence_ so we can iterate the elements
  for x in GeneratorSequence(source.generate()) {
    result += x
  }
  return result.take()
}

func ~> <  
  C: Collection
>(
  source: C, _:(_CopyToNativeArrayBuffer, ())
) -> NativeArrayBuffer<C.GeneratorType.Element>
{
  return _copyCollectionToNativeArrayBuffer(source)
}

func _copyCollectionToNativeArrayBuffer<C: protocol<_Collection,_Sequence_>>(
  source: C
) -> NativeArrayBuffer<C.GeneratorType.Element>
{
  let count = countElements(source)
  if count == 0 {
    return NativeArrayBuffer()
  }
  
  var result = NativeArrayBuffer<C.GeneratorType.Element>(
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

  typealias Buffer : ArrayBufferType
  var buffer: Buffer {get}
}

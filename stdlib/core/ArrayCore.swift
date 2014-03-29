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

// Array<T> may be backed by one of these when T can be an @objc class
@objc @class_protocol
protocol CocoaArray : ObjCClassType {
  func objectAtIndex(index: Int) -> AnyObject
  func getObjects(UnsafePointer<AnyObject>)range(_SwiftNSRange)
  
  // Gets us fast bulk access to elements when we need it
  func countByEnumeratingWithState(
    state: UnsafePointer<_SwiftNSFastEnumerationState>)
    objects(buffer: UnsafePointer<AnyObject>)
    count(len: Int) -> Int

  func copyWithZone(zone: COpaquePointer) -> CocoaArray
  var count: Int {get}
}

// Base class of the heap buffer implementation backing the new Array
// design.  
@objc
class NSSwiftArray : HeapBufferStorageBase, CocoaArray {
  typealias Buffer = HeapBuffer<CountAndCapacity, AnyObject>
  
  func objectAtIndex(index: Int) -> AnyObject {
    return (reinterpretCast(self) as Buffer)[index]
  }
  
  func getObjects(
    objects: UnsafePointer<AnyObject>)range(range: _SwiftNSRange) {
    var src = (reinterpretCast(self) as Buffer).elementStorage + range.location
    var dst = objects
    for i in 0...range.length {
      src++.set(dst++.get())
    }
  }

  func copyWithZone(_: COpaquePointer) -> CocoaArray {
    let my = reinterpretCast(self) as Buffer
    if _fastPath(my.value.managedByCopyOnWrite) {
      return self
    }
    
    let count = my.value.count
    let result = Buffer(
      storageClass: NativeArrayStorage<AnyObject>.self, 
      initializer: CountAndCapacity(),
      capacity: count
    )
    
    // The result of this method is treated as immutable by Cocoa,
    // so it is effectively managedByCopyOnWrite, meaning that Swift
    // code won't be mutating the buffer through a SharedArray<T>
    result.value = CountAndCapacity(
      count, result._capacity(), managedByCopyOnWrite: true)

    for i in 0...count {
      (result.elementStorage + i).initialize(my[i])
    }
    return reinterpretCast(result)
  }

  func countByEnumeratingWithState(
    state: UnsafePointer<_SwiftNSFastEnumerationState>)
    objects(UnsafePointer<AnyObject>) count(Int)
  -> Int {
    var enumerationState = state.get()
    
    if enumerationState.state != 0 {
      return 0
    }

    let my = reinterpretCast(self) as Buffer
    enumerationState.mutationsPtr = reinterpretCast(self)
    enumerationState.itemsPtr = my.elementStorage
    enumerationState.state = 1
    state.set(enumerationState)
    return my.value.count
  }
  
  var count: Int {
    return (reinterpretCast(self) as Buffer).value.count
  }
}

// The empty array prototype is always considered to be "managed by
// COW," which prevents any actual copying when its copyWithZone
// method is called.  Since it contains no elements and isn't
// resizeable, that's always safe.
let emptyNSSwiftArray : NSSwiftArray
  = reinterpretCast(NativeArrayBuffer<Int>(0,0,true))

// The class that implements the storage for a NativeArray<T>
class NativeArrayStorage<T> : NSSwiftArray {
  typealias Masquerade = HeapBufferStorage<CountAndCapacity,T>
  deinit {
    let b = HeapBuffer(
      reinterpretCast(self) as Masquerade
    )
    for i in 0...b.value.count {
      (b.elementStorage + i).destroy()
    }
    b._value.destroy()
  }
}

struct NativeArrayBuffer<T> : ArrayBufferType, LogicValue {
  typealias Element = T
  typealias Base = HeapBuffer<CountAndCapacity,T>

  init() {
    base = HeapBuffer()
  }

  // For uniformity with ArrayBuffer; this smooths out some differences
  init(other: NativeArrayBuffer) {
    self = other
  }
  
  init(storage: NativeArrayStorage<T>?) {
    base = reinterpretCast(storage)
  }
  
  init(count: Int, capacity: Int, managedByCopyOnWrite: Bool)
  {
    base = HeapBuffer(
      NativeArrayStorage<T>.self, CountAndCapacity(), capacity)

    base.value = CountAndCapacity(
      count, base._capacity(), managedByCopyOnWrite)
  }

  mutating func append(x: T) {
    let count = self.count
    
    if _fastPath(count < capacity) {
      self.count = count + 1
      (self.elementStorage + count).initialize(x)
    }
    else {
      let newMe = NativeArrayBuffer(count + 1, max(capacity * 2, 1), false)
      if base {
        newMe.elementStorage.moveInitializeFrom(elementStorage, count)
        base.value.count = 0
      }
      base = newMe.base
      (self.elementStorage + count).initialize(x)
    }
  }

  @conversion func __conversion() -> Base {
    return base
  }

  func getLogicValue() -> Bool {
    return base.getLogicValue()
  }

  var elementStorage: UnsafePointer<T> {
    return base ? base.elementStorage : nil
  }

  subscript(i: Int) -> T {
    get {
      assert(i >= 0 && i < count, "Array index out of range")
      return elementStorage[i]
    }
    @!mutating
    set {
      assert(i >= 0 && i < count, "Array index out of range")
      elementStorage[i] = newValue
    }
  }
  
  var count: Int {
    get {
      return base ? base.value.count : 0
    }
    @!mutating
    set {
      assert(newValue >= 0)
      
      assert(
        newValue <= capacity,
        "Can't grow an array buffer past its capacity")

      // Allow zero here for the case where elements have been moved
      // out of the buffer during reallocation
      assert(
        newValue == 0 || newValue >= count,
        "We don't yet know how to shrink an array")
      
      if base {
        base.value.count = newValue
      }
    }
  }
  
  var capacity: Int {
    return base ? base.value.capacity : 0
  }

  func isCopyOnWriteCompatible(copyOnWrite: Bool) -> Bool {
    return !base || base.value.managedByCopyOnWrite == copyOnWrite
  }

  func withUnsafePointerToElements<R>(body: (UnsafePointer<T>)->R) -> R {
    let p = base.elementStorage
    return withExtendedLifetime(base) { body(p) }
  }

  mutating func take(managedByCopyOnWrite: Bool) -> NativeArrayBuffer {
    if !base {
      return NativeArrayBuffer()
    }
    assert(base.isUniquelyReferenced(), "Can't \"take\" a shared array buffer")
    let result = self
    result.base.value.managedByCopyOnWrite = managedByCopyOnWrite
    base = Base()
    return result
  }

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
    return dst
  }
  
  subscript(subRange: Range<Int>) -> SliceBuffer<T>
  {
    return SliceBuffer(
      base.storage,
      elementStorage + subRange.startIndex,
      subRange.endIndex - subRange.startIndex,
      true)
  }
  
  mutating func isUniquelyReferenced() -> Bool {
    return base.isUniquelyReferenced()
  }

  func isMutable() -> Bool {
    return true
  }

  var storage: NativeArrayStorage<T>? {
    return reinterpretCast(base.storage)
  }
  
  func asCocoaArray() -> CocoaArray {
    assert(
      isBridgedToObjectiveC(T.self),
      "Array element type is not bridged to ObjectiveC")
    return count > 0 ? reinterpretCast(base.storage) : emptyNSSwiftArray
  }
  
  func asNativeBuffer() -> NativeArrayBuffer<Element> {
    return self
  }
  
  var base: Base
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

/// Serves as the buffer for an ArrayType.  An ArrayBufferType does
/// not impose value semantics on its elements, and whether its
/// elements are actually being managed by copy-on-write (COW) is, in
/// principle, unknown to the buffer.
protocol ArrayBufferType {
  /// The type of elements stored in the buffer
  typealias Element

  /// create an empty buffer
  init()

  /// Adopt the storage of x
  init(x: NativeArrayBuffer<Element>)

  /// Copy the given subRange of this buffer into uninitialized memory
  /// starting at target.  Return a pointer past-the-end of the
  /// just-initialized memory.
  func _uninitializedCopy(subRange: Range<Int>, target: UnsafePointer<Element>)
    -> UnsafePointer<Element>

  /// Convert to an NSArray in O(1).  
  /// Precondition: isBridgedToObjectiveC(Element.self)
  func asCocoaArray() -> CocoaArray
  
  /// If exactly the elements in this buffer (and no more elements)
  /// are held in a NativeArrayBuffer, return it.  Otherwise,
  /// return an empty NativeArrayBuffer.
  func asNativeBuffer() -> NativeArrayBuffer<Element>

  /// Return true iff this buffer's storage is uniquely-referenced.
  /// NOTE: this does not mean the buffer is mutable.  Other factors
  /// may need to be considered, such as whether the buffer could be
  /// some immutable Cocoa container.
  mutating func isUniquelyReferenced() -> Bool

  /// Returns true iff this buffer is mutable. NOTE: a true result
  /// does not mean the buffer is uniquely-referenced.
  func isMutable() -> Bool
  
  /// Get the value of the ith element
  subscript(i: Int) -> Element {get set}

  /// Return a SliceBuffer containing the given subRange of values
  /// from this buffer.
  subscript(subRange: Range<Int>) -> SliceBuffer<Element> {get}
  
  /// How many elements the buffer stores
  var count: Int {get set}

  /// How many elements the buffer can store without reallocation
  var capacity: Int {get}

}

// Most of this struct currently appears in an extension; see NewArray.swift.gyb
struct SliceBuffer<T> {
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
  
  func _invariantCheck() {
    let isNative = _hasNativeBuffer
    assert(
      (owner as NativeStorage).getLogicValue() == isNative
    )
    if isNative {
      assert(count <= (reinterpretCast(owner) as NativeBuffer).count)
    }
  }
  
  var _hasNativeBuffer: Bool {
    assert(
      owner || (_countAndFlags & 1) == 0,
      "Something went wrong: an unowned buffer cannot have a native buffer")
    return (_countAndFlags & 1) != 0
  }

  var count: Int {
    get {
      return Int(_countAndFlags >> 1)
    }
  }
  
  var owner: AnyObject?
  var start: UnsafePointer<T>
  var _countAndFlags: UInt
}

func ~> <
  S: _Sequence_
>(
  source: S, (_:_AsNativeArrayBuffer, (managedByCopyOnWrite: Bool))
) -> NativeArrayBuffer<S.GeneratorType.Element>
{
  var result = NativeArrayBuffer<S.GeneratorType.Element>()
  
  // Using GeneratorSequence here essentially promotes the sequence to
  // a Sequence from _Sequence_ so we can iterate the elements
  for x in GeneratorSequence(source.generate()) {
    result.append(x)
  }
  return result.take(managedByCopyOnWrite)
}

func ~> <
  // REPORT BUG AGAINST COMPILER: changing this constraint to ":
  // Collection" segfaults
  C: protocol<_Collection,_Sequence_>
>(
  source: C, (_:_AsNativeArrayBuffer, (managedByCopyOnWrite: Bool))
) -> NativeArrayBuffer<C.GeneratorType.Element>
{
  return _collectionAsNativeArrayBuffer(source, managedByCopyOnWrite)
}

func _collectionAsNativeArrayBuffer<
  C: protocol<_Collection,_Sequence_>
>(
  source: C, managedByCopyOnWrite: Bool
) -> NativeArrayBuffer<C.GeneratorType.Element>
{
  let count = countElements(source)
  if count == 0 {
    return NativeArrayBuffer()
  }
  
  var result = NativeArrayBuffer<C.GeneratorType.Element>(
    count: numericCast(count),
    capacity: numericCast(count),
    managedByCopyOnWrite 
  )

  var p = result.elementStorage
  for x in GeneratorSequence(source.generate()) {
    (p++).initialize(x)
  }
  
  return result
}

protocol _ArrayType : Collection {
  var count: Int {get}

  // Return the NativeArrayBuffer storing the elements in this
  // collection, if it exists, or an empty buffer if it does not.
  func _asNativeBuffer() -> NativeArrayBuffer<Self.GeneratorType.Element>
}

func ~> <
  A: _ArrayType
>(
  source: A, (_:_AsNativeArrayBuffer, (managedByCopyOnWrite: Bool))
) -> NativeArrayBuffer<A.GeneratorType.Element>
{
  let buf = source._asNativeBuffer()
  if buf.count == source.count
     && buf.isCopyOnWriteCompatible(managedByCopyOnWrite) {
    return buf
  }

  return _collectionAsNativeArrayBuffer(source, managedByCopyOnWrite)
}

func asNativeArrayBuffer<S: Sequence>(source: S, managedByCopyOnWrite: Bool)
  -> NativeArrayBuffer<S.GeneratorType.Element>
{
  return source~>_asNativeArrayBuffer(managedByCopyOnWrite)
}

//===----------------------------------------------------------------------===//
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
typealias _HeapObject = SwiftShims.HeapObject

// Provides a common type off of which to hang swift_bufferAllocate.
@objc class HeapBufferStorageBase {}

@asmname("swift_bufferAllocate")
func swift_bufferAllocate(
  bufferType: HeapBufferStorageBase.Type, size: Int) -> AnyObject

@asmname("malloc_size")
func c_malloc_size(heapMemory: UnsafePointer<Void>) -> Int

/// A class containing an ivar "value" of type Value, and
/// containing storage for an array of Element whose size is
/// determined at create time.
///
/// The analogous C++-ish class template would be::
///
///   template <class Value, class Element>
///   struct HeapBuffer {
///     Value value;
///     Element elementStorage[];        // length determined at creation time
///
///     HeapBuffer() = delete
///     static shared_ptr<HeapBuffer> create(Value init, int capacity);
///   }
///
/// Note that the Element array is RAW MEMORY.  You are expected to
/// construct and---if necessary---destroy Elements there yourself,
/// either in a derived class, or it can be in some manager object
/// that owns the HeapBuffer.
@objc class HeapBufferStorage<Value,Element> : HeapBufferStorageBase {
  typealias Buffer = HeapBuffer<Value, Element>
  deinit {
    Buffer(self)._value.destroy()
  }
}

@asmname("_swift_isUniquelyReferenced")
func _swift_isUniquelyReferenced(_: UnsafePointer<HeapObject>) -> Bool

// Return true if x is the only (strong) reference to the given RawBuffer
// 
// This is an inout function for two reasons:
// 
// 1. You should only call it when about to mutate the object.
//    Doing so otherwise implies a race condition if the buffer is
//    shared across threads.
//
// 2. When it is not an inout function, self is passed by
//    value... thus bumping the reference count and disturbing the
//    result we are trying to observe, Dr. Heisenberg!
//
// NOTE: this is not as safe as it could be; class types that come
// from Cocoa don't have a reference count stored inline where we're
// checking for it.  However, we have no way to restrict T to being a
// native Swift class, and in fact we have no reasonable way of
// getting a class pointer out of some other types, such as an enum
// whose first case is a native Swift object and is statically known
// to be in that case, without affecting its reference count.  Instead
// we accept everything; reinterpretCast will at least catch
// inappropriately-sized things at runtime.
func isUniquelyReferenced<T>(inout x: T) -> Bool {
  return _swift_isUniquelyReferenced(reinterpretCast(x))
}

struct HeapBuffer<Value, Element> : LogicValue, Equatable {
  typealias Storage = HeapBufferStorage<Value, Element>
  let storage: Storage?
  
  static func _valueOffset() -> Int {
    return roundUpToAlignment(sizeof(_HeapObject.self), alignof(Value.self))
  }

  static func _elementOffset() -> Int {
    return roundUpToAlignment(_valueOffset() + sizeof(Value.self),
                              alignof(Element.self))
  }

  var _address: UnsafePointer<Int8> {
    return UnsafePointer(
      Builtin.bridgeToRawPointer(self as Builtin.NativeObject))
  }

  var _value: UnsafePointer<Value> {
    return UnsafePointer(
      HeapBuffer._valueOffset() + _address)
  }

  var elementStorage: UnsafePointer<Element> {
    return UnsafePointer(HeapBuffer._elementOffset() + _address)
  }

  /// Return the actual number of `Elements` we can possibly store.
  func _capacity() -> Int {
    let allocatedSize = c_malloc_size(UnsafePointer(_address))
    return (allocatedSize - HeapBuffer._elementOffset())
      / Int(Builtin.strideof(Element.self))
  }

  init() {
    self.storage = .None
  }
  
  init(_ storage: Storage) {
    self.storage = storage
  }
  
  /// Create a `HeapBuffer` with `self.value = initializer` and
  /// `self._capacity() >= capacity`.
  init(
    _ storageClass: HeapBufferStorageBase.Type,
    _ initializer: Value, _ capacity: Int
  ) {
    _sanityCheck(capacity >= 0)

    let totalSize = HeapBuffer._elementOffset() +
        capacity * Int(Builtin.strideof(Element.self))

    self.storage = reinterpretCast(
      swift_bufferAllocate(storageClass, totalSize))
    self._value.initialize(initializer)
  }

  var value : Value {
    get {
      return _value.pointee
    }
    nonmutating set(newValue) {
      _value.pointee = newValue
    }
  }

  func getLogicValue() -> Bool {
    return storage.getLogicValue()
  }

  subscript(i: Int) -> Element {
    get {
      return elementStorage[i]
    }
    nonmutating set(newValue) {
      elementStorage[i] = newValue
    }
  }

  @conversion
  func __conversion() -> Builtin.NativeObject {
    return reinterpretCast(storage)
  }

  static func fromNativeObject(x: Builtin.NativeObject) -> HeapBuffer {
    return HeapBuffer(Builtin.castFromNativeObject(x) as Storage)
  }

  mutating func isUniquelyReferenced() -> Bool {
    if !storage {
      return false
    }
    var workaroundForRadar16119895 = reinterpretCast(storage) as COpaquePointer
    return Swift.isUniquelyReferenced(&workaroundForRadar16119895)
  }
}

// HeapBuffers are equal when they reference the same buffer
func == <Value, Element> (
  lhs: HeapBuffer<Value, Element>,
  rhs: HeapBuffer<Value, Element>) -> Bool {
  return (lhs as Builtin.NativeObject) == (rhs as Builtin.NativeObject)
}

// OnHeap<T>
// 
// A way to store a value on the heap.  These values are likely to be
// implicitly shared, so it's safest if they're immutable.
//
struct OnHeap<T> {
  typealias Buffer = HeapBuffer<T, Void>
  
  init(_ value: T) {
    _storage = HeapBuffer(Buffer.Storage.self, value, 0)
  }
  
  @conversion func __conversion() -> T {
    return _storage._value.pointee
  }
  
  var _storage: Buffer
}

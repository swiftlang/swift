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

@asmname("swift_bufferAllocate")
func _swift_bufferAllocate(
  bufferType: AnyClass, size: Int, alignMask: Int) -> AnyObject

/// A class containing an ivar "value" of type Value, and
/// containing storage for an array of Element whose size is
/// determined at create time.
///
/// The analogous C++-ish class template would be::
///
///   template <class Value, class Element>
///   struct HeapBuffer {
///     Value value;
///     Element baseAddress[];        // length determined at creation time
///
///     HeapBuffer() = delete
///     static shared_ptr<HeapBuffer> create(Value init, int capacity);
///   }
///
/// Note that the Element array is RAW MEMORY.  You are expected to
/// construct and---if necessary---destroy Elements there yourself,
/// either in a derived class, or it can be in some manager object
/// that owns the HeapBuffer.
public class HeapBufferStorage<Value,Element> {
  /// The type used to actually manage instances of
  /// `HeapBufferStorage<Value,Element>`
  public typealias Buffer = HeapBuffer<Value, Element>
  deinit {
    Buffer(self)._value.destroy()
  }
  public final func __getInstanceSizeAndAlignMask() -> (Int,Int) {
    return Buffer(self)._allocatedSizeAndAlignMask()
  }
}

@asmname("_swift_isUniquelyReferenced")
func _swift_isUniquelyReferenced(_: UnsafeMutablePointer<HeapObject>) -> Bool

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
// we accept everything; unsafeBitCast will at least catch
// inappropriately-sized things at runtime.
public func _isUniquelyReferenced<T>(inout x: T) -> Bool {
  return _swift_isUniquelyReferenced(
    unsafeBitCast(x, UnsafeMutablePointer<HeapObject>.self))
}

/// Management API for `HeapBufferStorage<Value, Element>`
public struct HeapBuffer<Value, Element> : Equatable {
  public typealias Storage = HeapBufferStorage<Value, Element>
  let storage: Storage?
  
  static func _valueOffset() -> Int {
    return _roundUpToAlignment(sizeof(_HeapObject.self), alignof(Value.self))
  }

  static func _elementOffset() -> Int {
    return _roundUpToAlignment(_valueOffset() + sizeof(Value.self),
        alignof(Element.self))
  }

  static func _requiredAlignMask() -> Int {
    // We can't use max here because it can allocate an array.
    let heapAlign = alignof(_HeapObject.self) &- 1
    let valueAlign = alignof(Value.self) &- 1
    let elementAlign = alignof(Element.self) &- 1
    return (heapAlign < valueAlign
            ? (valueAlign < elementAlign ? elementAlign : valueAlign)
            : (heapAlign < elementAlign ? elementAlign : heapAlign))
  }

  var _address: UnsafeMutablePointer<Int8> {
    return UnsafeMutablePointer(
      Builtin.bridgeToRawPointer(self._nativeObject))
  }

  var _value: UnsafeMutablePointer<Value> {
    return UnsafeMutablePointer(
      HeapBuffer._valueOffset() + _address)
  }

  var baseAddress: UnsafeMutablePointer<Element> {
    return UnsafeMutablePointer(HeapBuffer._elementOffset() + _address)
  }

  func _allocatedSize() -> Int {
    return Int(bitPattern: malloc_size(UnsafeMutablePointer(_address)))
  }

  func _allocatedAlignMask() -> Int {
    return HeapBuffer._requiredAlignMask()
  }

  func _allocatedSizeAndAlignMask() -> (Int, Int) {
    return (_allocatedSize(), _allocatedAlignMask())
  }

  /// Return the actual number of `Elements` we can possibly store.
  func _capacity() -> Int {
    return (_allocatedSize() - HeapBuffer._elementOffset())
      / strideof(Element.self)
  }

  init() {
    self.storage = .None
  }
  
  init(_ storage: Storage) {
    self.storage = storage
  }
  
  /// Create a `HeapBuffer` with `self.value = initializer` and
  /// `self._capacity() >= capacity`.
  public init(
    _ storageClass: AnyClass,
    _ initializer: Value, _ capacity: Int
  ) {
    _sanityCheck(capacity >= 0, "creating a HeapBuffer with negative capacity")

    let totalSize = HeapBuffer._elementOffset() +
        capacity * strideof(Element.self)
    let alignMask = HeapBuffer._requiredAlignMask()

    self.storage = unsafeBitCast(
      _swift_bufferAllocate(storageClass, totalSize, alignMask), Storage.self)
    self._value.initialize(initializer)
  }

  var value : Value {
    get {
      return _value.memory
    }
    nonmutating set(newValue) {
      _value.memory = newValue
    }
  }

  /// True if storage is non-\ `nil`
  public var hasStorage: Bool {
    return storage != nil
  }

  subscript(i: Int) -> Element {
    get {
      return baseAddress[i]
    }
    nonmutating set(newValue) {
      baseAddress[i] = newValue
    }
  }

  var _nativeObject: Builtin.NativeObject {
    return unsafeBitCast(storage, Builtin.NativeObject.self)
  }

  static func fromNativeObject(x: Builtin.NativeObject) -> HeapBuffer {
    return HeapBuffer(Builtin.castFromNativeObject(x) as Storage)
  }

  public mutating func isUniquelyReferenced() -> Bool {
    if storage == nil {
      return false
    }
    var workaroundForRadar16119895 = unsafeBitCast(storage, COpaquePointer.self)
    return Swift._isUniquelyReferenced(&workaroundForRadar16119895)
  }
}

// HeapBuffers are equal when they reference the same buffer
public func == <Value, Element> (
  lhs: HeapBuffer<Value, Element>,
  rhs: HeapBuffer<Value, Element>) -> Bool {
  return lhs._nativeObject == rhs._nativeObject
}

// OnHeap<T>
// 
// A way to store a value on the heap.  These values are likely to be
// implicitly shared, so it's safest if they're immutable.
//
public struct OnHeap<T> {
  typealias Buffer = HeapBuffer<T, Void>
  
  init(_ value: T) {
    _storage = HeapBuffer(Buffer.Storage.self, value, 0)
  }
  
  var _value: T { return _storage._value.memory }

  var _storage: Buffer
}

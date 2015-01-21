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

@availability(*, unavailable, message="Please use ManagedBuffer<Value,Element> instead.")
public class HeapBufferStorage<Value,Element> {}

@availability(*, unavailable, message="Please use class ManagedBuffer<Value,Element> instead.")
public struct HeapBuffer<Value,Element> {}

/// A class containing an ivar "value" of type Value, and
/// containing storage for an array of Element whose size is
/// determined at create time.
///
/// The analogous C++-ish class template would be::
///
///   template <class Value, class Element>
///   struct _HeapBuffer {
///     Value value;
///     Element baseAddress[];        // length determined at creation time
///
///     _HeapBuffer() = delete
///     static shared_ptr<_HeapBuffer> create(Value init, int capacity);
///   }
///
/// Note that the Element array is RAW MEMORY.  You are expected to
/// construct and---if necessary---destroy Elements there yourself,
/// either in a derived class, or it can be in some manager object
/// that owns the _HeapBuffer.
public // @testable (test/Prototypes/MutableIndexableDict.swift)
class _HeapBufferStorage<Value,Element> : NonObjectiveCBase {
  /// The type used to actually manage instances of
  /// `_HeapBufferStorage<Value,Element>`
  typealias Buffer = _HeapBuffer<Value, Element>
  deinit {
    Buffer(self)._value.destroy()
  }
  final func __getInstanceSizeAndAlignMask() -> (Int,Int) {
    return Buffer(self)._allocatedSizeAndAlignMask()
  }
}

// Return true if x is non-nil, and the only (strong) reference to the
// given object.
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
internal func _isUniquelyReferenced_native(
  inout x: Builtin.NativeObject?
) -> Bool {
  let p: UnsafePointer<_HeapObject> = Builtin.reinterpretCast(x)
  let result = _swift_isUniquelyReferenced_native(p)
  Builtin.fixLifetime(x)
  return result
}

/// Management API for `_HeapBufferStorage<Value, Element>`
internal struct _HeapBuffer<Value, Element> : Equatable {
  /// A default type to use as a backing store
  typealias Storage = _HeapBufferStorage<Value, Element>
  
  let _storage: Builtin.NativeObject?
  var storage: AnyObject? {
    return _storage.map { Builtin.castFromNativeObject($0) }
  }
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
      _HeapBuffer._valueOffset() + _address)
  }

  var baseAddress: UnsafeMutablePointer<Element> {
    return UnsafeMutablePointer(_HeapBuffer._elementOffset() + _address)
  }

  func _allocatedSize() -> Int {
    return Int(bitPattern: swift_malloc_size(UnsafeMutablePointer(_address)))
  }

  func _allocatedAlignMask() -> Int {
    return _HeapBuffer._requiredAlignMask()
  }

  func _allocatedSizeAndAlignMask() -> (Int, Int) {
    return (_allocatedSize(), _allocatedAlignMask())
  }

  /// Return the actual number of `Elements` we can possibly store.
  func _capacity() -> Int {
    return (_allocatedSize() - _HeapBuffer._elementOffset())
      / strideof(Element.self)
  }

  init() {
    self._storage = .None
  }

  init(_ storage: _HeapBufferStorage<Value,Element>) {
    self._storage = Builtin.castToNativeObject(storage)
  }
  
  init(_ storage: AnyObject) {
    _sanityCheck(
      _usesNativeSwiftReferenceCounting(storage.dynamicType),
      "HeapBuffer manages only native objects"
    )
    self._storage = Builtin.castToNativeObject(storage)
  }
  
  init<T: AnyObject>(_ storage: T?) {
    self = storage.map { _HeapBuffer($0) } ?? _HeapBuffer()
  }
  
  init(nativeStorage: Builtin.NativeObject?) {
    self._storage = nativeStorage
  }
  
  /// Create a `_HeapBuffer` with `self.value = initializer` and
  /// `self._capacity() >= capacity`.
  init(
    _ storageClass: AnyClass,
    _ initializer: Value, _ capacity: Int
  ) {
    _sanityCheck(capacity >= 0, "creating a _HeapBuffer with negative capacity")
    _sanityCheck(
      _usesNativeSwiftReferenceCounting(storageClass),
      "HeapBuffer can only create native objects"
    )

    let totalSize = _HeapBuffer._elementOffset() +
        capacity * strideof(Element.self)
    let alignMask = _HeapBuffer._requiredAlignMask()

    let object: AnyObject = _swift_bufferAllocate(
      storageClass, totalSize, alignMask)
    self._storage = Builtin.castToNativeObject(object)
    self._value.initialize(initializer)
  }

  var value : Value {
    unsafeAddress {
      return UnsafePointer(_value)
    }
    nonmutating unsafeMutableAddress {
      return _value
    }
  }

  /// True if storage is non-\ `nil`
  var hasStorage: Bool {
    return _storage != nil
  }

  subscript(i: Int) -> Element {
    unsafeAddress {
      return UnsafePointer(baseAddress + i)
    }
    nonmutating unsafeMutableAddress {
      return baseAddress + i
    }
  }

  var _nativeObject: Builtin.NativeObject {
    return _storage!
  }

  static func fromNativeObject(x: Builtin.NativeObject) -> _HeapBuffer {
    return _HeapBuffer(nativeStorage: x)
  }

  mutating func isUniquelyReferenced() -> Bool {
    let o: UnsafePointer<HeapObject> = Builtin.reinterpretCast(_storage)
    let result = _swift_isUniquelyReferenced_native(o)
    Builtin.fixLifetime(_storage)
    return result
  }

  mutating func isUniquelyReferencedOrPinned() -> Bool {
    let o: UnsafePointer<HeapObject> = Builtin.reinterpretCast(_storage)
    let result = _swift_isUniquelyReferencedOrPinned_native(o)
    Builtin.fixLifetime(_storage)
    return result
  }
}

// HeapBuffers are equal when they reference the same buffer
func == <Value, Element> (
  lhs: _HeapBuffer<Value, Element>,
  rhs: _HeapBuffer<Value, Element>) -> Bool {
  return lhs._nativeObject == rhs._nativeObject
}


@availability(*, unavailable, message="Please use ManagedBuffer<T,Void> instead")
public struct OnHeap<T> {}

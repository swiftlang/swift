//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims
typealias _HeapObject = SwiftShims.HeapObject

@_silgen_name("swift_bufferAllocate")
internal func _swift_bufferAllocate(
  bufferType type: AnyClass,
  size: Int,
  alignmentMask: Int
) -> AnyObject

/// A class containing an ivar "value" of type Value, and
/// containing storage for an array of Element whose size is
/// determined at create time.
///
/// The analogous C++-ish class template would be:
///
///     template <class Value, class Element>
///     struct _HeapBuffer {
///       Value value;
///       Element baseAddress[];        // length determined at creation time
///
///       _HeapBuffer() = delete
///       static shared_ptr<_HeapBuffer> create(Value init, int capacity);
///     }
///
/// Note that the Element array is RAW MEMORY.  You are expected to
/// construct and---if necessary---destroy Elements there yourself,
/// either in a derived class, or it can be in some manager object
/// that owns the _HeapBuffer.
public // @testable (test/Prototypes/MutableIndexableDict.swift)
class _HeapBufferStorage<Value, Element> {
  public init() {}

  /// The type used to actually manage instances of
  /// `_HeapBufferStorage<Value, Element>`.
  typealias Buffer = _HeapBuffer<Value, Element>
  deinit {
    Buffer(self)._value.deinitialize()
  }

  final func __getInstanceSizeAndAlignMask() -> (Int, Int) {
    return Buffer(self)._allocatedSizeAndAlignMask()
  }
}

/// Management API for `_HeapBufferStorage<Value, Element>`
public // @testable
struct _HeapBuffer<Value, Element> : Equatable {
  /// A default type to use as a backing store.
  typealias Storage = _HeapBufferStorage<Value, Element>

  // _storage is passed inout to _isUnique.  Although its value
  // is unchanged, it must appear mutable to the optimizer.
  internal var _storage: Builtin.NativeObject?

  public // @testable
  var storage: AnyObject? {
    return _storage.map { Builtin.castFromNativeObject($0) }
  }

  internal static func _valueOffset() -> Int {
    return _roundUp(
      MemoryLayout<_HeapObject>.size,
      toAlignment: MemoryLayout<Value>.alignment)
  }

  internal static func _elementOffset() -> Int {
    return _roundUp(
      _valueOffset() + MemoryLayout<Value>.size,
      toAlignment: MemoryLayout<Element>.alignment)
  }

  internal static func _requiredAlignMask() -> Int {
    // We can't use max here because it can allocate an array.
    let heapAlign = MemoryLayout<_HeapObject>.alignment &- 1
    let valueAlign = MemoryLayout<Value>.alignment &- 1
    let elementAlign = MemoryLayout<Element>.alignment &- 1
    return (heapAlign < valueAlign
            ? (valueAlign < elementAlign ? elementAlign : valueAlign)
            : (heapAlign < elementAlign ? elementAlign : heapAlign))
  }

  internal var _address: UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(
      Builtin.bridgeToRawPointer(self._nativeObject))
  }

  internal var _value: UnsafeMutablePointer<Value> {
    return (_HeapBuffer._valueOffset() + _address).assumingMemoryBound(
      to: Value.self)
  }

  public // @testable
  var baseAddress: UnsafeMutablePointer<Element> {
    return (_HeapBuffer._elementOffset() + _address).assumingMemoryBound(
      to: Element.self)
  }

  internal func _allocatedSize() -> Int {
    return _swift_stdlib_malloc_size(_address)
  }

  internal func _allocatedAlignMask() -> Int {
    return _HeapBuffer._requiredAlignMask()
  }

  internal func _allocatedSizeAndAlignMask() -> (Int, Int) {
    return (_allocatedSize(), _allocatedAlignMask())
  }

  /// Returns the actual number of `Elements` we can possibly store.
  internal func _capacity() -> Int {
    return (_allocatedSize() - _HeapBuffer._elementOffset())
      / MemoryLayout<Element>.stride
  }

  internal init() {
    self._storage = nil
  }

  public // @testable
  init(_ storage: _HeapBufferStorage<Value, Element>) {
    self._storage = Builtin.castToNativeObject(storage)
  }

  internal init(_ storage: AnyObject) {
    _sanityCheck(
      _usesNativeSwiftReferenceCounting(type(of: storage)),
      "HeapBuffer manages only native objects"
    )
    self._storage = Builtin.castToNativeObject(storage)
  }

  internal init<T : AnyObject>(_ storage: T?) {
    self = storage.map { _HeapBuffer($0) } ?? _HeapBuffer()
  }

  internal init(nativeStorage: Builtin.NativeObject?) {
    self._storage = nativeStorage
  }

  /// Create a `_HeapBuffer` with `self.value = initializer` and
  /// `self._capacity() >= capacity`.
  public // @testable
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
        capacity * MemoryLayout<Element>.stride
    let alignMask = _HeapBuffer._requiredAlignMask()

    let object: AnyObject = _swift_bufferAllocate(
      bufferType: storageClass,
      size: totalSize,
      alignmentMask: alignMask)
    self._storage = Builtin.castToNativeObject(object)
    self._value.initialize(to: initializer)
  }

  public // @testable
  var value: Value {
    unsafeAddress {
      return UnsafePointer(_value)
    }
    nonmutating unsafeMutableAddress {
      return _value
    }
  }

  /// `true` if storage is non-`nil`.
  internal var hasStorage: Bool {
    return _storage != nil
  }

  internal subscript(i: Int) -> Element {
    unsafeAddress {
      return UnsafePointer(baseAddress + i)
    }
    nonmutating unsafeMutableAddress {
      return baseAddress + i
    }
  }

  internal var _nativeObject: Builtin.NativeObject {
    return _storage!
  }

  internal static func fromNativeObject(_ x: Builtin.NativeObject) -> _HeapBuffer {
    return _HeapBuffer(nativeStorage: x)
  }

  public // @testable
  mutating func isUniquelyReferenced() -> Bool {
    return _isUnique(&_storage)
  }

  public // @testable
  mutating func isUniquelyReferencedOrPinned() -> Bool {
    return _isUniqueOrPinned(&_storage)
  }
}

// HeapBuffers are equal when they reference the same buffer
public // @testable
func == <Value, Element>(
  lhs: _HeapBuffer<Value, Element>,
  rhs: _HeapBuffer<Value, Element>) -> Bool {
  return lhs._nativeObject == rhs._nativeObject
}

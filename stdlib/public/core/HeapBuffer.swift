//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims
typealias _HeapObject = SwiftShims.HeapObject

@warn_unused_result
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
class _HeapBufferStorage<Value, Element> : NonObjectiveCBase {
  public override init() {}

  /// The type used to actually manage instances of
  /// `_HeapBufferStorage<Value, Element>`.
  typealias Buffer = _HeapBuffer<Value, Element>
  deinit {
    Buffer(self)._value.deinitialize()
  }

  @warn_unused_result
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

  @warn_unused_result
  internal static func _valueOffset() -> Int {
    return _roundUp(
      sizeof(_HeapObject.self),
      toAlignment: alignof(Value.self))
  }

  @warn_unused_result
  internal static func _elementOffset() -> Int {
    return _roundUp(
      _valueOffset() + sizeof(Value.self),
      toAlignment: alignof(Element.self))
  }

  @warn_unused_result
  internal static func _requiredAlignMask() -> Int {
    // We can't use max here because it can allocate an array.
    let heapAlign = alignof(_HeapObject.self) &- 1
    let valueAlign = alignof(Value.self) &- 1
    let elementAlign = alignof(Element.self) &- 1
    return (heapAlign < valueAlign
            ? (valueAlign < elementAlign ? elementAlign : valueAlign)
            : (heapAlign < elementAlign ? elementAlign : heapAlign))
  }

  internal var _address: UnsafeMutablePointer<Int8> {
    return UnsafeMutablePointer(
      Builtin.bridgeToRawPointer(self._nativeObject))
  }

  internal var _value: UnsafeMutablePointer<Value> {
    return UnsafeMutablePointer(
      _HeapBuffer._valueOffset() + _address)
  }

  public // @testable
  var baseAddress: UnsafeMutablePointer<Element> {
    return UnsafeMutablePointer(_HeapBuffer._elementOffset() + _address)
  }

  @warn_unused_result
  internal func _allocatedSize() -> Int {
    return _swift_stdlib_malloc_size(_address)
  }

  @warn_unused_result
  internal func _allocatedAlignMask() -> Int {
    return _HeapBuffer._requiredAlignMask()
  }

  @warn_unused_result
  internal func _allocatedSizeAndAlignMask() -> (Int, Int) {
    return (_allocatedSize(), _allocatedAlignMask())
  }

  /// Returns the actual number of `Elements` we can possibly store.
  @warn_unused_result
  internal func _capacity() -> Int {
    return (_allocatedSize() - _HeapBuffer._elementOffset())
      / strideof(Element.self)
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
      _usesNativeSwiftReferenceCounting(storage.dynamicType),
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
        capacity * strideof(Element.self)
    let alignMask = _HeapBuffer._requiredAlignMask()

    let object: AnyObject = _swift_bufferAllocate(
      bufferType: storageClass,
      size: totalSize,
      alignmentMask: alignMask)
    self._storage = Builtin.castToNativeObject(object)
    self._value.initialize(with: initializer)
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

  @warn_unused_result
  internal static func fromNativeObject(_ x: Builtin.NativeObject) -> _HeapBuffer {
    return _HeapBuffer(nativeStorage: x)
  }

  @warn_unused_result
  public // @testable
  mutating func isUniquelyReferenced() -> Bool {
    return _isUnique(&_storage)
  }

  @warn_unused_result
  public // @testable
  mutating func isUniquelyReferencedOrPinned() -> Bool {
    return _isUniqueOrPinned(&_storage)
  }
}

// HeapBuffers are equal when they reference the same buffer
@warn_unused_result
public // @testable
func == <Value, Element> (
  lhs: _HeapBuffer<Value, Element>,
  rhs: _HeapBuffer<Value, Element>) -> Bool {
  return lhs._nativeObject == rhs._nativeObject
}

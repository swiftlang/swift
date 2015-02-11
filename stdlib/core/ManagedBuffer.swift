//===--- ManagedBuffer.swift - variable-sized buffer of aligned memory ---===//
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

/// A common base class for classes that need to be non-\ `@objc`, 
/// recognizably in the type system.
///
/// See `isUniquelyReferenced`
public class NonObjectiveCBase {}

/// A base class of `ManagedBuffer<Value,Element>`, used during
/// instance creation.
///
/// During instance creation, in particular during
/// `ManagedBuffer.create`\ 's call to initialize, `ManagedBuffer`\ 's
/// `value` property is as-yet uninitialized, and therefore
/// `ManagedProtoBuffer` does not offer access to the as-yet
/// uninitialized `value` property of `ManagedBuffer`.
public class ManagedProtoBuffer<Value, Element> : NonObjectiveCBase {
  /// The actual number of elements that can be stored in this object.
  ///
  /// This value may be nontrivial to compute; it is usually a good
  /// idea to store this information in the "value" area when
  /// an instance is created.
  public final var allocatedElementCount: Int {
    let p = ManagedBufferPointer<Value,Element>(self)
    return p.allocatedElementCount
  }

  /// Call `body` with an `UnsafeMutablePointer` to the stored
  /// `Value`.  **Note**: this pointer is only valid
  /// for the duration of the call to `body`.
  public final func withUnsafeMutablePointerToValue<R>(
    body: (UnsafeMutablePointer<Value>)->R
  ) -> R {
    return withUnsafeMutablePointers { (v, e) in return body(v) }
  }
  
  /// Call `body` with an `UnsafeMutablePointer` to the `Element`
  /// storage.  **Note**: this pointer is only valid
  /// for the duration of the call to `body`.
  public final func withUnsafeMutablePointerToElements<R>(
    body: (UnsafeMutablePointer<Element>)->R
  ) -> R {
    return withUnsafeMutablePointers { return body($0.1) }
  }

  /// Call `body` with `UnsafeMutablePointer`\ s to the stored `Value`
  /// and raw `Element` storage.  **Note**: these pointers are only valid
  /// for the duration of the call to `body`.
  public final func withUnsafeMutablePointers<R>(
    body: (_: UnsafeMutablePointer<Value>, _: UnsafeMutablePointer<Element>)->R
  ) -> R {
    return ManagedBufferPointer(self).withUnsafeMutablePointers(body)
  }
  
  //===--- internal/private API -------------------------------------------===//
  
  /// Make ordinary initialization unavailable
  internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only initialize these by calling create")
  }
}

/// A class whose instances contain a property of type `Value` and raw
/// storage for an array of `Element`, whose size is determined at
/// instance creation.
///
/// Note that the `Element` array is suitably-aligned **raw memory**.
/// You are expected to construct and---if necessary---destroy objects
/// there yourself, using the APIs on `UnsafeMutablePointer<Element>`.
/// Typical usage stores a count and capacity in `Value` and destroys
/// any live elements in the `deinit` of a subclass.  Note: subclasses
/// must not have any stored properties; any storage needed should be
/// included in `Value`.
public class ManagedBuffer<Value, Element>
  : ManagedProtoBuffer<Value, Element> {
  
  /// Create a new instance of the most-derived class, calling
  /// `initializeValue` on the partially-constructed object to
  /// generate an initial `Value`.
  public final class func create(
    minimumCapacity: Int,
    initialValue: (ManagedProtoBuffer<Value,Element>)->Value
  ) -> ManagedBuffer<Value,Element> {
    
    let p = ManagedBufferPointer<Value,Element>(
      bufferClass: self,
      minimumCapacity: minimumCapacity,
      initialValue: { buffer, _ in initialValue(unsafeDowncast(buffer)) })

    return unsafeDowncast(p.buffer)
  }

  /// Destroy the stored Value
  deinit {
    ManagedBufferPointer(self).withUnsafeMutablePointerToValue { $0.destroy() }
  }

  /// The stored `Value` instance.
  public final var value: Value {
    unsafeAddress {
      return ManagedBufferPointer(self).withUnsafeMutablePointerToValue { UnsafePointer($0) }
    }
    unsafeMutableAddress {
      return ManagedBufferPointer(self).withUnsafeMutablePointerToValue { $0 }
    }
  }
}

/// Contains a buffer object, and provides access to an instance of
/// `Value` and contiguous storage for an arbitrary number of
/// `Element` instances stored in that buffer.
///
/// For most purposes, the `ManagedBuffer` class works fine for this
/// purpose, and can simply be used on its own.  However, in cases
/// where objects of various different classes must serve as storage,
/// `ManagedBufferPointer` is needed.
///
/// A valid buffer class is non-`@objc`, with no declared stored
///   properties.  Its `deinit` must destroy its
///   stored `Value` and any constructed `Element`\ s.
///
/// Example Buffer Class
/// --------------------
///
/// ::
///
///    class MyBuffer<Element> { // non-@objc
///      typealias Manager = ManagedBufferPointer<(Int,String), Element>
///      deinit {
///        Manager(unsafeBufferObject: self).withUnsafeMutablePointers {
///          (pointerToValue, pointerToElements)->Void in
///          pointerToElements.destroy(self.count)
///          pointerToValue.destroy()
///        }
///      }
///
///      // All properties are *computed* based on members of the Value
///      var count: Int {
///        return Manager(unsafeBufferObject: self).value.0
///      }
///      var name: String {
///        return Manager(unsafeBufferObject: self).value.1
///      }
///    }
///
public struct ManagedBufferPointer<Value, Element> : Equatable {

  /// Create with new storage containing an initial `Value` and space
  /// for at least `minimumCapacity` `element`\ s.
  ///
  /// :param: `bufferClass` the class of the object used for storage.
  /// :param: `minimumCapacity` the minimum number of `Element`\ s that
  ///   must be able to be stored in the new buffer.
  /// :param: `initialValue` a function that produces the initial
  ///   `Value` instance stored in the buffer, given the `buffer`
  ///   object and a function that can be called on it to get the actual
  ///   number of allocated elements.
  ///
  /// Requires: minimumCapacity >= 0, and the type indicated by
  /// `bufferClass` is a non-`@objc` class with no declared stored
  /// properties.  The `deinit` of `bufferClass` must destroy its
  /// stored `Value` and any constructed `Element`\ s.
  public init(
    bufferClass: AnyClass,
    minimumCapacity: Int,
    initialValue: (buffer: AnyObject, allocatedCount: (AnyObject)->Int)->Value
  ) {
    self = ManagedBufferPointer(bufferClass: bufferClass, minimumCapacity: minimumCapacity)

    // initialize the value field
    withUnsafeMutablePointerToValue {
      $0.initialize(
        initialValue(
          buffer: self.buffer,
          allocatedCount: {
            ManagedBufferPointer(unsafeBufferObject: $0).allocatedElementCount
          }))
    }
    // FIXME: workaround for <rdar://problem/18619176>.  If we don't
    // access value somewhere, its addressor gets linked away
    let x = value 
  }

  /// Manage the given `buffer`.
  ///
  /// Requires: `buffer` is an instance of a non-`@objc` class whose
  /// `deinit` destroys its stored `Value` and any constructed
  /// `Element`\ s.
  public init(unsafeBufferObject buffer: AnyObject) {
    ManagedBufferPointer._checkValidBufferClass(buffer.dynamicType)

    self._nativeBuffer = Builtin.castToNativeObject(buffer)
  }

  /// Internal version for use by _ContiguousArrayBuffer where we know that we
  /// have a valid buffer class.
  /// This version of the init function gets called from
  ///  _ContiguousArrayBuffer's deinit function. Since 'deinit' does not get
  /// specialized with current versions of the compiler, we can't get rid of the
  /// _debugPreconditions in _checkValidBufferClass for any array. Since we know
  /// for the _ContiguousArrayBuffer that this check must always succeed we omit
  /// it in this specialized constructor.
  internal init(_uncheckedUnsafeBufferObject buffer: AnyObject) {
    ManagedBufferPointer._sanityCheckValidBufferClass(buffer.dynamicType)
    self._nativeBuffer = Builtin.castToNativeObject(buffer)
  }

  /// The stored `Value` instance.
  public var value: Value {    
    unsafeAddress {
      return UnsafePointer(_valuePointer)
    }
    unsafeMutableAddress {
      return _valuePointer
    }
  }

  /// Return the object instance being used for storage.
  public var buffer: AnyObject {
    return Builtin.castFromNativeObject(_nativeBuffer)
  }

  /// The actual number of elements that can be stored in this object.
  ///
  /// This value may be nontrivial to compute; it is usually a good
  /// idea to store this information in the "value" area when
  /// an instance is created.
  public var allocatedElementCount: Int {
    return (_allocatedByteCount &- _My._elementOffset) / strideof(Element)
  }

  /// Call `body` with an `UnsafeMutablePointer` to the stored
  /// `Value`.  **Note**: this pointer is only valid
  /// for the duration of the call to `body`
  public func withUnsafeMutablePointerToValue<R>(
    body: (UnsafeMutablePointer<Value>)->R
  ) -> R {
    return withUnsafeMutablePointers { (v, e) in return body(v) }
  }
  
  /// Call `body` with an `UnsafeMutablePointer` to the `Element`
  /// storage.  **Note**: this pointer is only valid
  /// for the duration of the call to `body`.
  public func withUnsafeMutablePointerToElements<R>(
    body: (UnsafeMutablePointer<Element>)->R
  ) -> R {
    return withUnsafeMutablePointers { return body($0.1) }
  }

  /// Call `body` with `UnsafeMutablePointer`\ s to the stored `Value`
  /// and raw `Element` storage.  **Note**: these pointers are only valid
  /// for the duration of the call to `body`.
  public func withUnsafeMutablePointers<R>(
    body: (_: UnsafeMutablePointer<Value>, _: UnsafeMutablePointer<Element>)->R
  ) -> R {
    let result = body(_valuePointer, _elementPointer)
    _fixLifetime(_nativeBuffer)
    return result
  }

  /// Returns true iff `self` holds the only strong reference to its buffer.
  ///
  /// See `isUniquelyReferenced` for details.
  public mutating func holdsUniqueReference() -> Bool {
    let o = UnsafePointer<HeapObject>(Builtin.bridgeToRawPointer(_nativeBuffer))
    let result = _swift_isUniquelyReferenced_nonNull_native(o)
    _fixLifetime(_nativeBuffer)
    return result
  }

  /// Returns true iff either `self` holds the only strong reference
  /// to its buffer or the pinned has been 'pinned'.
  ///
  /// See `isUniquelyReferenced` for details.
  public mutating func holdsUniqueOrPinnedReference() -> Bool {
    let o = UnsafePointer<HeapObject>(Builtin.bridgeToRawPointer(_nativeBuffer))
    let result = _swift_isUniquelyReferencedOrPinned_nonNull_native(o)
    _fixLifetime(_nativeBuffer)
    return result
  }
  
  //===--- internal/private API -------------------------------------------===//
  
  /// Create with new storage containing space for an initial `Value`
  /// and at least `minimumCapacity` `element`\ s.
  ///
  /// :param: `bufferClass` the class of the object used for storage.
  /// :param: `minimumCapacity` the minimum number of `Element`\ s that
  ///   must be able to be stored in the new buffer.
  ///
  /// Requires: minimumCapacity >= 0, and the type indicated by
  /// `bufferClass` is a non-`@objc` class with no declared stored
  /// properties.  The `deinit` of `bufferClass` must destroy its
  /// stored `Value` and any constructed `Element`\ s.
  internal init(
    bufferClass: AnyClass,
    minimumCapacity: Int
  ) {
    ManagedBufferPointer._checkValidBufferClass(bufferClass, creating: true)
    _precondition(
      minimumCapacity >= 0,
      "ManagedBufferPointer must have non-negative capacity")

    let totalSize = _My._elementOffset
      +  minimumCapacity * strideof(Element.self)

    let newBuffer: AnyObject = _swift_bufferAllocate(
      bufferClass, totalSize, _My._alignmentMask)

    self._nativeBuffer = Builtin.castToNativeObject(newBuffer)
  }

  /// Internal version for use by _ContiguousArrayBuffer.init where we know that
  /// we have a valid buffer class and that the capacity is >= 0.
  internal init(
    _uncheckedBufferClass: AnyClass,
    minimumCapacity: Int
  ) {
    ManagedBufferPointer._sanityCheckValidBufferClass(_uncheckedBufferClass, creating: true)
    _sanityCheck(
      minimumCapacity >= 0,
      "ManagedBufferPointer must have non-negative capacity")

    let totalSize = _My._elementOffset
      +  minimumCapacity * strideof(Element.self)

    let newBuffer: AnyObject = _swift_bufferAllocate(
      _uncheckedBufferClass, totalSize, _My._alignmentMask)

    self._nativeBuffer = Builtin.castToNativeObject(newBuffer)
  }

  /// Manage the given `buffer`.
  ///
  /// **Note:** it is an error to use the `value` property of the resulting
  /// instance unless it has been initialized.
  internal init(_ buffer: ManagedProtoBuffer<Value, Element>) {
    _nativeBuffer = Builtin.castToNativeObject(buffer)
  }
  
  internal typealias _My = ManagedBufferPointer

  internal static func _checkValidBufferClass(
    bufferClass: AnyClass, creating: Bool = false
  ) {
#if !arch(arm) && !arch(i386)
    // FIXME: test disabled until we figure out what's wrong on this
    // platform: <rdar://problem/18682097> Generic and non-generic
    // class instances have different sizes on armv7
    _debugPrecondition(
      _class_getInstancePositiveExtentSize(bufferClass) == sizeof(_HeapObject.self)
      || (
        !creating
        && _class_getInstancePositiveExtentSize(bufferClass)
          == _valueOffset + sizeof(Value.self)),
      "ManagedBufferPointer buffer class has illegal stored properties"
    )
#endif
    _debugPrecondition(
      _usesNativeSwiftReferenceCounting(bufferClass),
      "ManagedBufferPointer buffer class must be non-@objc"
    )
  }

  internal static func _sanityCheckValidBufferClass(
    bufferClass: AnyClass, creating: Bool = false
  ) {
#if !arch(arm) && !arch(i386)
    // FIXME: test disabled until we figure out what's wrong on this
    // platform: <rdar://problem/18682097> Generic and non-generic
    // class instances have different sizes on armv7
    _sanityCheck(
      _class_getInstancePositiveExtentSize(bufferClass) == sizeof(_HeapObject.self)
      || (
        !creating
        && _class_getInstancePositiveExtentSize(bufferClass)
          == _valueOffset + sizeof(Value.self)),
      "ManagedBufferPointer buffer class has illegal stored properties"
    )
#endif
    _sanityCheck(
      _usesNativeSwiftReferenceCounting(bufferClass),
      "ManagedBufferPointer buffer class must be non-@objc"
    )
  }
  
  /// The required alignment for allocations of this type, minus 1
  internal static var _alignmentMask: Int {
    return max(
      alignof(_HeapObject.self),
      max(alignof(Value.self), alignof(Element.self))) &- 1
  }

  /// The actual number of bytes allocated for this object.
  internal var _allocatedByteCount: Int {
    return swift_malloc_size(_address)
  }
  
  /// The address of this instance in a convenient pointer-to-bytes form
  internal var _address: UnsafePointer<UInt8> {
    return UnsafePointer(Builtin.bridgeToRawPointer(_nativeBuffer))
  }

  /// Offset from the allocated storage for `self` to the stored `Value`
  internal static var _valueOffset: Int {
    return _roundUpToAlignment(sizeof(_HeapObject.self), alignof(Value.self))
  }

  /// An **unmanaged** pointer to the storage for the `Value`
  /// instance.  Not safe to use without _fixLifetime calls to
  /// guarantee it doesn't dangle
  internal var _valuePointer: UnsafeMutablePointer<Value> {
    return UnsafeMutablePointer(_address + _My._valueOffset)
  }

  /// An **unmanaged** pointer to the storage for `Element`\ s.  Not
  /// safe to use without _fixLifetime calls to guarantee it doesn't
  /// dangle.
  internal var _elementPointer: UnsafeMutablePointer<Element> {
    return UnsafeMutablePointer(_address + _My._elementOffset)
  }

  /// Offset from the allocated storage for `self` to the `Element` storage
  internal static var _elementOffset: Int {
    return _roundUpToAlignment(
      _valueOffset + sizeof(Value.self), alignof(Element.self))
  }
  
  internal var _nativeBuffer: Builtin.NativeObject
}

public func == <Value, Element>(
  lhs: ManagedBufferPointer<Value, Element>,
  rhs: ManagedBufferPointer<Value, Element>
) -> Bool {
  return lhs._address == rhs._address
}

// FIXME: when our calling convention changes to pass self at +0,
// inout should be dropped from the arguments to these functions.

/// Returns `true` iff `object` is a non-\ `@objc` class instance with
/// a single strong reference.
///
/// * Does *not* modify `object`; the use of `inout` is an
///   implementation artifact.
/// * If `object` is an Objective-C class instance, returns `false`.
/// * Weak references do not affect the result of this function.
///  
/// Useful for implementing the copy-on-write optimization for the
/// deep storage of value types::
///
///   mutating func modifyMe(arg: X) {
///     if isUniquelyReferencedNonObjC(&myStorage) {
///       myStorage.modifyInPlace(arg)
///     }
///     else {
///       myStorage = self.createModified(myStorage, arg)
///     }
///   }
///
/// This function is safe to use for `mutating` functions in
/// multithreaded code because a false positive would imply that there
/// is already a user-level data race on the value being mutated.
public func isUniquelyReferencedNonObjC<T: AnyObject>(inout object: T) -> Bool {
  
  // Note: the pointer must be extracted in a separate step or an
  // extra reference will be held during the check below
  let o = UnsafePointer<Void>(Builtin.bridgeToRawPointer(object))
  let result = _swift_isUniquelyReferencedNonObjC_nonNull(o)
  Builtin.fixLifetime(object)
  return result
}

internal func isUniquelyReferencedOrPinnedNonObjC<T: AnyObject>(inout object: T) -> Bool {
  // Note: the pointer must be extracted in a separate step or an
  // extra reference will be held during the check below
  let o = UnsafePointer<Void>(Builtin.bridgeToRawPointer(object))
  let result = _swift_isUniquelyReferencedOrPinnedNonObjC_nonNull(o)
  Builtin.fixLifetime(object)
  return result
}

/// Returns `true` iff `object` is a non-\ `@objc` class instance with a single
/// strong reference.
///
/// * Does *not* modify `object`; the use of `inout` is an
///   implementation artifact.
/// * Weak references do not affect the result of this function.
///  
/// Useful for implementing the copy-on-write optimization for the
/// deep storage of value types::
///
///   mutating func modifyMe(arg: X) {
///     if isUniquelyReferenced(&myStorage) {
///       myStorage.modifyInPlace(arg)
///     }
///     else {
///       myStorage = myStorage.createModified(arg)
///     }
///   }
///
/// This function is safe to use for `mutating` functions in
/// multithreaded code because a false positive would imply that there
/// is already a user-level data race on the value being mutated.
public func isUniquelyReferenced<T: NonObjectiveCBase>(
  inout object: T
) -> Bool {
  // Note: the pointer must be extracted in a separate step or an
  // extra reference will be held during the check below
  let o = UnsafePointer<HeapObject>(Builtin.bridgeToRawPointer(object))
  let result = _swift_isUniquelyReferenced_nonNull_native(o)
  Builtin.fixLifetime(object)
  return result
}

/// Returns `true` iff `object` is a non-\ `@objc` class instance with
/// a single strong reference.
///
/// * Does *not* modify `object`; the use of `inout` is an
///   implementation artifact.
/// * If `object` is an Objective-C class instance, returns `false`.
/// * Weak references do not affect the result of this function.
///  
/// Useful for implementing the copy-on-write optimization for the
/// deep storage of value types::
///
///   mutating func modifyMe(arg: X) {
///     if isUniquelyReferencedNonObjC(&myStorage) {
///       myStorage.modifyInPlace(arg)
///     }
///     else {
///       myStorage = self.createModified(myStorage, arg)
///     }
///   }
///
/// This function is safe to use for `mutating` functions in
/// multithreaded code because a false positive would imply that there
/// is already a user-level data race on the value being mutated.
public func isUniquelyReferencedNonObjC<T: AnyObject>(
  inout object: T?
) -> Bool {
  
  // Note: the pointer must be extracted in a separate step or an
  // extra reference will be held during the check below
  let o = Builtin.reinterpretCast(object) as UnsafePointer<Void>
  let result = _swift_isUniquelyReferencedNonObjC(o)
  Builtin.fixLifetime(object)
  return result
}


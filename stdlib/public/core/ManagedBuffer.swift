//===--- ManagedBuffer.swift - variable-sized buffer of aligned memory ----===//
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

/// A common base class for classes that need to be non-`@objc`,
/// recognizably in the type system.
///
/// - SeeAlso: `isUniquelyReferenced`
public class NonObjectiveCBase {
  public init() {}
}

/// A base class of `ManagedBuffer<Header, Element>`, used during
/// instance creation.
///
/// During instance creation, in particular during
/// `ManagedBuffer.create`'s call to initialize, `ManagedBuffer`'s
/// `header` property is as-yet uninitialized, and therefore
/// `ManagedProtoBuffer` does not offer access to the as-yet
/// uninitialized `header` property of `ManagedBuffer`.
public class ManagedProtoBuffer<Header, Element> : NonObjectiveCBase {
  /// The actual number of elements that can be stored in this object.
  ///
  /// This header may be nontrivial to compute; it is usually a good
  /// idea to store this information in the "header" area when
  /// an instance is created.
  public final var capacity: Int {
    let p = ManagedBufferPointer<Header, Element>(self)
    return p.capacity
  }

  /// Call `body` with an `UnsafeMutablePointer` to the stored
  /// `Header`.
  ///
  /// - Note: This pointer is only valid for the duration of the
  ///   call to `body`.
  public final func withUnsafeMutablePointerToHeader<R>(
    _ body: @noescape (UnsafeMutablePointer<Header>) throws -> R
  ) rethrows -> R {
    return try withUnsafeMutablePointers { (v, e) in return try body(v) }
  }

  /// Call `body` with an `UnsafeMutablePointer` to the `Element`
  /// storage.
  ///
  /// - Note: This pointer is only valid for the duration of the
  ///   call to `body`.
  public final func withUnsafeMutablePointerToElements<R>(
    _ body: @noescape (UnsafeMutablePointer<Element>) throws -> R
  ) rethrows -> R {
    return try withUnsafeMutablePointers { return try body($0.1) }
  }

  /// Call `body` with `UnsafeMutablePointer`s to the stored `Header`
  /// and raw `Element` storage.
  ///
  /// - Note: These pointers are only valid for the duration of the
  ///   call to `body`.
  public final func withUnsafeMutablePointers<R>(
    _ body: @noescape (UnsafeMutablePointer<Header>, UnsafeMutablePointer<Element>) throws -> R
  ) rethrows -> R {
    return try ManagedBufferPointer(self).withUnsafeMutablePointers(body)
  }

  //===--- internal/private API -------------------------------------------===//

  /// Make ordinary initialization unavailable
  internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only initialize these by calling create")
  }
}

/// A class whose instances contain a property of type `Header` and raw
/// storage for an array of `Element`, whose size is determined at
/// instance creation.
///
/// Note that the `Element` array is suitably-aligned **raw memory**.
/// You are expected to construct and---if necessary---destroy objects
/// there yourself, using the APIs on `UnsafeMutablePointer<Element>`.
/// Typical usage stores a count and capacity in `Header` and destroys
/// any live elements in the `deinit` of a subclass.
/// - Note: Subclasses must not have any stored properties; any storage
///   needed should be included in `Header`.
public class ManagedBuffer<Header, Element>
  : ManagedProtoBuffer<Header, Element> {

  /// Create a new instance of the most-derived class, calling
  /// `initialHeader` on the partially-constructed object to
  /// generate an initial `Header`.
  public final class func create(
    minimumCapacity: Int,
    initialHeader: @noescape (ManagedProtoBuffer<Header, Element>) throws -> Header
  ) rethrows -> ManagedBuffer<Header, Element> {

    let p = try ManagedBufferPointer<Header, Element>(
      bufferClass: self,
      minimumCapacity: minimumCapacity,
      initialHeader: { buffer, _ in
        try initialHeader(
          unsafeDowncast(buffer, to: ManagedProtoBuffer<Header, Element>.self))
      })

    return unsafeDowncast(p.buffer, to: ManagedBuffer<Header, Element>.self)
  }

  /// Destroy the stored Header.
  deinit {
    ManagedBufferPointer(self).withUnsafeMutablePointerToHeader { $0.deinitialize() }
  }

  /// The stored `Header` instance.
  public final var header: Header {
    addressWithNativeOwner {
      return (
        ManagedBufferPointer(self).withUnsafeMutablePointerToHeader { UnsafePointer($0) },
        Builtin.castToNativeObject(self))
    }
    mutableAddressWithNativeOwner {
      return (
        ManagedBufferPointer(self).withUnsafeMutablePointerToHeader { $0 },
        Builtin.castToNativeObject(self))
    }
  }
}

/// Contains a buffer object, and provides access to an instance of
/// `Header` and contiguous storage for an arbitrary number of
/// `Element` instances stored in that buffer.
///
/// For most purposes, the `ManagedBuffer` class works fine for this
/// purpose, and can simply be used on its own.  However, in cases
/// where objects of various different classes must serve as storage,
/// `ManagedBufferPointer` is needed.
///
/// A valid buffer class is non-`@objc`, with no declared stored
///   properties.  Its `deinit` must destroy its
///   stored `Header` and any constructed `Element`s.
///
/// Example Buffer Class
/// --------------------
///
///      class MyBuffer<Element> { // non-@objc
///        typealias Manager = ManagedBufferPointer<(Int, String), Element>
///        deinit {
///          Manager(unsafeBufferObject: self).withUnsafeMutablePointers {
///            (pointerToHeader, pointerToElements) -> Void in
///            pointerToElements.deinitialize(count: self.count)
///            pointerToHeader.deinitialize()
///          }
///        }
///
///        // All properties are *computed* based on members of the Header
///        var count: Int {
///          return Manager(unsafeBufferObject: self).header.0
///        }
///        var name: String {
///          return Manager(unsafeBufferObject: self).header.1
///        }
///      }
///
@_fixed_layout
public struct ManagedBufferPointer<Header, Element> : Equatable {

  /// Create with new storage containing an initial `Header` and space
  /// for at least `minimumCapacity` `element`s.
  ///
  /// - parameter bufferClass: The class of the object used for storage.
  /// - parameter minimumCapacity: The minimum number of `Element`s that
  ///   must be able to be stored in the new buffer.
  /// - parameter initialHeader: A function that produces the initial
  ///   `Header` instance stored in the buffer, given the `buffer`
  ///   object and a function that can be called on it to get the actual
  ///   number of allocated elements.
  ///
  /// - Precondition: `minimumCapacity >= 0`, and the type indicated by
  ///   `bufferClass` is a non-`@objc` class with no declared stored
  ///   properties.  The `deinit` of `bufferClass` must destroy its
  ///   stored `Header` and any constructed `Element`s.
  public init(
    bufferClass: AnyClass,
    minimumCapacity: Int,
    initialHeader: @noescape (buffer: AnyObject, capacity: @noescape (AnyObject) -> Int) throws -> Header
  ) rethrows {
    self = ManagedBufferPointer(bufferClass: bufferClass, minimumCapacity: minimumCapacity)

    // initialize the header field
    try withUnsafeMutablePointerToHeader {
      $0.initialize(with: 
        try initialHeader(
          buffer: self.buffer,
          capacity: {
            ManagedBufferPointer(unsafeBufferObject: $0).capacity
          }))
    }
    // FIXME: workaround for <rdar://problem/18619176>.  If we don't
    // access header somewhere, its addressor gets linked away
    _ = header
  }

  /// Manage the given `buffer`.
  ///
  /// - Precondition: `buffer` is an instance of a non-`@objc` class whose
  ///   `deinit` destroys its stored `Header` and any constructed `Element`s.
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
  @_versioned
  internal init(_uncheckedUnsafeBufferObject buffer: AnyObject) {
    ManagedBufferPointer._sanityCheckValidBufferClass(buffer.dynamicType)
    self._nativeBuffer = Builtin.castToNativeObject(buffer)
  }

  /// The stored `Header` instance.
  public var header: Header {
    addressWithNativeOwner {
      return (UnsafePointer(_headerPointer), _nativeBuffer)
    }
    mutableAddressWithNativeOwner {
      return (_headerPointer, _nativeBuffer)
    }
  }

  /// Returns the object instance being used for storage.
  public var buffer: AnyObject {
    return Builtin.castFromNativeObject(_nativeBuffer)
  }

  /// The actual number of elements that can be stored in this object.
  ///
  /// This value may be nontrivial to compute; it is usually a good
  /// idea to store this information in the "header" area when
  /// an instance is created.
  public var capacity: Int {
    return (_capacityInBytes &- _My._elementOffset) / strideof(Element.self)
  }

  /// Call `body` with an `UnsafeMutablePointer` to the stored
  /// `Header`.
  ///
  /// - Note: This pointer is only valid
  ///   for the duration of the call to `body`.
  public func withUnsafeMutablePointerToHeader<R>(
    _ body: @noescape (UnsafeMutablePointer<Header>) throws -> R
  ) rethrows -> R {
    return try withUnsafeMutablePointers { (v, e) in return try body(v) }
  }

  /// Call `body` with an `UnsafeMutablePointer` to the `Element`
  /// storage.
  ///
  /// - Note: This pointer is only valid for the duration of the
  ///   call to `body`.
  public func withUnsafeMutablePointerToElements<R>(
    _ body: @noescape (UnsafeMutablePointer<Element>) throws -> R
  ) rethrows -> R {
    return try withUnsafeMutablePointers { return try body($0.1) }
  }

  /// Call `body` with `UnsafeMutablePointer`s to the stored `Header`
  /// and raw `Element` storage.
  ///
  /// - Note: These pointers are only valid for the duration of the
  ///   call to `body`.
  public func withUnsafeMutablePointers<R>(
    _ body: @noescape (UnsafeMutablePointer<Header>, UnsafeMutablePointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(_nativeBuffer) }
    return try body(_headerPointer, _elementPointer)
  }

  /// Returns `true` iff `self` holds the only strong reference to its buffer.
  ///
  /// See `isUniquelyReferenced` for details.
  public mutating func holdsUniqueReference() -> Bool {
    return _isUnique(&_nativeBuffer)
  }

  /// Returns `true` iff either `self` holds the only strong reference
  /// to its buffer or the pinned has been 'pinned'.
  ///
  /// See `isUniquelyReferenced` for details.
  public mutating func holdsUniqueOrPinnedReference() -> Bool {
    return _isUniqueOrPinned(&_nativeBuffer)
  }

  //===--- internal/private API -------------------------------------------===//

  /// Create with new storage containing space for an initial `Header`
  /// and at least `minimumCapacity` `element`s.
  ///
  /// - parameter bufferClass: The class of the object used for storage.
  /// - parameter minimumCapacity: The minimum number of `Element`s that
  ///   must be able to be stored in the new buffer.
  ///
  /// - Precondition: `minimumCapacity >= 0`, and the type indicated by
  ///   `bufferClass` is a non-`@objc` class with no declared stored
  ///   properties.  The `deinit` of `bufferClass` must destroy its
  ///   stored `Header` and any constructed `Element`s.
  internal init(
    bufferClass: AnyClass,
    minimumCapacity: Int
  ) {
    ManagedBufferPointer._checkValidBufferClass(bufferClass, creating: true)
    _precondition(
      minimumCapacity >= 0,
      "ManagedBufferPointer must have non-negative capacity")

    self.init(
      _uncheckedBufferClass: bufferClass, minimumCapacity: minimumCapacity)
  }

  /// Internal version for use by _ContiguousArrayBuffer.init where we know that
  /// we have a valid buffer class and that the capacity is >= 0.
  @_versioned
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
      bufferType: _uncheckedBufferClass,
      size: totalSize,
      alignmentMask: _My._alignmentMask)

    self._nativeBuffer = Builtin.castToNativeObject(newBuffer)
  }

  /// Manage the given `buffer`.
  ///
  /// - Note: It is an error to use the `header` property of the resulting
  ///   instance unless it has been initialized.
  internal init(_ buffer: ManagedProtoBuffer<Header, Element>) {
    _nativeBuffer = Builtin.castToNativeObject(buffer)
  }

  internal typealias _My = ManagedBufferPointer

  internal static func _checkValidBufferClass(
    _ bufferClass: AnyClass, creating: Bool = false
  ) {
    _debugPrecondition(
      _class_getInstancePositiveExtentSize(bufferClass) == sizeof(_HeapObject.self)
      || (
        !creating
        && _class_getInstancePositiveExtentSize(bufferClass)
          == _headerOffset + sizeof(Header.self)),
      "ManagedBufferPointer buffer class has illegal stored properties"
    )
    _debugPrecondition(
      _usesNativeSwiftReferenceCounting(bufferClass),
      "ManagedBufferPointer buffer class must be non-@objc"
    )
  }

  internal static func _sanityCheckValidBufferClass(
    _ bufferClass: AnyClass, creating: Bool = false
  ) {
    _sanityCheck(
      _class_getInstancePositiveExtentSize(bufferClass) == sizeof(_HeapObject.self)
      || (
        !creating
        && _class_getInstancePositiveExtentSize(bufferClass)
          == _headerOffset + sizeof(Header.self)),
      "ManagedBufferPointer buffer class has illegal stored properties"
    )
    _sanityCheck(
      _usesNativeSwiftReferenceCounting(bufferClass),
      "ManagedBufferPointer buffer class must be non-@objc"
    )
  }

  /// The required alignment for allocations of this type, minus 1
  internal static var _alignmentMask: Int {
    return max(
      alignof(_HeapObject.self),
      max(alignof(Header.self), alignof(Element.self))) &- 1
  }

  /// The actual number of bytes allocated for this object.
  internal var _capacityInBytes: Int {
    return _swift_stdlib_malloc_size(_address)
  }

  /// The address of this instance in a convenient pointer-to-bytes form
  internal var _address: UnsafePointer<UInt8> {
    return UnsafePointer(Builtin.bridgeToRawPointer(_nativeBuffer))
  }

  /// Offset from the allocated storage for `self` to the stored `Header`
  internal static var _headerOffset: Int {
    _onFastPath()
    return _roundUp(
      sizeof(_HeapObject.self),
      toAlignment: alignof(Header.self))
  }

  /// An **unmanaged** pointer to the storage for the `Header`
  /// instance.  Not safe to use without _fixLifetime calls to
  /// guarantee it doesn't dangle
  internal var _headerPointer: UnsafeMutablePointer<Header> {
    _onFastPath()
    return UnsafeMutablePointer(_address + _My._headerOffset)
  }

  /// An **unmanaged** pointer to the storage for `Element`s.  Not
  /// safe to use without _fixLifetime calls to guarantee it doesn't
  /// dangle.
  internal var _elementPointer: UnsafeMutablePointer<Element> {
    _onFastPath()
    return UnsafeMutablePointer(_address + _My._elementOffset)
  }

  /// Offset from the allocated storage for `self` to the `Element` storage
  internal static var _elementOffset: Int {
    _onFastPath()
    return _roundUp(
      _headerOffset + sizeof(Header.self),
      toAlignment: alignof(Element.self))
  }

  internal var _nativeBuffer: Builtin.NativeObject
}

public func == <Header, Element>(
  lhs: ManagedBufferPointer<Header, Element>,
  rhs: ManagedBufferPointer<Header, Element>
) -> Bool {
  return lhs._address == rhs._address
}

// FIXME: when our calling convention changes to pass self at +0,
// inout should be dropped from the arguments to these functions.

/// Returns `true` iff `object` is a non-`@objc` class instance with
/// a single strong reference.
///
/// * Does *not* modify `object`; the use of `inout` is an
///   implementation artifact.
/// * If `object` is an Objective-C class instance, returns `false`.
/// * Weak references do not affect the result of this function.
///
/// Useful for implementing the copy-on-write optimization for the
/// deep storage of value types:
///
///     mutating func modifyMe(_ arg: X) {
///       if isUniquelyReferencedNonObjC(&myStorage) {
///         myStorage.modifyInPlace(arg)
///       }
///       else {
///         myStorage = self.createModified(myStorage, arg)
///       }
///     }
///
/// This function is safe to use for `mutating` functions in
/// multithreaded code because a false positive would imply that there
/// is already a user-level data race on the value being mutated.
public func isUniquelyReferencedNonObjC<T : AnyObject>(_ object: inout T) -> Bool
{
  return _isUnique(&object)
}

internal func isUniquelyReferencedOrPinnedNonObjC<T : AnyObject>(_ object: inout T) -> Bool {
  return _isUniqueOrPinned(&object)
}

/// Returns `true` iff `object` is a non-`@objc` class instance with a single
/// strong reference.
///
/// * Does *not* modify `object`; the use of `inout` is an
///   implementation artifact.
/// * Weak references do not affect the result of this function.
///
/// Useful for implementing the copy-on-write optimization for the
/// deep storage of value types:
///
///     mutating func modifyMe(_ arg: X) {
///       if isUniquelyReferenced(&myStorage) {
///         myStorage.modifyInPlace(arg)
///       }
///       else {
///         myStorage = myStorage.createModified(arg)
///       }
///     }
///
/// This function is safe to use for `mutating` functions in
/// multithreaded code because a false positive would imply that there
/// is already a user-level data race on the value being mutated.
public func isUniquelyReferenced<T : NonObjectiveCBase>(
  _ object: inout T
) -> Bool {
  return _isUnique(&object)
}

/// Returns `true` iff `object` is a non-`@objc` class instance with
/// a single strong reference.
///
/// * Does *not* modify `object`; the use of `inout` is an
///   implementation artifact.
/// * If `object` is an Objective-C class instance, returns `false`.
/// * Weak references do not affect the result of this function.
///
/// Useful for implementing the copy-on-write optimization for the
/// deep storage of value types:
///
///     mutating func modifyMe(_ arg: X) {
///       if isUniquelyReferencedNonObjC(&myStorage) {
///         myStorage.modifyInPlace(arg)
///       }
///       else {
///         myStorage = self.createModified(myStorage, arg)
///       }
///     }
///
/// This function is safe to use for `mutating` functions in
/// multithreaded code because a false positive would imply that there
/// is already a user-level data race on the value being mutated.
public func isUniquelyReferencedNonObjC<T : AnyObject>(
  _ object: inout T?
) -> Bool {
  return _isUnique(&object)
}

extension ManagedBufferPointer {
  @available(*, unavailable, renamed: "capacity")
  public var allocatedElementCount: Int {
    Builtin.unreachable()
  }
}

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
///
/// `ManagedProtoBuffer` does not offer
/// access to the as-yet uninitialized `value` property of
/// `ManagedBuffer`,
public class ManagedProtoBuffer<Value, Element> : NonObjectiveCBase {
  /// The actual number of elements that can be stored in this object.
  ///
  /// This value may be nontrivial to compute; it is usually a good
  /// idea to store this information in the "value" area when
  /// an instance is created.
  public final var allocatedElementCount : Int {
    return (
      _allocatedByteCount &- ManagedProtoBuffer._elementOffset &+ sizeof(Element) &- 1
    ) &/ sizeof(Element)
  }

  /// Call `body` with an `UnsafeMutablePointer` to the stored `Value`
  public final func withUnsafeMutablePointerToValue<R>(
    body: (UnsafeMutablePointer<Value>)->R
  ) -> R {
    return withUnsafeMutablePointers { (v, e) in return body(v) }
  }
  
  /// Call body with an `UnsafeMutablePointer` to the `Element` storage
  public final func withUnsafeMutablePointerToElements<R>(
    body: (UnsafeMutablePointer<Element>)->R
  ) -> R {
    return withUnsafeMutablePointers { return body($0.1) }
  }

  /// Call body with `UnsafeMutablePointer`\ s to the stored `Value`
  /// and raw `Element` storage
  public final func withUnsafeMutablePointers<R>(
    body: (_: UnsafeMutablePointer<Value>, _: UnsafeMutablePointer<Element>)->R
  ) -> R {
    let result = body(
      UnsafeMutablePointer(_address + ManagedProtoBuffer._valueOffset),
      UnsafeMutablePointer(_address + ManagedProtoBuffer._elementOffset)
    )
    _fixLifetime(self)
    return result
  }
  
  //===--- internal/private API -------------------------------------------===//
  
  /// Make ordinary initialization unavailable
  internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only initialize these by calling create")
  }
  
  /// The required alignment for allocations of this type, minus 1
  internal final class var _alignmentMask : Int {
    return max(
      alignof(_HeapObject.self),
      max(alignof(Value.self), alignof(Element.self))) &- 1
  }

  /// The actual number of bytes allocated for this object.
  internal final var _allocatedByteCount : Int {
    return Int(bitPattern: malloc_size(unsafeAddressOf(self)))
  }
  
  /// The address of this instance in a convenient pointer-to-bytes form
  internal final var _address : UnsafePointer<UInt8> {
    return UnsafePointer(unsafeAddressOf(self))
  }

  /// Offset from the allocated storage for `self` to the stored `Value`
  internal final class var _valueOffset : Int {
    return _roundUpToAlignment(sizeof(_HeapObject.self), alignof(Value.self))
  }

  /// Offset from the allocated storage for `self` to the `Element` storage
  internal final class var _elementOffset : Int {
    return _roundUpToAlignment(
      _valueOffset + sizeof(Value.self), alignof(Element.self))
  }
  
  /// A hack that gives the deallocator information about our
  /// allocated size.  Probably completely unused per
  /// <rdar://problem/18156440>
  internal final func __getInstanceSizeAndAlignMask() -> (Int,Int) {
    return (_allocatedByteCount, ManagedProtoBuffer._alignmentMask)
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
  /// `initialize` on the partially-constructed object to generate an
  /// initial `Value`.
  ///
  /// Note, in particular, accessing `value` inside the `initialize`
  /// function is undefined.
  public final class func create(
    minimumCapacity: Int, initialize: (ManagedProtoBuffer<Value,Element>)->Value
  ) -> ManagedBuffer {
    _precondition(
      minimumCapacity >= 0,
      "ManagedBuffer must have non-negative capacity")

    let totalSize = ManagedBuffer._elementOffset
      +  minimumCapacity * strideof(Element.self)

    let alignMask = ManagedBuffer._alignmentMask

    let result: ManagedBuffer = unsafeDowncast(
        _swift_bufferAllocate(self, totalSize, alignMask)
      )
    
    result.withUnsafeMutablePointerToValue {
      $0.initialize(initialize(result))
    }
    return result
  }

  /// Destroy the stored Value
  deinit {
    // FIXME: doing the work in a helper is a workaround for
    // <rdar://problem/18158010>
    _deinit()
  }

  // FIXME: separating this from the real deinit is a workaround for
  // <rdar://problem/18158010>
  /// The guts of deinit(); do not call
  internal final func _deinit() {
    withUnsafeMutablePointerToValue { $0.destroy() }
  }

  /// The stored `Value` instance.
  ///
  /// Note: this value must not be accessed during instance creation.
  public final var value: Value {
    address {
      return withUnsafeMutablePointerToValue { UnsafePointer($0) }
    }
    mutableAddress {
      return withUnsafeMutablePointerToValue { $0 }
    }
  }
}

// FIXME: when our calling convention changes to pass self at +0,
// inout should be dropped from the arguments to these functions.

/// Returns `true` iff `object` is a non-\ `@objc` class with a single
/// strong reference.
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
  return result != 0
}

/// Returns `true` iff `object` is a non-\ `@objc` class with a single
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
  return result != 0
}


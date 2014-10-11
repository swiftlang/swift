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
    let p = ManagedBufferPointer<Value,Element>(self)
    return p.allocatedElementCount
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
      { buffer, _ in initialValue(
          // FIXME: should be an unsafeDowncast <rdar://problem/18618169> 
          Builtin.bridgeFromRawPointer(Builtin.bridgeToRawPointer(buffer)))
      })

    // FIXME: should be an unsafeDowncast <rdar://problem/18618169> 
    return Builtin.bridgeFromRawPointer(Builtin.bridgeToRawPointer(p.buffer))
  }

  /// Destroy the stored Value
  deinit {
    ManagedBufferPointer(self).withUnsafeMutablePointerToValue { $0.destroy() }
  }

  /// The stored `Value` instance.
  public final var value: Value {
    address {
      return ManagedBufferPointer(self).withUnsafeMutablePointerToValue {
        UnsafePointer($0)
      }
    }
    mutableAddress {
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
///   stored `Value` and any constructed `Elements`.
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
  /// :param: `minimumCapacity` the minimum number of `Elements` that
  ///   must be able to be stored in the new buffer.
  /// :param: `initialValue` a function that produces the initial
  ///   `Value` instance stored in the buffer, given the `buffer`
  ///   object and a function that can be called on it to get the actual
  ///   number of allocated elements.
  ///
  /// Requires: minimumCapacity >= 0, and the type indicated by
  /// `bufferClass` is a non-`@objc` class with no declared stored
  /// properties.  The `deinit` of `bufferClass` must destroy its
  /// stored `Value` and any constructed `Elements`.
  public init(
    bufferClass: AnyClass,
    minimumCapacity: Int,
    initialValue: (buffer: AnyObject, allocatedCount: (AnyObject)->Int)->Value
  ) {
    ManagedBufferPointer._checkValidBufferClass(bufferClass)
    _precondition(
      minimumCapacity >= 0,
      "ManagedBufferPointer must have non-negative capacity")

    let totalSize = _My._elementOffset
      +  minimumCapacity * strideof(Element.self)

    let newBuffer: AnyObject = _swift_bufferAllocate(
      bufferClass, totalSize, _My._alignmentMask)

    self._nativeBuffer = Builtin.castToNativeObject(newBuffer)

    // initialize the value field
    withUnsafeMutablePointerToValue {
      $0.initialize(
        initialValue(
          buffer: newBuffer,
          allocatedCount: {
            ManagedBufferPointer(unsafeBufferObject: $0).allocatedElementCount
          }))
    }
  }

  /// Manage the given `buffer`.
  ///
  /// Requires: `buffer` is an instance of a non-`@objc` class with no
  /// declared stored properties, whose `deinit` destroys its
  /// stored `Value` and any constructed `Elements`.
  public init(unsafeBufferObject buffer: AnyObject) {
    ManagedBufferPointer._checkValidBufferClass(buffer.dynamicType)
    self._nativeBuffer = Builtin.castToNativeObject(buffer)
  }

  /// The stored `Value` instance.
  public var value: Value {    
    /*
    address {
      return withUnsafeMutablePointerToValue { UnsafePointer($0) }
    }
    mutableAddress {
      return withUnsafeMutablePointerToValue { $0 }
    }
    */
    // FIXME: <rdar://problem/18619176> replace get/set with
    // addressors => link error
    get { return withUnsafeMutablePointerToValue { $0.memory } }
    set { withUnsafeMutablePointerToValue { $0.memory = newValue } }
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
  public var allocatedElementCount : Int {
    return (
      _allocatedByteCount &- _My._elementOffset &+ sizeof(Element) &- 1
    ) &/ sizeof(Element)
  }

  /// Call `body` with an `UnsafeMutablePointer` to the stored `Value`
  public func withUnsafeMutablePointerToValue<R>(
    body: (UnsafeMutablePointer<Value>)->R
  ) -> R {
    return withUnsafeMutablePointers { (v, e) in return body(v) }
  }
  
  /// Call body with an `UnsafeMutablePointer` to the `Element` storage
  public func withUnsafeMutablePointerToElements<R>(
    body: (UnsafeMutablePointer<Element>)->R
  ) -> R {
    return withUnsafeMutablePointers { return body($0.1) }
  }

  /// Call body with `UnsafeMutablePointer`\ s to the stored `Value`
  /// and raw `Element` storage
  public func withUnsafeMutablePointers<R>(
    body: (_: UnsafeMutablePointer<Value>, _: UnsafeMutablePointer<Element>)->R
  ) -> R {
    let result = body(
      UnsafeMutablePointer(_address + _My._valueOffset),
      UnsafeMutablePointer(_address + _My._elementOffset)
    )
    _fixLifetime(_nativeBuffer)
    return result
  }
  
  //===--- internal/private API -------------------------------------------===//
  
  /// Manage the given `buffer`.
  ///
  /// **Note:** it is an error to use the `value` property of the resulting
  /// instance unless it has been initialized.
  internal init(_ buffer: ManagedProtoBuffer<Value, Element>) {
    _nativeBuffer = Builtin.castToNativeObject(buffer)
  }
  
  internal typealias _My = ManagedBufferPointer

  internal static func _checkValidBufferClass(bufferClass: AnyClass) {
    _debugPrecondition(
      _class_getInstanceSize(bufferClass)
      == _class_getInstanceSize(ManagedBuffer<Int,Int>.self),
      "ManagedBufferPointer buffer class has declared stored properties"
    )
    _debugPrecondition(
      _usesNativeSwiftReferenceCounting(bufferClass),
      "ManagedBufferPointer buffer class must be non-@objc"
    )
  }
  
  /// The required alignment for allocations of this type, minus 1
  internal static var _alignmentMask : Int {
    return max(
      alignof(_HeapObject.self),
      max(alignof(Value.self), alignof(Element.self))) &- 1
  }

  /// The actual number of bytes allocated for this object.
  internal var _allocatedByteCount : Int {
    return Int(bitPattern: malloc_size(_address))
  }
  
  /// The address of this instance in a convenient pointer-to-bytes form
  internal var _address : UnsafePointer<UInt8> {
    return UnsafePointer(Builtin.bridgeToRawPointer(_nativeBuffer))
  }

  /// Offset from the allocated storage for `self` to the stored `Value`
  internal static var _valueOffset : Int {
    return _roundUpToAlignment(sizeof(_HeapObject.self), alignof(Value.self))
  }

  /// Offset from the allocated storage for `self` to the `Element` storage
  internal static var _elementOffset : Int {
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


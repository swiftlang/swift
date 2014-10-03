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

/// A common base class for all `ManagedBuffer<V,E>`\ 's.
public class ManagedBufferBase {}

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
public class ManagedBuffer<Value, Element> : ManagedBufferBase {
  
  /// Create a new instance of the most-derived class, calling
  /// `initialize` on the partially-constructed object to generate an
  /// initial `Value`.
  ///
  /// Note, in particular, accessing `value` inside the `initialize`
  /// function is undefined.
  public final class func create(
    minimumCapacity: Int, initialize: (ManagedBuffer)->Value
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

  /// Make ordinary initialization unavailable
  internal init(_doNotCallMe: ()) {
    fatalError("Only initialize these by calling create")
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
  
  /// The actual number of elements that can be stored in this object.
  ///
  /// This value may be nontrivial to compute; it is usually a good
  /// idea to store this information in the "value" area when
  /// an instance is created.
  public final var allocatedElementCount : Int {
    return (
      _allocatedByteCount &- ManagedBuffer._elementOffset &+ sizeof(Element) &- 1
    ) &/ sizeof(Element)
  }

  /// The stored `Value` instance.
  ///
  /// Note: this value must not be accessed during instance creation.
  public final var value: Value {
    get {
      return withUnsafeMutablePointerToValue {
        return $0.memory
      }
    }
    set {
      withUnsafeMutablePointerToValue {
        $0.memory = newValue
      }
    }
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
      UnsafeMutablePointer(_address + ManagedBuffer._valueOffset),
      UnsafeMutablePointer(_address + ManagedBuffer._elementOffset)
    )
    _fixLifetime(self)
    return result
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
    return (_allocatedByteCount, ManagedBuffer._alignmentMask)
  }
}

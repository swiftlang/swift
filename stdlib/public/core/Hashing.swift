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

//
// This file implements helpers for hashing collections.
//

/// The inverse of the default hash table load factor.  Factored out so that it
/// can be used in multiple places in the implementation and stay consistent.
/// Should not be used outside `Dictionary` implementation.
@usableFromInline @_transparent
internal var _hashContainerDefaultMaxLoadFactorInverse: Double {
  return 1.0 / 0.75
}

#if _runtime(_ObjC)
/// Call `[lhs isEqual: rhs]`.
///
/// This function is part of the runtime because `Bool` type is bridged to
/// `ObjCBool`, which is in Foundation overlay.
/// FIXME(sil-serialize-all): this should be internal
@usableFromInline // FIXME(sil-serialize-all)
@_silgen_name("swift_stdlib_NSObject_isEqual")
internal func _stdlib_NSObject_isEqual(_ lhs: AnyObject, _ rhs: AnyObject) -> Bool
#endif


/// A temporary view of an array of AnyObject as an array of Unmanaged<AnyObject>
/// for fast iteration and transformation of the elements.
///
/// Accesses the underlying raw memory as Unmanaged<AnyObject> using untyped
/// memory accesses. The memory remains bound to managed AnyObjects.
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal struct _UnmanagedAnyObjectArray {
  /// Underlying pointer.
  @usableFromInline // FIXME(sil-serialize-all)
  internal var value: UnsafeMutableRawPointer

  @inlinable // FIXME(sil-serialize-all)
  internal init(_ up: UnsafeMutablePointer<AnyObject>) {
    self.value = UnsafeMutableRawPointer(up)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal init?(_ up: UnsafeMutablePointer<AnyObject>?) {
    guard let unwrapped = up else { return nil }
    self.init(unwrapped)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal subscript(i: Int) -> AnyObject {
    get {
      let unmanaged = value.load(
        fromByteOffset: i * MemoryLayout<AnyObject>.stride,
        as: Unmanaged<AnyObject>.self)
      return unmanaged.takeUnretainedValue()
    }
    nonmutating set(newValue) {
      let unmanaged = Unmanaged.passUnretained(newValue)
      value.storeBytes(of: unmanaged,
        toByteOffset: i * MemoryLayout<AnyObject>.stride,
        as: Unmanaged<AnyObject>.self)
    }
  }
}

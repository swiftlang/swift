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

import SwiftShims

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
@_silgen_name("swift_stdlib_NSObject_isEqual")
internal func _stdlib_NSObject_isEqual(_ lhs: AnyObject, _ rhs: AnyObject) -> Bool
#endif


/// A temporary view of an array of AnyObject as an array of Unmanaged<AnyObject>
/// for fast iteration and transformation of the elements.
///
/// Accesses the underlying raw memory as Unmanaged<AnyObject> using untyped
/// memory accesses. The memory remains bound to managed AnyObjects.
@unsafe
internal struct _UnmanagedAnyObjectArray {
  /// Underlying pointer.
  internal var value: UnsafeMutableRawPointer

  internal init(_ up: UnsafeMutablePointer<AnyObject>) {
    unsafe self.value = UnsafeMutableRawPointer(up)
  }

  internal init?(_ up: UnsafeMutablePointer<AnyObject>?) {
    guard let unwrapped = unsafe up else { return nil }
    unsafe self.init(unwrapped)
  }

  internal subscript(i: Int) -> AnyObject {
    get {
      let unmanaged = unsafe value.load(
        fromByteOffset: i * MemoryLayout<AnyObject>.stride,
        as: Unmanaged<AnyObject>.self)
      return unsafe unmanaged.takeUnretainedValue()
    }
    nonmutating set(newValue) {
      let unmanaged = unsafe Unmanaged.passUnretained(newValue)
      unsafe value.storeBytes(of: unmanaged,
        toByteOffset: i * MemoryLayout<AnyObject>.stride,
        as: Unmanaged<AnyObject>.self)
    }
  }
}

#if _runtime(_ObjC)
/// An NSEnumerator implementation returning zero elements. This is useful when
/// a concrete element type is not recoverable from the empty singleton.
// NOTE: older runtimes called this class _SwiftEmptyNSEnumerator. The two
// must coexist without conflicting ObjC class names, so it was
// renamed. The old name must not be used in the new runtime.
final internal class __SwiftEmptyNSEnumerator
  : __SwiftNativeNSEnumerator, _NSEnumerator {
  internal override required init() {
    super.init()
    _internalInvariant(_orphanedFoundationSubclassesReparented)
  }

  @objc
  internal func nextObject() -> AnyObject? {
    return nil
  }

  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>,
    count: Int
  ) -> Int {
    // Even though we never do anything in here, we need to update the
    // state so that callers know we actually ran.
    var theState = unsafe state.pointee
    if unsafe theState.state == 0 {
      unsafe theState.state = 1 // Arbitrary non-zero value.
      unsafe theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      unsafe theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
    }
    unsafe state.pointee = theState
    return 0
  }
}
#endif

#if _runtime(_ObjC)
/// This is a minimal class holding a single tail-allocated flat buffer,
/// representing hash table storage for AnyObject elements. This is used to
/// store bridged elements in deferred bridging scenarios.
///
/// Using a dedicated class for this rather than a _BridgingBuffer makes it easy
/// to recognize these in heap dumps etc.
// NOTE: older runtimes called this class _BridgingHashBuffer.
// The two must coexist without a conflicting ObjC class name, so it
// was renamed. The old name must not be used in the new runtime.
@unsafe
internal final class __BridgingHashBuffer
  : ManagedBuffer<__BridgingHashBuffer.Header, AnyObject> {
  @unsafe
  struct Header {
    internal var owner: AnyObject
    internal var hashTable: _HashTable

    init(owner: AnyObject, hashTable: _HashTable) {
      unsafe self.owner = owner
      unsafe self.hashTable = unsafe hashTable
    }
  }

  internal static func allocate(
    owner: AnyObject,
    hashTable: _HashTable
  ) -> __BridgingHashBuffer {
    let buffer = unsafe self.create(minimumCapacity: hashTable.bucketCount) { _ in
      unsafe Header(owner: owner, hashTable: hashTable)
    }
    return unsafe unsafeDowncast(buffer, to: __BridgingHashBuffer.self)
  }

  deinit {
    for unsafe bucket in unsafe header.hashTable {
      unsafe (firstElementAddress + bucket.offset).deinitialize(count: 1)
    }
    unsafe _fixLifetime(self)
  }

  internal subscript(bucket: _HashTable.Bucket) -> AnyObject {
    @inline(__always) get {
      unsafe _internalInvariant(header.hashTable.isOccupied(bucket))
      defer { unsafe _fixLifetime(self) }
      return unsafe firstElementAddress[bucket.offset]
    }
  }

  @inline(__always)
  internal func initialize(at bucket: _HashTable.Bucket, to object: AnyObject) {
    unsafe _internalInvariant(header.hashTable.isOccupied(bucket))
    unsafe (firstElementAddress + bucket.offset).initialize(to: object)
    unsafe _fixLifetime(self)
  }
}
#endif

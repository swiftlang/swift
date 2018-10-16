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
internal struct _UnmanagedAnyObjectArray {
  /// Underlying pointer.
  internal var value: UnsafeMutableRawPointer

  internal init(_ up: UnsafeMutablePointer<AnyObject>) {
    self.value = UnsafeMutableRawPointer(up)
  }

  internal init?(_ up: UnsafeMutablePointer<AnyObject>?) {
    guard let unwrapped = up else { return nil }
    self.init(unwrapped)
  }

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

#if _runtime(_ObjC)
/// An NSEnumerator implementation returning zero elements. This is useful when
/// a concrete element type is not recoverable from the empty singleton.
final internal class _SwiftEmptyNSEnumerator
  : __SwiftNativeNSEnumerator, _NSEnumerator {
  internal override required init() {}

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
    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
    }
    state.pointee = theState
    return 0
  }
}
#endif

#if _runtime(_ObjC)
/// This is a minimal class holding a single tail-allocated flat buffer,
/// representing hash table storage for AnyObject elements. This is used to
/// store bridged elements in deferred bridging scenarios.
///
/// Using a dedicated class for this rather than a _HeapBuffer makes it easy to
/// recognize these in heap dumps etc.
internal final class _BridgingHashBuffer
  : ManagedBuffer<_BridgingHashBuffer.Header, AnyObject> {
  struct Header {
    internal var owner: AnyObject
    internal var hashTable: _HashTable

    init(owner: AnyObject, hashTable: _HashTable) {
      self.owner = owner
      self.hashTable = hashTable
    }
  }

  internal static func allocate(
    owner: AnyObject,
    hashTable: _HashTable
  ) -> _BridgingHashBuffer {
    let buffer = self.create(minimumCapacity: hashTable.bucketCount) { _ in
      Header(owner: owner, hashTable: hashTable)
    }
    return unsafeDowncast(buffer, to: _BridgingHashBuffer.self)
  }

  deinit {
    for bucket in header.hashTable {
      (firstElementAddress + bucket.offset).deinitialize(count: 1)
    }
    _fixLifetime(self)
  }

  internal subscript(bucket: _HashTable.Bucket) -> AnyObject {
    @inline(__always) get {
      _sanityCheck(header.hashTable.isOccupied(bucket))
      defer { _fixLifetime(self) }
      return firstElementAddress[bucket.offset]
    }
  }

  @inline(__always)
  internal func initialize(at bucket: _HashTable.Bucket, to object: AnyObject) {
    _sanityCheck(header.hashTable.isOccupied(bucket))
    (firstElementAddress + bucket.offset).initialize(to: object)
    _fixLifetime(self)
  }
}
#endif

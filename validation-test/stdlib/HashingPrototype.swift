// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

/*

<rdar://problem/14196462> Hashing in the standard library

New Hashing APIs
================

This is a proposal for new hashing APIs in the Swift standard library.

Current stdlib design has the Hashable protocol with 'hashValue: Int' property,
and the library leaves it up to the user how to implement it.  In the proposed
design in the 99% case the user only has to enumerate the properties they want
to be included in the hash, and the Swift library will compute a good hash.

Problem
=======

Threading a single Hasher through all computations reduces the cost to set up
and finalize hash value computation.  Unfortunately, it is not clear how to
allow this combineIntoHash() API to interoperate with Objective-C, in
particular, with Objective-C subclasses of Swift Hashable classes.  See FIXME
below.

*/

protocol NewHashable /*: Equatable*/ {
  func combineIntoHash<H : Hasher>(_ hasher: inout H)
}

struct UserTypeA : NewHashable {
  var a1: Int
  var a2: Float

  func combineIntoHash<H : Hasher>(_ hasher: inout H) {
    hasher.combine(a1)
    hasher.combine(a2)
  }
}

struct UserTypeB : NewHashable {
  var b1: Int
  var b2: UserTypeA // User-defined hashable type
  var b3: [Int]

  func combineIntoHash<H : Hasher>(_ hasher: inout H) {
    hasher.combine(b1)
    hasher.combine(b2)
    hasher.combineSequence(b3)
  }
}

class UserClassA : NSObject {
  var a1: Int = 0

  // error: declarations from extensions cannot be overridden yet
  //func combineIntoHash<H : Hasher>(_ hasher: inout H) {
  //  hasher.combine(a1)
  //}

  // FIXME: Problem: what method should a derived Objective-C subclass
  // override?  'combineIntoHash' is not @objc.
}

//===----------------------------------------------------------------------===//
// Implementation
//===----------------------------------------------------------------------===//

/// A hasher object computes a hash value.
///
/// Precondition: two hasher objects compute the same hash value when
/// the same sequence of `combine(...)` calls with equal arguments is
/// performed on both of them.
protocol Hasher {
  //
  // Primary APIs
  //

  mutating func combine(_ value: Int)
  mutating func combine(_ value: Float)
  // ... overloads for other primitive types...

  mutating func squeezeHashValue<I : SignedInteger>(
    _ resultRange: Range<I>) -> I
  mutating func squeezeHashValue<I : UnsignedInteger>(
    _ resultRange: Range<I>) -> I

  //
  // Convenience APIs; would be completely implemented by default
  // implementations if we had them.
  //

  // This handles arrays, UnsafeBufferPointer, user-defined
  // collections.
  mutating func combineSequence<
    S : Sequence
  >(_ s: S)
  where S.Iterator.Element : NewHashable

  mutating func combine<H : NewHashable>(_ value: H)
}

/// A hasher for in-process, non-persistent hashtables.
struct InProcessHashtableHasher : Hasher {
  // Only for exposition.
  var _state: Int

  init() {
    // Should initialize to per-process seed.
    _state = 0
  }

  mutating func combine(_ value: Int) {
    // Only for exposition.
    _state = _state ^ value
  }

  mutating func combine(_ value: Float) {
    // Only for exposition.
    _state = _state ^ Int(value.bitPattern)
  }

  mutating func combineSequence<
    S : Sequence
  >(_ s: S)
  where S.Iterator.Element : NewHashable {
    for v in s {
      v.combineIntoHash(&self)
    }
  }

  mutating func combine<H : NewHashable>(_ value: H) {
    value.combineIntoHash(&self)
  }

  mutating func squeezeHashValue<I : SignedInteger>(
    _ resultRange: Range<I>) -> I {
    // ... finalize hash value computation first...
    return I(Int64(_state)) // Should actually clamp the value
  }
  mutating func squeezeHashValue<I : UnsignedInteger>(
    _ resultRange: Range<I>) -> I {
    // ... finalize hash value computation first...
    return I(UInt64(_state)) // Should actually clamp the value
  }
}

/// A hasher with 128-bit output and a well-defined algorithm stable
/// *across platforms*; useful for on-disk or distributed hash tables.
/// Not a cryptographic hash.
// struct StableFingerprint128Hasher : Hasher {}

extension Int : NewHashable {
  func combineIntoHash<H : Hasher>(_ hasher: inout H) {
    hasher.combine(self)
  }
}

//===----------------------------------------------------------------------===//
// Foundation overlay: interoperability with NSObject.hash
//===----------------------------------------------------------------------===//

import Foundation

extension NSObject : NewHashable {
  func combineIntoHash<H : Hasher>(_ hasher: inout H) {
    hasher.combine(self.hash)
  }
}


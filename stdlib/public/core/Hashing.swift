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
// This file implements helpers for constructing non-cryptographic hash
// functions.
//
// This code was ported from LLVM's ADT/Hashing.h.
//
// Currently the algorithm is based on CityHash, but this is an implementation
// detail.  Even more, there are facilities to mix in a per-execution seed to
// ensure that hash values differ between executions.
//

import SwiftShims

@_frozen // FIXME(sil-serialize-all)
public // @testable
enum _HashingDetail {

  // FIXME(hasher): Remove
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  internal static func getExecutionSeed() -> UInt64 {
    // FIXME: This needs to be a per-execution seed. This is just a placeholder
    // implementation.
    return 0xff51afd7ed558ccd
  }

  // FIXME(hasher): Remove
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  internal static func hash16Bytes(_ low: UInt64, _ high: UInt64) -> UInt64 {
    // Murmur-inspired hashing.
    let mul: UInt64 = 0x9ddfea08eb382d69
    var a: UInt64 = (low ^ high) &* mul
    a ^= (a >> 47)
    var b: UInt64 = (high ^ a) &* mul
    b ^= (b >> 47)
    b = b &* mul
    return b
  }
}

//
// API functions.
//

//
// _mix*() functions all have type (T) -> T.  These functions don't compress
// their inputs and just exhibit avalanche effect.
//

// FIXME(hasher): Remove
@inlinable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _mixUInt32(_ value: UInt32) -> UInt32 {
  // Zero-extend to 64 bits, hash, select 32 bits from the hash.
  //
  // NOTE: this differs from LLVM's implementation, which selects the lower
  // 32 bits.  According to the statistical tests, the 3 lowest bits have
  // weaker avalanche properties.
  let extendedValue = UInt64(value)
  let extendedResult = _mixUInt64(extendedValue)
  return UInt32((extendedResult >> 3) & 0xffff_ffff)
}

// FIXME(hasher): Remove
@inlinable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _mixInt32(_ value: Int32) -> Int32 {
  return Int32(bitPattern: _mixUInt32(UInt32(bitPattern: value)))
}

// FIXME(hasher): Remove
@inlinable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _mixUInt64(_ value: UInt64) -> UInt64 {
  // Similar to hash_4to8_bytes but using a seed instead of length.
  let seed: UInt64 = _HashingDetail.getExecutionSeed()
  let low: UInt64 = value & 0xffff_ffff
  let high: UInt64 = value >> 32
  return _HashingDetail.hash16Bytes(seed &+ (low << 3), high)
}

// FIXME(hasher): Remove
@inlinable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _mixInt64(_ value: Int64) -> Int64 {
  return Int64(bitPattern: _mixUInt64(UInt64(bitPattern: value)))
}

// FIXME(hasher): Remove
@inlinable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _mixUInt(_ value: UInt) -> UInt {
#if arch(i386) || arch(arm)
  return UInt(_mixUInt32(UInt32(value)))
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  return UInt(_mixUInt64(UInt64(value)))
#endif
}

// FIXME(hasher): Remove
@inlinable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _mixInt(_ value: Int) -> Int {
#if arch(i386) || arch(arm)
  return Int(_mixInt32(Int32(value)))
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  return Int(_mixInt64(Int64(value)))
#endif
}

/// Returns a new value that combines the two given hash values.
///
/// Combining is performed using [a hash function][ref] described by T.C. Hoad
/// and J. Zobel, which is also adopted in the Boost C++ libraries.
///
/// This function is used by synthesized implementations of `hashValue` to
/// combine the hash values of individual `struct` fields and associated values
/// of `enum`s. It is factored out into a standard library function so that the
/// specific hashing logic can be refined without requiring major changes to the
/// code that creates the synthesized AST nodes.
///
/// [ref]: https://pdfs.semanticscholar.org/03bf/7be88e88ba047c6ab28036d0f28510299226.pdf
@_transparent
public // @testable
func _combineHashValues(_ firstValue: Int, _ secondValue: Int) -> Int {
  // Use a magic number based on the golden ratio
  // (0x1.9e3779b97f4a7c15f39cc0605cedc8341082276bf3a27251f86c6a11d0c18e95p0).
#if arch(i386) || arch(arm)
  let magic = 0x9e3779b9 as UInt
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  let magic = 0x9e3779b97f4a7c15 as UInt
#endif
  var x = UInt(bitPattern: firstValue)
  x ^= UInt(bitPattern: secondValue) &+ magic &+ (x &<< 6) &+ (x &>> 2)
  return Int(bitPattern: x)
}

// FIXME(hasher): This hasher emulates Swift 4.1 hashValues. It is purely for
// benchmarking; to be removed.
internal struct _LegacyHasher {
  internal var _hash: Int

  @inline(__always)
  internal init(key: (UInt64, UInt64) = (0, 0)) { // key is ignored
    _hash = 0
  }

  @inline(__always)
  internal mutating func append(_ value: Int) {
    _hash = (_hash == 0 ? value : _combineHashValues(_hash, value))
  }

  @inline(__always)
  internal mutating func append(_ value: UInt) {
    append(Int(bitPattern: value))
  }

  @inline(__always)
  internal mutating func append(_ value: UInt32) {
    append(Int(truncatingIfNeeded: value))
  }

  @inline(__always)
  internal mutating func append(_ value: UInt64) {
    if UInt64.bitWidth > Int.bitWidth {
      append(Int(truncatingIfNeeded: value ^ (value &>> 32)))
    } else {
      append(Int(truncatingIfNeeded: value))
    }
  }

  @inline(__always)
  internal mutating func finalize() -> UInt64 {
    return UInt64(
      _truncatingBits: UInt(bitPattern: _mixInt(_hash))._lowWord)
  }
}


// NOT @_fixed_layout
@_fixed_layout // FIXME - remove - radar 38549901
public struct _Hasher {
  internal typealias Core = _SipHash13

  // NOT @usableFromInline
  internal var _core: Core

  // NOT @inlinable
  @effects(releasenone)
  public init() {
    self._core = Core(key: _Hasher._seed)
  }

  // NOT @inlinable
  @effects(releasenone)
  public init(seed: (UInt64, UInt64)) {
    self._core = Core(key: seed)
  }

  /// Indicates whether we're running in an environment where hashing needs to
  /// be deterministic. If this is true, the hash seed is not random, and hash
  /// tables do not apply per-instance perturbation that is not repeatable.
  /// This is not recommended for production use, but it is useful in certain
  /// test environments where randomization may lead to unwanted nondeterminism
  /// of test results.
  public // SPI
  static var _isDeterministic: Bool {
    @inlinable
    @inline(__always)
    get {
      return _swift_stdlib_Hashing_parameters.deterministic;
    }
  }

  /// The 128-bit hash seed used to initialize the hasher state. Initialized
  /// once during process startup.
  public // SPI
  static var _seed: (UInt64, UInt64) {
    @inlinable
    @inline(__always)
    get {
      // The seed itself is defined in C++ code so that it is initialized during
      // static construction.  Almost every Swift program uses hash tables, so
      // initializing the seed during the startup seems to be the right
      // trade-off.
      return (
        _swift_stdlib_Hashing_parameters.seed0,
        _swift_stdlib_Hashing_parameters.seed1)
    }
  }

  @inlinable
  @inline(__always)
  public mutating func append<H: Hashable>(_ value: H) {
    value._hash(into: &self)
  }

  // NOT @inlinable
  @effects(releasenone)
  public mutating func append(bits: UInt) {
    _core.append(bits)
  }
  // NOT @inlinable
  @effects(releasenone)
  public mutating func append(bits: UInt32) {
    _core.append(bits)
  }
  // NOT @inlinable
  @effects(releasenone)
  public mutating func append(bits: UInt64) {
    _core.append(bits)
  }

  // NOT @inlinable
  @effects(releasenone)
  public mutating func append(bits: Int) {
    _core.append(UInt(bitPattern: bits))
  }
  // NOT @inlinable
  @effects(releasenone)
  public mutating func append(bits: Int32) {
    _core.append(UInt32(bitPattern: bits))
  }
  // NOT @inlinable
  @effects(releasenone)
  public mutating func append(bits: Int64) {
    _core.append(UInt64(bitPattern: bits))
  }

  // NOT @inlinable
  @effects(releasenone)
  public mutating func finalize() -> Int {
    return Int(truncatingIfNeeded: _core.finalize())
  }
}

/// This protocol is only used for compile-time checks that
/// every buffer type implements all required operations.
internal protocol _HashBuffer {
  associatedtype Key
  associatedtype Value
  associatedtype Index
  associatedtype SequenceElement
  associatedtype SequenceElementWithoutLabels
  var startIndex: Index { get }
  var endIndex: Index { get }

  func index(after i: Index) -> Index

  func formIndex(after i: inout Index)

  func index(forKey key: Key) -> Index?

  func assertingGet(_ i: Index) -> SequenceElement

  func assertingGet(_ key: Key) -> Value

  func maybeGet(_ key: Key) -> Value?

  @discardableResult
  mutating func updateValue(_ value: Value, forKey key: Key) -> Value?

  @discardableResult
  mutating func insert(
    _ value: Value, forKey key: Key
  ) -> (inserted: Bool, memberAfterInsert: Value)

  @discardableResult
  mutating func remove(at index: Index) -> SequenceElement

  @discardableResult
  mutating func removeValue(forKey key: Key) -> Value?

  mutating func removeAll(keepingCapacity keepCapacity: Bool)

  var count: Int { get }

  static func fromArray(_ elements: [SequenceElementWithoutLabels]) -> Self
}

/// The inverse of the default hash table load factor.  Factored out so that it
/// can be used in multiple places in the implementation and stay consistent.
/// Should not be used outside `Dictionary` implementation.
@inlinable // FIXME(sil-serialize-all)
@_transparent
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

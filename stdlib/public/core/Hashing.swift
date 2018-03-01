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

@_fixed_layout // FIXME(sil-serialize-all)
public // @testable
enum _Hashing {
  // FIXME(ABI)#41 : make this an actual public API.
  @_inlineable // FIXME(sil-serialize-all)
  public // SPI
  static var secretKey: (UInt64, UInt64) {
    get {
      // The variable itself is defined in C++ code so that it is initialized
      // during static construction.  Almost every Swift program uses hash
      // tables, so initializing the secret key during the startup seems to be
      // the right trade-off.
      return (
        _swift_stdlib_Hashing_secretKey.key0,
        _swift_stdlib_Hashing_secretKey.key1)
    }
    set {
      (_swift_stdlib_Hashing_secretKey.key0,
       _swift_stdlib_Hashing_secretKey.key1) = newValue
    }
  }
}

@_fixed_layout // FIXME(sil-serialize-all)
public // @testable
enum _HashingDetail {

  // FIXME(hasher): Remove
  @_inlineable // FIXME(sil-serialize-all)
  public // @testable
  static var fixedSeedOverride: UInt64 {
    get {
      // HACK: the variable itself is defined in C++ code so that it is
      // guaranteed to be statically initialized.  This is a temporary
      // workaround until the compiler can do the same for Swift.
      return _swift_stdlib_HashingDetail_fixedSeedOverride
    }
    set {
      _swift_stdlib_HashingDetail_fixedSeedOverride = newValue
    }
  }

  // FIXME(hasher): Remove
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned
  @_transparent
  internal static func getExecutionSeed() -> UInt64 {
    // FIXME: This needs to be a per-execution seed. This is just a placeholder
    // implementation.
    let seed: UInt64 = 0xff51afd7ed558ccd
    return _HashingDetail.fixedSeedOverride == 0 ? seed : fixedSeedOverride
  }

  // FIXME(hasher): Remove
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned
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
@_inlineable // FIXME(sil-serialize-all)
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
@_inlineable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _mixInt32(_ value: Int32) -> Int32 {
  return Int32(bitPattern: _mixUInt32(UInt32(bitPattern: value)))
}

// FIXME(hasher): Remove
@_inlineable // FIXME(sil-serialize-all)
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
@_inlineable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _mixInt64(_ value: Int64) -> Int64 {
  return Int64(bitPattern: _mixUInt64(UInt64(bitPattern: value)))
}

// FIXME(hasher): Remove
@_inlineable // FIXME(sil-serialize-all)
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
@_inlineable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _mixInt(_ value: Int) -> Int {
#if arch(i386) || arch(arm)
  return Int(_mixInt32(Int32(value)))
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  return Int(_mixInt64(Int64(value)))
#endif
}

/// Given a hash value, returns an integer value in the range of
/// 0..<`upperBound` that corresponds to a hash value.
///
/// The `upperBound` must be positive and a power of 2.
///
/// This function is superior to computing the remainder of `hashValue` by
/// the range length.  Some types have bad hash functions; sometimes simple
/// patterns in data sets create patterns in hash values and applying the
/// remainder operation just throws away even more information and invites
/// even more hash collisions.  This effect is especially bad because the
/// range is a power of two, which means to throws away high bits of the hash
/// (which would not be a problem if the hash was known to be good). This
/// function mixes the bits in the hash value to compensate for such cases.
///
/// Of course, this function is a compressing function, and applying it to a
/// hash value does not change anything fundamentally: collisions are still
/// possible, and it does not prevent malicious users from constructing data
/// sets that will exhibit pathological collisions.
@_inlineable // FIXME(sil-serialize-all)
public // @testable
func _squeezeHashValue(_ hashValue: Int, _ upperBound: Int) -> Int {
  _sanityCheck(_isPowerOf2(upperBound))
  let mixedHashValue = _mixInt(hashValue)

  // As `upperBound` is a power of two we can do a bitwise-and to calculate
  // mixedHashValue % upperBound.
  return mixedHashValue & (upperBound &- 1)
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

/// An unsafe wrapper around a stateful hash function, presenting a faux purely
/// functional interface to eliminate ARC overhead.
///
/// This is not a true value type; calling `appending` or `finalized` actually
/// mutates `self`'s state.
@_fixed_layout
public struct _UnsafeHasher {
  @_versioned
  internal let _rawState: UnsafeMutableRawPointer

  internal var _state: UnsafeMutablePointer<_Hasher> {
    @inline(__always)
    get { return _rawState.assumingMemoryBound(to: _Hasher.self) }
  }

  @inline(__always)
  @_versioned
  internal init(_ state: UnsafeMutablePointer<_Hasher>) {
    self._rawState = UnsafeMutableRawPointer(state)
  }

  @_versioned
  // not @_inlineable
  @effects(readonly) // FIXME(hasher): Unjustified
  static func hashValue<H: Hashable>(for pointer: UnsafePointer<H>) -> Int {
    var hasher = _Hasher()
    return withUnsafeMutablePointer(to: &hasher) { p in
      return pointer.pointee._hash(into: _UnsafeHasher(p))._finalized()
    }
  }

  @_versioned
  // not @_inlineable
  @effects(readonly)
  internal func appending(bits value: UInt) -> _UnsafeHasher {
    // The effects attribute is a lie; however, it enables the compiler to
    // eliminate unnecessary retain/releases protecting Hashable state around
    // calls to `_Hasher.append(_:)`.
    //
    // We don't have a way to describe the side-effects of an opaque function --
    // if it doesn't have an @effects attribute, the compiler has no choice but
    // to assume it may mutate the hashable we're visiting. We know that won't
    // be the case (the stdlib owns the hash function), but the only way to tell
    // this to the compiler is to pretend the state update is pure.
    _state.pointee.append(value)
    return self
  }

  @_versioned
  // not @_inlineable
  @effects(readonly) // See comment in appending(_: UInt)
  internal func appending(bits value: UInt32) -> _UnsafeHasher {
    _state.pointee.append(value)
    return self
  }

  @_versioned
  // not @_inlineable
  @effects(readonly) // See comment in appending(_: UInt)
  internal func appending(bits value: UInt64) -> _UnsafeHasher {
    _state.pointee.append(value)
    return self
  }

  @_inlineable
  @inline(__always)
  public func appending<H: Hashable>(_ value: H) -> _UnsafeHasher {
    return value._hash(into: self)
  }

  @inline(__always)
  internal func _finalized() -> Int {
    return Int(_truncatingBits: _state.pointee.finalize()._lowWord)
  }
}

// FIXME(hasher): This is purely for benchmarking; to be removed.
internal struct _LegacyHasher {
  internal var _hash: Int

  @inline(__always)
  internal init() {
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

internal typealias _Hasher = _LegacyHasher

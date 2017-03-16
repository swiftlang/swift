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

public // @testable
struct _Hashing {
  // FIXME(ABI)#41 : make this an actual public API.
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

public // @testable
struct _HashingDetail {

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

  @_versioned
  @_transparent
  static func getExecutionSeed() -> UInt64 {
    return _HashingDetail.fixedSeedOverride == 0 ?
              _swift_stdlib_HashingDetail_getRandomSeed() : fixedSeedOverride
  }

  @_versioned
  @_transparent
  static func hash16Bytes(_ low: UInt64, _ high: UInt64) -> UInt64 {
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

@_transparent
public // @testable
func _mixInt32(_ value: Int32) -> Int32 {
  return Int32(bitPattern: _mixUInt32(UInt32(bitPattern: value)))
}

@_transparent
public // @testable
func _mixUInt64(_ value: UInt64) -> UInt64 {
  // Similar to hash_4to8_bytes but using a seed instead of length.
  let seed: UInt64 = _HashingDetail.getExecutionSeed()
  let low: UInt64 = value & 0xffff_ffff
  let high: UInt64 = value >> 32
  return _HashingDetail.hash16Bytes(seed &+ (low << 3), high)
}

@_transparent
public // @testable
func _mixInt64(_ value: Int64) -> Int64 {
  return Int64(bitPattern: _mixUInt64(UInt64(bitPattern: value)))
}

@_transparent
public // @testable
func _mixUInt(_ value: UInt) -> UInt {
#if arch(i386) || arch(arm)
  return UInt(_mixUInt32(UInt32(value)))
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  return UInt(_mixUInt64(UInt64(value)))
#endif
}

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
public // @testable
func _squeezeHashValue(_ hashValue: Int, _ upperBound: Int) -> Int {
  _sanityCheck(_isPowerOf2(upperBound))
  let mixedHashValue = _mixInt(hashValue)

  // As `upperBound` is a power of two we can do a bitwise-and to calculate
  // mixedHashValue % upperBound.
  return mixedHashValue & (upperBound &- 1)
}


//===----------------------------------------------------------------------===//
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

  @transparent
  static func getExecutionSeed() -> UInt64 {
    // FIXME: This needs to be a per-execution seed. This is just a placeholder
    // implementation.
    let seed: UInt64 = 0xff51afd7ed558ccd
    return _HashingDetail.fixedSeedOverride == 0 ? seed : fixedSeedOverride
  }

  @transparent
  static func hash16Bytes(low: UInt64, _ high: UInt64) -> UInt64 {
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
// their inputs and just exhibit avalance effect.
//

@transparent
public // @testable
func _mixUInt32(value: UInt32) -> UInt32 {
  // Zero-extend to 64 bits, hash, select 32 bits from the hash.
  //
  // NOTE: this differs from LLVM's implementation, which selects the lower
  // 32 bits.  According to the statistical tests, the 3 lowest bits have
  // weaker avalanche properties.
  let extendedValue = UInt64(value)
  let extendedResult = _mixUInt64(extendedValue)
  return UInt32((extendedResult >> 3) & 0xffff_ffff)
}

@transparent
public // @testable
func _mixInt32(value: Int32) -> Int32 {
  return Int32(bitPattern: _mixUInt32(UInt32(bitPattern: value)))
}

@transparent
public // @testable
func _mixUInt64(value: UInt64) -> UInt64 {
  // Similar to hash_4to8_bytes but using a seed instead of length.
  let seed: UInt64 = _HashingDetail.getExecutionSeed()
  let low: UInt64 = value & 0xffff_ffff
  let high: UInt64 = value >> 32
  return _HashingDetail.hash16Bytes(seed &+ (low << 3), high)
}

@transparent
public // @testable
func _mixInt64(value: Int64) -> Int64 {
  return Int64(bitPattern: _mixUInt64(UInt64(bitPattern: value)))
}

@transparent
public // @testable
func _mixUInt(value: UInt) -> UInt {
#if arch(i386) || arch(arm)
  return UInt(_mixUInt32(UInt32(value)))
#elseif arch(x86_64) || arch(arm64)
  return UInt(_mixUInt64(UInt64(value)))
#endif
}

@transparent
public // @testable
func _mixInt(value: Int) -> Int {
#if arch(i386) || arch(arm)
  return Int(_mixInt32(Int32(value)))
#elseif arch(x86_64) || arch(arm64)
  return Int(_mixInt64(Int64(value)))
#endif
}

/// Given a hash value, returns an integer value within the given range that
/// corresponds to a hash value.
///
/// This function is superior to computing the remainder of `hashValue` by
/// the range length.  Some types have bad hash functions; sometimes simple
/// patterns in data sets create patterns in hash values and applying the
/// remainder operation just throws away even more information and invites
/// even more hash collisions.  This effect is especially bad if the length
/// of the required range is a power of two -- applying the remainder
/// operation just throws away high bits of the hash (which would not be
/// a problem if the hash was known to be good).  This function mixes the
/// bits in the hash value to compensate for such cases.
///
/// Of course, this function is a compressing function, and applying it to a
/// hash value does not change anything fundamentally: collisions are still
/// possible, and it does not prevent malicious users from constructing data
/// sets that will exhibit pathological collisions.
public // @testable
func _squeezeHashValue(hashValue: Int, _ resultRange: Range<Int>) -> Int {
  // Length of a Range<Int> does not fit into an Int, but fits into an UInt.
  // An efficient way to compute the length is to rely on two's complement
  // arithmetic.
  let resultCardinality =
    UInt(bitPattern: resultRange.endIndex &- resultRange.startIndex)

  // Calculate the result as `UInt` to handle the case when
  // `resultCardinality >= Int.max`.
  let unsignedResult =
    _squeezeHashValue(hashValue, UInt(0)..<resultCardinality)

  // We perform the unchecked arithmetic on `UInt` (instead of doing
  // straightforward computations on `Int`) in order to handle the following
  // tricky case: `startIndex` is negative, and `resultCardinality >= Int.max`.
  // We can not convert the latter to `Int`.
  return
    Int(bitPattern:
      UInt(bitPattern: resultRange.startIndex) &+ unsignedResult)
}

public // @testable
func _squeezeHashValue(hashValue: Int, _ resultRange: Range<UInt>) -> UInt {
  let mixedHashValue = UInt(bitPattern: _mixInt(hashValue))
  let resultCardinality: UInt = resultRange.endIndex - resultRange.startIndex
  if _isPowerOf2(resultCardinality) {
    return mixedHashValue & (resultCardinality - 1)
  }
  return resultRange.startIndex + (mixedHashValue % resultCardinality)
}


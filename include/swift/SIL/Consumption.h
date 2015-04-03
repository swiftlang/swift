//===--- Consumption.h - Value consumption for SIL --------------*- C++ -*-===//
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
// This file defines the CastConsumptionKind enum, which describes
// under what circumstances an operation consumes a value.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_CONSUMPTION_H
#define SWIFT_SIL_CONSUMPTION_H

namespace swift {

/// Is an operation a "take"?  A take consumes the original value,
/// leaving it uninitialized.
enum IsTake_t : bool { IsNotTake, IsTake };

/// Is an operation an "initialization"?  An initialization simply
/// fills in an uninitialized address with a value; a
/// non-initialization also consumes the value already there.
enum IsInitialization_t : bool { IsNotInitialization, IsInitialization };

/// The behavior of a dynamic cast operation on the source value.
enum class CastConsumptionKind : unsigned char {
  /// The source value is always taken, regardless of whether the cast
  /// succeeds.  That is, if the cast fails, the source value is
  /// destroyed.
  TakeAlways,

  /// The source value is taken only on a successful cast; otherwise,
  /// it is left in place.
  TakeOnSuccess,

  /// The source value is always left in place, and the destination
  /// value is copied into on success.
  CopyOnSuccess,
};

/// Should the source value be destroyed if the cast fails?
inline bool shouldDestroyOnFailure(CastConsumptionKind kind) {
  return (kind == CastConsumptionKind::TakeAlways);
}

/// Should the source value be taken if the cast succeeds?
inline IsTake_t shouldTakeOnSuccess(CastConsumptionKind kind) {
  return IsTake_t(kind != CastConsumptionKind::CopyOnSuccess);
}

}  // end namespace swift

#endif

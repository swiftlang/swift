//===--- Consumption.h - Value consumption for SIL --------------*- C++ -*-===//
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
enum class CastConsumptionKind : uint8_t {
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

  /// The source value is never taken, regardless of whether the cast
  /// succeeds. Instead, we always borrow the source value and feed it through.
  ///
  /// NOTE: This can only be used with objects. We do not support borrowing of
  /// addresses. If an address is needed for a cast operation, a BorrowAlways
  /// value must be copied into a temporary and operated upon. If the result of
  /// the cast is a loadable type then the value is loaded using a
  /// load_borrow. If an address only value is returned, we continue processing
  /// the value as an owned TakeAlways value.
  BorrowAlways,
};

/// Should the source value be destroyed if the cast fails?
inline bool shouldDestroyOnFailure(CastConsumptionKind kind) {
  switch (kind) {
  case CastConsumptionKind::TakeAlways:
    return true;
  case CastConsumptionKind::TakeOnSuccess:
  case CastConsumptionKind::CopyOnSuccess:
  case CastConsumptionKind::BorrowAlways:
    return false;
  }
}

/// Should the source value be taken if the cast succeeds?
inline IsTake_t shouldTakeOnSuccess(CastConsumptionKind kind) {
  switch (kind) {
  case CastConsumptionKind::TakeAlways:
  case CastConsumptionKind::TakeOnSuccess:
    return IsTake;
  case CastConsumptionKind::CopyOnSuccess:
  case CastConsumptionKind::BorrowAlways:
    return IsNotTake;
  }
}

} // end namespace swift

#endif

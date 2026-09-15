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

#include "llvm/Support/ErrorHandling.h"
#include <cstdint>

namespace swift {

/// Is an operation a "take"?  A take consumes the original value,
/// leaving it uninitialized.
enum IsTake_t : bool { IsNotTake, IsTake };

/// Is an operation an "initialization"?  An initialization simply
/// fills in an uninitialized address with a value; a
/// non-initialization also consumes the value already there.
enum IsInitialization_t : bool { IsNotInitialization, IsInitialization };

/// The behavior of a dynamic cast operation on its operands.
///
/// Most cases describe only what happens to the *source* value; a cast normally
/// initializes its destination on success, so there was nothing to say about it.
/// `TestOnly` is the exception: it describes a destination that is not written
/// at all. See the note on that case.
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

  /// The cast only reports whether it would have succeeded. The source value is
  /// never taken and never copied, and *no destination value is produced* --
  /// the destination operand must be `SILUndef`, which the verifier enforces.
  ///
  /// This is what lets `is` and `case is T` apply to a value that cannot be
  /// copied: producing the result at all would be a copy the type forbids, and
  /// taking it would destroy the very thing being asked about.
  ///
  /// NOTE: Unlike the cases above, this constrains the *destination* as well as
  /// the source. Anything reading the destination of a cast must skip it here;
  /// with `SILUndef` in place, forgetting to do so shows up as undef in the IR
  /// rather than as a value that was silently never written.
  TestOnly,
};

/// Should the source value be destroyed if the cast fails?
inline bool shouldDestroyOnFailure(CastConsumptionKind kind) {
  switch (kind) {
  case CastConsumptionKind::TakeAlways:
    return true;
  case CastConsumptionKind::TakeOnSuccess:
  case CastConsumptionKind::CopyOnSuccess:
  case CastConsumptionKind::BorrowAlways:
  case CastConsumptionKind::TestOnly:
    return false;
  }
  llvm_unreachable("covered switch");
}

/// Should the source value be taken if the cast succeeds?
inline IsTake_t shouldTakeOnSuccess(CastConsumptionKind kind) {
  switch (kind) {
  case CastConsumptionKind::TakeAlways:
  case CastConsumptionKind::TakeOnSuccess:
    return IsTake;
  case CastConsumptionKind::CopyOnSuccess:
  case CastConsumptionKind::BorrowAlways:
  case CastConsumptionKind::TestOnly:
    return IsNotTake;
  }
  llvm_unreachable("covered switch");
}

/// Does this cast produce a value in its destination operand?
///
/// When false, the destination operand is `SILUndef` and must not be read,
/// written, or tracked as initialized.
inline bool producesDestinationValue(CastConsumptionKind kind) {
  switch (kind) {
  case CastConsumptionKind::TakeAlways:
  case CastConsumptionKind::TakeOnSuccess:
  case CastConsumptionKind::CopyOnSuccess:
  case CastConsumptionKind::BorrowAlways:
    return true;
  case CastConsumptionKind::TestOnly:
    return false;
  }
  llvm_unreachable("covered switch");
}

} // end namespace swift

#endif

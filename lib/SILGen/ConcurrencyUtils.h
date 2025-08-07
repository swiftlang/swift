//===--- ConcurrencyUtils.h -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILGEN_CONCURRENCYUTILS_H
#define SWIFT_SILGEN_CONCURRENCYUTILS_H

#include "RValue.h"
#include "SILGenFunction.h"

#include "swift/SIL/ConcurrencyUtils.h"

namespace swift {

class SILLocation;
class Expr;

namespace Lowering {

class SILGenFunction;
class RValue;
class ManagedValue;

inline ManagedValue clearImplicitActorBits(SILGenFunction &SGF, SILLocation loc,
                                           ManagedValue implicitIsolatedActor,
                                           SILType type = {}) {
  return ManagedValue::forBorrowedRValue(clearImplicitActorBits(
      SGF.B, loc, implicitIsolatedActor.getUnmanagedValue(), type));
}

/// Clear the TBI bits if AArch64HasTBI is set. Otherwise clear the low tagged
/// bits.
///
/// \param expr - the expression which yielded this r-value; its type
///   will become the substituted formal type of this r-value
/// \param implicitIsolatedActor should be an Optional<any Actor>.
inline RValue clearImplicitActorBits(SILGenFunction &SGF, Expr *expr,
                                     ManagedValue implicitIsolatedActor,
                                     SILType type = {}) {
  return RValue(SGF, expr,
                clearImplicitActorBits(SGF, SILLocation(expr),
                                       implicitIsolatedActor, type));
}

inline ManagedValue setImplicitActorBits(SILGenFunction &SGF, SILLocation loc,
                                         ManagedValue implicitIsolatedActor) {
  return ManagedValue::forBorrowedRValue(setImplicitActorBits(
      SGF.B, loc, implicitIsolatedActor.getUnmanagedValue()));
}

inline RValue setImplicitActorBits(SILGenFunction &SGF, Expr *expr,
                                   ManagedValue implicitIsolatedActor) {
  return RValue(
      SGF, expr,
      setImplicitActorBits(SGF, SILLocation(expr), implicitIsolatedActor));
}

} // namespace Lowering

} // namespace swift

#endif

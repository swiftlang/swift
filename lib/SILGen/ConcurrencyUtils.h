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

namespace swift {

class SILLocation;
class Expr;

namespace Lowering {

class SILGenFunction;
class RValue;
class ManagedValue;

/// Clear the TBI bits if AArch64HasTBI is set. Otherwise clear the low tagged
/// bits.
///
/// \param expr - the expression which yielded this r-value; its type
///   will become the substituted formal type of this r-value
/// \param implicitIsolatedActor should be an Optional<any Actor>.
RValue clearImplicitIsolatedActorBits(SILGenFunction &SGF, Expr *expr,
                                      ManagedValue implicitIsolatedActor);

ManagedValue clearImplicitIsolatedActorBits(SILGenFunction &SGF,
                                            SILLocation loc,
                                            ManagedValue implicitIsolatedActor);

RValue setImplicitIsolatedActorBits(SILGenFunction &SGF, Expr *expr,
                                    ManagedValue implicitIsolatedActor);

ManagedValue setImplicitIsolatedActorBits(SILGenFunction &SGF, SILLocation loc,
                                          ManagedValue implicitIsolatedActor);

} // namespace Lowering

} // namespace swift

#endif

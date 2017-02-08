//===--- ValueTracking.h - SIL Value Tracking Analysis ----------*- C++ -*-===//
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
// This file contains routines which analyze chains of computations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_VALUETRACKING_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_VALUETRACKING_H

#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"

namespace swift {

/// Returns true if \p V is a function argument which may not alias to
/// any other pointer in the function.
/// The \p assumeInoutIsNotAliasing specifies in no-aliasing is assumed for
/// the @inout convention. See swift::isNotAliasedIndirectParameter().
bool isNotAliasingArgument(SILValue V, InoutAliasingAssumption isInoutAliasing =
                                         InoutAliasingAssumption::Aliasing);

/// Returns true if \p V is local inside its function. This means its underlying
/// object either is a non-aliasing function argument or a locally allocated
/// object.
/// The \p assumeInoutIsNotAliasing specifies in no-aliasing is assumed for
/// the @inout convention. See swift::isNotAliasedIndirectParameter().
bool pointsToLocalObject(SILValue V, InoutAliasingAssumption isInoutAliasing =
                                         InoutAliasingAssumption::Aliasing);

enum class IsZeroKind {
  Zero,
  NotZero,
  Unknown
};

/// Check if the value \p Value is known to be zero, non-zero or unknown.
IsZeroKind isZeroValue(SILValue Value);

/// Checks if a sign bit of a value is known to be set, not set or unknown.
/// Essentially, it is a simple form of a range analysis.
/// This approach is inspired by the corresponding implementation of
/// ComputeSignBit in LLVM's value tracking implementation.
/// It is planned to extend this approach to track all bits of a value.
/// Therefore it can be considered to be the beginning of a range analysis
/// infrastructure for the Swift compiler.
Optional<bool> computeSignBit(SILValue Value);

/// Check if execution of a given builtin instruction can result in overflows.
/// Returns true of an overflow can happen. Otherwise returns false.
bool canOverflow(BuiltinInst *BI);

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_ANALYSIS_VALUETRACKING_H

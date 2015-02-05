//===-- ValueTracking.h - SIL Value Tracking Analysis ----------*- C++ -*--===//
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

#ifndef SWIFT_SILANALYSIS_VALUETRACKING_H
#define SWIFT_SILANALYSIS_VALUETRACKING_H

#include "swift/SIL/SILInstruction.h"

namespace swift {

class SILValue;

/// Strip off casts/indexing insts/address projections from V until there is
/// nothing left to strip.
SILValue getUnderlyingObject(SILValue V);

/// Return true if the pointer is to a function-local object that never escapes
/// from the function.
bool isNonEscapingLocalObject(SILValue V);

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

/// Return true if I is a thin_to_thick_function or partial_apply that is local
/// to the given function, does not escape, and has only ref count
/// users. Returns false otherwise. The relevant ref count users are returned in
/// RC on success.
///
/// TODO: Refactor SILCombine dead closure removal to use this code.
bool isLocalDeadClosure(SILInstruction *I,
                        llvm::SmallVectorImpl<SILInstruction *> &RC);

} // end namespace swift

#endif // SWIFT_SILANALYSIS_VALUETRACKING_H

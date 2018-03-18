//===--- SimplifyInstruction.h - Fold instructions --------------*- C++ -*-===//
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
// An analysis that provides utilities for folding instructions. Since it is an
// analysis it does not modify the IR in anyway. This is left to actual
// SIL Transforms.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_SIMPLIFYINSTRUCTION_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_SIMPLIFYINSTRUCTION_H

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"

namespace swift {

class SILInstruction;

/// \brief Try to simplify the specified instruction, performing local
/// analysis of the operands of the instruction, without looking at its uses
/// (e.g. constant folding).  If a simpler result can be found, it is
/// returned, otherwise a null SILValue is returned.
SILValue simplifyInstruction(SILInstruction *I);

/// Replace an instruction with a simplified result and erase it. If the
/// instruction initiates a scope, do not replace the end of its scope; it will
/// be deleted along with its parent.
///
/// If it is nonnull, eraseNotify will be called before each instruction is
/// deleted.
void replaceAllSimplifiedUsesAndErase(
    SILInstruction *I, SILValue result,
    std::function<void(SILInstruction *)> eraseNotify = nullptr);

/// Simplify invocations of builtin operations that may overflow.
/// All such operations return a tuple (result, overflow_flag).
/// This function try to simplify such operations, but returns only a
/// simplified first element of a tuple. The overflow flag is not returned
/// explicitly, because this simplification is only possible if there is
/// no overflow. Therefore the overflow flag is known to have a value of 0 if
/// simplification was successful.
/// In case when a simplification is not possible, a null SILValue is returned.
SILValue simplifyOverflowBuiltinInstruction(BuiltinInst *BI);

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_ANALYSIS_SIMPLIFYINSTRUCTION_H

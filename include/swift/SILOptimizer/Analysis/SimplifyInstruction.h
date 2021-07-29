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
struct InstModCallbacks;

/// Replace an instruction with a simplified result and erase it. If the
/// instruction initiates a scope, do not replace the end of its scope; it will
/// be deleted along with its parent.
///
/// NOTE: When OSSA is enabled this API assumes OSSA is properly formed and will
/// insert compensating instructions.
SILBasicBlock::iterator
replaceAllSimplifiedUsesAndErase(SILInstruction *I, SILValue result,
                                 InstModCallbacks &callbacks,
                                 DeadEndBlocks *deadEndBlocks = nullptr);

/// Attempt to map \p inst to a simplified result. Upon success, replace \p inst
/// with this simplified result and erase \p inst. If the instruction initiates
/// a scope, do not replace the end of its scope; it will be deleted along with
/// its parent.
///
/// NOTE: When OSSA is enabled this API assumes OSSA is properly formed and will
/// insert compensating instructions.
/// NOTE: When \p I is in an OSSA function, this fails to optimize if \p
/// deadEndBlocks is null.
SILBasicBlock::iterator simplifyAndReplaceAllSimplifiedUsesAndErase(
    SILInstruction *I, InstModCallbacks &callbacks,
    DeadEndBlocks *deadEndBlocks = nullptr);

// Simplify invocations of builtin operations that may overflow.
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

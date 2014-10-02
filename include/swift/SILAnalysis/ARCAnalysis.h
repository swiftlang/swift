//===--------------- ARCAnalysis.h - SIL ARC Analysis ----*- C++ -*--------===//
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

#ifndef SWIFT_SILANALYSIS_ARCANALYSIS_H
#define SWIFT_SILANALYSIS_ARCANALYSIS_H

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILBasicBlock.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SetVector.h"

namespace swift {

class SILValue;
class SILInstruction;
class AliasAnalysis;
class PostOrderAnalysis;
class RCIdentityAnalysis;
class SILFunction;

} // end namespace swift

namespace swift {
namespace arc {

/// \returns True if the user \p User decrement the ref count of pointer \p Ptr.
bool canDecrementRefCount(SILInstruction *User, SILValue Ptr,
                          AliasAnalysis *AA);

/// \returns True if the user \p User checks the ref count of a pointer.
bool canCheckRefCount(SILInstruction *User);

/// \returns True if the user \p User can use the pointer \p Ptr in a manner
/// that requires \p Ptr to be alive before Inst.
bool canUseValue(SILInstruction *User, SILValue Ptr, AliasAnalysis *AA);

/// If \p Op has arc uses in the instruction range [Start, End), return the
/// first such instruction. Otherwise return None. We assume that
/// Start and End are both in the same basic block.
Optional<SILBasicBlock::iterator>
valueHasARCUsesInInstructionRange(SILValue Op,
                                  SILBasicBlock::iterator Start,
                                  SILBasicBlock::iterator End,
                                  AliasAnalysis *AA);

/// If \p Op has instructions in the instruction range (Start, End] which may
/// decrement it, return the first such instruction. Returns None
/// if no such instruction exists. We assume that Start and End are both in the
/// same basic block.
Optional<SILBasicBlock::iterator>
valueHasARCDecrementOrCheckInInstructionRange(SILValue Op,
                                              SILBasicBlock::iterator Start,
                                              SILBasicBlock::iterator End,
                                              AliasAnalysis *AA);

/// Match a call to a trap BB with no ARC relevant side effects.
bool isARCInertTrapBB(SILBasicBlock *BB);

/// A set of matching reference count increments, decrements, increment
/// insertion pts, and decrement insertion pts.
struct ARCMatchingSet {
  SILValue Ptr;
  llvm::SetVector<SILInstruction *> Increments;
  llvm::SetVector<SILInstruction *> IncrementInsertPts;
  llvm::SetVector<SILInstruction *> Decrements;
  llvm::SetVector<SILInstruction *> DecrementInsertPts;

  void clear() {
    Ptr = SILValue();
    Increments.clear();
    IncrementInsertPts.clear();
    Decrements.clear();
    DecrementInsertPts.clear();
  }
};

/// An opaque context that contains cached information that can be used on
/// multiple calls to computeARCMatchingSet on the same function.
struct ARCMatchingSetComputationContext;

/// Create an opaque arc mutation set computation context for SILFunction F
/// using AliasAnalysis AA.
ARCMatchingSetComputationContext *
createARCMatchingSetComputationContext(SILFunction &F, AliasAnalysis *AA,
                                       PostOrderAnalysis *POTA,
                                       RCIdentityAnalysis *RCIA);

/// Destroy the context.
void
destroyARCMatchingSetComputationContext(ARCMatchingSetComputationContext *Ctx);

/// Use the opaque context to recompute the matching set for the input function.
bool computeARCMatchingSet(ARCMatchingSetComputationContext *Ctx,
                           std::function<void (ARCMatchingSet&)> Fun);

} // end namespace arc
} // end namespace swift

#endif

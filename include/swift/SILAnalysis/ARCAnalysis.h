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
#include "llvm/ADT/MapVector.h"
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

/// \returns True if the user \p User decrement the ref count of pointer \p Ptr.
bool mayDecrementRefCount(SILInstruction *User, SILValue Ptr,
                          AliasAnalysis *AA);

/// \returns True if the user \p User checks the ref count of a pointer.
bool mayCheckRefCount(SILInstruction *User);

/// \returns True if the user \p User can use the pointer \p Ptr in a manner
/// that requires \p Ptr to be alive before Inst.
bool mayUseValue(SILInstruction *User, SILValue Ptr, AliasAnalysis *AA);

/// \returns True if \p User can never use a value in a way that requires the
/// value to be alive.
///
/// This is purposefully a negative query to contrast with canUseValue which is
/// about a specific value while this is about general values.
bool canNeverUseValues(SILInstruction *User);

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
///
/// \param Ctx The opaque context for the computation.
/// \param FreezeOwningPtrEpiloqueReleases Should we not attempt to move, remove
/// epilogue release pointers and instead use them as post dominating releases
/// for other pointers.
/// \param Fun The function to call with the ARC matching
bool computeARCMatchingSet(ARCMatchingSetComputationContext *Ctx,
                           bool FreezeOwningPtrEpiloqueReleases,
                           std::function<void (ARCMatchingSet&)> Fun);

/// A class that attempts to match owned arguments and corresponding epilogue
/// releases for a specific function.
///
/// TODO: This really needs a better name.
class ConsumedArgToEpilogueReleaseMatcher {
  llvm::SmallMapVector<SILArgument *, SILInstruction *, 8> ArgInstMap;

public:
  ConsumedArgToEpilogueReleaseMatcher(RCIdentityAnalysis *RCIA,
                                      SILFunction *F);

  bool argumentHasRelease(SILArgument *Arg) const {
    return ArgInstMap.find(Arg) != ArgInstMap.end();
  }

  bool argumentHasRelease(SILValue V) const {
    auto *Arg = dyn_cast<SILArgument>(V);
    if (!Arg)
      return false;
    return argumentHasRelease(Arg);
  }

  SILInstruction *releaseForArgument(SILArgument *Arg) const {
    auto I = ArgInstMap.find(Arg);
    if (I == ArgInstMap.end())
      return nullptr;
    return I->second;
  }

  bool isReleaseMatchedToArgument(SILInstruction *Inst) const {
    auto Pred = [&Inst](const std::pair<SILArgument *,
                                        SILInstruction *> &P) -> bool {
      return P.second == Inst;
    };
    return std::count_if(ArgInstMap.begin(), ArgInstMap.end(), Pred);
  }

  using iterator = decltype(ArgInstMap)::iterator;
  using const_iterator = decltype(ArgInstMap)::const_iterator;
  iterator begin() { return ArgInstMap.begin(); }
  iterator end() { return ArgInstMap.end(); }
  const_iterator begin() const { return ArgInstMap.begin(); }
  const_iterator end() const { return ArgInstMap.end(); }

  using reverse_iterator = decltype(ArgInstMap)::reverse_iterator;
  using const_reverse_iterator = decltype(ArgInstMap)::const_reverse_iterator;
  reverse_iterator rbegin() { return ArgInstMap.rbegin(); }
  reverse_iterator rend() { return ArgInstMap.rend(); }
  const_reverse_iterator rbegin() const { return ArgInstMap.rbegin(); }
  const_reverse_iterator rend() const { return ArgInstMap.rend(); }

  unsigned size() const { return ArgInstMap.size(); }

  Range<iterator> getRange() { return swift::make_range(begin(), end()); }
};

} // end namespace swift

#endif

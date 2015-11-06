//===--- GlobalARCPairingAnalysis.h ---------------------------------------===//
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

#ifndef SWIFT_SILPASSES_GLOBALARCPAIRINGANALYSIS_H
#define SWIFT_SILPASSES_GLOBALARCPAIRINGANALYSIS_H

#include "swift/SIL/SILValue.h"
#include "llvm/ADT/SetVector.h"

namespace swift {

class SILInstruction;
class SILFunction;
class AliasAnalysis;
class PostOrderAnalysis;
class LoopRegionFunctionInfo;
class SILLoopInfo;
class RCIdentityFunctionInfo;

/// A set of matching reference count increments, decrements, increment
/// insertion pts, and decrement insertion pts.
struct ARCMatchingSet {
  SILValue Ptr;
  llvm::SetVector<SILInstruction *> Increments;
  llvm::SetVector<SILInstruction *> IncrementInsertPts;
  llvm::SetVector<SILInstruction *> Decrements;
  llvm::SetVector<SILInstruction *> DecrementInsertPts;

  // This is a data structure that can not be moved.
  ARCMatchingSet() = default;
  ARCMatchingSet(const ARCMatchingSet &) = delete;
  ARCMatchingSet(ARCMatchingSet &&) = delete;
  ARCMatchingSet &operator=(const ARCMatchingSet &) = delete;
  ARCMatchingSet &operator=(ARCMatchingSet &&) = delete;

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
ARCMatchingSetComputationContext *createARCMatchingSetComputationContext(
    SILFunction &F, AliasAnalysis *AA, PostOrderAnalysis *POTA,
    LoopRegionFunctionInfo *LRFI, SILLoopInfo *SLI,
    RCIdentityFunctionInfo *RCIA, bool EnableLoopARC = false);

/// Destroy the context.
void
destroyARCMatchingSetComputationContext(ARCMatchingSetComputationContext *Ctx);

/// A structure that via its virtual calls recieves the results of the analysis.
///
/// TODO: We could potentially pass in all of the matching sets as a list and
/// use less virtual calls at the cost of potentially greater numbers of moves.
struct ARCMatchingSetCallback {
  virtual ~ARCMatchingSetCallback() = default;

  /// This call should process \p Set and modify any internal state of
  /// ARCMatchingSetCallback given \p Set. This call should not remove any
  /// instructions since any removed instruction might be used as an insertion
  /// point for another retain, release pair.
  virtual void processMatchingSet(ARCMatchingSet &Set) {}

  /// This call should perform any deletion of instructions necessary. It is run
  /// strictly after all computation has been completed.
  virtual void finalize() {}
};

/// Use the opaque context to recompute the matching set for the input function.
///
/// \param Ctx The opaque context for the computation.
/// \param FreezeOwningPtrEpiloqueReleases Should we not attempt to move, remove
/// epilogue release pointers and instead use them as post dominating releases
/// for other pointers.
/// \param Callback The callback used to propagate information to analysis
/// users.
bool computeARCMatchingSet(ARCMatchingSetComputationContext *Ctx,
                           bool FreezeOwningPtrEpiloqueReleases,
                           ARCMatchingSetCallback &Callback);

} // end swift namespace

#endif

//===--- GlobalARCPairingAnalysis.h ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_GLOBALARCPAIRINGANALYSIS_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_GLOBALARCPAIRINGANALYSIS_H

#include "GlobalARCSequenceDataflow.h"
#include "GlobalLoopARCSequenceDataflow.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
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

  /// The pointer that this ARCMatchingSet is providing matching increment and
  /// decrement sets for.
  ///
  /// TODO: This should really be called RCIdentity.
  SILValue Ptr;

  /// The set of reference count increments that were paired.
  llvm::SetVector<SILInstruction *> Increments;

  /// An insertion point for an increment means the earliest point in the
  /// program after the increment has occurred that the increment can be moved to
  /// without moving the increment over an instruction that may decrement a
  /// reference count.
  llvm::SetVector<SILInstruction *> IncrementInsertPts;

  /// The set of reference count decrements that were paired.
  llvm::SetVector<SILInstruction *> Decrements;

  /// An insertion point for a decrement means the latest point in the program
  /// before the decrement that the optimizer conservatively assumes that a
  /// reference counted value could be used.
  llvm::SetVector<SILInstruction *> DecrementInsertPts;

  // This is a data structure that cannot be moved or copied.
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

class CodeMotionOrDeleteCallback {
  bool Changed = false;
  llvm::SmallVector<SILInstruction *, 16> InstructionsToDelete;

public:
  /// This call should process \p Set and modify any internal state of
  /// ARCMatchingSetCallback given \p Set. This call should not remove any
  /// instructions since any removed instruction might be used as an insertion
  /// point for another retain, release pair.
  void processMatchingSet(ARCMatchingSet &Set);

  // Delete instructions after we have processed all matching sets so that we do
  // not remove instructions that may be insertion points for other retain,
  // releases.
  void finalize() {
    while (!InstructionsToDelete.empty()) {
      InstructionsToDelete.pop_back_val()->eraseFromParent();
    }
  }

  bool madeChange() const { return Changed; }
};

/// A wrapper around the results of the bottom-up/top-down dataflow that knows how
/// to pair the retains/releases in those results.
struct ARCPairingContext {
  SILFunction &F;
  BlotMapVector<SILInstruction *, TopDownRefCountState> DecToIncStateMap;
  BlotMapVector<SILInstruction *, BottomUpRefCountState> IncToDecStateMap;
  RCIdentityFunctionInfo *RCIA;

  ARCPairingContext(SILFunction &F, RCIdentityFunctionInfo *RCIA)
      : F(F), DecToIncStateMap(), IncToDecStateMap(), RCIA(RCIA) {}
  bool performMatching(CodeMotionOrDeleteCallback &Callback);
};

/// A composition of an ARCSequenceDataflowEvaluator and an
/// ARCPairingContext. The evaluator performs top down/bottom up dataflows
/// clearing the dataflow at loop boundaries. Then the results of the evaluator
/// are placed into the ARCPairingContext and then the ARCPairingContext is used
/// to pair retains/releases.
struct BlockARCPairingContext {
  ARCPairingContext Context;
  ARCSequenceDataflowEvaluator Evaluator;

  BlockARCPairingContext(SILFunction &F, AliasAnalysis *AA,
                         PostOrderAnalysis *POTA, RCIdentityFunctionInfo *RCFI,
                         ProgramTerminationFunctionInfo *PTFI)
      : Context(F, RCFI),
        Evaluator(F, AA, POTA, RCFI, PTFI, Context.DecToIncStateMap,
                  Context.IncToDecStateMap) {}

  bool run(bool FreezePostDomReleases, CodeMotionOrDeleteCallback &Callback) {
    bool NestingDetected = Evaluator.run(FreezePostDomReleases);
    Evaluator.clear();

    bool MatchedPair = Context.performMatching(Callback);
    return NestingDetected && MatchedPair;
  }
};

/// A composition of a LoopARCSequenceDataflowEvaluator and an
/// ARCPairingContext. The loop nest is processed bottom up. For each loop, we
/// run the evaluator on the loop and then use the ARCPairingContext to pair
/// retains/releases and eliminate them.
struct LoopARCPairingContext : SILLoopVisitor {
  ARCPairingContext Context;
  LoopARCSequenceDataflowEvaluator Evaluator;
  LoopRegionFunctionInfo *LRFI;
  SILLoopInfo *SLI;
  CodeMotionOrDeleteCallback Callback;

  LoopARCPairingContext(SILFunction &F, AliasAnalysis *AA,
                        LoopRegionFunctionInfo *LRFI, SILLoopInfo *SLI,
                        RCIdentityFunctionInfo *RCFI,
                        ProgramTerminationFunctionInfo *PTFI)
      : SILLoopVisitor(&F, SLI), Context(F, RCFI),
        Evaluator(F, AA, LRFI, SLI, RCFI, PTFI, Context.DecToIncStateMap,
                  Context.IncToDecStateMap),
        LRFI(LRFI), SLI(SLI), Callback() {}

  bool process() {
    run();
    if (!Callback.madeChange())
      return false;
    run();
    return true;
  }

  bool madeChange() const { return Callback.madeChange(); }

  void runOnLoop(SILLoop *L) override;
  void runOnFunction(SILFunction *F) override;

  bool processRegion(const LoopRegion *R, bool FreezePostDomReleases,
                     bool RecomputePostDomReleases);
};

} // end swift namespace

#endif

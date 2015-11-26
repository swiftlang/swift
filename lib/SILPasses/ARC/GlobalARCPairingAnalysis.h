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

#include "GlobalARCSequenceDataflow.h"
#include "GlobalLoopARCSequenceDataflow.h"
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

struct ARCPairingContext {
  SILFunction &F;
  BlotMapVector<SILInstruction *, TopDownRefCountState> DecToIncStateMap;
  BlotMapVector<SILInstruction *, BottomUpRefCountState> IncToDecStateMap;
  RCIdentityFunctionInfo *RCIA;

  ARCPairingContext(SILFunction &F, RCIdentityFunctionInfo *RCIA)
      : F(F), DecToIncStateMap(), IncToDecStateMap(), RCIA(RCIA) {}
  bool performMatching(ARCMatchingSetCallback &Callback);
};

struct BlockARCPairingContext {
  ARCPairingContext Context;
  ARCSequenceDataflowEvaluator Evaluator;

  BlockARCPairingContext(SILFunction &F, AliasAnalysis *AA,
                         PostOrderAnalysis *POTA, RCIdentityFunctionInfo *RCFI)
      : Context(F, RCFI), Evaluator(F, AA, POTA, RCFI, Context.DecToIncStateMap,
                                    Context.IncToDecStateMap) {}

  bool run(bool FreezePostDomReleases, ARCMatchingSetCallback &Callback) {
    bool NestingDetected = Evaluator.run(FreezePostDomReleases);
    Evaluator.clear();

    bool MatchedPair = Context.performMatching(Callback);
    return NestingDetected && MatchedPair;
  }
};

struct LoopARCPairingContext {
  ARCPairingContext Context;
  LoopARCSequenceDataflowEvaluator Evaluator;
  LoopRegionFunctionInfo *LRFI;
  SILLoopInfo *SLI;

  LoopARCPairingContext(SILFunction &F, AliasAnalysis *AA,
                        LoopRegionFunctionInfo *LRFI, SILLoopInfo *SLI,
                        RCIdentityFunctionInfo *RCFI)
      : Context(F, RCFI),
        Evaluator(F, AA, LRFI, SLI, RCFI, Context.DecToIncStateMap,
                  Context.IncToDecStateMap),
        LRFI(LRFI), SLI(SLI) {}

  bool run(bool FreezePostDomReleases, ARCMatchingSetCallback &Callback);
  void processLoop(const LoopRegion *R, bool FreezePostDomReleases,
                   ARCMatchingSetCallback &Callback);
};

} // end swift namespace

#endif

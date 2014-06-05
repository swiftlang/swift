//===-------------- SILLoopInfo.h - SIL Loop Analysis -*- C++ -*--------===//
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

#ifndef SWIFT_SILANALYSIS_LOOPINFOANALYSIS_H
#define SWIFT_SILANALYSIS_LOOPINFOANALYSIS_H

#include "swift/SIL/CFG.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SILAnalysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Analysis/LoopInfo.h"

namespace swift {
  class DominanceInfo;
  class SILLoop;
  class SILPassManager;
}

// Implementation in LoopInfoImpl.h
#ifdef __GNUC__
__extension__ extern template class llvm::LoopBase<swift::SILBasicBlock, swift::SILLoop>;
__extension__ extern template class llvm::LoopInfoBase<swift::SILBasicBlock, swift::SILLoop>;
#endif

namespace swift {

class SILLoop;

/// Information about a single natural loop.
class SILLoop : public llvm::LoopBase<SILBasicBlock, SILLoop> {
public:
  SILLoop() {}
  void dump() const;
private:
  friend class llvm::LoopInfoBase<SILBasicBlock, SILLoop>;

  explicit SILLoop(SILBasicBlock *BB) : llvm::LoopBase<SILBasicBlock, SILLoop>(BB) {}
};

/// Information about loops in a function.
class SILLoopInfo {
  friend class llvm::LoopBase<SILBasicBlock, SILLoop>;
  using SILLoopInfoBase = llvm::LoopInfoBase<SILBasicBlock, SILLoop>;

  SILLoopInfoBase LI;

  void operator=(const SILLoopInfo &) = delete;
  SILLoopInfo(const SILLoopInfo &) = delete;

public:
  SILLoopInfo(SILFunction *F, DominanceInfo *DT);

  /// iterator/begin/end - The interface to the top-level loops in the current
  /// function.
  ///
  using iterator = SILLoopInfoBase::iterator;
  iterator begin() const { return LI.begin(); }
  iterator end() const { return LI.end(); }
  bool empty() const { return LI.empty(); }

  /// getLoopFor - Return the inner most loop that BB lives in.  If a basic
  /// block is in no loop (for example the entry node), null is returned.
  ///
  SILLoop *getLoopFor(const SILBasicBlock *BB) const {
    return LI.getLoopFor(BB);
  }

  /// operator[] - same as getLoopFor...
  ///
  const SILLoop *operator[](const SILBasicBlock *BB) const {
    return LI.getLoopFor(BB);
  }

  /// getLoopDepth - Return the loop nesting level of the specified block...
  ///
  unsigned getLoopDepth(const SILBasicBlock *BB) const {
    return LI.getLoopDepth(BB);
  }

  // isLoopHeader - True if the block is a loop header node
  bool isLoopHeader(SILBasicBlock *BB) const {
    return LI.isLoopHeader(BB);
  }

  /// removeLoop - This removes the specified top-level loop from this loop info
  /// object.  The loop is not deleted, as it will presumably be inserted into
  /// another loop.
  SILLoop *removeLoop(iterator I) { return LI.removeLoop(I); }

  /// changeLoopFor - Change the top-level loop that contains BB to the
  /// specified loop.  This should be used by transformations that restructure
  /// the loop hierarchy tree.
  void changeLoopFor(SILBasicBlock *BB, SILLoop *L) {
    LI.changeLoopFor(BB, L);
  }

  /// changeTopLevelLoop - Replace the specified loop in the top-level loops
  /// list with the indicated loop.
  void changeTopLevelLoop(SILLoop *OldLoop, SILLoop *NewLoop) {
    LI.changeTopLevelLoop(OldLoop, NewLoop);
  }

  /// addTopLevelLoop - This adds the specified loop to the collection of
  /// top-level loops.
  void addTopLevelLoop(SILLoop *New) {
    LI.addTopLevelLoop(New);
  }

  /// removeBlock - This method completely removes BB from all data structures,
  /// including all of the Loop objects it is nested in and our mapping from
  /// SILBasicBlocks to loops.
  void removeBlock(SILBasicBlock *BB) {
    LI.removeBlock(BB);
  }
};

/// Computes natural loop information for SIL basic blocks.
class SILLoopAnalysis : public SILAnalysis {
  using LoopInfoMap = llvm::DenseMap<SILFunction *, SILLoopInfo *>;

  LoopInfoMap LoopInfos;
  SILPassManager *PM;
public:
  SILLoopAnalysis(SILModule *, SILPassManager *PM)
      : SILAnalysis(AnalysisKind::LoopInfo), PM(PM) {}

  virtual ~SILLoopAnalysis() {
    for (auto LI : LoopInfos)
      delete LI.second;
  }

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::LoopInfo;
  }

  virtual void invalidate(InvalidationKind K) {
    if (K >= InvalidationKind::CFG) {
      for (auto LI : LoopInfos)
        delete LI.second;

      // Clear the maps.
      LoopInfos.clear();
    }
  }

  virtual void invalidate(SILFunction* F, InvalidationKind K) {
    if (K >= InvalidationKind::CFG) {
      if (LoopInfos.count(F)) {
        delete LoopInfos[F];
        LoopInfos.erase(F);
      }
    }
  }

  // Computes loop information for the function using dominance information or
  // returns a cached result if available.
  SILLoopInfo *getLoopInfo(SILFunction *F);
};

} // end namespace swift

#endif

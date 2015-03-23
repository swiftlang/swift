//===-------------- LoopAnalysis.h - SIL Loop Analysis -*- C++ -*----------===//
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
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SILAnalysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
  class DominanceInfo;
  class SILLoop;
  class SILPassManager;
}

namespace swift {

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

  virtual void invalidate(SILAnalysis::PreserveKind K) {
    if (K & PreserveKind::Branches) return;

    for (auto LI : LoopInfos)
      delete LI.second;

    // Clear the maps.
    LoopInfos.clear();
  }

  virtual void invalidate(SILFunction* F, SILAnalysis::PreserveKind K) {
    if (K & PreserveKind::Branches) return;

    if (LoopInfos.count(F)) {
      delete LoopInfos[F];
      LoopInfos.erase(F);
    }

  }

  // Computes loop information for the function using dominance information or
  // returns a cached result if available.
  SILLoopInfo *getLoopInfo(SILFunction *F);

};

} // end namespace swift

#endif

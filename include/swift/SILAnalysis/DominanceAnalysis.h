//===--- DominanceAnalysis.h - SIL Dominance Analysis -*- C++ -*-----------===//
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

#ifndef SWIFT_SILANALYSIS_DOMINANCEANALYSIS_H
#define SWIFT_SILANALYSIS_DOMINANCEANALYSIS_H

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/Dominance.h"
#include "swift/SILAnalysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
class SILModule;
class SILValue;
class SILInstruction;

  class DominanceAnalysis : public SILAnalysis {
    typedef llvm::DenseMap<SILFunction *, DominanceInfo*> DomMap;
    typedef llvm::DenseMap<SILFunction *, PostDominanceInfo*> PDomMap;
    DomMap DomInfo;
    PDomMap PostDomInfo;

  public:
    virtual ~DominanceAnalysis() {
      // Delete Dominance Info.
      for (auto D : DomInfo)
        delete D.second;

      // Delete PostDominanceInfo.
      for (auto P : PostDomInfo)
        delete P.second;
    }

    DominanceAnalysis(SILModule *) : SILAnalysis(AnalysisKind::Dominance) {}

    DominanceInfo* getDomInfo(SILFunction *F) {
      auto &it = DomInfo.FindAndConstruct(F);
      if (!it.second)
        it.second = new DominanceInfo(F);
      return it.second;
    }

    PostDominanceInfo* getPostDomInfo(SILFunction *F) {
      auto &it = PostDomInfo.FindAndConstruct(F);
      if (!it.second)
        it.second = new PostDominanceInfo(F);
      return it.second;
    }

    static bool classof(const SILAnalysis *S) {
      return S->getKind() == AnalysisKind::Dominance;
    }

    virtual void invalidate(InvalidationKind K) {
      assert(!isLocked() && "invalidating a locked analysis?!");
      // FIXME: Invalidating the call graph should not invalidate the domtrees
      // of all functions.
      if (K >= InvalidationKind::CFG) {
        // Delete Dominance Info.
        for (auto D : DomInfo)
          delete D.second;

        // Delete PostDominanceInfo.
        for (auto P : PostDomInfo)
          delete P.second;

        // Clear the maps.
        DomInfo.clear();
        PostDomInfo.clear();
      }
    }

    virtual void invalidate(SILFunction* F, InvalidationKind K) {
      if (K >= InvalidationKind::CFG) {
        auto &it= DomInfo.FindAndConstruct(F);
        if (it.second) {
          delete it.second;
          it.second = nullptr;
        }

        auto &pit= PostDomInfo.FindAndConstruct(F);
        if (pit.second) {
          delete pit.second;
          pit.second = nullptr;
        }
      }
    }
  };
} // end namespace swift



#endif

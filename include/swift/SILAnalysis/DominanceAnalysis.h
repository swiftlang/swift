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

class DominanceAnalysis : public FunctionAnalysisBase<DominanceInfo> {
public:
  DominanceAnalysis()
  : FunctionAnalysisBase<DominanceInfo>(AnalysisKind::Dominance) {}

  DominanceAnalysis(const DominanceAnalysis &) = delete;
  DominanceAnalysis &operator=(const DominanceAnalysis &) = delete;

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::Dominance;
  }

  DominanceInfo *newFunctionAnalysis(SILFunction *F) {
    return new DominanceInfo(F);
  }

  virtual bool shouldInvalidate(SILAnalysis::PreserveKind K) {
    bool branchesPreserved = K & PreserveKind::Branches;
    return !branchesPreserved;
  }
};

class PostDominanceAnalysis : public FunctionAnalysisBase<PostDominanceInfo> {
public:
  PostDominanceAnalysis()
  : FunctionAnalysisBase<PostDominanceInfo>(AnalysisKind::PostDominance) {}

  PostDominanceAnalysis(const PostDominanceAnalysis &) = delete;
  PostDominanceAnalysis &operator=(const PostDominanceAnalysis &) = delete;

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::PostDominance;
  }

  PostDominanceInfo *newFunctionAnalysis(SILFunction *F) {
    return new PostDominanceInfo(F);
  }

  virtual bool shouldInvalidate(SILAnalysis::PreserveKind K) {
    bool branchesPreserved = K & PreserveKind::Branches;
    return !branchesPreserved;
  }
};

} // end namespace swift



#endif

//===--- CallerAnalysis.h - Determine callees per call site -----*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_CALLERANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_CALLERANALYSIS_H

#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"

namespace swift {

using ApplyList = llvm::SmallVector<FullApplySite, 4>;

struct CallerAnalysisFunctionInfo {
  /// A list of all the functions this function calls.
  llvm::SmallVector<SILFunction *, 4> Callees;
  /// A map between all the callers and the callsites in them which
  /// calls this function.
  llvm::DenseMap<SILFunction *, ApplyList> CallSites; 
};

class CallerAnalysis : public SILAnalysis {
  /// Current module we are analyzing.
  SILModule &Mod;

  /// A map between all the functions and their callsites in the module.
  llvm::DenseMap<SILFunction *, CallerAnalysisFunctionInfo> CallInfo;

  /// A list of functions that needs to be recomputed.
  llvm::DenseSet<SILFunction *> RecomputeFunctionList;

  /// Iterate over all the call sites in the function and update
  /// FuncToCallsites.
  void processFunctionCallSites(SILFunction *F); 

  /// This function is about to become "unknown" to us. Invalidate any 
  /// callsite information related to it.
  void invalidateExistingCalleeRelation(SILFunction *F); 

public:
  CallerAnalysis(SILModule *M) : SILAnalysis(AnalysisKind::Caller), Mod(*M) {
    // Make sure we compute everything first time called.
    for(auto &F : Mod) {
      RecomputeFunctionList.insert(&F);
    }
  }

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::Caller;
  }

  virtual void notifyAnalysisOfFunction(SILFunction *F) {
    RecomputeFunctionList.insert(F);
  }

  virtual void invalidate(SILFunction *F, InvalidationKind K) {
    // This function has become "unknown" to us. Invalidate any callsite
    // information related to this function.
    invalidateExistingCalleeRelation(F);
    // Make sure this function is recomputed next time.
    RecomputeFunctionList.insert(F);
  }

  /// Return true if the function has a caller inside current module.
  bool existCaller(SILFunction *F) {
    // Recompute every function in the invalidated function list and empty the
    // list.
    for (auto &F : RecomputeFunctionList) {
      processFunctionCallSites(F);
    }
    RecomputeFunctionList.clear(); 

    auto Iter = CallInfo.FindAndConstruct(F);
    return !Iter.second.CallSites.empty();
  }

};

} // end namespace swift

#endif

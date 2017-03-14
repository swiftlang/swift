//===--- DestructorAnalysis.h -----------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_DESTRUCTORANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_DESTRUCTORANALYSIS_H

#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

/// This analysis determines memory effects during destruction.
class DestructorAnalysis : public SILAnalysis {
  SILModule *Mod;
  llvm::DenseMap<CanType, bool> Cached;
public:

  DestructorAnalysis(SILModule *M)
      : SILAnalysis(AnalysisKind::Destructor), Mod(M) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::Destructor;
  }

  /// Returns true if destruction of T may store to memory.
  bool mayStoreToMemoryOnDestruction(SILType T);

  /// No invalidation is needed.
  virtual void invalidate() override {
    // Nothing can invalidate, because types are static and cannot be changed
    // during the SIL pass pipeline.
  }

  /// No invalidation is needed.
  virtual void invalidate(SILFunction *F, InvalidationKind K)  override {
    // Nothing can invalidate, because types are static and cannot be changed
    // during the SIL pass pipeline.
  }

  /// Notify the analysis about a newly created function.
  virtual void notifyAddFunction(SILFunction *F) override { }

  /// Notify the analysis about a function which will be deleted from the
  /// module.
  virtual void notifyDeleteFunction(SILFunction *F) override { }

  /// Notify the analysis about changed witness or vtables.
  virtual void invalidateFunctionTables() override { }

protected:
  bool cacheResult(CanType Type, bool Result);
  bool isSafeType(CanType Ty);
  bool implementsDestructorSafeContainerProtocol(NominalTypeDecl *NomDecl);
  bool areTypeParametersSafe(CanType Ty);
  ASTContext &getASTContext();
};
}
#endif

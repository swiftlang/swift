//===--- DestructorAnalysis.h ------------------------------*- C++ -*------===//
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
#ifndef SWIFT_SILANALYSIS_DESTRUCTORANALYSIS_H
#define SWIFT_SILANALYSIS_DESTRUCTORANALYSIS_H

#include "swift/SIL/SILValue.h"
#include "swift/SILAnalysis/Analysis.h"
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

protected:
  /// \brief Clear the cache of type safety results.
  void clearTypeCache();
  bool cacheResult(CanType Type, bool Result);
  bool isSafeType(Type);
  bool implementsDestructorSafeContainerProtocol(NominalTypeDecl *);
  bool areTypeParametersSafe(CanType);
  ASTContext &getASTContext();
};
}
#endif

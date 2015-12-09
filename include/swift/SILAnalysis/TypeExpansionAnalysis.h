//===--- TypeExpansionAnalysis.h ------------------------------*- C++ -*------===//
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
#ifndef SWIFT_SILANALYSIS_TYPEEXPANSIONANALYSIS_H
#define SWIFT_SILANALYSIS_TYPEEXPANSIONANALYSIS_H

#include "swift/SIL/SILValue.h"
#include "swift/SILAnalysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

/// This analysis determines memory effects during destruction.
class TypeExpansionAnalysis : public SILAnalysis {
  // The module we are currently processing.
  SILModule *Mod;

  //
  using TypeExpansionMap = llvm::DenseMap<SILType, ProjectionPathList>;
  TypeExpansionMap TypeToNodeExpansionCache;
  TypeExpansionMap TypeToLeafExpansionCache;

public:
  TypeExpansionAnalysis(SILModule *M)
      : SILAnalysis(AnalysisKind::TypeExpansion), Mod(M) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::TypeExpansion;
  }
protected:
};
}
#endif

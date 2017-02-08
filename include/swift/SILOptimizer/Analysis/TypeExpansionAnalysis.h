//===--- TypeExpansionAnalysis.h --------------------------------*- C++ -*-===//
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
#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_TYPEEXPANSIONANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_TYPEEXPANSIONANALYSIS_H

#include "swift/SIL/Projection.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

/// This analysis determines memory effects during destruction.
class TypeExpansionAnalysis : public SILAnalysis {
  llvm::DenseMap<SILType, ProjectionPathList> ExpansionCache;
public:
  TypeExpansionAnalysis(SILModule *M)
      : SILAnalysis(AnalysisKind::TypeExpansion) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::TypeExpansion;
  }

  /// Return ProjectionPath to every leaf or intermediate node of the given type.
  const ProjectionPathList &getTypeExpansion(SILType B, SILModule *Mod);
};

}
#endif

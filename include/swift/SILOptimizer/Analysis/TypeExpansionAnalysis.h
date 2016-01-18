//===--- TypeExpansionAnalysis.h --------------------------------*- C++ -*-===//
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
#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_TYPEEXPANSIONANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_TYPEEXPANSIONANALYSIS_H

#include "swift/SIL/Projection.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

/// Type expansion kind.
enum class TEKind { 
  TELeaf, // Leaf nodes expansion.
  TENode  // Intermediate and leaf nodes expansion.
}; 

using TypeExpansionMap = llvm::DenseMap<SILType, ProjectionPathList>;

/// This analysis determines memory effects during destruction.
class TypeExpansionAnalysis : public SILAnalysis {
  /// Caches the type to leaf node expansion.
  TypeExpansionMap TELeafCache;
  /// Caches the type to each node expansion, including intermediate nodes as
  /// well as leaf nodes in the type tree.
  TypeExpansionMap TENodeCache;

public:
  TypeExpansionAnalysis(SILModule *M)
      : SILAnalysis(AnalysisKind::TypeExpansion) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::TypeExpansion;
  }

  /// Return ProjectionPath to every leaf or intermediate node of the given type.
  const ProjectionPathList &getTypeExpansionProjectionPaths(SILType B,
                                                            SILModule *Mod,
                                                            TEKind K);
};
}
#endif

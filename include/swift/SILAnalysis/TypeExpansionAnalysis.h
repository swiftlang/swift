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

#include "swift/SIL/Projection.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILAnalysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

/// This analysis determines memory effects during destruction.
class TypeExpansionAnalysis : public SILAnalysis {
  using TypeExpansionMap = llvm::DenseMap<SILType, ProjectionPathList>;
  /// Caches the type to leaf node expansion.
  TypeExpansionMap LeafTECache;
  /// Caches the type to each node expansion, including intermediate nodes as
  /// well as leaf nodes in the type tree.
  TypeExpansionMap NodeTECache;

public:
  TypeExpansionAnalysis(SILModule *M)
      : SILAnalysis(AnalysisKind::TypeExpansion) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::TypeExpansion;
  }

  /// Return ProjectionPath to every leaf node of the given type.
  const ProjectionPathList &getTypeLeafExpansion(SILType B, SILModule *Mod);
  /// Returns the ProjectionPath to every leaf and intermediate node of the
  /// given type.
  const ProjectionPathList &getTypeNodeExpansion(SILType B, SILModule *Mod);
};
}
#endif

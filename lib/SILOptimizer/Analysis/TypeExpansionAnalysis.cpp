//===--- TypeExpansionAnalysis.cpp - Type Expansion Analysis --------------===//
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

#define DEBUG_TYPE "typeexpansion-analysis"
#include "swift/SILOptimizer/Analysis/TypeExpansionAnalysis.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "llvm/Support/Debug.h"

using namespace swift;

// The TypeExpansion Cache must not grow beyond this size.
// We limit the size of the MB cache to 2**12 because we want to limit the
// memory usage of this cache.
static const int TypeExpansionAnalysisMaxCacheSize = 4096;

const ProjectionPathList &
TypeExpansionAnalysis::getTypeExpansionProjectionPaths(SILType B, SILModule *Mod,
                                                       TEKind Kind) {
  // Which cache we should be looking up.
  bool IsLeaf = Kind == TEKind::TELeaf;
  TypeExpansionMap &Cache = IsLeaf ? TELeafCache : TENodeCache;

  // Check whether we have the type expansion.
  auto Iter = Cache.find(B);
  if (Iter != Cache.end()) {
    return Iter->second;
  }   

  // Flush the cache if the size of the cache is too large.
  if (Cache.size() > TypeExpansionAnalysisMaxCacheSize) {
    Cache.clear();
  }

  // Build the type expansion for the leaf nodes.
  if (IsLeaf) {
    ProjectionPath::expandTypeIntoLeafProjectionPaths(B, Mod, Cache[B]);
    return Cache[B];
  }

  // Build the type expansion for the internal and leaf nodes.
  ProjectionPath::expandTypeIntoNodeProjectionPaths(B, Mod, Cache[B]);
  return Cache[B];
}

SILAnalysis *swift::createTypeExpansionAnalysis(SILModule *M) {
  return new TypeExpansionAnalysis(M);
}

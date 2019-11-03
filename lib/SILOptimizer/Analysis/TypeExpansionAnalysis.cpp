//===--- TypeExpansionAnalysis.cpp - Type Expansion Analysis --------------===//
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

#define DEBUG_TYPE "typeexpansion-analysis"
#include "swift/SILOptimizer/Analysis/TypeExpansionAnalysis.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/Support/Debug.h"

using namespace swift;

// The TypeExpansion Cache must not grow beyond this size.
// We limit the size of the MB cache to 2**12 because we want to limit the
// memory usage of this cache.
static const int TypeExpansionAnalysisMaxCacheSize = 4096;

const ProjectionPathList&
TypeExpansionAnalysis::getTypeExpansion(SILType B, SILModule *Mod) {
  // Check whether we have the type expansion.
  auto Iter = ExpansionCache.find(B);
  if (Iter != ExpansionCache.end()) {
    return Iter->second;
  }

  // Don't expand large types. This would defeat keeping them in memory.
  if (!shouldExpand(*Mod, B)) {
    // Push the empty projection path.
    ProjectionPath P(B);
    ExpansionCache[B].push_back(P);
    return ExpansionCache[B];
  }

  // Flush the cache if the size of the cache is too large.
  if (ExpansionCache.size() > TypeExpansionAnalysisMaxCacheSize) {
    ExpansionCache.clear();
  }

  // Build the type expansion for the leaf nodes.
  ProjectionPath::expandTypeIntoLeafProjectionPaths(B, Mod, ExpansionCache[B]);
  return ExpansionCache[B];
}

SILAnalysis *swift::createTypeExpansionAnalysis(SILModule *M) {
  return new TypeExpansionAnalysis(M);
}

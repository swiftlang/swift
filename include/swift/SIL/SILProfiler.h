//===--- SILProfiler.h - Instrumentation based profiling ===-----*- C++ -*-===//
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
//
// This file defines SILProfiler, which contains the profiling state for one
// function. It's used to drive PGO and generate code coverage reports.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_PROFILER_H
#define SWIFT_SIL_PROFILER_H

#include "swift/AST/ASTNode.h"
#include "swift/Basic/ProfileCounter.h"
#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILDeclRef.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

class AbstractFunctionDecl;
class SILCoverageMap;
class SILFunction;
class SILModule;

/// Returns whether the given AST node requires profiling instrumentation.
bool doesASTRequireProfiling(SILModule &M, ASTNode N);

/// SILProfiler - Maps AST nodes to profile counters.
class SILProfiler : public SILAllocated<SILProfiler> {
private:
  SILModule &M;

  ASTNode Root;

  bool EmitCoverageMapping;

  SILCoverageMap *CovMap = nullptr;

  StringRef CurrentFileName;

  std::string PGOFuncName;

  uint64_t PGOFuncHash = 0;

  unsigned NumRegionCounters = 0;

  llvm::DenseMap<ASTNode, unsigned> RegionCounterMap;

  llvm::DenseMap<ASTNode, ProfileCounter> RegionLoadedCounterMap;

  llvm::DenseMap<ASTNode, ASTNode> RegionCondToParentMap;

  std::vector<std::tuple<std::string, uint64_t, std::string>> CoverageData;

  SILProfiler(SILModule &M, ASTNode Root, bool EmitCoverageMapping)
      : M(M), Root(Root), EmitCoverageMapping(EmitCoverageMapping) {}

public:
  static SILProfiler *create(SILModule &M, ForDefinition_t forDefinition,
                             ASTNode N);

  /// Check if the function is set up for profiling.
  bool hasRegionCounters() const { return NumRegionCounters != 0; }

  /// Get the execution count corresponding to \p Node from a profile, if one
  /// is available.
  ProfileCounter getExecutionCount(ASTNode Node);

  /// Get the node's parent ASTNode (e.g to get the parent IfStmt or IfCond of
  /// a condition), if one is available.
  Optional<ASTNode> getPGOParent(ASTNode Node);

  /// Get the function name mangled for use with PGO.
  StringRef getPGOFuncName() const { return PGOFuncName; }

  /// Get the function hash.
  uint64_t getPGOFuncHash() const { return PGOFuncHash; }

  /// Get the number of region counters.
  unsigned getNumRegionCounters() const { return NumRegionCounters; }

  /// Get the mapping from \c ASTNode to its corresponding profile counter.
  const llvm::DenseMap<ASTNode, unsigned> &getRegionCounterMap() const {
    return RegionCounterMap;
  }

private:
  /// Map counters to ASTNodes and set them up for profiling the function.
  void assignRegionCounters();
};

} // end namespace swift

#endif // SWIFT_SIL_PROFILER_H

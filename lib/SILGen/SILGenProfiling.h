//===--- SILGenProfiling.h - Instrumentation based profiling ----*- C++ -*-===//
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

#ifndef SWIFT_SILGEN_PROFILING_H
#define SWIFT_SILGEN_PROFILING_H

#include "swift/AST/ASTNode.h"
#include "swift/AST/Stmt.h"
#include "swift/Basic/ProfileCounter.h"
#include "swift/SIL/FormalLinkage.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ProfileData/InstrProf.h"

namespace swift {

class AbstractFunctionDecl;

namespace Lowering {

class SILGenModule;
class SILGenBuilder;

struct ProfilerRAII;

/// Profiling state.
class SILGenProfiling {
private:
  SILGenModule &SGM;
  bool EmitCoverageMapping;

  // The current function's name and counter data.
  std::string CurrentFuncName;
  StringRef CurrentFileName;
  FormalLinkage CurrentFuncLinkage;
  unsigned NumRegionCounters;
  uint64_t FunctionHash;
  std::string PGOFuncName;
  llvm::DenseMap<ASTNode, unsigned> RegionCounterMap;
  llvm::DenseMap<ASTNode, ProfileCounter> PGORegionLoadedCounterMap;
  llvm::DenseMap<ASTNode, ASTNode> PGORegionCondToParentMap;

  std::vector<std::tuple<std::string, uint64_t, std::string>> CoverageData;

public:
  SILGenProfiling(SILGenModule &SGM, bool EmitCoverageMapping)
      : SGM(SGM), EmitCoverageMapping(EmitCoverageMapping),
        NumRegionCounters(0), FunctionHash(0) {}

  bool hasRegionCounters() const { return NumRegionCounters != 0; }

  /// Emit SIL to increment the counter for \c Node.
  void emitCounterIncrement(SILGenBuilder &Builder, ASTNode Node);

  /// Get previously Loaded the execution count corresponding to \p Node
  /// from a profile, if one is available.
  ProfileCounter getExecutionCount(ASTNode Node);

  /// Get PGORegion's parent ASTNode
  /// used to get a condition's parent IfStmt or IfCond
  Optional<ASTNode> getPGOParent(ASTNode Node);

private:
  /// Map counters to ASTNodes and set them up for profiling the given function.
  void assignRegionCounters(Decl *Root);

  friend struct ProfilerRAII;
};

/// RAII object to set up profiling for a function.
struct ProfilerRAII {
  SILGenModule &SGM;
  std::unique_ptr<SILGenProfiling> PreviousProfiler;

  ProfilerRAII(SILGenModule &SGM, Decl *D);
  ~ProfilerRAII();
};

} // end namespace Lowering

} // end namespace swift

#endif // SWIFT_SILGEN_PROFILING

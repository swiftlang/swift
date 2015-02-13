//===--- SILGenProfiling.h - Instrumentation based profiling ----*- C++ -*-===//
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

#ifndef SWIFT_SILGEN_PROFILING_H
#define SWIFT_SILGEN_PROFILING_H

#include "llvm/ADT/DenseMap.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/Stmt.h"
#include "swift/SIL/SILBuilder.h"

namespace llvm {
using swift::ASTNode;
template <> struct DenseMapInfo<ASTNode> {
  static inline ASTNode getEmptyKey() {
    return DenseMapInfo<swift::Expr *>::getEmptyKey();
  }
  static inline ASTNode getTombstoneKey() {
    return DenseMapInfo<swift::Expr *>::getTombstoneKey();
  }
  static unsigned getHashValue(const ASTNode Val) {
    return DenseMapInfo<void *>::getHashValue(Val.getOpaqueValue());
  }
  static bool isEqual(const ASTNode LHS, const ASTNode RHS) {
    return LHS.getOpaqueValue() == RHS.getOpaqueValue();
  }
};
}

namespace swift {
namespace Lowering {

/// Profiling state.
class SILGenProfiling {
private:
  SILGenModule &SGM;
  bool EmitCoverageMapping;

  // The current function's name and counter data.
  std::string CurrentFuncName;
  unsigned NumRegionCounters;
  uint64_t FunctionHash;
  llvm::DenseMap<ASTNode, unsigned> RegionCounterMap;

  std::vector<std::tuple<std::string, uint64_t, std::string>> CoverageData;

public:
  SILGenProfiling(SILGenModule &SGM, bool EmitCoverageMapping)
      : SGM(SGM), EmitCoverageMapping(EmitCoverageMapping),
        NumRegionCounters(0), FunctionHash(0) {}

  bool hasRegionCounters() const { return NumRegionCounters != 0; }

  /// Map counters to ASTNodes and set them up for profiling the given function.
  void assignRegionCounters(ASTNode Root, SILFunction &Fn);

  /// Emit SIL to increment the counter for \c Node.
  void emitCounterIncrement(SILBuilder &Builder, ASTNode Node);

private:
  /// Set the current function's name.
  void setFuncName(SILFunction &Fn);
};

} // end namespace Lowering
} // end namespace swift

#endif // SWIFT_SILGEN_PROFILING

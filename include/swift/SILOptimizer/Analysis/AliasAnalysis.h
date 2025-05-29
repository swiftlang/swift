//===--- AliasAnalysis.h - SIL Alias Analysis -------------------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_ALIASANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_ALIASANALYSIS_H

#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

/// This class is a simple wrapper around an alias analysis cache. This is
/// needed since we do not have an "analysis" infrastructure.
///
/// This wrapper sits above the SwiftCompilerSource implementation of
/// AliasAnalysis. The implementation calls into AliasAnalysis.swift via
/// BridgedAliasAnalysis whenever the result may depend on escape analysis.
class AliasAnalysis {
private:
  void *swiftSpecificData[4];

  SILPassManager *PM;

  void initSwiftSpecificData();

public:
  AliasAnalysis(SILPassManager *PM) : PM(PM) {
    initSwiftSpecificData();
  }

  ~AliasAnalysis();

  static SILAnalysisKind getAnalysisKind() { return SILAnalysisKind::Alias; }

  /// Convenience method that returns true if V1, V2 may alias.
  bool mayAlias(SILValue V1, SILValue V2);

  /// Compute the effects of Inst's memory behavior on the memory pointed to by
  /// the value V.
  MemoryBehavior computeMemoryBehavior(SILInstruction *Inst, SILValue V);

  /// Returns true if \p Inst may read from memory at address \p V.
  ///
  /// For details see MemoryBehavior::MayRead.
  bool mayReadFromMemory(SILInstruction *Inst, SILValue V) {
    auto B = computeMemoryBehavior(Inst, V);
    return B == MemoryBehavior::MayRead ||
           B == MemoryBehavior::MayReadWrite ||
           B == MemoryBehavior::MayHaveSideEffects;
  }

  /// Returns true if \p Inst may write to memory or deinitialize memory at
  /// address \p V.
  ///
  /// For details see MemoryBehavior::MayWrite.
  bool mayWriteToMemory(SILInstruction *Inst, SILValue V) {
    auto B = computeMemoryBehavior(Inst, V);
    return B == MemoryBehavior::MayWrite ||
           B == MemoryBehavior::MayReadWrite ||
           B == MemoryBehavior::MayHaveSideEffects;
  }

  /// Returns true if \p Inst may read from memory, write to memory or
  /// deinitialize memory at address \p V.
  ///
  /// For details see MemoryBehavior.
  bool mayReadOrWriteMemory(SILInstruction *Inst, SILValue V) {
    auto B = computeMemoryBehavior(Inst, V);
    return MemoryBehavior::None != B;
  }

  /// Returns true if \p Ptr may be released in the function call \p FAS.
  bool canApplyDecrementRefCount(FullApplySite FAS, SILValue Ptr);

  /// Returns true if \p Ptr may be released by the builtin \p BI.
  bool canBuiltinDecrementRefCount(BuiltinInst *BI, SILValue Ptr);

  /// Returns true if the object(s of) `obj` can escape to `toInst`.
  ///
  /// Special entry point into BridgedAliasAnalysis (escape analysis) for use in
  /// ARC analysis.
  bool isObjectReleasedByInst(SILValue obj, SILInstruction *toInst);

  /// Is the `addr` within all reachable objects/addresses, when start walking
  /// from `obj`?
  ///
  /// Special entry point into BridgedAliasAnalysis (escape analysis) for use in
  /// ARC analysis.
  bool isAddrVisibleFromObject(SILValue addr, SILValue obj);
};

} // end namespace swift

#endif

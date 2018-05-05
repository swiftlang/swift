//===--- AccessedStorageAnalysis.h - Accessed Storage Analysis --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements an interprocedural analysis pass that summarizes the
// dynamically enforced formal accesses within a function. These summaries are
// used by AccessEnforcementOpts to locally fold access scopes and remove
// dynamic checks based on whole module analysis.
//
// Note: This can be easily augmented to simultaneously compute
// FunctionSideEffects by adding FunctionSideEffects as a member of
// FunctionAccessedStorage. However, currently, the only use of
// FunctionAccessedStorage is local to summarizeFunction().
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_ACCESSED_STORAGE_ANALYSIS_H_
#define SWIFT_SILOPTIMIZER_ANALYSIS_ACCESSED_STORAGE_ANALYSIS_H_

#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/SideEffectAnalysis.h"

namespace swift {

/// Information about a formal access within a function pertaining to a
/// particular AccessedStorage location.
struct StorageAccessInfo {
  SILAccessKind accessKind;
  bool noNestedConflict = false;

  StorageAccessInfo() {}

  template <typename B>
  explicit StorageAccessInfo(B *beginAccess)
      : accessKind(beginAccess->getAccessKind()),
        noNestedConflict(beginAccess->hasNoNestedConflict()) {}

  bool mergeFrom(const StorageAccessInfo &RHS);
};

/// The per-function result of AccessedStorageAnalysis.
///
/// Maps each unique AccessedStorage location to StorageAccessInfo.
///
/// Any unidentified accesses are summarized as a single unidentifiedAccess
/// property.
class FunctionAccessedStorage {
  using AccessedStorageMap =
      llvm::SmallDenseMap<AccessedStorage, StorageAccessInfo, 8>;

  AccessedStorageMap storageAccessMap;
  Optional<SILAccessKind> unidentifiedAccess;

public:
  FunctionAccessedStorage() {}

  void clear() {
    storageAccessMap.clear();
    unidentifiedAccess = None;
  }

  /// Sets the most conservative effects, if we don't know anything about the
  /// function.
  void setWorstEffects() {
    storageAccessMap.clear();
    unidentifiedAccess = SILAccessKind::Modify;
  }

  /// Summarize the given function's effects using this FunctionAccessedStorage
  /// object.
  //
  // Return true if the function's' effects have been fully summarized without
  // visiting it's body.
  bool summarizeFunction(SILFunction *F);

  /// Summarize the callee side effects of a call instruction using this
  /// FunctionAccessedStorage object without analyzing the callee function
  /// bodies or scheduling the callees for bottom-up propagation.
  ///
  /// The side effects are represented from the callee's perspective. Parameter
  /// effects are not translated into information on the caller's argument, and
  /// local effects are not dropped.
  ///
  /// Return true if this call-site's effects are summarized without visiting
  /// the callee.
  ///
  /// TODO: Summarize ArraySemanticsCall accesses.
  bool summarizeCall(FullApplySite fullApply) {
    assert(storageAccessMap.empty() && "expected uninitialized results.");
    return false;
  }

  /// Merge effects directly from \p RHS.
  bool mergeFrom(const FunctionAccessedStorage &RHS);

  /// Merge the effects represented in calleeAccess into this
  /// FunctionAccessedStorage object. calleeAccess must correspond to at least
  /// one callee at the apply site `fullApply`. Merging drops any local effects,
  /// and translates parameter effects into effects on the caller-side
  /// arguments.
  ///
  /// The full caller-side effects at a call site can be obtained with
  /// AccessedStorageAnalysis::getCallSiteEffects().
  bool mergeFromApply(const FunctionAccessedStorage &calleeAccess,
                      FullApplySite fullApply);

  /// Analyze the side-effects of a single SIL instruction \p I.
  /// Visited callees are added to \p BottomUpOrder until \p RecursionDepth
  /// reaches MaxRecursionDepth.
  void analyzeInstruction(SILInstruction *I);

  /// Does any of the accesses represented by this FunctionAccessedStorage
  /// object conflict with the given access kind and storage.
  bool mayConflictWith(SILAccessKind otherAccessKind,
                       const AccessedStorage &otherStorage);

  void print(raw_ostream &os) const;
  void dump() const;

protected:
  bool updateUnidentifiedAccess(SILAccessKind accessKind);

  bool mergeAccesses(
      const FunctionAccessedStorage &RHS,
      std::function<AccessedStorage(const AccessedStorage &)> transformStorage);

  template <typename B> void visitBeginAccess(B *beginAccess);
};

/// Summarizes the dynamic accesses performed within a function and its
/// callees.
///
/// When summarizing multiple accesses, a Read access indicates that all
/// accesses are read only. A Write access indicates potential reads and writes.
///
/// Unidentified accesses are not recorded individually. Rather, the function is
/// marked as potentially executing unidentified reads or writes. An incomplete
/// function, without a known callee set, is considered to have unidentified
/// writes.
class AccessedStorageAnalysis
    : public GenericFunctionEffectAnalysis<FunctionAccessedStorage> {
public:
  AccessedStorageAnalysis()
      : GenericFunctionEffectAnalysis<FunctionAccessedStorage>(
            AnalysisKind::AccessedStorage) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::AccessedStorage;
  }
};

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_ANALYSIS_ACCESSED_STORAGE_ANALYSIS_H_

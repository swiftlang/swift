//===--- AccessStorageAnalysis.h - Accessed Storage Analysis --*- C++ -*-===//
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
// This analysis may return conservative results by setting
// FunctionAccessStorage.unidentifiedAccess. This does not imply that all
// accesses within the function have Unidentified AccessStorage.
//
// Note: This interprocedural analysis can be easily augmented to simultaneously
// compute FunctionSideEffects, without using a separate analysis, by adding
// FunctionSideEffects as a member of FunctionAccessStorage. However, passes
// that use AccessStorageAnalysis do not currently need SideEffectAnalysis.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_ACCESSED_STORAGE_ANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_ACCESSED_STORAGE_ANALYSIS_H

#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/SideEffectAnalysis.h"

namespace swift {

/// Information about a formal access within a function pertaining to a
/// particular AccessStorage location.
class StorageAccessInfo : public AccessStorage {
public:
  StorageAccessInfo(AccessStorage storage, SILAccessKind accessKind,
                    bool noNestedConflict)
    : AccessStorage(storage) {
    Bits.StorageAccessInfo.accessKind = unsigned(accessKind);
    Bits.StorageAccessInfo.noNestedConflict = noNestedConflict;
    Bits.StorageAccessInfo.storageIndex = 0;
  }

  // Initialize AccessStorage from the given storage argument and fill in
  // subclass fields from otherStorageInfo.
  StorageAccessInfo(AccessStorage storage, StorageAccessInfo otherStorageInfo)
      : StorageAccessInfo(storage, otherStorageInfo.getAccessKind(),
                          otherStorageInfo.hasNoNestedConflict()) {}

  template <typename B>
  StorageAccessInfo(AccessStorage storage, B *beginAccess)
      : StorageAccessInfo(storage, beginAccess->getAccessKind(),
                          beginAccess->hasNoNestedConflict()) {
    // Currently limited to dynamic Read/Modify access.
    assert(beginAccess->getEnforcement() == SILAccessEnforcement::Dynamic);
  }

  /// Get the merged access kind of all accesses on this storage. If any access
  /// is a Modify, the return Modify, otherwise return Read.
  SILAccessKind getAccessKind() const {
    return SILAccessKind(Bits.StorageAccessInfo.accessKind);
  }

  void setAccessKind(SILAccessKind accessKind) {
    Bits.StorageAccessInfo.accessKind = unsigned(accessKind);
  }

  /// Get a unique index for this accessed storage within a function.
  unsigned getStorageIndex() const {
    return Bits.StorageAccessInfo.storageIndex;
  }

  void setStorageIndex(unsigned index) {
    Bits.StorageAccessInfo.storageIndex = index;
    assert(unsigned(Bits.StorageAccessInfo.storageIndex) == index);
  }

  /// Return true if all accesses of this storage within a function have the
  /// [no_nested_conflict] flag set.
  bool hasNoNestedConflict() const {
    return Bits.StorageAccessInfo.noNestedConflict;
  }

  void setNoNestedConflict(bool val) {
    Bits.StorageAccessInfo.noNestedConflict = val;
  }

  bool mergeFrom(const StorageAccessInfo &RHS);

  void print(raw_ostream &os) const;
  void dump() const;
};
} // namespace swift

namespace llvm {
// Use the same DenseMapInfo for StorageAccessInfo as for AccessStorage. None
// of the subclass bitfields participate in the Key.
template <> struct DenseMapInfo<swift::StorageAccessInfo> {
  static swift::StorageAccessInfo getEmptyKey() {
    auto key = DenseMapInfo<swift::AccessStorage>::getEmptyKey();
    return static_cast<swift::StorageAccessInfo &>(key);
  }

  static swift::StorageAccessInfo getTombstoneKey() {
    auto key = DenseMapInfo<swift::AccessStorage>::getTombstoneKey();
    return static_cast<swift::StorageAccessInfo &>(key);
  }
  static unsigned getHashValue(swift::StorageAccessInfo storage) {
    return DenseMapInfo<swift::AccessStorage>::getHashValue(storage);
  }
  static bool isEqual(swift::StorageAccessInfo LHS,
                      swift::StorageAccessInfo RHS) {
    return DenseMapInfo<swift::AccessStorage>::isEqual(LHS, RHS);
  }
};
}

namespace swift {
using AccessStorageSet = llvm::SmallDenseSet<StorageAccessInfo, 8>;

/// Records each unique AccessStorage in a set of StorageAccessInfo
/// objects. Hashing and equality only sees the AccesedStorage data. The
/// additional StorageAccessInfo bits are recorded as results of this analysis.
///
/// Any unidentified accesses are summarized as a single unidentifiedAccess
/// property. This property may also be used to conservatively summarize
/// results, either because the call graph is unknown or the access sets are too
/// large. It does not imply that all accesses have Unidentified
/// AccessStorage, which is never allowed for class or global access.
class AccessStorageResult {
  AccessStorageSet storageAccessSet;
  Optional<SILAccessKind> unidentifiedAccess;

public:
  AccessStorageResult() {}

  // ---------------------------------------------------------------------------
  // Accessing the results.

  const AccessStorageSet &getStorageSet() const { return storageAccessSet; }

  bool isEmpty() const {
    return storageAccessSet.empty() && !unidentifiedAccess;
  }

  bool hasUnidentifiedAccess() const { return unidentifiedAccess != None; }

  /// Return true if the analysis has determined all accesses of otherStorage
  /// have the [no_nested_conflict] flag set.
  ///
  /// Only call this if there is no unidentifiedAccess in the region and the
  /// given storage is uniquely identified.
  bool hasNoNestedConflict(const AccessStorage &otherStorage) const;

  /// Does any of the accesses represented by this AccessStorageResult
  /// object conflict with the given access kind and storage.
  bool mayConflictWith(SILAccessKind otherAccessKind,
                       const AccessStorage &otherStorage) const;

  /// Raw access to the result for a given AccessStorage location.
  StorageAccessInfo
  getStorageAccessInfo(const AccessStorage &otherStorage) const;

  // ---------------------------------------------------------------------------
  // Constructing the results.

  void clear() {
    storageAccessSet.clear();
    unidentifiedAccess = None;
  }

  /// Return true if these effects are fully conservative.
  bool hasWorstEffects() { return unidentifiedAccess == SILAccessKind::Modify; }

  /// Sets the most conservative effects, if we don't know anything about the
  /// function.
  void setWorstEffects() {
    storageAccessSet.clear();
    unidentifiedAccess = SILAccessKind::Modify;
  }

  void setUnidentifiedAccess(SILAccessKind kind) { unidentifiedAccess = kind; }

  /// Merge effects directly from \p RHS.
  bool mergeFrom(const AccessStorageResult &other);

  /// Merge the effects represented in calleeAccess into this
  /// FunctionAccessStorage object. calleeAccess must correspond to at least
  /// one callee at the apply site `fullApply`. Merging drops any local effects,
  /// and translates parameter effects into effects on the caller-side
  /// arguments.
  ///
  /// The full caller-side effects at a call site can be obtained with
  /// AccessStorageAnalysis::getCallSiteEffects().
  bool mergeFromApply(const AccessStorageResult &calleeAccess,
                      FullApplySite fullApply);

  /// Record any access scopes entered by the given single SIL instruction. 'I'
  /// must not be a FullApply; use mergeFromApply instead.
  void analyzeInstruction(SILInstruction *I);

  void print(raw_ostream &os) const;
  void dump() const;

protected:
  std::pair<AccessStorageSet::iterator, bool>
  insertStorageAccess(StorageAccessInfo storageAccess) {
    storageAccess.setStorageIndex(storageAccessSet.size());
    return storageAccessSet.insert(storageAccess);
  }

  bool updateUnidentifiedAccess(SILAccessKind accessKind);

  bool mergeAccesses(const AccessStorageResult &other,
                     std::function<StorageAccessInfo(const StorageAccessInfo &)>
                         transformStorage);

  template <typename B> void visitBeginAccess(B *beginAccess);
};
} // namespace swift

namespace swift {
/// The per-function result of AccessStorageAnalysis.
class FunctionAccessStorage {
  AccessStorageResult accessResult;

public:
  FunctionAccessStorage() {}

  // ---------------------------------------------------------------------------
  // Accessing the results.

  const AccessStorageResult &getResult() const { return accessResult; }

  bool hasUnidentifiedAccess() const {
    return accessResult.hasUnidentifiedAccess();
  }

  /// Return true if the analysis has determined all accesses of otherStorage
  /// have the [no_nested_conflict] flag set.
  ///
  /// Only call this if there is no unidentifiedAccess in the function and the
  /// given storage is uniquely identified.
  bool hasNoNestedConflict(const AccessStorage &otherStorage) const {
    return accessResult.hasNoNestedConflict(otherStorage);
  }

  /// Does any of the accesses represented by this FunctionAccessStorage
  /// object conflict with the given access kind and storage.
  bool mayConflictWith(SILAccessKind otherAccessKind,
                       const AccessStorage &otherStorage) const {
    return accessResult.mayConflictWith(otherAccessKind, otherStorage);
  }

  /// Raw access to the result for a given AccessStorage location.
  StorageAccessInfo
  getStorageAccessInfo(const AccessStorage &otherStorage) const {
    return accessResult.getStorageAccessInfo(otherStorage);
  }

  // ---------------------------------------------------------------------------
  // Constructing the results.

  void clear() { accessResult.clear(); }

  /// Return true if these effects are fully conservative.
  bool hasWorstEffects() { return accessResult.hasWorstEffects(); }

  /// Sets the most conservative effects, if we don't know anything about the
  /// function.
  void setWorstEffects() { accessResult.setWorstEffects(); }

  /// Summarize the given function's effects using this FunctionAccessStorage
  /// object.
  //
  // Return true if the function's' effects have been fully summarized without
  // visiting it's body.
  bool summarizeFunction(SILFunction *F);

  /// Summarize the callee side effects of a call instruction using this
  /// FunctionAccessStorage object without analyzing the callee function
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
  bool summarizeCall(FullApplySite fullApply);

  /// Merge effects directly from \p RHS.
  bool mergeFrom(const FunctionAccessStorage &RHS) {
    return accessResult.mergeFrom(RHS.accessResult);
  }

  /// Merge the effects represented in calleeAccess into this
  /// FunctionAccessStorage object. calleeAccess must correspond to at least
  /// one callee at the apply site `fullApply`. Merging drops any local effects,
  /// and translates parameter effects into effects on the caller-side
  /// arguments.
  ///
  /// The full caller-side effects at a call site can be obtained with
  /// AccessStorageAnalysis::getCallSiteEffects().
  bool mergeFromApply(const FunctionAccessStorage &calleeAccess,
                      FullApplySite fullApply) {
    return accessResult.mergeFromApply(calleeAccess.accessResult, fullApply);
  }

  /// Analyze the side-effects of a single SIL instruction \p I.
  /// Visited callees are added to \p BottomUpOrder until \p RecursionDepth
  /// reaches MaxRecursionDepth.
  void analyzeInstruction(SILInstruction *I) {
    accessResult.analyzeInstruction(I);
  }

  void print(raw_ostream &os) const { accessResult.print(os); }
  void dump() const { accessResult.dump(); }
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
///
/// Use the GenericFunctionEffectAnalysis API to get the results of the analysis:
/// - geEffects(SILFunction*)
/// - getCallSiteEffects(FunctionEffects &callEffects, FullApplySite fullApply)
class AccessStorageAnalysis
    : public GenericFunctionEffectAnalysis<FunctionAccessStorage> {
public:
  AccessStorageAnalysis()
      : GenericFunctionEffectAnalysis<FunctionAccessStorage>(
            SILAnalysisKind::AccessStorage) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::AccessStorage;
  }
};

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_ANALYSIS_ACCESSED_STORAGE_ANALYSIS_H

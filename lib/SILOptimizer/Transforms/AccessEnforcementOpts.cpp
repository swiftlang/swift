//===------ AccessEnforcementOpts.cpp - Optimize access enforcement -------===//
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
///
/// Pass order dependencies:
///
/// - Will benefit from running after AccessEnforcementSelection.
///
/// - Should run immediately before the AccessEnforcementWMO to share
///   AccessedStorageAnalysis results.
///
/// This pass optimizes access enforcement as follows:
///
/// **Access marker folding**
///
/// Find begin/end access scopes that are uninterrupted by a potential
/// conflicting access. Flag those as [nontracking] access.
///
/// Folding must prove that no dynamic conflicts occur inside of an access
/// scope. That is, a scope has no "nested inner conflicts". The access itself
/// may still conflict with an outer scope. If successful, folding simply sets
/// the [no_nested_conflict] attribute on the begin_[unpaired_]access
/// instruction and removes all corresponding end_[unpaired_]access
/// instructions.
///
/// This analysis is conceptually similar to DiagnoseStaticExclusivity. The
/// difference is that it conservatively considers any dynamic access that may
/// alias, as opposed to only the obviously aliasing accesses (it is the
/// complement of the static diagnostic pass in that respect). This makes a
/// considerable difference in the implementation. For example,
/// DiagnoseStaticExclusivity must be able to fully analyze all @inout_aliasable
/// parameters because they aren't dynamically enforced. This optimization
/// completely ignores @inout_aliasable paramters because it only cares about
/// dynamic enforcement. This optimization also does not attempt to
/// differentiate accesses on disjoint subaccess paths, because it should not
/// weaken enforcement in any way--a program that traps at -Onone should also
/// trap at -O.
///
/// Access folding is a forward data flow analysis that tracks open accesses. If
/// any path to an access' end of scope has a potentially conflicting access,
/// then that access is marked as a nested conflict.
///
/// **Local access marker removal**
///
/// When none of the local accesses on local storage (box/stack) have nested
/// conflicts, then all the local accesses may be disabled by setting their
/// enforcement to `static`. This is somwhat rare because static diagnostics
/// already promote the obvious cases to static checks. However, there are two
/// reasons that dynamic local markers may be disabled: (1) inlining may cause
/// closure access to become local access (2) local storage may truly escape,
/// but none of the the local access scopes cross a call site.
///
/// TODO: Perform another run of AccessEnforcementSelection immediately before
/// this pass. Currently, that pass only works well when run before
/// AllocBox2Stack. Ideally all such closure analysis passes are combined into a
/// shared analysis with a set of associated optimizations that can be rerun at
/// any point in the pipeline. Until then, we could settle for a partially
/// working AccessEnforcementSelection, or expand it somewhat to handle
/// alloc_stack.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-enforcement-opts"

#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/Analysis/AccessedStorageAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/SmallBitVector.h"

using namespace swift;

namespace swift {
/// Represents the identity of a storage location being accessed.
///
/// A value-based subclass of AccessedStorage with identical layout. This
/// provides access to pass-specific data in reserved bits.
///
/// The fully descriptive class name allows forward declaration in order to
/// define bitfields in AccessedStorage.
///
/// Aliased to AccessInfo in this file.
class AccessEnforcementOptsInfo : public AccessedStorage {
public:
  AccessEnforcementOptsInfo(const AccessedStorage &storage)
    : AccessedStorage(storage) {
    Bits.AccessEnforcementOptsInfo.beginAccessIndex = 0;
    Bits.AccessEnforcementOptsInfo.seenNestedConflict = false;
  }

  /// Get a unique index for this access within its function.
  unsigned getAccessIndex() const {
    return Bits.AccessEnforcementOptsInfo.beginAccessIndex;
  }

  void setAccessIndex(unsigned index) {
    Bits.AccessEnforcementOptsInfo.beginAccessIndex = index;
    assert(unsigned(Bits.AccessEnforcementOptsInfo.beginAccessIndex) == index);
  }

  /// Has the analysis seen a conflicting nested access on any path within this
  /// access' scope.
  bool seenNestedConflict() const {
    return Bits.AccessEnforcementOptsInfo.seenNestedConflict;
  }

  void setSeenNestedConflict() {
    Bits.AccessEnforcementOptsInfo.seenNestedConflict = 1;
  }

  void dump() const {
    AccessedStorage::dump();
    llvm::dbgs() << "  access index: " << getAccessIndex() << " <"
                 << (seenNestedConflict() ? "" : "no ") << "conflict>\n";
  }
};
using AccessInfo = AccessEnforcementOptsInfo;
} // namespace swift

namespace {
// Sparse access sets are used temporarily for fast operations by local
// reachability analysis. We don't care if they take more space.
class SparseAccessSet;

/// A dense set of begin_access instructions as a compact vector. Reachability
/// results are stored here because very few accesses are typically in-progress
/// at a particular program point, particularly at block boundaries.
using DenseAccessSet = SmallVector<BeginAccessInst *, 4>;

/// Analyze a function's formal access to determine nested conflicts.
///
/// Maps each begin access instruction to its AccessInfo, which:
/// - identifies the accessed memory for conflict detection
/// - contains a pass-specific reachability set index
/// - contains a pass-specific flag that indicates the presence of a conflict
///   on any path.
///
/// If, after computing reachability, an access' conflict flag is still not set,
/// then all paths in its scope are conflict free. Reachability begins at a
/// begin_access instruction and ends either at a potential conflict
/// or at the end_access instruction that is associated with the
/// begin_access.
///
/// Forward data flow computes `blockOutAccess` for each block. At a control
/// flow merge, this analysis forms an intersection of reachable accesses on
/// each path. Only visited predecessors are merged (unvisited paths
/// optimistically assume reachability). Before a block is visited, it has no
/// map entry in blockOutAccess. Blocks are processed in RPO order, and a single
/// begin_access dominates all associated end_access instructions. Consequently,
/// when a block is first visited, blockOutAccess contains the maximal
/// reachability set. Further iteration would only reduce this set.
///
/// The only result of this analysis is the seenNestedConflict flags in
/// AccessInfo. Since reducing a reachability set cannot further detect
/// conflicts, there is no need to iterate to a reachability fix point.
class AccessConflictAnalysis {
public:
  using AccessMap = llvm::SmallDenseMap<BeginAccessInst *, AccessInfo, 32>;
  // This result of this analysis is a map from all BeginAccessInst in this
  // function to AccessInfo.
  struct Result {
    /// Map each begin access to its AccessInfo with index, data, and flags.
    /// Iterating over this map is nondeterministic. If it is necessary to order
    /// the accesses, then AccessInfo::getAccessIndex() can be used.
    AccessMap accessMap;

    /// Convenience.
    ///
    /// Note: If AccessInfo has already been retrieved, get the index directly
    /// from it instead of calling this to avoid additional hash lookup.
    unsigned getAccessIndex(BeginAccessInst *beginAccess) const {
      return getAccessInfo(beginAccess).getAccessIndex();
    }

    /// Get the AccessInfo for a BeginAccessInst within this function. All
    /// accesses are mapped by identifyBeginAccesses().
    AccessInfo &getAccessInfo(BeginAccessInst *beginAccess) {
      auto iter = accessMap.find(beginAccess);
      assert(iter != accessMap.end());
      return iter->second;
    }
    const AccessInfo &getAccessInfo(BeginAccessInst *beginAccess) const {
      return const_cast<Result &>(*this).getAccessInfo(beginAccess);
    }
  };

private:
  SILFunction *F;
  PostOrderFunctionInfo *PO;
  AccessedStorageAnalysis *ASA;

  /// Tracks the in-scope accesses at the end of each block, for the purpose of
  /// finding nested conflicts.  (Out-of-scope accesses are currently only
  /// tracked locally for the purpose of merging access scopes.)
  llvm::SmallDenseMap<SILBasicBlock *, DenseAccessSet, 32> blockOutAccess;

  Result result;

public:
  AccessConflictAnalysis(SILFunction *F, PostOrderFunctionInfo *PO,
                         AccessedStorageAnalysis *ASA)
      : F(F), PO(PO), ASA(ASA) {}

  Result &&analyze() &&;

protected:
  void identifyBeginAccesses();

  void visitBeginAccess(BeginAccessInst *beginAccess,
                        SparseAccessSet &accessMap);

  void visitEndAccess(EndAccessInst *endAccess, SparseAccessSet &accessMap);

  void visitFullApply(FullApplySite fullApply, SparseAccessSet &accessMap);

  void mergePredAccesses(SILBasicBlock *succBB,
                         SparseAccessSet &mergedAccesses);

  void visitBlock(SILBasicBlock *BB);
};

/// A sparse set of in-flight accesses.
///
/// Explodes a DenseAccessSet into a temporary sparse set for comparison
/// and membership.
///
/// The AccessConflictAnalysis result provides the instruction to index
/// mapping.
class SparseAccessSet {
  AccessConflictAnalysis::Result &result;

  // Mark the in-scope accesses.
  // (Most functions have < 64 accesses.)
  llvm::SmallBitVector inScopeBitmask;
  // Mark a potential conflicts on each access since the last begin/end marker.
  llvm::SmallBitVector conflictBitmask;
  DenseAccessSet denseVec; // Hold all local accesses seen thus far.

public:
  /// Iterate over in-scope, conflict free access.
  class NoNestedConflictIterator {
    const SparseAccessSet &sparseSet;
    DenseAccessSet::const_iterator denseIter;

  public:
    NoNestedConflictIterator(const SparseAccessSet &set)
        : sparseSet(set), denseIter(set.denseVec.begin()) {}

    BeginAccessInst *next() {
      auto end = sparseSet.denseVec.end();
      while (denseIter != end) {
        BeginAccessInst *beginAccess = *denseIter;
        ++denseIter;
        unsigned sparseIndex = sparseSet.result.getAccessIndex(beginAccess);
        if (sparseSet.inScopeBitmask[sparseIndex]
            && !sparseSet.conflictBitmask[sparseIndex]) {
          return beginAccess;
        }
      }
      return nullptr;
    }
  };

  SparseAccessSet(AccessConflictAnalysis::Result &result)
      : result(result), inScopeBitmask(result.accessMap.size()),
        conflictBitmask(result.accessMap.size()) {}

  // All accessed in the given denseVec are presumed to be in-scope and conflict
  // free.
  SparseAccessSet(const DenseAccessSet &denseVec,
                  AccessConflictAnalysis::Result &result)
      : result(result), inScopeBitmask(result.accessMap.size()),
        conflictBitmask(result.accessMap.size()), denseVec(denseVec) {
    for (BeginAccessInst *beginAccess : denseVec)
      inScopeBitmask.set(result.getAccessIndex(beginAccess));
  }
  bool hasConflictFreeAccess() const {
    NoNestedConflictIterator iterator(*this);
    return iterator.next() != nullptr;
  }

  bool hasInScopeAccess() const {
    return llvm::any_of(denseVec, [this](BeginAccessInst *beginAccess) {
      unsigned sparseIndex = result.getAccessIndex(beginAccess);
      return inScopeBitmask[sparseIndex];
    });
  }

  bool isInScope(unsigned index) const { return inScopeBitmask[index]; }

  // Insert the given BeginAccessInst with its corresponding reachability index.
  // Set the in-scope bit and reset the conflict bit.
  bool enterScope(BeginAccessInst *beginAccess, unsigned index) {
    assert(!inScopeBitmask[index]
           && "nested access should not be dynamically enforced.");
    inScopeBitmask.set(index);
    conflictBitmask.reset(index);
    denseVec.push_back(beginAccess);
    return true;
  }

  /// End the scope of the given access by marking it in-scope and clearing the
  /// conflict bit. (The conflict bit only marks conflicts since the last begin
  /// *or* end access).
  void exitScope(unsigned index) { inScopeBitmask.reset(index); }

  bool seenConflict(unsigned index) const { return conflictBitmask[index]; }

  void setConflict(unsigned index) { conflictBitmask.set(index); }

  // Only merge accesses that are present on the `other` map. i.e. erase
  // all accesses in this map that are not present in `other`.
  void merge(const SparseAccessSet &other) {
    inScopeBitmask &= other.inScopeBitmask;
    // Currently only conflict free accesses are preserved across blocks by this
    // analysis. Otherwise, taking the union of conflict bits would be valid.
    assert(other.conflictBitmask.none());
  }

  void copyNoNestedConflictInto(DenseAccessSet &other) {
    other.clear();
    NoNestedConflictIterator iterator(*this);
    while (BeginAccessInst *beginAccess = iterator.next())
      other.push_back(beginAccess);
  }

  // Dump only the accesses with no conflict up to this point.
  void dump() const {
    for (BeginAccessInst *beginAccess : denseVec) {
      unsigned sparseIndex = result.getAccessIndex(beginAccess);
      if (conflictBitmask[sparseIndex])
        continue;

      llvm::dbgs() << *beginAccess << "  ";
      if (!inScopeBitmask[sparseIndex])
        llvm::dbgs() << " [noscope]";
      result.getAccessInfo(beginAccess).dump();
    }
  }
};
} // namespace

// Top-level driver for AccessConflictAnalysis.
AccessConflictAnalysis::Result &&AccessConflictAnalysis::analyze() && {
  // Populate beginAccessMap.
  identifyBeginAccesses();

  // Perform data flow and set the conflict flags on AccessInfo.
  for (auto *BB : PO->getReversePostOrder())
    visitBlock(BB);

  return std::move(result);
}

// Find all begin access operations in this function. Map each access to
// AccessInfo, which includes its identified memory location, identifying
// index, and analysis result flags.
//
// TODO: begin_unpaired_access is not tracked. Even though begin_unpaired_access
// isn't explicitly paired, it may be possible after devirtualization and
// inlining to find all uses of the scratch buffer. However, this doesn't
// currently happen in practice (rdar://40033735).
void AccessConflictAnalysis::identifyBeginAccesses() {
  for (auto &BB : *F) {
    for (auto &I : BB) {
      auto *beginAccess = dyn_cast<BeginAccessInst>(&I);
      if (!beginAccess)
        continue;

      if (beginAccess->getEnforcement() != SILAccessEnforcement::Dynamic)
        continue;

      // The accessed base is expected to be valid for begin_access, but for
      // now, since this optimization runs at the end of the pipeline, we
      // gracefully ignore unrecognized source address patterns, which show up
      // here as an invalid `storage` value.
      const AccessedStorage &storage =
          findAccessedStorageNonNested(beginAccess->getSource());
      if (!storage)
        continue;

      auto iterAndSuccess = result.accessMap.try_emplace(
          beginAccess, static_cast<const AccessInfo &>(storage));
      (void)iterAndSuccess;
      assert(iterAndSuccess.second);

      // Add a pass-specific access index to the mapped storage object.
      AccessInfo &info = iterAndSuccess.first->second;
      info.setAccessIndex(result.accessMap.size() - 1);
      assert(!info.seenNestedConflict());
    }
  }
}

/// When a conflict is detected, flag the access so it can't be folded, and
/// remove its index from the current access set so we stop checking for
/// conflicts. Erasing from SparseAccessSet does not invalidate any iterators.
static void recordConflict(AccessInfo &info, SparseAccessSet &accessSet) {
  info.setSeenNestedConflict();
  accessSet.setConflict(info.getAccessIndex());
}

// Given an "inner" access, check for potential conflicts with any outer access.
// Allow these overlapping accesses:
// - read/read
// - different bases, both valid, and at least one is local
//
// Remove any outer access that may conflict from the accessSet.
// and flag the conflict in beginAccessMap.
//
// Record the inner access in the accessSet.
//
void AccessConflictAnalysis::visitBeginAccess(BeginAccessInst *innerBeginAccess,
                                              SparseAccessSet &accessSet) {
  if (innerBeginAccess->getEnforcement() != SILAccessEnforcement::Dynamic)
    return;

  const AccessInfo &innerAccess = result.getAccessInfo(innerBeginAccess);
  SILAccessKind innerAccessKind = innerBeginAccess->getAccessKind();

  SparseAccessSet::NoNestedConflictIterator accessIter(accessSet);
  while (BeginAccessInst *outerBeginAccess = accessIter.next()) {
    // If both are reads, keep the mapped access.
    if (!accessKindMayConflict(innerAccessKind,
                               outerBeginAccess->getAccessKind())) {
      continue;
    }
    AccessInfo &outerAccess = result.getAccessInfo(outerBeginAccess);

    // If there is no potential conflict, leave the outer access mapped.
    if (!outerAccess.isDistinctFrom(innerAccess))
      continue;

    LLVM_DEBUG(innerAccess.dump(); llvm::dbgs() << "  may conflict with:\n";
               outerAccess.dump());

    recordConflict(outerAccess, accessSet);
  }
  LLVM_DEBUG(llvm::dbgs() << "Recording access: " << *innerBeginAccess;
             llvm::dbgs() << "  at: "; innerAccess.dump());

  // Record the current access in the map. It can potentially be folded
  // regardless of whether it may conflict with an outer access.
  bool inserted =
      accessSet.enterScope(innerBeginAccess, innerAccess.getAccessIndex());
  (void)inserted;
  assert(inserted && "the same begin_access cannot be seen twice.");
}

void AccessConflictAnalysis::visitEndAccess(EndAccessInst *endAccess,
                                   SparseAccessSet &accessSet) {
  auto *beginAccess = endAccess->getBeginAccess();
  if (beginAccess->getEnforcement() != SILAccessEnforcement::Dynamic)
    return;

  unsigned index = result.getAccessIndex(beginAccess);
  LLVM_DEBUG(if (accessSet.seenConflict(index)) llvm::dbgs()
             << "No conflict on one path from " << *beginAccess << " to "
             << *endAccess);

  // Erase this access from the sparse set. We only want to detect conflicts
  // within the access scope.
  accessSet.exitScope(index);
}

void AccessConflictAnalysis::visitFullApply(FullApplySite fullApply,
                                   SparseAccessSet &accessSet) {
  FunctionAccessedStorage callSiteAccesses;
  ASA->getCallSiteEffects(callSiteAccesses, fullApply);

  SparseAccessSet::NoNestedConflictIterator accessIter(accessSet);
  while (BeginAccessInst *outerBeginAccess = accessIter.next()) {

    // If there is no potential conflict, leave the outer access mapped.
    SILAccessKind accessKind = outerBeginAccess->getAccessKind();
    AccessInfo &outerAccess = result.getAccessInfo(outerBeginAccess);
    if (!callSiteAccesses.mayConflictWith(accessKind, outerAccess))
      continue;

    LLVM_DEBUG(llvm::dbgs() << *fullApply.getInstruction()
                            << "  call site access: ";
               callSiteAccesses.dump();
               llvm::dbgs() << "  may conflict with:\n";
               outerAccess.dump());

    recordConflict(outerAccess, accessSet);
  }
}

// Merge all predecessor accesses into the local acces set. Only propagate
// accesses that are still present in all predecessors. The absence of a begin
// access from a visited predecessor indicates the presence of a conflict. A
// block has been visited if it has a map entry in blockOutAccess.
void AccessConflictAnalysis::mergePredAccesses(SILBasicBlock *succBB,
                                      SparseAccessSet &mergedAccesses) {
  for (SILBasicBlock *predBB : succBB->getPredecessorBlocks()) {
    auto mapI = blockOutAccess.find(predBB);
    if (mapI == blockOutAccess.end())
      continue;

    const DenseAccessSet &predSet = mapI->second;
    mergedAccesses.merge(SparseAccessSet(predSet, result));
  }
}

// Compute access reachability within the given block.
void AccessConflictAnalysis::visitBlock(SILBasicBlock *BB) {
  // Sparse set for tracking accesses with an individual block.
  SparseAccessSet accessSet(result);
  mergePredAccesses(BB, accessSet);

  for (auto &I : *BB) {
    if (auto *BAI = dyn_cast<BeginAccessInst>(&I)) {
      visitBeginAccess(BAI, accessSet);
      continue;
    }
    if (auto *EAI = dyn_cast<EndAccessInst>(&I)) {
      visitEndAccess(EAI, accessSet);
      continue;
    }
    if (auto fullApply = FullApplySite::isa(&I)) {
      visitFullApply(fullApply, accessSet);
    }
  }
  LLVM_DEBUG(if (accessSet.hasConflictFreeAccess()) {
    llvm::dbgs() << "Initializing no-conflict access out of bb"
                 << BB->getDebugID() << "\n";
    accessSet.dump();
  });
  if (BB->getTerminator()->isFunctionExiting())
    assert(!accessSet.hasInScopeAccess() && "no postdominating end_access");

  // Initialize blockOutAccess for this block with the current access set.
  accessSet.copyNoNestedConflictInto(blockOutAccess[BB]);
}

// -----------------------------------------------------------------------------
// MARK: Access Enforcement Optimization
// -----------------------------------------------------------------------------

/// Perform access folding.
///
/// Data-flow analysis is now complete. Any begin_access that has seen a
/// conflict can be given the [no_nested_conflict] instruction attribute.
///
/// Note: If we later support marking begin_unpaired_access
/// [no_nested_conflict], then we also need to remove any corresponding
/// end_unpaired_access. That can be done either by recording the
/// end_unpaired_access instructions during analysis and deleting them here in
/// the same order, or sorting them here by their begin_unpaired_access index.
static bool
foldNonNestedAccesses(AccessConflictAnalysis::AccessMap &accessMap) {
  bool changed = false;
  // Iteration over accessMap is nondeterministic. Setting the conflict flags
  // can be done in any order.
  for (auto &beginAccessAndInfo : accessMap) {
    BeginAccessInst *beginAccess = beginAccessAndInfo.first;
    AccessInfo &info = beginAccessAndInfo.second;
    if (info.seenNestedConflict())
      continue;

    // Optimize this begin_access by setting [no_nested_conflict].
    beginAccess->setNoNestedConflict(true);
    changed = true;
    LLVM_DEBUG(llvm::dbgs() << "Folding " << *beginAccess);
  }
  return changed;
}

/// Perform local access marker elimination.
///
/// Disable accesses to uniquely identified local storage for which no
/// accesses can have nested conflicts. This is only valid if the function's
/// local storage cannot be potentially modified by unidentified access:
///
/// - Arguments cannot alias with local storage, so accessing an argument has no
///   effect on analysis of the current function. When a callee accesses an
///   argument, AccessedStorageAnalysis will either map the accessed storage to
///   a value in the caller's function, or mark it as unidentified.
///
/// - Stack or Box local storage could potentially be accessed via Unidentified
///   access. (Some Unidentified accesses are for initialization or for
///   temporary storage instead, but those should never have Dynamic
///   enforcement). These accesses can only be eliminated when there is no
///   Unidentified access within the function without the [no_nested_conflict]
///   flag.
static bool
removeLocalNonNestedAccess(const AccessConflictAnalysis::Result &result,
                           const FunctionAccessedStorage &functionAccess) {
  if (functionAccess.hasUnidentifiedAccess())
    return false;

  bool changed = false;
  SmallVector<BeginAccessInst *, 8> deadAccesses;
  for (auto &beginAccessAndInfo : result.accessMap) {
    BeginAccessInst *beginAccess = beginAccessAndInfo.first;
    const AccessInfo &info = beginAccessAndInfo.second;
    if (info.seenNestedConflict() || !info.isLocal())
      continue;

    // This particular access to local storage is marked
    // [no_nested_conflict]. Now check FunctionAccessedStorage to determine if
    // that is true for all access to the same storage.
    if (functionAccess.hasNoNestedConflict(info)) {
      LLVM_DEBUG(llvm::dbgs() << "Disabling dead access " << *beginAccess);
      beginAccess->setEnforcement(SILAccessEnforcement::Static);
      changed = true;
    }
  }
  return changed;
}

namespace {
struct AccessEnforcementOpts : public SILFunctionTransform {
  void run() override {
    SILFunction *F = getFunction();
    if (F->empty())
      return;

    LLVM_DEBUG(llvm::dbgs() << "Running local AccessEnforcementOpts on "
                            << F->getName() << "\n");

    PostOrderFunctionInfo *PO = getAnalysis<PostOrderAnalysis>()->get(F);
    AccessedStorageAnalysis *ASA = getAnalysis<AccessedStorageAnalysis>();
    auto result = AccessConflictAnalysis(F, PO, ASA).analyze();

    // Perform access folding by setting the [no_nested_conflict] flag on
    // begin_access instructions.
    if (!foldNonNestedAccesses(result.accessMap))
      return;

    // Recompute AccessStorageAnalysis, just for this function, to update the
    // StorageAccessInfo::noNestedConflict status for each accessed storage.
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);

    // Use the updated AccessedStorageAnalysis to find any uniquely identified
    // local storage that has no nested conflict on any of its accesses within
    // this function. All the accesses can be marked as statically enforced.
    //
    // Note that the storage address may be passed as an argument and there may
    // be nested conflicts within that call, but none of the accesses within
    // this function will overlap.
    const FunctionAccessedStorage &functionAccess = ASA->getEffects(F);
    if (removeLocalNonNestedAccess(result, functionAccess))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};
} // namespace

SILTransform *swift::createAccessEnforcementOpts() {
  return new AccessEnforcementOpts();
}

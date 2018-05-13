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
/// This pass optimizes access enforcement as follows:
///
/// Access marker folding: Find begin/end access scopes that are uninterrupted
/// by a potential conflicting access. Flag those as [nontracking] access.
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
/// dynamic enforcement. This optimization also does not attempt to look at
/// subaccess paths, because it should not weaken enforcement in any way--a
/// program that traps at -Onone should also trap at -O.
///
/// AccessFolding is a forward data flow analysis that tracks open accesses. If
/// any path to an access' end of scope has a potentially conflicting access,
/// then that access is marked as a nested conflict.
///
/// Pass order dependencies:
/// - Benefits from running after AccessEnforcementSelection.
///
/// TODO: Perform another run of AccessEnforcementSelection immediately before
/// this pass. Currently, that pass only works well when run before
/// AllocBox2Stack. Ideally all such closure analysis passes are combined into a
/// shared anlysis with a set of associated optimizations that can be rerun at
/// any point in the pipeline. Until then, we could settle for a partially
/// working AccessEnforcementSelection, or expand it somewhat to handle
/// alloc_stack.
///
/// TODO: Add test case. Use SideEffectAnalysis to ignore calls without global
/// effects. Also scan the arguments to see if there are closure captures. If
/// this isn't sufficient, add a bit to SideEffectAnalysis for NonlocalAccess.
///
/// TODO: Add test case. For Local variables. Ignore global side effects if
/// none of the intervening calls can access the captured variable.
///
/// TODO: Add test case. For Class/Global variables. Ignore global side effects
/// if callee analysis and interprocedural analysis tell us that none of the
/// intervening calls access that variable.
///
/// TODO: As a separate module pass, find global/class accesses to properties
/// that are never passed "reentrantly". That optmimization could make use of
/// the no_nested_conflict flags set in this pass. If *none* of a module-private
/// variable's accesses have nested conflict, then they can be downgraded to
/// static checks en masse.
///
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
/// A value-based subclass of AccessedStorage with identical layout. This
/// provides access to pass-specific data in reserved bits.
///
/// The fully-qualified class name allows for the declaration of bitfields in
/// AccessedStorage. In this file, it is aliased to AccessInfo.
class AccessEnforcementOptsInfo : public AccessedStorage {
public:
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
} // namespace swift

using AccessInfo = AccessEnforcementOptsInfo;

namespace {
class SparseAccessSet;

/// A dense set of begin_[unpaired_]access instructions... because very few
/// accesses are typically in-progress at a particular program point,
/// particularly at block boundaries.
using DenseAccessSet = SmallVector<BeginAccessInst *, 4>;

/// Perform the access folding optimization on a function.
///
/// Maps each begin access instruction to its AccessInfo, which:
/// - identifies the accessed memory for conflict detection
/// - contains a pass-specific reachability set index
/// - contains a pass-specific flag that indicates the presence of a conflict
///   on any path.
///
/// If, after computing reachability, an access' conflict flag is still not set,
/// then all paths in its scope are conflict free. Reachability begins at a
/// begin_[unpaired_]access instruction and ends either at a potential conflict
/// or at the end_[unpaired_]access instruction that is associated with the
/// begin access.
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
/// The only result of this analysis used by the optimization is the conflict
/// flag in AccessInfo. Since reducing a reachability set cannot further
/// detect conflicts, there is no need to iterate to a reachability fix point.
class AccessFolding {
  friend class SparseAccessSet;

  SILFunction *F;
  PostOrderFunctionInfo *PO;
  AccessedStorageAnalysis *ASA;

  /// Map each begin access to its index, data, and flags.
  llvm::SmallDenseMap<BeginAccessInst *, AccessInfo, 32> beginAccessMap;

  /// Tracks the in-progress accesses at the end of each block.
  llvm::SmallDenseMap<SILBasicBlock *, DenseAccessSet, 32> blockOutAccess;

public:
  AccessFolding(SILFunction *F, PostOrderFunctionInfo *PO,
                AccessedStorageAnalysis *ASA)
      : F(F), PO(PO), ASA(ASA) {}

  void perform();

protected:
  /// Get the AccessInfo for a BeginAccessInst within this function. All
  /// accesses are mapped by identifyBeginAccesses().
  AccessInfo &getAccessInfo(BeginAccessInst *beginAccess) {
    auto iter = beginAccessMap.find(beginAccess);
    assert(iter != beginAccessMap.end());
    return iter->second;
  }
  const AccessInfo &getAccessInfo(BeginAccessInst *beginAccess) const {
    return const_cast<AccessFolding &>(*this).getAccessInfo(beginAccess);
  }

  /// Convenience. If AccessInfo has already been retrieved, get the index
  /// directly from it instead of calling this to avoid additional hash lookup.
  unsigned getAccessIndex(BeginAccessInst *beginAccess) const {
    return getAccessInfo(beginAccess).getAccessIndex();
  }

  void identifyBeginAccesses();

  void visitBeginAccess(BeginAccessInst *beginAccess,
                        SparseAccessSet &accessMap);

  void visitEndAccess(EndAccessInst *endAccess, SparseAccessSet &accessMap);

  void visitFullApply(FullApplySite fullApply, SparseAccessSet &accessMap);

  void mergePredAccesses(SILBasicBlock *succBB,
                         SparseAccessSet &mergedAccesses);

  void visitBlock(SILBasicBlock *BB);

  void foldNonNestedAccesses();
};

/// A sparse set of in-flight accesses.
///
/// Explodes a DenseAccessSet into a temporary sparse set for comparison
/// and membership.
///
/// The AccessFolding pass provides the instruction to index mapping. Rather
/// than having all instances of SparseAccessSet reference the pass, it is only
/// passed in for the operations that require it.
class SparseAccessSet {
  llvm::SmallBitVector bitmask; // Most functions have < 64 accesses.
  DenseAccessSet denseVec;

public:
  /// Internally, the denseVec may contain entries that have been removed from
  /// the bitmask. Iteration checks for membership in the bitmask.
  class Iterator {
    const AccessFolding &pass;
    const SparseAccessSet &sparseSet;
    DenseAccessSet::const_iterator denseIter;

  public:
    Iterator(const SparseAccessSet &set, const AccessFolding &pass)
        : pass(pass), sparseSet(set), denseIter(set.denseVec.begin()) {}

    BeginAccessInst *next() {
      auto end = sparseSet.denseVec.end();
      while (denseIter != end) {
        BeginAccessInst *beginAccess = *denseIter;
        ++denseIter;
        unsigned sparseIndex = pass.getAccessIndex(beginAccess);
        if (sparseSet.bitmask[sparseIndex])
          return beginAccess;
      }
      return nullptr;
    }
  };

  SparseAccessSet(AccessFolding &pass) : bitmask(pass.beginAccessMap.size()) {}

  SparseAccessSet(const DenseAccessSet &denseVec, AccessFolding &pass)
      : bitmask(pass.beginAccessMap.size()), denseVec(denseVec) {
    for (BeginAccessInst *beginAccess : denseVec)
      bitmask.set(pass.getAccessIndex(beginAccess));
  }

  bool isEmpty(AccessFolding &pass) const {
    Iterator iterator(*this, pass);
    return iterator.next() == nullptr;
  }

  bool contains(unsigned index) const { return bitmask[index]; }

  // Insert the given BeginAccessInst with its corresponding reachability index.
  // Return true if the set was expanded.
  bool insert(BeginAccessInst *beginAccess, unsigned index) {
    if (bitmask[index])
      return false;

    bitmask.set(index);
    denseVec.push_back(beginAccess);
    return true;
  }

  /// Erase an access from this set based on the index provided by its mapped
  /// AccessInfo.
  ///
  /// Does not invalidate Iterator.
  void erase(unsigned index) { bitmask.reset(index); }

  bool isEquivalent(const SparseAccessSet &other) const {
    return bitmask == other.bitmask;
  }

  // Only merge accesses that are present on the `other` map. i.e. erase
  // all accesses in this map that are not present in `other`.
  void merge(const SparseAccessSet &other) { bitmask &= other.bitmask; }

  void copyInto(DenseAccessSet &other, AccessFolding &pass) {
    other.clear();
    Iterator iterator(*this, pass);
    while (BeginAccessInst *beginAccess = iterator.next()) {
      other.push_back(beginAccess);
    }
  }

  void dump(AccessFolding &pass) const {
    Iterator iterator(*this, pass);
    while (BeginAccessInst *beginAccess = iterator.next()) {
      llvm::dbgs() << *beginAccess << "  ";
      pass.getAccessInfo(beginAccess).dump();
    }
  }
};
} // namespace

// Find all begin access operations in this function. Map each access to
// AccessInfo, which includes its identified memory location, identifying
// index, and analysis result flags.
//
// TODO: begin_unpaired_access is not tracked. Even though begin_unpaired_access
// isn't explicitly paired, it may be possible after devirtualization and
// inlining to find all uses of the scratch buffer. However, this doesn't
// currently happen in practice (rdar://40033735).
void AccessFolding::identifyBeginAccesses() {
  for (auto &BB : *F) {
    for (auto &I : BB) {
      auto *beginAccess = dyn_cast<BeginAccessInst>(&I);
      if (!beginAccess)
        continue;

      // The accessed base is expected to be valid for begin_access, but for
      // now, since this optimization runs at the end of the pipeline, we
      // gracefully handle unrecognized source address patterns, which show up
      // here as an invalid `storage` value.
      const AccessedStorage &storage =
          findAccessedStorageNonNested(beginAccess->getSource());
      if (!storage)
        continue;

      auto iterAndSuccess = beginAccessMap.try_emplace(
          beginAccess, static_cast<const AccessInfo &>(storage));
      (void)iterAndSuccess;
      assert(iterAndSuccess.second);

      // Add a pass-specific access index to the mapped storage object.
      AccessInfo &info = iterAndSuccess.first->second;
      info.setAccessIndex(beginAccessMap.size()-1);
      assert(!info.seenNestedConflict());
    }
  }
}

/// When a conflict is detected, flag the access so it can't be folded, and
/// remove its index from the current access set so we stop checking for
/// conflicts. Erasing from SparseAccessSet does not invalidate any iterators.
static void recordConflict(AccessInfo &info, SparseAccessSet &accessSet) {
  info.setSeenNestedConflict();
  accessSet.erase(info.getAccessIndex());
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
void AccessFolding::visitBeginAccess(BeginAccessInst *innerBeginAccess,
                                     SparseAccessSet &accessSet) {
  if (innerBeginAccess->getEnforcement() != SILAccessEnforcement::Dynamic)
    return;

  const AccessInfo &innerAccess = getAccessInfo(innerBeginAccess);
  SILAccessKind innerAccessKind = innerBeginAccess->getAccessKind();

  SparseAccessSet::Iterator accessIter(accessSet, *this);
  while (BeginAccessInst *outerBeginAccess = accessIter.next()) {
    // If both are reads, keep the mapped access.
    if (!accessKindMayConflict(innerAccessKind,
                               outerBeginAccess->getAccessKind())) {
      continue;
    }
    AccessInfo &outerAccess = getAccessInfo(outerBeginAccess);

    // If there is no potential conflict, leave the outer access mapped.
    if (!outerAccess.isDistinctFrom(innerAccess))
      continue;

    DEBUG(innerAccess.dump(); llvm::dbgs() << "  may conflict with:\n";
          outerAccess.dump());

    recordConflict(outerAccess, accessSet);
  }
  DEBUG(llvm::dbgs() << "Recording access: " << *innerBeginAccess;
        llvm::dbgs() << "  at: "; innerAccess.dump());

  // Record the current access in the map. It can potentially be folded
  // regardless of whether it may conflict with an outer access.
  bool inserted =
      accessSet.insert(innerBeginAccess, innerAccess.getAccessIndex());
  (void)inserted;
  assert(inserted && "the same begin_access cannot be seen twice.");
}

void AccessFolding::visitEndAccess(EndAccessInst *endAccess,
                                   SparseAccessSet &accessSet) {
  auto *beginAccess = endAccess->getBeginAccess();
  if (beginAccess->getEnforcement() != SILAccessEnforcement::Dynamic)
    return;

  unsigned index = getAccessIndex(beginAccess);
  DEBUG(if (accessSet.contains(index)) llvm::dbgs()
        << "No conflict on one path from " << *beginAccess << " to "
        << *endAccess);

  // Erase this access from the sparse set. We only want to detect conflicts
  // within the access scope.
  accessSet.erase(index);
}

void AccessFolding::visitFullApply(FullApplySite fullApply,
                                   SparseAccessSet &accessSet) {
  FunctionAccessedStorage callSiteAccesses;
  ASA->getCallSiteEffects(callSiteAccesses, fullApply);

  SparseAccessSet::Iterator accessIter(accessSet, *this);
  while (BeginAccessInst *outerBeginAccess = accessIter.next()) {

    // If there is no potential conflict, leave the outer access mapped.
    SILAccessKind accessKind = outerBeginAccess->getAccessKind();
    AccessInfo &outerAccess = getAccessInfo(outerBeginAccess);
    if (!callSiteAccesses.mayConflictWith(accessKind, outerAccess))
      continue;

    DEBUG(llvm::dbgs() << *fullApply.getInstruction() << "  call site access: ";
          callSiteAccesses.dump(); llvm::dbgs() << "  may conflict with:\n";
          outerAccess.dump());

    recordConflict(outerAccess, accessSet);
  }
}

// Merge all predecessor accesses into the local acces set. Only propagate
// accesses that are still present in all predecessors. The absence of a begin
// access from a visited predecessor indicates the presence of a conflict. A
// block has been visited if it has a map entry in blockOutAccess.
void AccessFolding::mergePredAccesses(SILBasicBlock *succBB,
                                      SparseAccessSet &mergedAccesses) {
  for (SILBasicBlock *predBB : succBB->getPredecessorBlocks()) {
    auto mapI = blockOutAccess.find(predBB);
    if (mapI == blockOutAccess.end())
      continue;

    const DenseAccessSet &predSet = mapI->second;
    mergedAccesses.merge(SparseAccessSet(predSet, *this));
  }
}

// Compute access reachability within the given block.
void AccessFolding::visitBlock(SILBasicBlock *BB) {
  // Sparse set for tracking accesses with an individual block.
  SparseAccessSet accessSet(*this);
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
  if (BB->getTerminator()->isFunctionExiting())
    assert(accessSet.isEquivalent(*this) && "no postdominating end_access");

  DEBUG(if (!accessSet.isEmpty(*this)) {
    llvm::dbgs() << "Initializing accesses out of bb" << BB->getDebugID()
                 << "\n";
    accessSet.dump(*this);
  });

  // Initialize blockOutAccess for this block with the current access set.
  accessSet.copyInto(blockOutAccess[BB], *this);
}

// Data-flow analysis is now complete. Any begin_access remaining in
// nonNestedBeginAccessInstructions can be marked "non-nested".
//
// Note: If we later support marking begin_unpaired_access [no_nested_conflict],
// then we also need to remove any corresponding end_unpaired_access. That can
// be done either by recording the end_unpaired_access instructions during
// analysis and deleting them here in the same order, or sorting them here by
// their begin_unpaired_access index.
void AccessFolding::foldNonNestedAccesses() {
  for (auto &beginAccessAndInfo : beginAccessMap) {
    BeginAccessInst *beginAccess = beginAccessAndInfo.first;
    AccessInfo &info = beginAccessAndInfo.second;
    if (info.seenNestedConflict())
      continue;

    // Optimize this begin_access by setting [no_nested_conflict].
    beginAccess->setNoNestedConflict(true);
  }
}

// Top-level driver for AccessFolding forward data flow optimization.
void AccessFolding::perform() {
  // Populate beginAccessMap.
  identifyBeginAccesses();

  // Perform data flow and set the conflict flags on AccessInfo.
  for (auto *BB : PO->getReversePostOrder())
    visitBlock(BB);

  // Fold any accesses that don't have nested conflicts.
  foldNonNestedAccesses();
}

namespace {
struct AccessEnforcementOpts : public SILFunctionTransform {
  void run() override {
    SILFunction *F = getFunction();
    if (F->empty())
      return;

    // Perform access folding. May mark begin_access with no_nested_conflict and
    // may removed end_unpaired_access.
    PostOrderFunctionInfo *PO = getAnalysis<PostOrderAnalysis>()->get(F);
    AccessedStorageAnalysis *ASA = getAnalysis<AccessedStorageAnalysis>();
    AccessFolding folding(F, PO, ASA);
    folding.perform();
  }
};
}

SILTransform *swift::createAccessEnforcementOpts() {
  return new AccessEnforcementOpts();
}

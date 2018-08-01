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
#include "swift/SILOptimizer/Analysis/LoopRegionAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/MapVector.h"
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
/// A dense set of begin_access instructions as a compact vector. Reachability
/// results are stored here because very few accesses are typically in-progress
/// at a particular program point, particularly at block boundaries.
using DenseAccessSet = SmallVector<BeginAccessInst *, 4>;

/// Maintains AccessConflictAnalysis' storage info for each sub-region
class RegionStorage;

/// Analyze a function's formal accesses.
/// determines nested conflicts and mergeable accesses.
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
/// Forward data flow computes `regionToStorageMap` for each region's blocks.
/// Loops are processed bottom-up.
/// Control flow within a loop or function top level is processed in RPO order.
/// At a block's control flow merge, this analysis forms an intersection of
/// reachable accesses on each path inside the region.
/// Only visited predecessors are merged (unvisited paths optimistically
/// assume reachability). Before a block is visited, it has no map entry in
/// regionToStorageMap. Blocks are processed in RPO order, and a single
/// begin_access dominates all associated end_access instructions. Consequently,
/// when a block is first visited, its storage accesses contains the maximal
/// reachability set. Further iteration would only reduce this set.
///
/// The only results of this analysis are:
//// 1) The seenNestedConflict flags in AccessInfo.
///     Since reducing a reachability set cannot further detect
///     conflicts, there is no need to iterate to a reachability fix point.
///  2) A deterministic order map of out-of-scope instructions that we can
///  merge.
///     The way we construct this map guarantees the accesses within it are
///     mergeable.
class AccessConflictAnalysis {
public:
  using AccessMap = llvm::SmallDenseMap<BeginAccessInst *, AccessInfo, 32>;
  // A map of instruction pairs we can merge from dominating instruction to
  // dominated
  using MergeableMap = llvm::MapVector<BeginAccessInst *, BeginAccessInst *>;
  // This result of this analysis is a map from all BeginAccessInst in this
  // function to AccessInfo.
  struct Result {
    /// Map each begin access to its AccessInfo with index, data, and flags.
    /// Iterating over this map is nondeterministic. If it is necessary to order
    /// the accesses, then AccessInfo::getAccessIndex() can be used.
    AccessMap accessMap;

    /// A map of instruction pairs we can merge the scope of
    MergeableMap mergeMap;

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
  using AccessedStorageLocs = llvm::SmallDenseSet<AccessedStorage, 8>;

private:
  LoopRegionFunctionInfo *LRFI;
  AccessedStorageAnalysis *ASA;

  using RegionToRegionStorageMap =
      llvm::SmallDenseMap<unsigned, RegionStorage *>;
  RegionToRegionStorageMap regionToStorageMap;
  AccessedStorageLocs storageAccesses;

  Result result;
public:
  AccessConflictAnalysis(LoopRegionFunctionInfo *LRFI,
                         AccessedStorageAnalysis *ASA)
      : LRFI(LRFI), ASA(ASA) {}
  ~AccessConflictAnalysis();

  Result &&analyze() &&;
protected:
  void identifyBeginAccesses();
  void identifyStorageLocations();
  void processBlockRegion(unsigned id);
  void processLoopRegion(unsigned id);

  void mergeBBPredAccesses(unsigned succID);

  void visitBeginAccess(BeginAccessInst *beginAccess);
  void visitEndAccess(EndAccessInst *endAccess);
  void detectApplyConflicts(const DenseAccessSet &conflictFreeSet,
                            RegionStorage *applyStorage,
                            const FunctionAccessedStorage &callSiteAccesses,
                            const FullApplySite &fullApply);

  void visitFullApply(FullApplySite fullApply);

  void calcBottomUpOrder(LoopRegion *region,
                         llvm::SmallVectorImpl<unsigned> &worklist);

  RegionStorage *getRegionStorageFromIns(SILInstruction *instr);
};

/// The per-region result of AccessConflictAnalysis.
class RegionStorage {
  LoopRegion *R;
  AccessConflictAnalysis::Result &result;

  // A set of accesses we care about in this scope.
  AccessConflictAnalysis::AccessedStorageLocs storageAccesses;

  DenseAccessSet inScopeConflictFreeAccesses;
  // Mark the in-scope accesses.
  // (Most functions have < 64 accesses.)
  // The bitvector makes the intersection operation easier
  llvm::SmallBitVector inScopeBitmask;
  DenseAccessSet outOfScopeConflictFreeAccesses;
  // Mark the out-of-scope accesses.
  // (Most functions have < 64 accesses.)
  // The bitvector makes the intersection operation easier
  llvm::SmallBitVector outOfScopeBitmask;

  Optional<SILAccessKind> unidentifiedAccess;

public:
  RegionStorage(
      LoopRegion *R, AccessConflictAnalysis::Result &result,
      const AccessConflictAnalysis::AccessedStorageLocs &storageAccesses)
      : R(R), result(result), storageAccesses(storageAccesses),
        inScopeBitmask(result.accessMap.size()),
        outOfScopeBitmask(result.accessMap.size()) {}
  RegionStorage(LoopRegion *R, AccessConflictAnalysis::Result &result)
      : R(R), result(result), inScopeBitmask(result.accessMap.size()),
        outOfScopeBitmask(result.accessMap.size()) {}

  const LoopRegion *getRegion() const { return R; }

  // ---------------------------------------------------------------------------
  // Accessing the results.

  bool hasUnidentifiedAccess() const { return unidentifiedAccess != None; }

  const DenseAccessSet &getInScopeAccesses() {
    return inScopeConflictFreeAccesses;
  }

  const DenseAccessSet &getOutOfScopeConflictFreeAccesses() {
    return outOfScopeConflictFreeAccesses;
  }

  const AccessConflictAnalysis::AccessedStorageLocs &getStorageLocs() {
    return storageAccesses;
  }

  /// Does any of the accesses represented by this AccessConflictAnalysis
  /// object conflict with the given access kind and storage.
  bool mayConflictWith(SILAccessKind otherAccessKind,
                       const AccessedStorage &otherStorage) const;

  // ---------------------------------------------------------------------------
  // Constructing the results.

  void mergeFrom(const RegionStorage &RHS);
  bool updateUnidentifiedAccess(Optional<SILAccessKind> accessKind);
  void recordStorageConflict(const AccessedStorage &storage);
  void addStorage(const AccessedStorage &storage) {
    storageAccesses.insert(storage);
  }
  void addInScopeAccess(BeginAccessInst *beginAccess);
  void removeInScopeAccess(BeginAccessInst *beginAccess);
  void addOutOfScopeAccess(BeginAccessInst *beginAccess);
  void reset() {
    outOfScopeBitmask.reset();
    inScopeBitmask.reset();
    storageAccesses.clear();
    inScopeConflictFreeAccesses.clear();
    outOfScopeConflictFreeAccesses.clear();
  }

protected:
  void mergeBBAccessSet(DenseAccessSet &LHS, const DenseAccessSet &RHS,
                        const llvm::SmallBitVector &scopeBitmask);
  void mergeBlockPred(const RegionStorage &RHS);
  void mergeSuccIntoLoopRegion(const RegionStorage &RHS);
  void mergeSuccBlockIntoLoopRegion(const RegionStorage &RHS);
  void mergeSuccLoopIntoLoopRegion(const RegionStorage &RHS);
  void removeConflictLocFromSet(DenseAccessSet &Set,
                                const AccessedStorage &storage,
                                llvm::SmallBitVector &scopeBitmask);
};
} // namespace

// Top-level driver for AccessConflictAnalysis
AccessConflictAnalysis::Result &&AccessConflictAnalysis::analyze() && {
  identifyBeginAccesses();
  identifyStorageLocations();
  // calculate the order in which we should work
  // on the function / loop sub-regions
  llvm::SmallVector<unsigned, 16> bottomUpWorklist;
  auto *topRegion = LRFI->getTopLevelRegion();
  calcBottomUpOrder(topRegion, bottomUpWorklist);

  while (!bottomUpWorklist.empty()) {
    auto id = bottomUpWorklist.pop_back_val();
    processLoopRegion(id);
  }

  return std::move(result);
}

void RegionStorage::removeConflictLocFromSet(
    DenseAccessSet &Set, const AccessedStorage &storage,
    llvm::SmallBitVector &scopeBitmask) {
  auto pred = [&](BeginAccessInst *inst) {
    auto &currStorage = result.getAccessInfo(inst);
    return !currStorage.isDistinctFrom(storage);
  };
  auto it = std::find_if(Set.begin(), Set.end(), pred);
  while (it != Set.end()) {
    auto &ai = result.getAccessInfo(*it);
    scopeBitmask.reset(ai.getAccessIndex());
    Set.erase(it);
    it = std::find_if(Set.begin(), Set.end(), pred);
  }
}

bool RegionStorage::mayConflictWith(SILAccessKind otherAccessKind,
                                    const AccessedStorage &otherStorage) const {
  if (hasUnidentifiedAccess() &&
      accessKindMayConflict(otherAccessKind, unidentifiedAccess.getValue())) {
    return true;
  }
  for (auto &storageAccess : storageAccesses) {
    if (!otherStorage.isDistinctFrom(storageAccess))
      return true;
  }
  return false;
}

void RegionStorage::recordStorageConflict(const AccessedStorage &storage) {
  removeConflictLocFromSet(outOfScopeConflictFreeAccesses, storage,
                           outOfScopeBitmask);
  DenseAccessSet tmpSet(inScopeConflictFreeAccesses);
  removeConflictLocFromSet(inScopeConflictFreeAccesses, storage,
                           inScopeBitmask);
  for (auto it : tmpSet) {
    if (std::find(inScopeConflictFreeAccesses.begin(),
                  inScopeConflictFreeAccesses.end(),
                  it) == inScopeConflictFreeAccesses.end()) {
      auto &info = result.getAccessInfo(it);
      info.setSeenNestedConflict();
    }
  }
}

void RegionStorage::addInScopeAccess(BeginAccessInst *beginAccess) {
  assert(std::find(inScopeConflictFreeAccesses.begin(),
                   inScopeConflictFreeAccesses.end(),
                   beginAccess) == inScopeConflictFreeAccesses.end() &&
         "the same begin_access cannot be seen twice.");
  assert(storageAccesses.count(result.getAccessInfo(beginAccess)) > 0 &&
         "begin_access need to have been added to region's storageAccesses");
  inScopeConflictFreeAccesses.push_back(beginAccess);
  auto &ai = result.getAccessInfo(beginAccess);
  inScopeBitmask.set(ai.getAccessIndex());
}

void RegionStorage::removeInScopeAccess(BeginAccessInst *beginAccess) {
  assert(storageAccesses.count(result.getAccessInfo(beginAccess)) > 0 &&
         "begin_access need to have been added to region's storageAccesses");
  auto it = std::find(inScopeConflictFreeAccesses.begin(),
                      inScopeConflictFreeAccesses.end(), beginAccess);
  assert(it != inScopeConflictFreeAccesses.end() &&
         "the begin_access should have been in Set.");
  inScopeConflictFreeAccesses.erase(it);
  auto &ai = result.getAccessInfo(beginAccess);
  inScopeBitmask.reset(ai.getAccessIndex());
}

void RegionStorage::addOutOfScopeAccess(BeginAccessInst *beginAccess) {
  assert(std::find(outOfScopeConflictFreeAccesses.begin(),
                   outOfScopeConflictFreeAccesses.end(),
                   beginAccess) == outOfScopeConflictFreeAccesses.end() &&
         "the same begin_access cannot be seen twice.");
  assert(storageAccesses.count(result.getAccessInfo(beginAccess)) > 0 &&
         "begin_access need to have been added to region's storageAccesses");

  auto newStorageInfo = result.getAccessInfo(beginAccess);
  auto pred = [&](BeginAccessInst *curr) {
    auto currStorageInfo = result.getAccessInfo(curr);
    return currStorageInfo.hasIdenticalBase(newStorageInfo);
  };
  auto it = std::find_if(outOfScopeConflictFreeAccesses.begin(),
                         outOfScopeConflictFreeAccesses.end(), pred);
  if (it == outOfScopeConflictFreeAccesses.end()) {
    // We don't have a match in outOfScopeConflictFreeAccesses
    // Just add it to dense set and return
    outOfScopeConflictFreeAccesses.push_back(beginAccess);
    outOfScopeBitmask.set(newStorageInfo.getAccessIndex());
    return;
  }

  auto *otherBegin = *it;
  LLVM_DEBUG(llvm::dbgs() << "Found mergable pair: " << *otherBegin << ", "
                          << *beginAccess << "\n");
  result.mergeMap.insert(std::make_pair(otherBegin, beginAccess));

  // Update outOfScopeConflictFreeAccesses
  // to this last seen instruction
  while (it != outOfScopeConflictFreeAccesses.end()) {
    outOfScopeConflictFreeAccesses.erase(it);
    auto &ai = result.getAccessInfo(*it);
    outOfScopeBitmask.reset(ai.getAccessIndex());
    it = std::find_if(outOfScopeConflictFreeAccesses.begin(),
                      outOfScopeConflictFreeAccesses.end(), pred);
  }
  outOfScopeConflictFreeAccesses.push_back(beginAccess);
  outOfScopeBitmask.set(newStorageInfo.getAccessIndex());
}

void RegionStorage::mergeBBAccessSet(DenseAccessSet &LHS,
                                     const DenseAccessSet &RHS,
                                     const llvm::SmallBitVector &scopeBitmask) {
  LHS.clear();
  for (auto itRHS : RHS) {
    auto &ai = result.getAccessInfo(itRHS);
    if (scopeBitmask[ai.getAccessIndex()])
      LHS.push_back(itRHS);
  }
}

void RegionStorage::mergeBlockPred(const RegionStorage &RHS) {
  // For a block, the initial locations we care about is the
  // intersection of all region predecessors
  inScopeBitmask &= RHS.inScopeBitmask;
  outOfScopeBitmask &= RHS.outOfScopeBitmask;
  AccessConflictAnalysis::AccessedStorageLocs locIntersect;
  for (auto itRHS : RHS.storageAccesses) {

    if (storageAccesses.count(itRHS) > 0) {
      locIntersect.insert(itRHS);
    }
  }
  storageAccesses.clear();
  storageAccesses.insert(locIntersect.begin(), locIntersect.end());

  mergeBBAccessSet(inScopeConflictFreeAccesses, RHS.inScopeConflictFreeAccesses,
                   inScopeBitmask);
  mergeBBAccessSet(outOfScopeConflictFreeAccesses,
                   RHS.outOfScopeConflictFreeAccesses, outOfScopeBitmask);
}

void RegionStorage::mergeSuccBlockIntoLoopRegion(const RegionStorage &RHS) {
  // Add storage locations accessed in block to loop:
  for (auto itRHS : RHS.storageAccesses) {
    storageAccesses.insert(itRHS);
  }
  // We don't maintain outOfScopeConflictFreeAccesses
  // cross-loops, don't update.
  assert(outOfScopeConflictFreeAccesses.empty() &&
         "Expected an empty out of scope set");
  // Same for in-scope
  assert(inScopeConflictFreeAccesses.empty() &&
         "Expected an empty in scope set");
}

void RegionStorage::mergeSuccLoopIntoLoopRegion(const RegionStorage &RHS) {
  for (auto itRHS : RHS.storageAccesses) {
    storageAccesses.insert(itRHS);
  }
}

void RegionStorage::mergeSuccIntoLoopRegion(const RegionStorage &RHS) {
  assert(!RHS.getRegion()->isFunction() &&
         "Did not expect a function successor");
  if (RHS.getRegion()->isBlock()) {
    mergeSuccBlockIntoLoopRegion(RHS);
  } else {
    mergeSuccLoopIntoLoopRegion(RHS);
  }
}

void RegionStorage::mergeFrom(const RegionStorage &RHS) {
  updateUnidentifiedAccess(RHS.unidentifiedAccess);
  if (getRegion()->isBlock()) {
    assert(RHS.getRegion()->isBlock() && "Expected a block region");
    mergeBlockPred(RHS);
  } else {
    mergeSuccIntoLoopRegion(RHS);
  }
}

bool RegionStorage::updateUnidentifiedAccess(
    Optional<SILAccessKind> accessKind) {
  return updateOptionalAccessKind(unidentifiedAccess, accessKind);
}

// Find all begin access operations in this function. Map each access to
// AccessInfo, which includes its identified memory location, identifying
// index, and analysis result flags.
//
// Also, add the storage location to the function's RegionStorage
//
// TODO: begin_unpaired_access is not tracked. Even though begin_unpaired_access
// isn't explicitly paired, it may be possible after devirtualization and
// inlining to find all uses of the scratch buffer. However, this doesn't
// currently happen in practice (rdar://40033735).
void AccessConflictAnalysis::identifyBeginAccesses() {
  for (auto &BB : *LRFI->getFunction()) {
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

void AccessConflictAnalysis::identifyStorageLocations() {
  for (auto it : result.accessMap) {
    auto storage = it.second;
    storageAccesses.insert(storage);
  }
}

// Returns a worklist of loop IDs is bottom-up order.
void AccessConflictAnalysis::calcBottomUpOrder(
    LoopRegion *region, llvm::SmallVectorImpl<unsigned> &worklist) {
  worklist.push_back(region->getID());
  for (auto regionIndex : region->getReverseSubregions()) {
    auto *region = LRFI->getRegion(regionIndex);
    if (region->isBlock())
      continue;
    calcBottomUpOrder(region, worklist);
  }
}

AccessConflictAnalysis::~AccessConflictAnalysis() {
  // clear the RegionToRegionStorageMap
  while (!regionToStorageMap.empty()) {
    auto curr = regionToStorageMap.begin();
    auto currData = curr->getSecond();
    regionToStorageMap.erase(curr);
    delete currData;
  }
}

void AccessConflictAnalysis::mergeBBPredAccesses(unsigned id) {
  auto currStorageIt = regionToStorageMap.find(id);
  assert(currStorageIt != regionToStorageMap.end() &&
         "Expected a region storage");
  auto *storage = currStorageIt->getSecond();
  auto *region = LRFI->getRegion(id);
  assert(region->isBlock() && "Expected a block");
  for (auto pred : region->getPreds()) {
    auto *predRegion = LRFI->getRegion(pred);
    auto storageIt = regionToStorageMap.find(pred);
    if (storageIt == regionToStorageMap.end()) {
      // Did not visit predecessor - bail
      storage->reset();
      return;
    }
    auto *subStorage = storageIt->getSecond();
    auto parentID = region->getParentID();
    if (predRegion->getParentID() != parentID || predRegion->isLoop()) {
      // predecessor out of current sub-region
      if (predRegion->isBlock()) {
        // Unhandled control flow - bail
        storage->reset();
        return;
      }
      assert(predRegion->isLoop() && "Expected a loop");
      SILAccessKind worstAccessKind = SILAccessKind::Modify;
      // We don't merge in/out of scope cross loops
      // we can ignore adding the outcome of the sub-loop
      // in the intersection
      // However, if we accessed any storage in said sub-loop
      // then we have to record a conflict for current accesses
      for (auto loc : subStorage->getStorageLocs()) {
        if (storage->mayConflictWith(worstAccessKind, loc)) {
          storage->recordStorageConflict(loc);
        }
      }
      continue;
    }
    storage->mergeFrom(*subStorage);
  }
}

RegionStorage *
AccessConflictAnalysis::getRegionStorageFromIns(SILInstruction *instr) {
  auto *parentBB = instr->getParent();
  auto *parentRegion = LRFI->getRegion(parentBB);
  auto parentRegionID = parentRegion->getID();
  auto storageIt = regionToStorageMap.find(parentRegionID);
  assert(storageIt != regionToStorageMap.end() &&
         "Expected region storage info");
  auto *parentStorage = storageIt->getSecond();
  return parentStorage;
}

// Given an "inner" access, check for potential conflicts with any outer access.
// Allow these overlapping accesses:
// - read/read
// - different bases, both valid, and at least one is local
//
// Remove any outer access that may conflict and flag the conflict
// in RegionStorage.
//
// Record the inner access in RegionStorage.
//
void AccessConflictAnalysis::visitBeginAccess(BeginAccessInst *beginAccess) {
  if (beginAccess->getEnforcement() != SILAccessEnforcement::Dynamic)
    return;

  // Get the Access info:
  auto &beginAccessInfo = result.getAccessInfo(beginAccess);
  SILAccessKind beginAccessKind = beginAccess->getAccessKind();
  // get the storage summary for the instruction's block:
  auto *beginStorage = getRegionStorageFromIns(beginAccess);
  beginStorage->addStorage(beginAccessInfo);
  if (beginAccessInfo.getKind() == AccessedStorage::Unidentified) {
    beginStorage->updateUnidentifiedAccess(beginAccessKind);
  }
  // check the current in-scope accesses for conflicts:
  for (auto *outerBeginAccess : beginStorage->getInScopeAccesses()) {
    // If both are reads, keep the mapped access.
    if (!accessKindMayConflict(beginAccessKind,
                               outerBeginAccess->getAccessKind())) {
      continue;
    }

    auto &outerAccessInfo = result.getAccessInfo(outerBeginAccess);

    // If there is no potential conflict, leave the outer access mapped.
    if (!outerAccessInfo.isDistinctFrom(beginAccessInfo))
      continue;

    LLVM_DEBUG(beginAccessInfo.dump(); llvm::dbgs() << "  may conflict with:\n";
               outerAccessInfo.dump());

    beginStorage->recordStorageConflict(outerAccessInfo);
    break;
  }
  // Record the current access to InScopeAccesses.
  // It can potentially be folded
  // regardless of whether it may conflict with an outer access.
  beginStorage->addInScopeAccess(beginAccess);
}

void AccessConflictAnalysis::detectApplyConflicts(
    const DenseAccessSet &conflictFreeSet, RegionStorage *applyStorage,
    const FunctionAccessedStorage &callSiteAccesses,
    const FullApplySite &fullApply) {
  for (auto *outerBeginAccess : conflictFreeSet) {
    // If there is no potential conflict, leave the outer access mapped.
    SILAccessKind accessKind = outerBeginAccess->getAccessKind();
    AccessInfo &outerAccessInfo = result.getAccessInfo(outerBeginAccess);
    if (!callSiteAccesses.mayConflictWith(accessKind, outerAccessInfo))
      continue;

    LLVM_DEBUG(
        llvm::dbgs() << *fullApply.getInstruction() << "  call site access: ";
        callSiteAccesses.dump(); llvm::dbgs() << "  may conflict with:\n";
        outerAccessInfo.dump());

    applyStorage->recordStorageConflict(outerAccessInfo);
    break;
  }
}

void AccessConflictAnalysis::visitFullApply(FullApplySite fullApply) {
  FunctionAccessedStorage callSiteAccesses;
  ASA->getCallSiteEffects(callSiteAccesses, fullApply);

  // get the storage summary for the instruction's block:
  auto *applyStorage = getRegionStorageFromIns(fullApply.getInstruction());

  detectApplyConflicts(applyStorage->getInScopeAccesses(), applyStorage,
                       callSiteAccesses, fullApply);
  detectApplyConflicts(applyStorage->getOutOfScopeConflictFreeAccesses(),
                       applyStorage, callSiteAccesses, fullApply);
}

void AccessConflictAnalysis::visitEndAccess(EndAccessInst *endAccess) {
  auto *beginAccess = endAccess->getBeginAccess();
  if (beginAccess->getEnforcement() != SILAccessEnforcement::Dynamic)
    return;

  auto *endStorage = getRegionStorageFromIns(endAccess);
  auto &inScope = endStorage->getInScopeAccesses();
  if (std::find(inScope.begin(), inScope.end(), beginAccess) != inScope.end()) {
    LLVM_DEBUG(llvm::dbgs() << "No conflict on one path from " << *beginAccess
                            << " to " << *endAccess);
    endStorage->removeInScopeAccess(beginAccess);
  }
  // We can merge out-of-scope regardless of having a conflict within a scope,
  // when encountering an end access instruction,
  // regardless of having it in the In scope set,
  // add it to the out of scope set.
  LLVM_DEBUG(llvm::dbgs() << "Got out of scope from " << *beginAccess << " to "
                          << *endAccess << "\n");
  endStorage->addOutOfScopeAccess(beginAccess);
}

void AccessConflictAnalysis::processBlockRegion(unsigned id) {
  assert(regionToStorageMap.find(id) == regionToStorageMap.end() &&
         "Should not process a region twice");
  auto *region = LRFI->getRegion(id);
  assert(region->isBlock() && "Expected a block");
  // A block's inital set is all of parent region's accesses
  auto *storage = new RegionStorage(region, result, storageAccesses);
  regionToStorageMap.insert(std::make_pair(id, storage));
  mergeBBPredAccesses(id);
  auto *bb = region->getBlock();
  for (auto &I : *bb) {
    if (auto *BAI = dyn_cast<BeginAccessInst>(&I)) {
      visitBeginAccess(BAI);
      continue;
    }
    if (auto *EAI = dyn_cast<EndAccessInst>(&I)) {
      visitEndAccess(EAI);
      continue;
    }
    if (auto fullApply = FullApplySite::isa(&I)) {
      visitFullApply(fullApply);
    }
  }
}

void AccessConflictAnalysis::processLoopRegion(unsigned id) {
  assert(regionToStorageMap.find(id) == regionToStorageMap.end() &&
         "Should not process a region twice");
  auto *region = LRFI->getRegion(id);
  assert(!region->isBlock() && "Did not expect a block");
  // A loop's inital storage access set is an empty set
  auto *storage = new RegionStorage(region, result);
  regionToStorageMap.insert(std::make_pair(id, storage));
  for (auto subID : region->getSubregions()) {
    auto *subRegion = LRFI->getRegion(subID);
    if (subRegion->isBlock()) {
      processBlockRegion(subID);
    }
    auto storageIt = regionToStorageMap.find(subID);
    assert(storageIt != regionToStorageMap.end() &&
           "Expected sub-region to have been processed");
    auto *subStorage = storageIt->getSecond();
    storage->mergeFrom(*subStorage);
    if (subRegion->isLoop()) {
      // We only merged storage locations
      // however, inside the current loop,
      // for each visited predecessor,
      // treat the in/out scope accesses as conflicting
      // with any successor loop access
      for (auto pred : subRegion->getPreds()) {
        auto *predRegion = LRFI->getRegion(pred);
        if (predRegion->getParentID() != region->getParentID()) {
          // predecessor outside of current loop
          continue;
        }
        auto predStorageIt = regionToStorageMap.find(pred);
        if (predStorageIt == regionToStorageMap.end()) {
          // Did not visit predecessor
          continue;
        }
        auto *predStorage = predStorageIt->getSecond();
        SILAccessKind worstAccessKind = SILAccessKind::Modify;
        for (auto loopLoc : subStorage->getStorageLocs()) {
          if (predStorage->mayConflictWith(worstAccessKind, loopLoc)) {
            predStorage->recordStorageConflict(loopLoc);
          }
        }
      }
    }
  }
  // Any block that exits the region, if it has in-scope accesses,
  // record a conflict there, we are being conservative:
  for (auto subID : region->getExitingSubregions()) {
    auto storageIt = regionToStorageMap.find(subID);
    assert(storageIt != regionToStorageMap.end() &&
           "Expected sub-region to have been processed");
    auto *subStorage = storageIt->getSecond();
    for (auto *BI : subStorage->getInScopeAccesses()) {
      auto &info = result.getAccessInfo(BI);
      info.setSeenNestedConflict();
    }
  }
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
///   argument, AccessConflictAnalysis will either map the accessed storage to
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

    AccessedStorageAnalysis *ASA = getAnalysis<AccessedStorageAnalysis>();
    LoopRegionFunctionInfo *LRFI = getAnalysis<LoopRegionAnalysis>()->get(F);
    auto result = AccessConflictAnalysis(LRFI, ASA).analyze();

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

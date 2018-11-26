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
///
/// **Access marker merger**
///
/// When a pair of non-overlapping accesses, where the first access dominates
/// the second and there are no conflicts on the same storage in the paths
/// between them, and they are part of the same sub-region
/// be it the same block or the sampe loop, merge those accesses to create
/// a new, larger, scope with a single begin_access for the accesses.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-enforcement-opts"

#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/Analysis/AccessedStorageAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopRegionAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SCCIterator.h"

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
/// A dense map of (index, begin_access instructions) as a compact vector.
/// Reachability results are stored here because very few accesses are
/// typically in-progress at a particular program point,
/// particularly at block boundaries.
using DenseAccessMap = llvm::SmallMapVector<unsigned, BeginAccessInst *, 4>;

// Tracks the local data flow result for a basic block
struct RegionInfo {
  struct AccessSummary {
    // The actual begin_access instructions
    DenseAccessMap conflictFreeAccesses;
    // Flag to Indicate if we started a merging process
    bool merged;
    AccessSummary(unsigned size) : merged(false) {}
  };

  AccessSummary inScopeConflictFreeAccesses;
  AccessSummary outOfScopeConflictFreeAccesses;
  bool unidentifiedAccess;

public:
  RegionInfo(unsigned size)
      : inScopeConflictFreeAccesses(size), outOfScopeConflictFreeAccesses(size),
        unidentifiedAccess(false) {}

  void reset() {
    inScopeConflictFreeAccesses.conflictFreeAccesses.clear();
    outOfScopeConflictFreeAccesses.conflictFreeAccesses.clear();
    outOfScopeConflictFreeAccesses.merged = true;
    unidentifiedAccess = false;
  }

  const DenseAccessMap &getInScopeAccesses() {
    return inScopeConflictFreeAccesses.conflictFreeAccesses;
  }

  const DenseAccessMap &getOutOfScopeAccesses() {
    return outOfScopeConflictFreeAccesses.conflictFreeAccesses;
  }
};

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
/// Forward data flow computes `BlockRegionInfo` for each region's blocks.
/// Loops are processed bottom-up.
/// Control flow within a loop or function top level is processed in RPO order.
/// At a block's control flow merge, this analysis forms an intersection of
/// reachable accesses on each path inside the region.
/// Before a block is visited, it has no `BlockRegionInfo` entry.
/// Blocks are processed in RPO order, and a single begin_access dominates
/// all associated end_access instructions. Consequently,
/// when a block is first visited, its storage accesses contains the maximal
/// reachability set. Further iteration would only reduce this set.
///
/// The only results of this analysis are:
//// 1) The seenNestedConflict flags in AccessInfo. For Each begin_access
///     Since reducing a reachability set cannot further detect
///     conflicts, there is no need to iterate to a reachability fix point.
///     This is derived from a block's in-scope accesses
///  2) A deterministic order map of out-of-scope instructions that we can
///     merge. The way we construct this map guarantees the accesses within
///     it are mergeable.
///
// Example:
// %1 = begin_access X
// %1 is in-scope
// ...
// %2 = begin_access Y // conflict with %1 if X (may-)aliases Y
// If it conflicts - seenNestedConflict
// ...
// end_access %1
// %1 is out-of-scope
// ...
// %3 = begin_access X // %1 reaches %3 -> we can merge
class AccessConflictAndMergeAnalysis {
public:
  using AccessMap = llvm::SmallDenseMap<BeginAccessInst *, AccessInfo, 32>;
  using AccessedStorageSet = llvm::SmallDenseSet<AccessedStorage, 8>;
  using LoopRegionToAccessedStorage =
      llvm::SmallDenseMap<unsigned, AccessedStorageSet>;
  using RegionIDToLocalInfoMap = llvm::DenseMap<unsigned, RegionInfo>;
  // Instruction pairs we can merge from dominating instruction to dominated
  using MergeablePairs =
      llvm::SmallVector<std::pair<BeginAccessInst *, BeginAccessInst *>, 64>;
  // This result of this analysis is a map from all BeginAccessInst in this
  // function to AccessInfo.
  struct Result {
    /// Map each begin access to its AccessInfo with index, data, and flags.
    /// Iterating over this map is nondeterministic. If it is necessary to order
    /// the accesses, then AccessInfo::getAccessIndex() can be used.
    AccessMap accessMap;

    /// Instruction pairs we can merge the scope of
    MergeablePairs mergePairs;

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
  LoopRegionFunctionInfo *LRFI;
  AccessedStorageAnalysis *ASA;

  Result result;

public:
  AccessConflictAndMergeAnalysis(LoopRegionFunctionInfo *LRFI,
                                 AccessedStorageAnalysis *ASA)
      : LRFI(LRFI), ASA(ASA) {}

  void analyze();

  const Result &getResult() { return result; }

protected:
  void identifyBeginAccesses();

  void
  propagateAccessSetsBottomUp(LoopRegionToAccessedStorage &regionToStorageMap,
                              llvm::SmallVector<unsigned, 16> worklist);

  void calcBottomUpOrder(llvm::SmallVectorImpl<unsigned> &worklist);

  void visitBeginAccess(BeginAccessInst *beginAccess, RegionInfo &info);

  void visitEndAccess(EndAccessInst *endAccess, RegionInfo &info);

  void visitFullApply(FullApplySite fullApply, RegionInfo &info);

  void mergePredAccesses(LoopRegion *region,
                         RegionIDToLocalInfoMap &localRegionInfos);

  void detectConflictsInLoop(LoopRegion *loopRegion,
                             RegionIDToLocalInfoMap &localRegionInfos,
                             LoopRegionToAccessedStorage &accessSetsOfRegions);

  void localDataFlowInBlock(LoopRegion *bbRegion,
                            RegionIDToLocalInfoMap &localRegionInfos);

private:
  void addInScopeAccess(RegionInfo &info, BeginAccessInst *beginAccess);
  void removeInScopeAccess(RegionInfo &info, BeginAccessInst *beginAccess);
  void recordConflict(RegionInfo &info, const AccessedStorage &storage);
  void addOutOfScopeAccess(RegionInfo &info, BeginAccessInst *beginAccess);
  void mergeAccessStruct(RegionInfo &info,
                         RegionInfo::AccessSummary &accessStruct,
                         const RegionInfo::AccessSummary &RHSAccessStruct);
  void merge(RegionInfo &info, const RegionInfo &RHS);
  void removeConflictFromStruct(RegionInfo &info,
                                RegionInfo::AccessSummary &accessStruct,
                                const AccessedStorage &storage);
  void visitSetForConflicts(
      const DenseAccessMap &accessSet, RegionInfo &info,
      AccessConflictAndMergeAnalysis::AccessedStorageSet &loopStorage);
  void
  detectApplyConflicts(const swift::FunctionAccessedStorage &callSiteAccesses,
                       const DenseAccessMap &conflictFreeSet,
                       const swift::FullApplySite &fullApply, RegionInfo &info);
};
} // namespace

void AccessConflictAndMergeAnalysis::addInScopeAccess(
    RegionInfo &info, BeginAccessInst *beginAccess) {
  auto &ai = result.getAccessInfo(beginAccess);
  auto index = ai.getAccessIndex();
  assert(info.getInScopeAccesses().find(index) ==
             info.inScopeConflictFreeAccesses.conflictFreeAccesses.end() &&
         "the begin_access should not have been in Set.");
  info.inScopeConflictFreeAccesses.conflictFreeAccesses.insert(
      std::make_pair(index, beginAccess));
}

void AccessConflictAndMergeAnalysis::removeInScopeAccess(
    RegionInfo &info, BeginAccessInst *beginAccess) {
  auto &ai = result.getAccessInfo(beginAccess);
  auto index = ai.getAccessIndex();
  auto it = info.inScopeConflictFreeAccesses.conflictFreeAccesses.find(index);
  assert(it != info.inScopeConflictFreeAccesses.conflictFreeAccesses.end() &&
         "the begin_access should have been in Set.");
  info.inScopeConflictFreeAccesses.conflictFreeAccesses.erase(it);
}

void AccessConflictAndMergeAnalysis::removeConflictFromStruct(
    RegionInfo &info, RegionInfo::AccessSummary &accessStruct,
    const AccessedStorage &storage) {
  auto pred = [&](const std::pair<unsigned, BeginAccessInst *> &pair) {
    auto &currStorage = result.getAccessInfo(pair.second);
    return !currStorage.isDistinctFrom(storage);
  };
  auto it = std::find_if(accessStruct.conflictFreeAccesses.begin(),
                         accessStruct.conflictFreeAccesses.end(), pred);
  while (it != accessStruct.conflictFreeAccesses.end()) {
    accessStruct.conflictFreeAccesses.erase(it);
    it = std::find_if(accessStruct.conflictFreeAccesses.begin(),
                      accessStruct.conflictFreeAccesses.end(), pred);
  }
}

void AccessConflictAndMergeAnalysis::recordConflict(
    RegionInfo &info, const AccessedStorage &storage) {
  removeConflictFromStruct(info, info.outOfScopeConflictFreeAccesses, storage);
  DenseAccessMap tmpSet(info.inScopeConflictFreeAccesses.conflictFreeAccesses);
  removeConflictFromStruct(info, info.inScopeConflictFreeAccesses, storage);
  for (auto it : tmpSet) {
    if (std::find(info.inScopeConflictFreeAccesses.conflictFreeAccesses.begin(),
                  info.inScopeConflictFreeAccesses.conflictFreeAccesses.end(),
                  it) ==
        info.inScopeConflictFreeAccesses.conflictFreeAccesses.end()) {
      auto &ai = result.getAccessInfo(it.second);
      ai.setSeenNestedConflict();
    }
  }
}

void AccessConflictAndMergeAnalysis::addOutOfScopeAccess(
    RegionInfo &info, BeginAccessInst *beginAccess) {
  auto newStorageInfo = result.getAccessInfo(beginAccess);
  auto newIndex = newStorageInfo.getAccessIndex();
  auto pred = [&](const std::pair<unsigned, BeginAccessInst *> &pair) {
    auto currStorageInfo = result.getAccessInfo(pair.second);
    return currStorageInfo.hasIdenticalBase(newStorageInfo);
  };

  auto it = std::find_if(
      info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.rbegin(),
      info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.rend(), pred);

  if (it == info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.rend()) {
    // We don't have a match in outOfScopeConflictFreeAccesses
    // Just add it and return
    info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.insert(
        std::make_pair(newIndex, beginAccess));
    return;
  }

  auto *otherBegin = it->second;
  auto index = it->first;
  info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.erase(index);

  auto predDistinct = [&](const std::pair<unsigned, BeginAccessInst *> &pair) {
    auto currStorageInfo = result.getAccessInfo(pair.second);
    return !currStorageInfo.isDistinctFrom(newStorageInfo);
  };

  auto itDistinct = std::find_if(
      info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.begin(),
      info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.end(),
      predDistinct);

  if (itDistinct ==
      info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.end()) {
    LLVM_DEBUG(llvm::dbgs() << "Found mergable pair: " << *otherBegin << ", "
                            << *beginAccess << "\n");
    result.mergePairs.push_back(std::make_pair(otherBegin, beginAccess));
  } else {
    while (itDistinct !=
           info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.end()) {
      info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.erase(
          itDistinct);
      itDistinct = std::find_if(
          info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.begin(),
          info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.end(),
          predDistinct);
    }
  }

  info.outOfScopeConflictFreeAccesses.conflictFreeAccesses.insert(
      std::make_pair(newIndex, beginAccess));
}

static void copyMap(DenseAccessMap &to, const DenseAccessMap &from) {
  for (auto it : from) {
    to.insert(it);
  }
}

void AccessConflictAndMergeAnalysis::mergeAccessStruct(
    RegionInfo &info, RegionInfo::AccessSummary &accessStruct,
    const RegionInfo::AccessSummary &RHSAccessStruct) {
  if (!accessStruct.merged) {
    copyMap(accessStruct.conflictFreeAccesses,
            RHSAccessStruct.conflictFreeAccesses);
    accessStruct.merged = true;
    return;
  }

  DenseAccessMap tmpMap;
  copyMap(tmpMap, accessStruct.conflictFreeAccesses);

  for (auto pair : tmpMap) {
    auto index = pair.first;
    if (RHSAccessStruct.conflictFreeAccesses.find(index) !=
        RHSAccessStruct.conflictFreeAccesses.end())
      continue;
    // Not in RHS - remove from intersect
    accessStruct.conflictFreeAccesses.erase(index);
  }
}

void AccessConflictAndMergeAnalysis::merge(RegionInfo &info,
                                           const RegionInfo &RHS) {
  info.unidentifiedAccess |= RHS.unidentifiedAccess;
  mergeAccessStruct(info, info.inScopeConflictFreeAccesses,
                    RHS.inScopeConflictFreeAccesses);
  mergeAccessStruct(info, info.outOfScopeConflictFreeAccesses,
                    RHS.outOfScopeConflictFreeAccesses);
}

// Top-level driver for AccessConflictAndMergeAnalysis
void AccessConflictAndMergeAnalysis::analyze() {
  identifyBeginAccesses();
  LoopRegionToAccessedStorage accessSetsOfRegions;
  llvm::SmallVector<unsigned, 16> worklist;
  calcBottomUpOrder(worklist);
  propagateAccessSetsBottomUp(accessSetsOfRegions, worklist);

  LLVM_DEBUG(llvm::dbgs() << "Processing Function: "
                          << LRFI->getFunction()->getName() << "\n");
  while (!worklist.empty()) {
    auto regionID = worklist.pop_back_val();
    LLVM_DEBUG(llvm::dbgs() << "Processing Sub-Region: " << regionID << "\n");
    auto *region = LRFI->getRegion(regionID);
    RegionIDToLocalInfoMap localRegionInfos;
    // This is RPO order of the sub-regions
    for (auto subID : region->getSubregions()) {
      auto *subRegion = LRFI->getRegion(subID);
      // testIrreducibleGraph2 in test/SILOptimizer/access_enforcement_opts:
      // If the sub-region is the source of a previously visited backedge,
      // Then the in-state is an empty set.
      bool disableCrossBlock = false;
      if (localRegionInfos.find(subID) != localRegionInfos.end()) {
        // Irreducible loop - we already set the predecessor to empty set
        disableCrossBlock = true;
      } else {
        localRegionInfos.insert(
            std::make_pair(subID, RegionInfo(result.accessMap.size())));
        mergePredAccesses(subRegion, localRegionInfos);
      }
      if (subRegion->isBlock()) {
        localDataFlowInBlock(subRegion, localRegionInfos);
      } else {
        assert(subRegion->isLoop() && "Expected a loop sub-region");
        detectConflictsInLoop(subRegion, localRegionInfos, accessSetsOfRegions);
      }
      // After doing the control flow on the region, and as mentioned above,
      // the sub-region is the source of a previously visited backedge,
      // we want to remove the merging candidates from its final state
      if (disableCrossBlock) {
        // Clear-out the out state: this is risky irreducible control flow
        // Only in-block conflict and merging is allowed
        localRegionInfos.find(subID)->getSecond().reset();
      }
    }
  }
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
void AccessConflictAndMergeAnalysis::identifyBeginAccesses() {
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

// Returns a mapping from each loop sub-region to all its access storage
// Propagates access summaries bottom-up from nested regions
void AccessConflictAndMergeAnalysis::propagateAccessSetsBottomUp(
    LoopRegionToAccessedStorage &regionToStorageMap,
    llvm::SmallVector<unsigned, 16> worklist) {
  while (!worklist.empty()) {
    auto regionID = worklist.pop_back_val();
    auto *region = LRFI->getRegion(regionID);
    assert(regionToStorageMap.find(regionID) == regionToStorageMap.end() &&
           "Should not process a region twice");
    AccessedStorageSet &accessedStorageSet = regionToStorageMap[regionID];
    for (auto subID : region->getSubregions()) {
      auto *subRegion = LRFI->getRegion(subID);
      if (subRegion->isLoop()) {
        // propagate access summaries bottom-up from nested loops.
        auto subRegionStorageIt = regionToStorageMap.find(subID);
        assert(subRegionStorageIt != regionToStorageMap.end() &&
               "Should have processed sub-region");
        for (auto storage : subRegionStorageIt->getSecond()) {
          accessedStorageSet.insert(storage);
        }
      } else {
        assert(subRegion->isBlock() && "Expected a block region");
        auto *bb = subRegion->getBlock();
        for (auto &instr : *bb) {
          if (auto *beginAccess = dyn_cast<BeginAccessInst>(&instr)) {
            const AccessedStorage &storage =
                findAccessedStorageNonNested(beginAccess->getSource());
            accessedStorageSet.insert(storage);
          }
          if (auto *beginAccess = dyn_cast<BeginUnpairedAccessInst>(&instr)) {
            const AccessedStorage &storage =
                findAccessedStorageNonNested(beginAccess->getSource());
            accessedStorageSet.insert(storage);
          }
        }
      }
    }
  }
}

// Helper function for calcBottomUpOrder
static void calcBottomUpOrderRecurse(LoopRegion *region,
                                     llvm::SmallVectorImpl<unsigned> &worklist,
                                     LoopRegionFunctionInfo *LRFI) {
  worklist.push_back(region->getID());
  for (auto regionIndex : region->getReverseSubregions()) {
    auto *region = LRFI->getRegion(regionIndex);
    if (region->isBlock())
      continue;
    calcBottomUpOrderRecurse(region, worklist, LRFI);
  }
}

// Returns a worklist of loop IDs is bottom-up order.
void AccessConflictAndMergeAnalysis::calcBottomUpOrder(
    llvm::SmallVectorImpl<unsigned> &worklist) {
  auto *topRegion = LRFI->getTopLevelRegion();
  calcBottomUpOrderRecurse(topRegion, worklist, LRFI);
}

void AccessConflictAndMergeAnalysis::visitBeginAccess(
    BeginAccessInst *beginAccess, RegionInfo &info) {
  if (beginAccess->getEnforcement() != SILAccessEnforcement::Dynamic)
    return;

  // Get the Access info:
  auto &beginAccessInfo = result.getAccessInfo(beginAccess);
  if (beginAccessInfo.getKind() == AccessedStorage::Unidentified) {
    info.unidentifiedAccess = true;
  }
  SILAccessKind beginAccessKind = beginAccess->getAccessKind();
  // check the current in-scope accesses for conflicts:
  bool changed = false;
  do {
    changed = false;
    for (auto pair : info.getInScopeAccesses()) {
      auto *outerBeginAccess = pair.second;
      // If both are reads, keep the mapped access.
      if (!accessKindMayConflict(beginAccessKind,
                                 outerBeginAccess->getAccessKind())) {
        continue;
      }

      auto &outerAccessInfo = result.getAccessInfo(outerBeginAccess);
      // If there is no potential conflict, leave the outer access mapped.
      if (!outerAccessInfo.isDistinctFrom(beginAccessInfo))
        continue;

      LLVM_DEBUG(beginAccessInfo.dump();
                 llvm::dbgs() << "  may conflict with:\n";
                 outerAccessInfo.dump());

      recordConflict(info, outerAccessInfo);
      changed = true;
      break;
    }
  } while (changed);

  // Record the current access to InScopeAccesses.
  // It can potentially be folded
  // regardless of whether it may conflict with an outer access.
  addInScopeAccess(info, beginAccess);
}

void AccessConflictAndMergeAnalysis::visitEndAccess(EndAccessInst *endAccess,
                                                    RegionInfo &info) {
  auto *beginAccess = endAccess->getBeginAccess();
  if (beginAccess->getEnforcement() != SILAccessEnforcement::Dynamic)
    return;
  auto &ai = result.getAccessInfo(beginAccess);
  auto &inScope = info.getInScopeAccesses();
  if (inScope.find(ai.getAccessIndex()) != inScope.end()) {
    LLVM_DEBUG(llvm::dbgs() << "No conflict on one path from " << *beginAccess
                            << " to " << *endAccess);
    removeInScopeAccess(info, beginAccess);
  }

  // We can merge out-of-scope regardless of having a conflict within a scope,
  // when encountering an end access instruction,
  // regardless of having it in the In scope set,
  // add it to the out of scope set.
  LLVM_DEBUG(llvm::dbgs() << "Got out of scope from " << *beginAccess << " to "
                          << *endAccess << "\n");

  addOutOfScopeAccess(info, beginAccess);
}

void AccessConflictAndMergeAnalysis::detectApplyConflicts(
    const swift::FunctionAccessedStorage &callSiteAccesses,
    const DenseAccessMap &conflictFreeSet,
    const swift::FullApplySite &fullApply, RegionInfo &info) {
  bool changed = false;
  do {
    changed = false;
    for (auto pair : conflictFreeSet) {
      auto *outerBeginAccess = pair.second;
      // If there is no potential conflict, leave the outer access mapped.
      SILAccessKind accessKind = outerBeginAccess->getAccessKind();
      AccessInfo &outerAccessInfo = result.getAccessInfo(outerBeginAccess);
      if (!callSiteAccesses.mayConflictWith(accessKind, outerAccessInfo))
        continue;

      LLVM_DEBUG(
          llvm::dbgs() << *fullApply.getInstruction() << "  call site access: ";
          callSiteAccesses.dump(); llvm::dbgs() << "  may conflict with:\n";
          outerAccessInfo.dump());

      recordConflict(info, outerAccessInfo);
      changed = true;
      break;
    }
  } while (changed);
}

void AccessConflictAndMergeAnalysis::visitFullApply(FullApplySite fullApply,
                                                    RegionInfo &info) {
  FunctionAccessedStorage callSiteAccesses;
  ASA->getCallSiteEffects(callSiteAccesses, fullApply);

  detectApplyConflicts(callSiteAccesses, info.getInScopeAccesses(), fullApply,
                       info);
  detectApplyConflicts(callSiteAccesses, info.getOutOfScopeAccesses(),
                       fullApply, info);
}

void AccessConflictAndMergeAnalysis::mergePredAccesses(
    LoopRegion *region, RegionIDToLocalInfoMap &localRegionInfos) {
  RegionInfo &info = localRegionInfos.find(region->getID())->getSecond();
  auto bbRegionParentID = region->getParentID();
  bool changed = false;
  for (auto pred : region->getPreds()) {
    auto *predRegion = LRFI->getRegion(pred);
    assert((predRegion->getParentID() == bbRegionParentID) &&
           "predecessor is not part of the parent region - unhandled control "
           "flow");
    (void)predRegion;
    (void)bbRegionParentID;
    if (localRegionInfos.find(pred) == localRegionInfos.end()) {
      // Backedge / irreducable control flow - bail
      info.reset();
      // Clear out the accesses of all predecessor:
      for (auto pred : region->getPreds()) {
        if (localRegionInfos.find(pred) == localRegionInfos.end()) {
          // Create a region info with empty-set for predecessors
          localRegionInfos.insert(
              std::make_pair(pred, RegionInfo(result.accessMap.size())));
        }
        RegionInfo &predInfo = localRegionInfos.find(pred)->getSecond();
        predInfo.reset();
      }
      return;
    }
    const RegionInfo &predInfo = localRegionInfos.find(pred)->getSecond();
    changed = true;
    merge(info, predInfo);
  }
  if (!changed) {
    // If there are no predecessors
    info.reset();
    return;
  }
}

void AccessConflictAndMergeAnalysis::visitSetForConflicts(
    const DenseAccessMap &accessSet, RegionInfo &info,
    AccessConflictAndMergeAnalysis::AccessedStorageSet &loopStorage) {
  bool changed = false;
  do {
    changed = false;
    for (auto pair : accessSet) {
      BeginAccessInst *beginAccess = pair.second;
      AccessInfo &accessInfo = result.getAccessInfo(beginAccess);

      for (auto loopAccess : loopStorage) {
        if (loopAccess.isDistinctFrom(accessInfo) && !info.unidentifiedAccess)
          continue;

        recordConflict(info, loopAccess);
        changed = true;
        break;
      }
      if (changed)
        break;
    }
  } while (changed);
}

void AccessConflictAndMergeAnalysis::detectConflictsInLoop(
    LoopRegion *loopRegion, RegionIDToLocalInfoMap &localRegionInfos,
    LoopRegionToAccessedStorage &accessSetsOfRegions) {
  assert(loopRegion->isLoop() && "Expected a loop region");
  auto loopID = loopRegion->getID();
  RegionInfo &info = localRegionInfos.find(loopID)->getSecond();
  AccessedStorageSet &loopStorage =
      accessSetsOfRegions.find(loopID)->getSecond();
  visitSetForConflicts(info.getInScopeAccesses(), info, loopStorage);
  visitSetForConflicts(info.getOutOfScopeAccesses(), info, loopStorage);
}

void AccessConflictAndMergeAnalysis::localDataFlowInBlock(
    LoopRegion *bbRegion, RegionIDToLocalInfoMap &localRegionInfos) {
  assert(bbRegion->isBlock() && "Expected a block region");
  auto *bb = bbRegion->getBlock();
  RegionInfo &info = localRegionInfos.find(bbRegion->getID())->getSecond();
  for (auto &instr : *bb) {
    if (auto *beginAccess = dyn_cast<BeginAccessInst>(&instr)) {
      visitBeginAccess(beginAccess, info);
      continue;
    }
    if (auto *endAccess = dyn_cast<EndAccessInst>(&instr)) {
      visitEndAccess(endAccess, info);
      continue;
    }
    if (auto fullApply = FullApplySite::isa(&instr)) {
      visitFullApply(fullApply, info);
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
foldNonNestedAccesses(AccessConflictAndMergeAnalysis::AccessMap &accessMap) {
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
/// Disable access checks for uniquely identified local storage for which no
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
removeLocalNonNestedAccess(const AccessConflictAndMergeAnalysis::Result &result,
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

// TODO: support multi-end access cases
static EndAccessInst *getSingleEndAccess(BeginAccessInst *inst) {
  EndAccessInst *end = nullptr;
  for (auto *currEnd : inst->getEndAccesses()) {
    if (end == nullptr)
      end = currEnd;
    else
      return nullptr;
  }
  return end;
}

struct SCCInfo {
  unsigned id;
  bool hasLoop;
};

static void mergeEndAccesses(BeginAccessInst *parentIns,
                             BeginAccessInst *childIns) {
  auto *endP = getSingleEndAccess(parentIns);
  if (!endP)
    llvm_unreachable("not supported");
  auto *endC = getSingleEndAccess(childIns);
  if (!endC)
    llvm_unreachable("not supported");

  endC->setOperand(parentIns);
  endP->eraseFromParent();
}

static bool canMergeEnd(BeginAccessInst *parentIns, BeginAccessInst *childIns) {
  auto *endP = getSingleEndAccess(parentIns);
  if (!endP)
    return false;

  auto *endC = getSingleEndAccess(childIns);
  if (!endC)
    return false;

  return true;
}

// TODO: support other merge patterns
static bool
canMergeBegin(PostDominanceInfo *postDomTree,
              const llvm::DenseMap<SILBasicBlock *, SCCInfo> &blockToSCCMap,
              BeginAccessInst *parentIns, BeginAccessInst *childIns) {
  if (!postDomTree->properlyDominates(childIns, parentIns)) {
    return false;
  }
  auto parentSCCIt = blockToSCCMap.find(parentIns->getParent());
  assert(parentSCCIt != blockToSCCMap.end() && "Expected block in SCC Map");
  auto childSCCIt = blockToSCCMap.find(childIns->getParent());
  assert(childSCCIt != blockToSCCMap.end() && "Expected block in SCC Map");
  auto parentSCC = parentSCCIt->getSecond();
  auto childSCC = childSCCIt->getSecond();
  if (parentSCC.id == childSCC.id) {
    return true;
  }
  if (parentSCC.hasLoop) {
    return false;
  }
  if (childSCC.hasLoop) {
    return false;
  }
  return true;
}

static bool
canMerge(PostDominanceInfo *postDomTree,
         const llvm::DenseMap<SILBasicBlock *, SCCInfo> &blockToSCCMap,
         BeginAccessInst *parentIns, BeginAccessInst *childIns) {
  if (!canMergeBegin(postDomTree, blockToSCCMap, parentIns, childIns))
    return false;

  return canMergeEnd(parentIns, childIns);
}

/// Perform access merging.
static bool mergeAccesses(
    SILFunction *F, PostDominanceInfo *postDomTree,
    const AccessConflictAndMergeAnalysis::MergeablePairs &mergePairs) {
  bool changed = false;

  // Compute a map from each block to its SCC -
  // For now we can't merge cross SCC boundary
  llvm::DenseMap<SILBasicBlock *, SCCInfo> blockToSCCMap;
  SCCInfo info;
  info.id = 0;
  for (auto sccIt = scc_begin(F); !sccIt.isAtEnd(); ++sccIt) {
    ++info.id;
    info.hasLoop = sccIt.hasLoop();
    for (auto *bb : *sccIt) {
      blockToSCCMap.insert(std::make_pair(bb, info));
    }
  }
  // make a temporary reverse copy to work on:
  // It is in reverse order just to make it easier to debug / follow
  AccessConflictAndMergeAnalysis::MergeablePairs workPairs;
  workPairs.append(mergePairs.rbegin(), mergePairs.rend());

  // Assume the result contains two access pairs to be merged:
  // (begin_access %1, begin_access %2)
  // = merge end_access %1 with begin_access %2
  // (begin_access %2, begin_access %3)
  // = merge end_access %2 with begin_access %3
  // After merging the first pair, begin_access %2 is removed,
  // so the second pair in the result list points to a to-be-deleted
  // begin_access instruction. We store (begin_access %2 -> begin_access %1)
  // to re-map a merged begin_access to it's replaced instruction.
  llvm::DenseMap<BeginAccessInst *, BeginAccessInst *> oldToNewMap;

  while (!workPairs.empty()) {
    auto curr = workPairs.pop_back_val();
    auto *parentIns = curr.first;
    auto *childIns = curr.second;
    if (oldToNewMap.count(parentIns) != 0) {
      parentIns = oldToNewMap[parentIns];
    }
    assert(oldToNewMap.count(childIns) == 0 &&
           "Can't have same child instruction twice in map");

    // The optimization might not currently support every mergeable pair
    // If the current pattern is not supported - skip
    if (!canMerge(postDomTree, blockToSCCMap, parentIns, childIns))
      continue;

    LLVM_DEBUG(llvm::dbgs()
               << "Merging: " << *childIns << " into " << *parentIns << "\n");

    // Change the type of access of parent:
    // should be the worse between it and child
    auto childAccess = childIns->getAccessKind();
    if (parentIns->getAccessKind() < childAccess) {
      parentIns->setAccessKind(childAccess);
    }

    // Change the no nested conflict of parent:
    // should be the worst case scenario: we might merge to non-conflicting
    // scopes to a conflicting one. f the new result does not conflict,
    // a later on pass will remove the flag
    parentIns->setNoNestedConflict(false);

    // remove end accesses and create new ones that cover bigger scope:
    mergeEndAccesses(parentIns, childIns);

    // In case the child instruction is at the map,
    // updated the oldToNewMap to reflect that we are getting rid of it:
    oldToNewMap.insert(std::make_pair(childIns, parentIns));

    // Modify the users of child instruction to use the parent:
    childIns->replaceAllUsesWith(parentIns);

    changed = true;
  }

  // Delete all old instructions from parent scopes:
  while (!oldToNewMap.empty()) {
    auto curr = oldToNewMap.begin();
    auto *oldIns = curr->getFirst();
    oldToNewMap.erase(oldIns);
    oldIns->eraseFromParent();
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

    LoopRegionFunctionInfo *LRFI = getAnalysis<LoopRegionAnalysis>()->get(F);
    AccessedStorageAnalysis *ASA = getAnalysis<AccessedStorageAnalysis>();
    AccessConflictAndMergeAnalysis a(LRFI, ASA);
    a.analyze();
    auto result = a.getResult();

    // Perform access folding by setting the [no_nested_conflict] flag on
    // begin_access instructions.
    if (foldNonNestedAccesses(result.accessMap)) {
      // Recompute AccessStorageAnalysis, just for this function, to update the
      // StorageAccessInfo::noNestedConflict status for each accessed storage.
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }

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

    // Perform the access merging
    // The inital version of the optimization requires a postDomTree
    PostDominanceAnalysis *postDomAnalysis =
        getAnalysis<PostDominanceAnalysis>();
    PostDominanceInfo *postDomTree = postDomAnalysis->get(F);
    if (mergeAccesses(F, postDomTree, result.mergePairs))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};
} // namespace

SILTransform *swift::createAccessEnforcementOpts() {
  return new AccessEnforcementOpts();
}

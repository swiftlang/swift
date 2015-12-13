//===--- ARCLoopHoisting.h ------------------------------------------------===//
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

#ifndef SWIFT_SILPASSES_ARC_ARCLOOPHOISTING_H
#define SWIFT_SILPASSES_ARC_ARCLOOPHOISTING_H

#include "swift/SIL/SILValue.h"
#include "swift/Basic/ImmutablePointerSet.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"

namespace swift {

class LoopRegion;
class SILInstruction;
class LoopRegionFunctionInfo;
class RCIdentityFunctionInfo;
class ProgramTerminationFunctionInfo;
class SideEffectAnalysis;
class AliasAnalysis;
struct LHContext;

struct UnhoistableInstSetCallbacks {
  std::function<void (SILInstruction *)> MakeUnhoistable;
  std::function<bool (SILInstruction *)> IsUnhoistable;  
};

/// A wrapper around a map from SILValue -> SmallPtrSet<SILInstruction *> that
/// ensures that the unhoistable inst sets are updated appropriately when values
/// are deleted from the map.
class RCIdentityToInstSetMap {
  using InnerMapTy = llvm::SmallDenseMap<SILValue,
    ImmutablePointerSet<SILInstruction> *, 4>;

  /// A map from a specific RCIdentity to the set of hoistable instructions on
  /// that RCIdentity that reach this block.
  InnerMapTy Insts;

  /// A reference to a global set of unhoistable instructions. Whenever we clear
  /// Insts, we need to update the set with the contents of Insts so we know not
  /// to hoist the instruction if along a different path it reaches a loop
  /// entrance/loop exit.
  UnhoistableInstSetCallbacks Callbacks;

public:
  RCIdentityToInstSetMap(UnhoistableInstSetCallbacks Callbacks)
      : Callbacks(Callbacks) {}
  RCIdentityToInstSetMap(const RCIdentityToInstSetMap &Map)
      : Insts(Map.Insts), Callbacks(Map.Callbacks) {}
  RCIdentityToInstSetMap &operator=(const RCIdentityToInstSetMap &Map) {
    Insts = Map.Insts;
    Callbacks = Map.Callbacks;
    return *this;
  }

  using iterator = decltype(Insts)::iterator;
  using const_iterator = decltype(Insts)::const_iterator;

  iterator begin() { return Insts.begin(); }
  iterator end() { return Insts.end(); }
  const_iterator begin() const { return Insts.begin(); }
  const_iterator end() const { return Insts.end(); }

  void clear();

  void reset();

  iterator find(SILValue V) { return Insts.find(V); }
  const_iterator find(SILValue V) const { return Insts.find(V); }

  unsigned size() const { return Insts.size(); }

  bool erase(SILValue V);

  ImmutablePointerSet<SILInstruction> *&operator[](SILValue V) { return Insts[V]; }

  void dump();
  void print(llvm::raw_ostream &os);
};

/// A data structure representing the current tracked state for a region.
class LHRegionState {
  /// The region that this state is matched to.
  LoopRegion *R;

  /// A list of instructions in this block that /may/ perform uniqueness check
  /// operations.
  llvm::SmallVector<SILInstruction *, 2> UniquenessChecks;

  /// The set of generated retains in the block discovered during the top down
  /// traversal. We use this during the bottom up traversal so that we do not
  /// need to revisit instructions.
  llvm::SmallVector<SILInstruction *, 2> GenRetains;

  /// A map from a specific RCIdentity to the set of hoistable releases on that
  /// RCIdentity that reach this block.
  RCIdentityToInstSetMap Retains;

  /// A map from a specific RCIdentity to the set of hoistable releases on that
  /// RCIdentity that reach this block.
  RCIdentityToInstSetMap Releases;

  /// Is this loop region an exiting region that is ignoreable.
  ///
  /// This can only happen currently if all of the region's exiting edges are to
  /// leaking blocks. In the future when we handle exiting references from
  /// loops, they will be included here.
  bool IgnoreableExitRegion = false;

public:
  LHRegionState(LoopRegion *Region,
                UnhoistableInstSetCallbacks &UnhoistableRetainCallbacks,
                UnhoistableInstSetCallbacks &UnhoistableReleaseCallbacks)
      : R(Region), UniquenessChecks(), GenRetains(),
        Retains(UnhoistableRetainCallbacks),
        Releases(UnhoistableReleaseCallbacks) {}
  ~LHRegionState() = default;

  LHRegionState(const LHRegionState &) = delete;
  LHRegionState(LHRegionState &&) = delete;

  void performTopDownDataflow(LHContext &State);
  void performBottomUpDataflow(LHContext &State);
  void summarize(LHContext &State);

  /// Returns true if \p Retain is an increment associated with \p V at the end
  /// of
  /// this state's region.
  bool hasRetainForValue(SILValue V, SILInstruction *Retain) const {
    auto Iter = Retains.find(V);
    if (Iter == Retains.end())
      return false;
    return Iter->second->count(Retain);
  }

  /// Returns true if \p Release is a decrement associated with \p V at the
  /// beginning of this state's region.
  bool hasReleaseForValue(SILValue V, SILInstruction *Release) const {
    auto Iter = Releases.find(V);
    if (Iter == Releases.end())
      return false;
    return Iter->second->count(Release);
  }

  const LoopRegion *getRegion() const { return R; }
  const decltype(Retains) &getRetains() const { return Retains; }

  const decltype(Releases) &getReleases() const { return Releases; }

  /// Returns true if:
  ///
  /// 1. \p self does not have any non-local successors.
  /// 2. \p self has non-local successors but no local successors.
  /// 3. \p self has non-local successors and local successors, but all
  /// non-local successors either leak memory or can be ignored since they are
  /// early exits that already have a release.
  ///
  /// Returns false otherwise.
  bool handleNonLocalSuccessors(LHContext &State);

  /// Is this an exiting region all of whose exiting edges can be ignored for
  /// our purposes.
  bool isIgnoreableExitRegion() const { return IgnoreableExitRegion; }

private:
  void performTopDownBlockDataflow(LHContext &State);
  void performTopDownLoopDataflow(LHContext &State);
  void performBottomUpBlockDataflow(LHContext &State);
  void performBottomUpLoopDataflow(LHContext &State);
  void performTopDownMerge(LHContext &State);
  void performBottomUpMerge(LHContext &State);
};

enum class RCModifierKind {
  
};

struct HoistableSets {
  ImmutablePointerSet<SILInstruction> *Retains;
  ImmutablePointerSet<SILInstruction> *Releases;

  /// The set of blocks where we need to insert balancing retains or releases
  /// due to the need to balance ref counts.
  llvm::SmallPtrSet<LoopRegion *, 1> ReleaseInsertPts;
};

/// A class containing the global state that we need for our computations. This
/// is separate from LoopHoister so that we can pass it into
/// LHRegionState where it is needed.
struct LHContext {
  /// A reference to the module that the function we are optimizing is contained
  /// within.
  SILModule &Mod;

  /// The allocator that we use to allocate our data structures.
  llvm::BumpPtrAllocator Allocator;

  /// A map from the ID of a region to the state that we are tracking for it.
  std::vector<LHRegionState *> RegionIDToStateMap;

  AliasAnalysis *AA;

  /// The loop region function info that we are using to determine loop
  /// dataflow.
  LoopRegionFunctionInfo *LRFI;

  /// The rc identity analysis we use to pair retains, releases.
  RCIdentityFunctionInfo *RCFI;

  /// The program termination function info we use to determine exits we can
  /// ignore.
  ProgramTerminationFunctionInfo *PTFI;

  SideEffectAnalysis *SEA;

  ImmutablePointerSetFactory<SILInstruction> SetFactory;

  bool MadeChange = false;

  /// A set of retain/releases that are disqualified from being hoisted since
  /// they are reachable or can be reached from a retain/release set that we are
  /// trying to hoist.
  ///
  /// TODO: Be clearer here. Conceptually what I am trying to say is that if we
  /// have a strong_retain and then we see another strong_retain along one path
  /// (so forget the first), but the first strong_retain along another path
  /// reaches the loop header, we can not hoist the first retain since if we
  /// did, we would be removing a retain along a path. A similar thing can
  /// happen with releases.
  ImmutablePointerSet<SILInstruction> *UnhoistableRetains =
    ImmutablePointerSetFactory<SILInstruction>::getEmptySet();
  ImmutablePointerSet<SILInstruction> *UnhoistableReleases =
    ImmutablePointerSetFactory<SILInstruction>::getEmptySet();

  // We need to store the actual SILInstruction that needs to be moved along
  // this block if we hoist it. Otherwise, if we have a situation where there is
  // a later SILInstruction that we run into and thus hoist the later, rather
  // than the early one, we do not try to compensate the release that is not
  // being hoisted.
  llvm::SmallVector<std::pair<ImmutablePointerSet<SILInstruction> *,
                              SILBasicBlock *>, 4>
  EdgesInNeedOfCompensation;

  /// A set of instructions that we have removed from the CFG but have not
  /// deleted yet. We need to delete instructions after we are done processing
  /// instructions to prevent a reallocated SILInstruction to be assigned an
  /// ImmutablePointerSet set that is associated with a freed pointer.
  DelayedInstructionDestroyer Destroyer;

  LHContext(SILModule &M, AliasAnalysis *AA, LoopRegionFunctionInfo *LRFI, RCIdentityFunctionInfo *RCFI,
            ProgramTerminationFunctionInfo *PTFI,
            SideEffectAnalysis *SEA);
  LHContext(const LHContext &) = delete;
  LHContext(LHContext &&) = delete;
  ~LHContext();

  bool isUniquenessCheck(SILInstruction &I, SILValue V = SILValue());

  void addUnhoistableRetain(SILInstruction *I) {
    UnhoistableRetains = SetFactory.merge(UnhoistableRetains, I);
  }

  void addUnhoistableRelease(SILInstruction *I) {
    UnhoistableReleases = SetFactory.merge(UnhoistableReleases, I);
  }

  void runOnLoopRegion(LoopRegion *R);

  void performTopDownDataflow(LoopRegion *R);
  void performBottomUpDataflow(LoopRegion *R);
  bool pairIncrementsDecrements(LoopRegion *R);
  void summarizeLoop(LoopRegion *R);

  void findHoistableIncrements(
    SILLoop *L, const RCIdentityToInstSetMap &DataflowResult,
    llvm::SmallDenseMap<SILValue, HoistableSets, 4> &HoistableSets,
    ImmutablePointerSet<SILInstruction> *UnhoistableIncrements,
    llvm::SmallPtrSet<SILValue, 4> &UnhoistableValues);
  bool findHoistableDecrementsForValue(
      SILValue V, HoistableSets &HSets, LHRegionState *BackedgeState,
      ImmutablePointerSet<SILInstruction> *UnhoistableDecrements);
  void findHoistableDecrements(
      LoopRegion *R, const RCIdentityToInstSetMap &DataflowResult,
      llvm::SmallDenseMap<SILValue, HoistableSets, 4> &HoistableSets,
      llvm::SmallPtrSet<SILValue, 4> &UnhoistableValues);
  void markInvertedOrderSetsUnhoistable(
      llvm::SmallDenseMap<SILValue, HoistableSets, 4> &HoistableSets,
      llvm::SmallPtrSet<SILValue, 4> &UnhoistableValues);
};

class LoopHoister : public SILLoopVisitor {
  /// All of the state needed for our computation.
  LHContext Ctx;

public:
  LoopHoister(SILFunction *F, SILLoopInfo *LI, AliasAnalysis *AA, RCIdentityFunctionInfo *RCFI,
              LoopRegionFunctionInfo *LRFI,
              ProgramTerminationFunctionInfo *PTFI,
              SideEffectAnalysis *SEA);

  ~LoopHoister() = default;

  void runOnFunction(SILFunction *F) override;
  void runOnLoop(SILLoop *L) override;
  bool madeChange() const { return Ctx.MadeChange; }
};

} // end swift namespace

#endif

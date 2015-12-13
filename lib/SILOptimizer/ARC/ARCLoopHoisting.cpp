//===--- ARCLoopHoisting.cpp ----------------------------------------------===//
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
///
/// \file
///
/// This file contains the ARCLoopHoisting algorithm. It enables code motion of
/// retains/releases out of loops in a safe manner. For more details about the
/// motiviation for the algorithm please see the "ARC Loop Hoisting" section of
/// ARCOptimization.rst. This header comment just describes the algorithm
/// inself.
///
/// First note that since we use the Loop Region Analysis, we are able to
/// perform pessimistic dataflows with reasoning about the loops themselves to
/// perform optimization in the face of loops. In the following when I say CFG,
/// I mean the CFG defined by the subregions of a specific loop region.
///
/// The general algorithm consists of a top down and then a bottom up dataflow
/// pass whose results are then processed to determine what we can hoist. We
/// always remove at most one retain/release set for each RCIdentity at a time
/// to simplify things in the first iteration of this. In the future this
/// iteration can be removed.
///
/// Dataflow
/// --------
///
/// A top down pass is made over the control flow graph. At any point during
/// that pass, there is one release set associated with a specific RC
/// Identity. Upon visiting a new decrement, the old release set that was being
/// tracked is added to the unhoistable release set and then we put the new
/// decrement into our newly emptied tracked release set. At merge points, if we
/// have release sets along all paths for a specific RCIdentity, we combine
/// together the release sets. Any merge point for which we do not have release
/// sets along all predecessors, we add all of the relevant retain/releases to
/// the unhoistable set.
///
/// During our dataflow if we see a uniqueness check, we clear all information
/// that we are tracking and add all tracked releases to the unhoistable release
/// pool.
///
/// The bottom up pass is similar except with retains and trying to prove a
/// retain can get to the header of the loop.
///
/// The reason why we use the unhoistable release set is that unlike in ARC
/// Sequence Optimization, we do not cross reference the dataflow passes in a
/// manner that would cause a value that is unoptimizable along one path to not
/// get optimized.
///
/// We additionally make sure that no retain is reachable by a release.
///
/// Once these dataflow passes are done, then we know that any set of releases
/// that are releases that reach the backedge block in the dataflow and that
/// also have retains in the header can be hoisted. Before we do that though, we
/// need to visit all loop exits and see if any of the retains/releases passed
/// them by during the dataflow. In such a case, we need to compensate that
/// early exit with a release.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-arc-loop-hoisting"
#include "ARCLoopHoisting.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopRegionAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/Analysis/ProgramTerminationAnalysis.h"
#include "swift/SILOptimizer/Analysis/SideEffectAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

STATISTIC(NumLoopHoisting, "Number of times a retain, release set was hoisted");
STATISTIC(NumHoistedRetains, "Number of retains hoisted");
STATISTIC(NumHoistedReleases, "Number of releases hoisted");

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

static bool isDecrement(const SILInstruction &I) {
  return isa<StrongReleaseInst>(I) || isa<ReleaseValueInst>(I);
}

static bool isIncrement(const SILInstruction &I) {
  return isa<StrongRetainInst>(I) || isa<RetainValueInst>(I);
}

// For now do not use side effect analysis and just assume that is_unique and
// all full apply sites can be a uniqueness check. This will be replaced by side
// effect analysis.
static bool isUniquenessCheck(const SILInstruction &I) {
  // For now assume that nothing is a uniqueness check.
  return false;
}

//===----------------------------------------------------------------------===//
//                           RCIdentityToInstSetMap
//===----------------------------------------------------------------------===//

void RCIdentityToInstSetMap::clear() {
  for (auto &P : Insts) {
    for (auto *I : *P.second) {
      DEBUG(llvm::dbgs() << "    Adding to unhoistable set: " << *I);
      Callbacks.MakeUnhoistable(I);
    }
  }
  Insts.clear();
}

void RCIdentityToInstSetMap::reset() { Insts.clear(); }

// TODO: This should be made more efficient.
bool RCIdentityToInstSetMap::erase(SILValue V) {
  auto Iter = find(V);
  if (Iter == end())
    return false;
  for (auto *I : *Iter->second) {
    DEBUG(llvm::dbgs() << "    Adding to unhoistable set: " << *I);
    Callbacks.MakeUnhoistable(I);
  }
  Insts.erase(V);
  return true;
}

void RCIdentityToInstSetMap::print(llvm::raw_ostream &os) {
  for (auto &P : Insts) {
    llvm::dbgs() << "        RCID: " << P.first;
    for (auto *I : *P.second) {
      bool isHoistable = !Callbacks.IsUnhoistable(I);
      llvm::dbgs() << "            CanHoist: " << (isHoistable ? "yes" : "no ")
                   << " Inst: " << *I;
    }
  }
}

void RCIdentityToInstSetMap::dump() { DEBUG(print(llvm::dbgs())); }

//===----------------------------------------------------------------------===//
//                             Top Down Dataflow
//===----------------------------------------------------------------------===//

void LHRegionState::performTopDownMerge(LHContext &State) {
  Releases.reset();
  GenRetains.clear();

  // TODO: Remove this clear it should not be necessary.
  UniquenessChecks.clear();

  DEBUG(llvm::dbgs() << "Performing top down merge for: " << R->getID()
                     << "\n");
  auto Preds = R->getPreds();
  auto PI = Preds.begin(), PE = Preds.end();
  if (PI == PE) {
    DEBUG(llvm::dbgs() << "    No predecessors... early returning.\n");
    return;
  }

  Releases = State.RegionIDToStateMap[*PI]->Releases;
  DEBUG(llvm::dbgs() << "    Initializing region with pred " << *PI << "\n");
  DEBUG(Releases.dump());

  ++PI;

  llvm::SmallVector<SILValue, 2> ValuesToDelete;
  llvm::SmallVector<SILInstruction *, 4> InstsToAdd;
  for (; PI != PE; ++PI) {
    auto &PState = *State.RegionIDToStateMap[*PI];

    for (auto &P : Releases) {
      auto Iter = PState.Releases.find(P.first);
      if (Iter == PState.Releases.end()) {
        ValuesToDelete.push_back(P.first);
        continue;
      }

      for (auto *I : *Iter->second) {
        P.second = State.SetFactory.merge(P.second, I);
      }
    }

    // Mark as unhoistable any releases associated with a SILValue in PStates
    // for a SILValue that we do not have in P.
    for (auto &P : PState.Releases) {
      auto Iter = Releases.find(P.first);
      if (Iter != Releases.end()) {
        continue;
      }

      // We need to make sure that values that we can not merge are never
      // hoisted. So even though we know it is not in releases, we abuse its
      for (auto *I : *P.second) {
        DEBUG(llvm::dbgs() << "    Marking unhoistable: " << *I);
        State.addUnhoistableRelease(I);
      }
    }

    DEBUG(llvm::dbgs() << "    After merging pred " << *PI << "\n");
    DEBUG(Releases.dump());
  }

  for (SILValue V : ValuesToDelete) {
    DEBUG(llvm::dbgs() << "    Removing value: " << V);
    Releases.erase(V);
  }

  DEBUG(llvm::dbgs() << "    After merging: \n");
  DEBUG(Releases.dump());
}

void LHRegionState::performTopDownDataflow(LHContext &State) {
  performTopDownMerge(State);

  // We can only hoist releases through this block if it is not an unknown
  // control flow edge tail. If it is an unknown control flow edge tail, we need
  // to not propagate the releases through and then black list the releases a
  // being unhoistable. We can still hoist any releases that are generated by
  // this block though, so we do not exit early.
  if (R->isUnknownControlFlowEdgeHead()) {
    DEBUG(llvm::dbgs() << "    Can not hoist pred state through block. "
                          "Clearing state before dataflow.\n");
    Releases.clear();
  }

  // Now perform the dataflow for this block.
  if (R->isBlock()) {
    performTopDownBlockDataflow(State);
  } else {
    performTopDownLoopDataflow(State);
  }

  DEBUG(llvm::dbgs() << "    After Top Down Dataflow:\n");
  DEBUG(Releases.dump());

  // Now we have the set of releases that can be sunk to the end of this
  // block. Any early exit edges that go past this point need to have a
  // compensating release put on edges.
  if (R->hasLocalSuccs()) {
    for (unsigned NonLocalSuccID : R->getNonLocalSuccs()) {
      SILBasicBlock *NonLocalSuccBlock =
        State.LRFI->getRegionForNonLocalSuccessor(R, NonLocalSuccID)->getBlock();
      if (State.PTFI->isProgramTerminatingBlock(NonLocalSuccBlock))
        continue;
      for (auto &Pair : Releases) {
        DEBUG(llvm::dbgs() << "Adding a compensating edge!\n");
        State.EdgesInNeedOfCompensation.push_back({Pair.second, NonLocalSuccBlock});
      }
    }
  }

  // Finally, determine if we can hoist releases from this block and if we can
  // not do so, mark all of the releases that we are currently tracking as being
  // unhoistable. We can only hoist releases the region is not an unknown
  // control flow edge tail.
  //
  // Unlike the case where we are processing bottom up, we can ignore the
  // possibility of having non-local successors/local successors since if this
  // release is not able to hit all other exit blocks from this point (due to us
  // tracking a different release or a dataflow merge failure), we will have
  // added the release to the unhoistable release set.
  if (!R->isUnknownControlFlowEdgeTail()) {
    return;
  }

  // Otherwise, we need to clear all releases we are tracking and mark them as
  // unhoistable so we are conservatively correct.
  DEBUG(llvm::dbgs() << "    Can't hoist! Clearing state!\n");
  Releases.clear();
}

void LHRegionState::performTopDownBlockDataflow(LHContext &State) {
  // For a block we perform the following work:
  //
  // 1. We merge any releases that are being tracked by our predecessors into
  // our release list.
  //
  // 2. We perform the top down dataflow. While doing this we prepare for the
  // bottom up dataflow ensuring we do not visit instructions twice. With that
  // in mind:
  //
  //    a. Any retains we see, we immediately add to our tracked retain
  //       list. After we see a potential uniqueness check, we stop adding
  //       retains.
  //
  //    b. Any releases we see, we immediately add to our tracked release
  //       list. We clear the release list whenever we see a potential
  //       uniqueness check.
  for (auto &II : *R->getBlock()) {
    if (UniquenessChecks.empty() && isIncrement(II)) {
      GenRetains.push_back(&II);
    }

    if (isDecrement(II)) {
      DEBUG(llvm::dbgs() << "    Found a decrement: " << II);
      SILValue V = State.RCFI->getRCIdentityRoot(II.getOperand(0));
      Releases.erase(V);
      Releases[V] = State.SetFactory.get(&II);
    }

    if (State.isUniquenessCheck(II)) {
      if (isa<FullApplySite>(&II)) {
        FullApplySite FAS(&II);
        SILFunction *Callee = FAS.getCalleeFunction();
        if (Callee) {
          DEBUG(llvm::dbgs() << "    Clearing b/c of uniqueness function: " << Callee->getName() << "\n");
        } else {
          DEBUG(llvm::dbgs() << "    Clearing b/c of uniqueness check: " << II);
        }
      } else {
        DEBUG(llvm::dbgs() << "    Clearing b/c of uniqueness check: " << II);
      }
      UniquenessChecks.push_back(&II);
      Releases.clear();
    }
  }
}

void LHRegionState::performTopDownLoopDataflow(LHContext &State) {
  // For now if we have any uniqueness checks, clear any tracked releases.
  if (UniquenessChecks.empty())
    return;
  Releases.clear();
}

//===----------------------------------------------------------------------===//
//                             Bottom Up Dataflow
//===----------------------------------------------------------------------===//

void LHRegionState::performBottomUpMerge(LHContext &State) {
  Retains.reset();

  DEBUG(llvm::dbgs() << "Performing bottom up merge for: " << R->getID()
                     << "\n");

  if (!R->hasLocalSuccs()) {
    DEBUG(llvm::dbgs() << "    Region has no local successors, initializing "
                          "with empty state and returning.\n");
    return;
  }

  auto Succs = R->getLocalSuccs();
  auto SI = Succs.begin(), SE = Succs.end();

  Retains = State.RegionIDToStateMap[*SI]->Retains;
  DEBUG(llvm::dbgs() << "    Initializing region with succ " << *SI << "\n");
  DEBUG(Retains.dump());
  ++SI;

  llvm::SmallVector<SILValue, 2> ValuesToDelete;
  for (; SI != SE; ++SI) {
    auto &PState = *State.RegionIDToStateMap[*SI];
    for (auto &P : Retains) {
      auto Iter = PState.Retains.find(P.first);
      if (Iter == PState.Retains.end()) {
        ValuesToDelete.push_back(P.first);
        continue;
      }

      for (auto *I : *Iter->second) {
        P.second = State.SetFactory.merge(P.second, I);
      }
    }

    // Mark as unhoistable any retains associated with a SILValue in PStates for
    // a SILValue that we do not have in P.
    for (auto &P : PState.Retains) {
      auto Iter = Retains.find(P.first);
      if (Iter != Retains.end()) {
        continue;
      }

      // We need to make sure that values that we can not merge are never
      // hoisted. So even though we know it is not in releases, we abuse its
      for (auto *I : *P.second) {
        DEBUG(llvm::dbgs() << "    Marking unhoistable: " << *I);
        State.UnhoistableRetains = State.SetFactory.merge(State.UnhoistableRetains, I);
      }
    }

    DEBUG(llvm::dbgs() << "    After merging succ " << *SI << "\n");
    DEBUG(Retains.dump());
  }

  for (SILValue V : ValuesToDelete) {
    DEBUG(llvm::dbgs() << "    Removing value: " << V);
    Retains.erase(V);
  }

  DEBUG(llvm::dbgs() << "    After merging: \n");
  DEBUG(Retains.dump());
}

// Returns true if we can prove that all of the non-local exits of the region
// fit into one of the categories.
//
// 1. Program terminating and thus ignoreable. These are the only non-local
//    edges allowed that are not in the next level of loop.
// 2. In the next level of loop (i.e. is a local edge).
//
// Returns false otherwise.
//
// *NOTE*. Loops can only hit case 1 or case 3 due to our loop
// canonicalizations.
bool LHRegionState::handleNonLocalSuccessors(LHContext &State) {
  return true;
#if 0
  // If there are no non-local successors to handle, we are done.
  if (!R->hasNonLocalSuccs())
    return true;

  // Otherwise, make sure that either R's grandparent is the non-local
  // successors's parent or that the non-local successor is a program
  // terminating block that can be ignored.
  IgnoreableExitRegion =
      all_of(R->getNonLocalSuccs(), [this, &State](unsigned ID) -> bool {
        auto *Region = State.LRFI->getRegionForNonLocalSuccessor(R, ID);

        if (auto RegionParentID = Region->getParentID()) {
          if (auto RGrandParentID = State.LRFI->getGrandparentID(R)) {
            if (*RegionParentID == *RGrandParentID)
              return true;
          }
        }

        return State.PTFI->isProgramTerminatingBlock(Region->getBlock());
      });

  return IgnoreableExitRegion;
#endif
}

void LHRegionState::performBottomUpDataflow(LHContext &State) {
  // First perform the bottom up merge.
  performBottomUpMerge(State);

  // Then check if we can hoist any of the merged retains. We can only hoist
  // retains from other blocks through this block if the block us not an unknown
  // control flow edge tail.
  if (R->isUnknownControlFlowEdgeTail()) {
    DEBUG(llvm::dbgs()
          << "    Can not hoist succ state through block. Clearing state.\n");
    Retains.clear();
  }

  // Check if we have any non-local successors and a local successor. If we do
  // and we want to move retains past this block, we will need to insert a
  // release along that edge.
  if (R->hasLocalSuccs()) {
    for (unsigned NonLocalSuccID : R->getNonLocalSuccs()) {
      SILBasicBlock *NonLocalSuccBlock =
        State.LRFI->getRegionForNonLocalSuccessor(R, NonLocalSuccID)->getBlock();
      if (State.PTFI->isProgramTerminatingBlock(NonLocalSuccBlock))
        continue;
      for (auto &Pair : Retains) {
        State.EdgesInNeedOfCompensation.push_back({Pair.second, NonLocalSuccBlock});
      }
    }
  }

  if (R->isBlock()) {
    performBottomUpBlockDataflow(State);
  } else {
    performBottomUpLoopDataflow(State);
  }

  DEBUG(llvm::dbgs() << "    After Bottom Up Dataflow:\n");
  DEBUG(Retains.dump());

  // If this block is an unknown control flow edge head, we can not propagate
  // retains into its predecessors. If we are not an unknown control flow edge
  // head, return. Otherwise, clear all retains and mark them as unhoistable.
  if (!R->isUnknownControlFlowEdgeHead()) {
    return;
  }

  DEBUG(llvm::dbgs() << "    Unknown control flow edge head, clearing "
                        "state.\n");
  Retains.clear();
}

void LHRegionState::performBottomUpBlockDataflow(LHContext &State) {
  DEBUG(llvm::dbgs() << "Performing bottom up dataflow for: " << R->getID()
                     << "\n");
  // For the bottom up dataflow, we have already found all retains that reach
  // the bottom of this block without hitting a uniqueness check. The retains
  // are in top down order, meaning that in order to ensure that we only
  for (auto *Retain : GenRetains) {
    DEBUG(llvm::dbgs() << "    Visiting GenRetain: " << *Retain);
    SILValue V = State.RCFI->getRCIdentityRoot(Retain->getOperand(0));
    Retains.erase(V);
    Retains[V] = State.SetFactory.get(Retain);
  }
}

void LHRegionState::performBottomUpLoopDataflow(LHContext &State) {
  // For a loop, we just want to clear any retains that we are tracking that
  // could have a uniqueness check applied to them. For now we just always bail
  // in such cases.
  if (UniquenessChecks.empty())
    return;
  Retains.clear();
}

//===----------------------------------------------------------------------===//
//                              Summarize Loops
//===----------------------------------------------------------------------===//

void LHRegionState::summarize(LHContext &State) {
  assert(!R->isBlock() && "Can not summarize a block");

  // In order to summarize loops we:
  //
  // 1. Gather up all uniqueness checks that we found in the subregion.
  for (unsigned SubregionID : R->getSubregions()) {
    auto *Subregion = State.RegionIDToStateMap[SubregionID];

    auto &Checks = Subregion->UniquenessChecks;
    std::copy(Checks.begin(), Checks.end(),
              std::back_inserter(UniquenessChecks));
  }
}


//===----------------------------------------------------------------------===//
//                                 LHContext
//===----------------------------------------------------------------------===//

LHContext::LHContext(SILModule &M, AliasAnalysis *AA, LoopRegionFunctionInfo *LRFI, RCIdentityFunctionInfo *RCFI,
                     ProgramTerminationFunctionInfo *PTFI,
                     SideEffectAnalysis *SEA)
    : Mod(M), Allocator(), RegionIDToStateMap(), AA(AA), LRFI(LRFI), RCFI(RCFI), PTFI(PTFI),
      SEA(SEA), SetFactory(Allocator), Destroyer(M) {
  UnhoistableInstSetCallbacks RetainCallbacks = {
    [this](SILInstruction *I) {
      UnhoistableRetains = SetFactory.merge(UnhoistableRetains, I);
    },
    [this](SILInstruction *I) -> bool {
      return UnhoistableRetains->count(I);
    }
  };

  UnhoistableInstSetCallbacks ReleaseCallbacks = {
    [this](SILInstruction *I) {
      UnhoistableReleases = SetFactory.merge(UnhoistableReleases, I);
    },
    [this](SILInstruction *I) -> bool {
      return UnhoistableReleases->count(I);
    }
  };
  for (auto *R : LRFI->getRegions()) {
    RegionIDToStateMap.push_back(new (Allocator) LHRegionState(
        R, RetainCallbacks, ReleaseCallbacks));
  }
}

LHContext::~LHContext() {
  for (auto *R : RegionIDToStateMap) {
    R->~LHRegionState();
  }
}

// For now do not use side effect analysis and just assume that is_unique and
// all full apply sites can be a uniqueness check. This will be replaced by side
// effect analysis.
bool LHContext::isUniquenessCheck(SILInstruction &I, SILValue V) {
#if 1
  return isa<FullApplySite>(I) || isa<IsUniqueInst>(I);
#else
  if (V) {
    V = RCFI->getRCIdentityRoot(V);
  }

  // Make this more aggressive.
  if (auto *IUI = dyn_cast<IsUniqueInst>(&I)) {
    if (!V)
      return true;
    return !AA->isNoAlias(RCFI->getRCIdentityRoot(IUI->getOperand()), V);
  }

  if (auto *IUOPI = dyn_cast<IsUniqueOrPinnedInst>(&I)) {
    if (!V)
      return true;
    return !AA->isNoAlias(RCFI->getRCIdentityRoot(IUOPI->getOperand()), V);
  }

  if (!isa<FullApplySite>(&I))
    return false;
  FullApplySite FAS(&I);
  SILFunction *F = FAS.getCalleeFunction();
  if (!F)
    return true;

  const auto &Effects = SEA->getEffects(F);
  // If no ref counts are read, just return false. We are done!
  if (!Effects.mayReadRC())
    return false;

  // If global effects says we may read a ref count, return false.
  if (Effects.getGlobalEffects().mayReadRC())
    return true;

  // Otherwise, check parameters.
  auto PE = Effects.getParameterEffects();
  auto Args = FAS.getArgumentsWithoutIndirectResults();
  for (unsigned i : indices(Args)) {
    SILValue RCID = RCFI->getRCIdentityRoot(Args[i]);
    if (AA->isNoAlias(RCID, V))
      continue;
    if (!PE[i].mayReadRC())
      continue;
    return true;
  }

  // We do not have any rc reads.
  return false;
#endif
}

void LHContext::runOnLoopRegion(LoopRegion *R) {
  assert(!R->isBlock() && "Expecting a non-block region");

  // If the loop region does not have a single back edge, we need to bail.

  // We process a loop as follows:
  //
  // 1. We begin by performing a top down dataflow discovering our gen retain,
  // gen release sets. Additionally, we determine the releases that we can sink
  // to exits. In this dataflow, we visit instructions.
  //
  // 2. Then we perform a bottom up dataflow using summarized data from the top
  // down dataflow. This just ensures that we do not need to touch instructions
  // again to save compile time.
  //
  // 3. Any set of retains that we can hoist to our entry block and any releases
  // that we can sink to our exit blocks are hoisted out of the loop.
  //
  // 4. We summarize our uniqueness check list into this loop for the next loop
  // level to use.
  bool Changed = false;
  do {
    performTopDownDataflow(R);
    performBottomUpDataflow(R);
    Changed = pairIncrementsDecrements(R);
  } while (Changed);
  RegionIDToStateMap[R->getID()]->summarize(*this);
  DEBUG(llvm::dbgs() << "\n");
}

void LHContext::performTopDownDataflow(LoopRegion *R) {
  for (unsigned SubregionID : R->getSubregions()) {
    auto *Subregion = LRFI->getRegion(SubregionID);
    RegionIDToStateMap[Subregion->getID()]->performTopDownDataflow(*this);
  }
}

void LHContext::performBottomUpDataflow(LoopRegion *R) {
  // TODO: Figure out why this is segfaulting if we use a reverse_iterator.
  auto Range = R->getSubregions();
  auto II = Range.begin(), IE = Range.end();

  if (II == IE)
    return;

  --IE;
  for (; II != IE; --IE) {
    unsigned SubregionID = *IE;
    auto *Subregion = LRFI->getRegion(SubregionID);
    RegionIDToStateMap[Subregion->getID()]->performBottomUpDataflow(*this);
  }

  {
    unsigned SubregionID = *II;
    auto *Subregion = LRFI->getRegion(SubregionID);
    RegionIDToStateMap[Subregion->getID()]->performBottomUpDataflow(*this);
  }
}

static void createIncrement(SILBuilder &B, SILValue Op) {
  auto Loc = RegularLocation(SourceLoc());
  if (Op->getType().hasReferenceSemantics()) {
    B.createStrongRetain(Loc, Op);
    return;
  }

  B.createRetainValue(Loc, Op);
}

static SILInstruction *createDecrement(SILBuilder &B, SILValue Op) {
  auto Loc = RegularLocation(SourceLoc());
  if (Op->getType().hasReferenceSemantics()) {
    return B.createStrongRelease(Loc, Op);
  }
  return B.createReleaseValue(Loc, Op);
}

static bool isNonInductionVarInLoop(SILLoop *L, SILValue V) {
  // If Arg is a PHI node whose parent is in side this loop, but is not the loop
  // header, we can not hoist.
  if (auto *Arg = dyn_cast<SILArgument>(V)) {
    SILBasicBlock *BB = Arg->getParent();
    return L->contains(BB) && Arg->getParent() != L->getHeader();
  }

  if (auto *I = dyn_cast<SILInstruction>(V)) {
    // If I is inside L, we can not hoist.
    return L->contains(I);
  }

  // Be conservative and return true.
  return true;
}

void LHContext::findHoistableIncrements(
    SILLoop *L, const RCIdentityToInstSetMap &DataflowResult,
    llvm::SmallDenseMap<SILValue, HoistableSets, 4> &HoistableSets,
    ImmutablePointerSet<SILInstruction> *UnhoistableIncrements,
    llvm::SmallPtrSet<SILValue, 4> &UnhoistableValues) {
  for (auto &P : DataflowResult) {
    // If P.first is defined inside the loop, but is not a PHI node of the loop
    // header, we can not hoist retains, releases upon it.
    if (isNonInductionVarInLoop(L, P.first)) {
      UnhoistableValues.insert(P.first);
      continue;
    }

    if (std::any_of(P.second->begin(), P.second->end(),
                    [&UnhoistableIncrements](SILInstruction *I)
                        -> bool { return UnhoistableIncrements->count(I); })) {
      UnhoistableValues.insert(P.first);
      continue;
    }

    HoistableSets[P.first] = {P.second,
      ImmutablePointerSetFactory<SILInstruction>::getEmptySet(), {}};
 }
}

bool LHContext::findHoistableDecrementsForValue(
    SILValue V, HoistableSets &HSets, LHRegionState *BackedgeState,
    ImmutablePointerSet<SILInstruction> *UnhoistableDecrements) {

  // Attempt to find a set of releases associated with V. If we can not find
  // one, then we can not hoist increments/decrements for v, so return false.
  const auto &Releases = BackedgeState->getReleases();
  auto Iter = Releases.find(V);
  if (Iter == Releases.end()) {
    HSets.Releases = ImmutablePointerSetFactory<SILInstruction>::getEmptySet();
    return false;
  }

  // Next make sure that none of the decrements are in the unhoistable
  // decrements set. If one of the decrements are in the unhoistable set, then
  // we know along at least one path through the loop, this decrement was
  // forgotten in favor of a later release. If we were to hoist such a
  // release, then along the aforementioned path we would be removing 2
  // dynamic release operations instead of 1.
  if (std::any_of(Iter->second->begin(), Iter->second->end(),
                  [&UnhoistableDecrements](SILInstruction *I)
                  -> bool { return UnhoistableDecrements->count(I); })) {
    HSets.Releases = ImmutablePointerSetFactory<SILInstruction>::getEmptySet();
    return false;
  }

  // This is an exiting block which we can use for our pairing. Add its
  // releases to the HoistableSet.
  HSets.Releases = SetFactory.merge(HSets.Releases, Iter->second);

  // We can pair! Return true.
  return true;
}

void LHContext::findHoistableDecrements(
    LoopRegion *R, const RCIdentityToInstSetMap &DataflowResult,
    llvm::SmallDenseMap<SILValue, HoistableSets, 4> &HoistableSets,
    llvm::SmallPtrSet<SILValue, 4> &UnhoistableValues) {

  // Get the LHRegionState for the back edge.
  unsigned BackedgeID = *R->getBackedgeRegion();
  auto *BackedgeState = RegionIDToStateMap[BackedgeID];

  // For each SILValue...
  for (auto &P : HoistableSets) {
    // If we already know that we can not hoist retains/releases on the
    // SILValue, just skip it.
    if (UnhoistableValues.count(P.first))
      continue;

    // Then see if we have release sets for the backedge block and that these
    // release sets do not contain unhoistable release instructions. If we do
    // not, then we know that this value is not hoistable. Bail.
    if (findHoistableDecrementsForValue(P.first, P.second, BackedgeState,
                                        UnhoistableReleases))
      continue;
    UnhoistableValues.insert(P.first);
  }
}

void LHContext::markInvertedOrderSetsUnhoistable(
    llvm::SmallDenseMap<SILValue, HoistableSets, 4> &HoistableSets,
    llvm::SmallPtrSet<SILValue, 4> &UnhoistableValues) {

  using InstStatePair = std::pair<SILInstruction *, LHRegionState *>;
  llvm::SmallVector<InstStatePair, 4> RetainStates;
  llvm::SmallVector<InstStatePair, 4> ReleaseStates;

  // For each SILValue...
  for (auto &P : HoistableSets) {
    // If we already know that we can not hoist retains/releases on the
    // SILValue, just skip it.
    if (UnhoistableValues.count(P.first)) {
      continue;
    }

    // This needs to be made more efficient b/c it is O(N)^2. In general our N
    // should be relatively low since loops do not have /that/ many exits. But
    // if it is an issue, we can probably cache this information as we perform
    // the dataflow.
    for (auto *I : *P.second.Retains) {
      unsigned RegionID = LRFI->getRegion(I->getParent())->getID();
      RetainStates.push_back({I, RegionIDToStateMap[RegionID]});
    }

    for (auto *I : *P.second.Releases) {
      unsigned RegionID = LRFI->getRegion(I->getParent())->getID();
      ReleaseStates.push_back({I, RegionIDToStateMap[RegionID]});
    }

    for (auto RetainP : RetainStates) {
      for (auto ReleaseP : ReleaseStates) {
        SILBasicBlock *RetainParent = RetainP.first->getParent();

        // If the retain, release are in different blocks, see if one or the
        // other reached the block.
        if (RetainParent != ReleaseP.first->getParent()) {
          if (!RetainP.second->hasReleaseForValue(P.first, ReleaseP.first) &&
              !ReleaseP.second->hasRetainForValue(P.first, RetainP.first)) {
            continue;
          }
          UnhoistableValues.insert(P.first);
          return;
        }

        // If the retain, release ar ein the same block, make sure that the
        // release is /after/ the retain.
        if (std::find_if(RetainP.first->getIterator(), RetainParent->end(),
                         [&ReleaseP](const SILInstruction &I) -> bool {
                           return &I == ReleaseP.first;
                         }) != RetainParent->end()) {
          continue;
        }

        // If we can not prove any of these things, we can not hoist this
        // retain.
        UnhoistableValues.insert(P.first);
        return;
      }
    }
  }
}

bool LHContext::pairIncrementsDecrements(LoopRegion *R) {
  // Put all of our cleanups in a defer block at the beginning so we get
  // RAII cleanups where ever we return. This will prevent careless mistakes and
  // more importantly centralize logic.
  defer {
    UnhoistableRetains = ImmutablePointerSetFactory<SILInstruction>::getEmptySet();
    UnhoistableReleases  = ImmutablePointerSetFactory<SILInstruction>::getEmptySet();
    EdgesInNeedOfCompensation.clear();
  };

  unsigned LoopHeaderID = *R->getSubregions().begin();
  auto *State = RegionIDToStateMap[LoopHeaderID];

  DEBUG(llvm::dbgs() << "Attempting to pair Increments, Decrements\n");

  // If we do not have any values to hoist, bail early.
  unsigned NumRetains = State->getRetains().size();
  if (!NumRetains) {
    DEBUG(llvm::dbgs() << "    No retains?! Bailing!\n");
    return false;
  }

  SILLoop *L = R->getLoop();

  // Look at the entrances and exits of R and see if we can find any retains,
  // releases that we can hoist.
  llvm::SmallDenseMap<SILValue, HoistableSets, 4> Values;
  llvm::SmallPtrSet<SILValue, 4> UnhoistableValues;

  findHoistableIncrements(L, State->getRetains(), Values,
                          UnhoistableRetains, UnhoistableValues);

  // If we did not find any values that we /could/ hoist, return early.
  if (UnhoistableValues.size() == NumRetains) {
    DEBUG(llvm::dbgs() << "    All values are unhoistable after searching for "
                          "increments?! Bailing!\n");
    return false;
  }

  // Otherwise, attempt to pair the increments that we decided could be hoisted
  // with decrements.
  findHoistableDecrements(R, State->getReleases(), Values,
                          UnhoistableValues);

  // If we do not have any values that we can hoist, there is nothing further to
  // do, bail.
  if (UnhoistableValues.size() == NumRetains) {
    DEBUG(llvm::dbgs() << "    All values are unhoistable after searching for "
                          "decrements?! Bailing!\n");
    return false;
  }

  // Make sure that our retain/release sets are not inverted in order. We can
  // prove this by noting that our retain set must joint-dominate our release
  // set and vis-a-versa. This means that to prove this property (without any
  // loss of generality considering just retains), we just have to prove for
  // each release that either the release is not in the same block as the retain
  // and is not in the release set for that block or if the release is in the
  // same block that they are ordered appropriately.
  markInvertedOrderSetsUnhoistable(Values, UnhoistableValues);

  // If we do not have any values that we can hoist, there is nothing further to
  // do, bail.
  if (UnhoistableValues.size() == NumRetains) {
    DEBUG(llvm::dbgs() << "    All values are unhoistable after checking for "
                          "inverted order sets?! Bailing!\n");
    return false;
  }

  // Ok, we have some retains, releases that we can hoist!
  ++NumLoopHoisting;

  assert((std::distance(R->pred_begin(), R->pred_end()) == 1) &&
         "Expected all headers to have a loop pre-header");
  auto *PreheaderState = RegionIDToStateMap[*R->pred_begin()];
  SILBuilder PredBuilder(
      PreheaderState->getRegion()->getBlock()->getTerminator());
  llvm::SmallVector<std::pair<LHRegionState *, SILBuilder>, 4> SuccBuilders;

  // We only need to put the release on the exit block from the loop latch.
  //
  // A key thing to notice here is that due to the way we canonicalize loops, we
  // know that all backedge blocks have a back edge or a back edge and an exit
  // edge. A back edge block can never have two exit edges. The reason why this
  // is true is that:
  //
  // 1. If we have a loop with a single back edge with two exits, then we can
  // not have a cond_br as the terminator of the loop latch, since a block with
  // a cond_br terminator can only have two successors. Then note that the back
  // edge block will naturally be a critical edge. But we do not allow for
  // critical edges to originate from non-'cond_br' terminators, a
  // contradiction. =><=.
  //
  // 2. If we originally had a loop with multiple back edges, we introduced a
  // canonicalizing back edge that only has one successor, the loop header.
  {
    ArrayRef<unsigned> BackedgeRegions = R->getBackedgeRegions();
    assert(BackedgeRegions.size() == 1); // See discussion above.

    // Due to the way that we canonicalize loop exits, we know that any block
    // that leaves a loop can not be summarized into a loop region itself.
    LoopRegion *BackedgeRegion = LRFI->getRegion(BackedgeRegions[0]);
    assert(BackedgeRegion->isBlock());

    // We know that we only have one non-local successor.    
    auto Range = BackedgeRegion->getNonLocalSuccs();
    auto SI = Range.begin();
    if (SI != Range.end()) {
      assert(std::next(SI) == Range.end());

      // Use the non local successors ID to look up the actual non-local successor.
      auto *NonLocalSucc =
        LRFI->getRegionForNonLocalSuccessor(BackedgeRegion, *SI);
      // It is guaranteed that the region returned by
      // getRegionForNonLocalSuccessor is a block. Be careful and assert in case
      // the API changes.
      assert(NonLocalSucc->isBlock());
      
      auto *Block = NonLocalSucc->getBlock();
      // Blocks that we know are program terminating blocks and thus leak can be
      // ignored.
      //
      // We will never hit this for inner loops since any program terminating
      // block would be a far jump through multiple levels of the loop nest. On
      // the other hand, a top level loop can have a program terminating block as
      // a local successor. This handles that case.
      if (!PTFI->isProgramTerminatingBlock(Block)) {
        auto *InsertPt = Block->getTerminator();
        auto *SuccState = RegionIDToStateMap[*SI];
        SuccBuilders.push_back({SuccState, SILBuilder(InsertPt)});
      }
    }
  }

  for (auto &P : Values) {
    if (UnhoistableValues.count(P.first))
      continue;

    DEBUG(llvm::dbgs() << "Hoisting:\n");
    DEBUG(for (auto *Retain
               : *P.second.Retains) { llvm::dbgs() << "    " << *Retain; });
    DEBUG(for (auto *Release
               : *P.second.Releases) { llvm::dbgs() << "    " << *Release; });

    // See if P.first is a PHI node of the loop header. If it is so, find the
    // incoming value from the pre-header.
    SILValue V = P.first;
    if (auto *Arg = dyn_cast<SILArgument>(V)) {
      if (Arg->getParent() == L->getHeader())
        V = Arg->getIncomingValue(R->getLoop()->getLoopPreheader());
    }

    // Create any compensating retain/releases that are needed.
    //
    // This is not the most efficient implementation but given the current
    // expected uses it should be sufficient. See comment above declaration of
    // Ctx.RCCompensationInsertPtList.
    for (auto RCCompensationP : EdgesInNeedOfCompensation) {
      ImmutablePointerSet<SILInstruction> *PSet = RCCompensationP.first;

      // If we have a non-empty intersection with either of our unhoistable
      // retain or unhoistable release set, continue.
      //
      // TODO: We can check what I is and only do one of these checks. I am
      // working fast and want to come back for this later.
      if (!PSet->hasEmptyIntersection(UnhoistableRetains) ||
          !PSet->hasEmptyIntersection(UnhoistableReleases)) {
        continue;
      }

      SILInstruction *I = *PSet->begin();
      assert(isa<SILInstruction>(I));

      SILValue V = RCFI->getRCIdentityRoot(I->getOperand(0));

      if (V != P.first)
        continue;

      SILBuilder B(RCCompensationP.second, RCCompensationP.second->begin());
      auto *Decrement = createDecrement(B, P.first);
      (void)Decrement;
      DEBUG(llvm::dbgs() << "    Inserting compensating release: "
            << *Decrement);
    }

    createIncrement(PredBuilder, V);
    for (auto *Retain : *P.second.Retains) {
      Destroyer.remove(Retain);
      ++NumHoistedRetains;
    }
    P.second.Retains = ImmutablePointerSetFactory<SILInstruction>::getEmptySet();

    for (auto &P : SuccBuilders) {
      createDecrement(P.second, V);
    }

    for (auto *Release : *P.second.Releases) {
      Destroyer.remove(Release);
      ++NumHoistedReleases;
    }
    P.second.Releases = ImmutablePointerSetFactory<SILInstruction>::getEmptySet();
  }

  MadeChange = true;
  return true;
}

//===----------------------------------------------------------------------===//
//                                Loop Hoister
//===----------------------------------------------------------------------===//

// We can not hoist out of functions, so we do not do anything beyond make the
// output pretty.
void LoopHoister::runOnFunction(SILFunction *F) { DEBUG(llvm::dbgs() << "\n"); }

void LoopHoister::runOnLoop(SILLoop *L) {
  Ctx.runOnLoopRegion(Ctx.LRFI->getRegion(L));
}


LoopHoister::LoopHoister(SILFunction *F, SILLoopInfo *LI,
                         AliasAnalysis *AA,
                         RCIdentityFunctionInfo *RCFI,
                         LoopRegionFunctionInfo *LRFI,
                         ProgramTerminationFunctionInfo *PTFI,
                         SideEffectAnalysis *SEA)
    : SILLoopVisitor(F, LI), Ctx{F->getModule(), AA, LRFI, RCFI, PTFI, SEA} {
  DEBUG(llvm::dbgs() << "**** Performing ARC Loop Hoisting for " << F->getName()
                     << " ****\n\n");
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class ARCLoopHoisting : public SILFunctionTransform {
  void run() override {
    if (!getOptions().EnableARCOptimizations)
      return;

    // Canonicalize the loops, invalidating if we need to.
    auto *LA = getAnalysis<SILLoopAnalysis>();
    auto *LI = LA->get(getFunction());
    auto *DA = getAnalysis<DominanceAnalysis>();
    auto *DI = DA->get(getFunction());

    if (canonicalizeAllLoops(DI, LI)) {
      // We preserve loop info and the dominator tree.
      DA->lockInvalidation();
      LA->lockInvalidation();
      PM->invalidateAnalysis(getFunction(),
                             SILAnalysis::InvalidationKind::FunctionBody);
      DA->unlockInvalidation();
      LA->unlockInvalidation();
    }

    auto *RCFI = getAnalysis<RCIdentityAnalysis>()->get(getFunction());
    auto *LRFI = getAnalysis<LoopRegionAnalysis>()->get(getFunction());
    auto *SEA = getAnalysis<SideEffectAnalysis>();
    auto *AA = getAnalysis<AliasAnalysis>();

    ProgramTerminationFunctionInfo PTFI(getFunction());
    LoopHoister L(getFunction(), LI, AA, RCFI, LRFI, &PTFI, SEA);
    L.run();
    if (L.madeChange()) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }

  StringRef getName() override { return "ARC Loop Hoisting"; }
};

} // end anonymous namespace

SILTransform *swift::createARCLoopHoisting() { return new ARCLoopHoisting(); }

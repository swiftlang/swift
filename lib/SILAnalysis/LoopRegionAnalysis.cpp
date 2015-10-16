//===--- LoopRegionAnalysis.cpp -------------------------------------------===//
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

#define DEBUG_TYPE "sil-loop-region-analysis"
#include "swift/SILAnalysis/LoopRegionAnalysis.h"
#include "swift/Basic/Range.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/GraphWriter.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                 LoopRegion
//===----------------------------------------------------------------------===//

LoopRegion::BlockTy *LoopRegion::getBlock() const {
  return Ptr.get<BlockTy *>();
}

LoopRegion::LoopTy *LoopRegion::getLoop() const {
  return Ptr.get<LoopTy *>();
}

LoopRegion::FunctionTy *LoopRegion::getFunction() const {
  return Ptr.get<FunctionTy *>();
}

void LoopRegion::dump() const {
  print(llvm::outs());
  llvm::outs() << "\n";
}

void LoopRegion::dumpName() const {
  printName(llvm::outs());
  llvm::outs() << "\n";
}

void LoopRegion::printName(llvm::raw_ostream &os) const {
  if (isBlock()) {
    os << "BB" << getID();
    return;
  }
  if (isLoop()) {
    os << "Loop" << getID();
    return;
  }
  assert(isFunction());
  os << "Function" << getID();
  return;
}

void LoopRegion::print(llvm::raw_ostream &os, bool isShort) const {
  os << "(region id:" << ID;
  if (isShort) {
    os << ")";
    return;
  }
  os << " kind:";
  if (isBlock()) {
    os << "bb  ";
  } else if (isLoop()) {
    os << "loop";
  } else if (isFunction()) {
    os << "func";
  } else {
    llvm_unreachable("Unknown region type");
  }

  os << " ucfh:" << (IsUnknownControlFlowEdgeHead? "true " : "false")
     << " ucft:" << (IsUnknownControlFlowEdgeTail? "true " : "false");
}

llvm::raw_ostream &llvm::operator<<(llvm::raw_ostream &os, LoopRegion &LR) {
  LR.print(os);
  return os;
}

LoopRegion::SuccRange LoopRegion::getSuccs() {
  auto Range = InnerSuccRange(Succs.begin(), Succs.end());
  return SuccRange(Range, SuccessorID::ToLiveSucc());
}

LoopRegion::LocalSuccRange LoopRegion::getLocalSuccs() {
  auto Range = InnerSuccRange(Succs.begin(), Succs.end());
  return LocalSuccRange(Range, SuccessorID::ToLiveLocalSucc());
}

LoopRegion::NonLocalSuccRange LoopRegion::getNonLocalSuccs() {
  auto Range = InnerSuccRange(Succs.begin(), Succs.end());
  return NonLocalSuccRange(Range, SuccessorID::ToLiveNonLocalSucc());
}

/// Replace OldSuccID by NewSuccID, just deleting OldSuccID if what NewSuccID
/// is already in the list.
void LoopRegion::replaceSucc(unsigned OldSuccID, unsigned NewSuccID,
                             bool IsNonLocal) {
  auto NewSucc = SuccessorID(NewSuccID, IsNonLocal);
  bool FoundResult = false;

  DEBUG(llvm::dbgs() << "                Replacing " << OldSuccID << " with "
                     << NewSuccID << ":" << (IsNonLocal ? "nonlocal" : "local")
                     << "\n");
  bool AlreadyHaveSucc = std::count(Succs.begin(), Succs.end(), NewSucc);
  for (auto &SuccID : Succs) {
    DEBUG(llvm::dbgs() << "                    Visiting Succ: " << SuccID.ID
                       << "; " << (SuccID.IsDead ? "dead" : "live") << " "
                       << (SuccID.IsNonLocal ? "nonlocal" : "local") << "\n");
    // If SuccID.ID equals NewSuccID, then we know that we already have a
    if (SuccID.ID != OldSuccID) {
      DEBUG(llvm::dbgs() << "                        Succ does not equal old "
                            "succ id, skipping!\n");
      continue;
    }

    if (AlreadyHaveSucc) {
      DEBUG(llvm::dbgs() << "                        Already have a version of "
                            "this successor, marking this as dead");
      FoundResult = true;
      SuccID.IsDead = true;
      continue;
    }

    DEBUG(llvm::dbgs() << "                        Do not have a version of "
                          "this successor, replacing.\n");
    SuccID = NewSucc;
    FoundResult = true;
  }

  assert(FoundResult && "Did not find any successor entries for old succ?!");
}

//===----------------------------------------------------------------------===//
//                           LoopRegionFunctionInfo
//===----------------------------------------------------------------------===//

LoopRegionFunctionInfo::LoopRegionFunctionInfo(FunctionTy *F,
                                               PostOrderFunctionInfo *PI,
                                               LoopInfoTy *LI)
    : F(F), Allocator(), BBToIDMap(), LoopToIDMap(),
#ifndef NDEBUG
      IDToRegionMap(PI->size()), AllBBRegionsCreated(false) {
#else
      IDToRegionMap(PI->size()) {
#endif
  DEBUG(llvm::dbgs() << "**** LOOP REGION FUNCTION INFO ****\n");
  DEBUG(llvm::dbgs() << "Analyzing function: " << F->getName() << "\n");
  initializeBlockRegions(PI, LI);
  initializeLoopRegions(LI);
  initializeFunctionRegion(LI->getTopLevelLoops());
#ifndef NDEBUG
  verify();
#endif
}

void LoopRegionFunctionInfo::verify() {
#ifndef NDEBUG
  llvm::SmallVector<unsigned, 8> UniquePredList;
  llvm::SmallVector<LoopRegion::SuccessorID, 8> UniqueSuccList;
  for (auto *R : IDToRegionMap) {
    // Make sure that our region has successor/pred lists without duplicates. We
    // do not care if the successor or predecessor lists are sorted, just that
    // they are unique.
    {
      assert(UniqueSuccList.empty());
      std::copy(R->Succs.begin(), R->Succs.end(), UniqueSuccList.end());
      std::sort(UniqueSuccList.begin(), UniqueSuccList.end());
      auto End = UniqueSuccList.end();
      assert(End == std::unique(UniqueSuccList.begin(), UniqueSuccList.end()) &&
             "Expected UniqueSuccList to not have any duplicate elements");
      UniqueSuccList.clear();
    }

    {
      assert(UniquePredList.empty());
      std::copy(R->Preds.begin(), R->Preds.end(), UniquePredList.end());
      std::sort(UniquePredList.begin(), UniquePredList.end());
      auto End = UniquePredList.end();
      assert(End == std::unique(UniquePredList.begin(), UniquePredList.end()) &&
             "Expected UniquePredList to not have any duplicate elements");
      UniquePredList.clear();
    }

    // If this node does not have a parent, it should have no non-local
    // successors.
    if (!R->ParentID.hasValue()) {
      auto NLSuccs = R->getNonLocalSuccs();
      assert(NLSuccs.begin() == NLSuccs.end() &&
             "Can not have non local "
             "successors without a parent node");
      continue;
    }

    // If this node /does/ have a parent, make sure that all non-local
    // successors point to a successor in the parent and match whether or not it
    // is dead.
    auto *ParentRegion = getRegion(*R->ParentID);
    unsigned NumParentSuccs = ParentRegion->succ_size();
    for (LoopRegion::SuccessorID ID : R->Succs) {
      // Skip local successors. We are not verifying anything here.
      if (!ID.IsNonLocal)
        continue;
      assert(ID.ID < NumParentSuccs && "Non local successor pointing off the "
                                       "parent node successor list?!");
      assert(ParentRegion->Succs[ID.ID].IsDead == ID.IsDead &&
             "non-local successor edge sources should have the same liveness "
             "properties as non-local successor edge targets");
    }
  }
#endif
}

//===---
// Region Creation Functions
//

LoopRegionFunctionInfo::RegionTy *
LoopRegionFunctionInfo::getRegion(BlockTy *BB) const {
  assert(AllBBRegionsCreated && "All BB Regions have not been created yet?!");
  // Check if we have allocated a region for this BB. If so, just return it.
  auto Iter = BBToIDMap.find(BB);
  if (Iter != BBToIDMap.end()) {
    return IDToRegionMap[Iter->second];
  }
  llvm_unreachable("Unknown BB?!");
}

LoopRegionFunctionInfo::RegionTy *
LoopRegionFunctionInfo::getRegion(FunctionTy *F) const {
  if (FunctionRegionID.hasValue()) {
    return IDToRegionMap[FunctionRegionID.getValue()];
  }

  auto &Self = const_cast<LoopRegionFunctionInfo &>(*this);
  unsigned Idx = IDToRegionMap.size();
  unsigned NumBytes = sizeof(RegionTy) + sizeof(LoopRegion::SubregionData);
  void *Memory = Self.Allocator.Allocate(NumBytes, alignof(RegionTy));
  auto *R = new (Memory) RegionTy(F, Idx);
  new ((void *)&R[1]) LoopRegion::SubregionData();
  Self.IDToRegionMap.push_back(R);
  Self.FunctionRegionID = Idx;
  return R;
}

LoopRegionFunctionInfo::RegionTy *
LoopRegionFunctionInfo::getRegion(LoopTy *Loop) const {
  auto Iter = LoopToIDMap.find(Loop);
  if (Iter != LoopToIDMap.end()) {
    return IDToRegionMap[Iter->second];
  }

  auto &Self = const_cast<LoopRegionFunctionInfo &>(*this);
  unsigned Idx = IDToRegionMap.size();
  unsigned NumBytes = sizeof(RegionTy) + sizeof(LoopRegion::SubregionData);
  void *Memory = Self.Allocator.Allocate(NumBytes, alignof(RegionTy));
  auto *R = new (Memory) RegionTy(Loop, Idx);
  new ((void *)&R[1]) LoopRegion::SubregionData();
  Self.IDToRegionMap.push_back(R);
  Self.LoopToIDMap[Loop] = Idx;
  return R;
}

LoopRegionFunctionInfo::RegionTy *
LoopRegionFunctionInfo::createRegion(BlockTy *BB, unsigned RPONum) {
  assert(!AllBBRegionsCreated && "All BB Regions have been created?!");
  // Check if we have allocated a region for this BB. If so, just return it.
  auto Iter = BBToIDMap.find(BB);
  if (Iter != BBToIDMap.end()) {
    return IDToRegionMap[Iter->second];
  }

  unsigned Idx = RPONum;
  auto *R = new (Allocator) RegionTy(BB, RPONum);
  IDToRegionMap[RPONum] = R;
  BBToIDMap[BB] = Idx;
  return R;
}

//===---
// Block Region Initialization
//

void LoopRegionFunctionInfo::initializeBlockRegionSuccessors(
    BlockTy *BB, RegionTy *BBRegion, PostOrderFunctionInfo *PI) {
  for (auto &SuccIter : BB->getSuccessors()) {
    unsigned SuccRPOIndex = *PI->getRPONumber(SuccIter.getBB());
    auto *SuccRegion = createRegion(SuccIter.getBB(), SuccRPOIndex);
    BBRegion->addSucc(SuccRegion);
    SuccRegion->addPred(BBRegion);
    DEBUG(llvm::dbgs() << "    Succ: ";
          SuccIter.getBB()->printAsOperand(llvm::dbgs());
          llvm::dbgs() << " RPONum: " << SuccRPOIndex << "\n");
  }
}

void LoopRegionFunctionInfo::markIrreducibleLoopPredecessorsOfNonLoopHeader(
    BlockTy *NonHeaderBB, RegionTy *NonHeaderBBRegion,
    PostOrderFunctionInfo *PI) {
  for (BlockTy *Pred : NonHeaderBB->getPreds()) {
    // If we do not have an RPO number for a predecessor, it is because the
    // predecessor is unreachable and a pass did not clean up after
    // itself. Just ignore it, it will be cleaned up by simplify-cfg.
    auto PredRPONumber = PI->getRPONumber(Pred);
    if (!PredRPONumber || *PredRPONumber < NonHeaderBBRegion->getRPONumber())
      continue;

    auto *PredRegion = createRegion(Pred, *PredRPONumber);
    DEBUG(llvm::dbgs() << "    Backedge: ";
          Pred->printAsOperand(llvm::dbgs());
          llvm::dbgs() << " " << PredRegion->getID() << "\n");

    // We mark the head/tail as unknown control flow regions since in CFGs like
    // the following:
    //
    //         /------\
    //         v      |
    // BB0 -> BB1 -> BB2 -> BB4
    //  |             |^
    //  |             v|
    //  \----------> BB3
    //
    // We need to ensure that the edge from BB0 -> BB3 does not propagate any
    // dataflow state into the irreducible loop even if the back edge we see is
    // from BB3 -> BB2.
    NonHeaderBBRegion->IsUnknownControlFlowEdgeHead = true;
    NonHeaderBBRegion->IsUnknownControlFlowEdgeTail = true;
    PredRegion->IsUnknownControlFlowEdgeHead = true;
    PredRegion->IsUnknownControlFlowEdgeTail = true;
  }
  DEBUG(llvm::dbgs() << "\n");
}

void
LoopRegionFunctionInfo::
markMultipleLoopLatchLoopBackEdges(RegionTy *LoopHeaderRegion, LoopTy *Loop,
                                   PostOrderFunctionInfo *PI) {
  llvm::SmallVector<BlockTy *, 4> Latches;
  Loop->getLoopLatches(Latches);
  assert(Latches.size() > 1 &&
         "Assumed that this loop had multiple loop latches");

  LoopHeaderRegion->IsUnknownControlFlowEdgeHead = true;
  for (auto *LatchBB : Latches) {
    auto *LatchBBRegion = createRegion(LatchBB, *PI->getRPONumber(LatchBB));
    LatchBBRegion->IsUnknownControlFlowEdgeTail = true;
  }
}

void LoopRegionFunctionInfo::initializeBlockRegions(PostOrderFunctionInfo *PI,
                                                    LoopInfoTy *LI) {
  DEBUG(llvm::dbgs() << "Visting BB Regions:\n");

  // Initialize regions for each BB and associate RPO numbers with each BB.
  //
  // We use the RPO number of a BB as its Index in our data structures.
  for (auto P : PI->getEnumeratedReversePostOrder()) {
    BlockTy *BB = P.first;
    unsigned RPOIndex = P.second;
    auto *BBRegion = createRegion(BB, RPOIndex);
    assert(BBRegion && "Create region fail to create a BB?");
    assert(*PI->getRPONumber(BB) == RPOIndex &&
           "Enumerated Reverse Post Order out of sync with RPO number");

    DEBUG(llvm::dbgs() << "Visiting BB: "; BB->printAsOperand(llvm::dbgs());
          llvm::dbgs() << " RPO: " << RPOIndex << "\n");

    // Wire up this BB as an "initial predecessor" of all of its successors
    // and make each of its successors a successor for the region.
    initializeBlockRegionSuccessors(BB, BBRegion, PI);

    // Then try to lookup the innermost loop that BB belongs to. If we get back
    // a nullptr, then we know that the BB belongs to the function region and is
    // also not a loop header.
    auto *Loop = LI->getLoopFor(BB);
    auto *ParentRegion = Loop? getRegion(Loop) : getRegion(F);

    // Add BBRegion to the ParentRegion.
    ParentRegion->addBlockSubregion(BBRegion);

    // Determine if this basic block is part of an irreducible loop by looking
    // at all blocks that are:
    //
    // 1. Not in any loop identified by loop info.
    // 2. Or are part of an identified loop but are not a loop header.
    //
    // In such a case, check if any predecessors have a smaller PO number. If so
    // then we know that both this BB and the predecessor are boundaries of a
    // loop that is not understood by SILLoopInfo. Mark them as unknown control
    // flow boundaries. Then add the BB as a subregion to its parent region.
    DEBUG(llvm::dbgs() << "Checking Preds for Back Edges\n");
    if (!Loop || !LI->isLoopHeader(BB)) {
      markIrreducibleLoopPredecessorsOfNonLoopHeader(BB, BBRegion, PI);
      continue;
    }
    assert(Loop && "Should have a non-null Loop at this point");
    assert(ParentRegion && "Expected a non-null LoopRegion");

    // Now we know that the block is in a loop and is a loop header. Check if
    // this loop has multiple latches. If so, mark the latch head/tail blocks as
    // head/tail unknown cfg boundaries.
    //
    // We rely on Loop canonicalization to separate such loops into separate
    // nested loops.
    ParentRegion->getSubregionData().RPONumOfHeaderBlock = RPOIndex;

    // If we have one loop latch, continue.
    if (Loop->getLoopLatch()) {
      DEBUG(llvm::dbgs() << "\n");
      continue;
    }

    // Otherwise, mark each of the loop latches as irreducible control flow edge
    // tails so we are conservative around them.
    markMultipleLoopLatchLoopBackEdges(BBRegion, Loop, PI);
    DEBUG(llvm::dbgs() << "\n");
  }

#ifndef NDEBUG
  AllBBRegionsCreated = true;
#endif
}

//===---
// Loop and Function Region Initialization
//

void LoopRegionFunctionInfo::initializeLoopRegions(LoopInfoTy *LI) {
  // Now visit the loop nest in a DFS.
  llvm::SmallVector<LoopTy *, 16> LoopWorklist(LI->begin(), LI->end());
  llvm::SmallPtrSet<LoopTy *, 16> VisitedLoops;

  while (LoopWorklist.size()) {
    LoopTy *L = LoopWorklist.back();
    auto *LRegion = getRegion(L);

    // If we have not visited this loop yet and it has subloops, add its
    // subloops to the worklist and continue.
    auto SubLoops = L->getSubLoopRange();
    if (VisitedLoops.insert(L).second && SubLoops.begin() != SubLoops.end()) {
      for (auto *SubLoop : SubLoops) {
        LoopWorklist.push_back(SubLoop);
      }
      continue;
    }
    LoopWorklist.pop_back();

    // Otherwise, process this loop. We have already visited all of its
    // potential children loops.
    initializeLoopFunctionRegion(LRegion, SubLoops);
  }
}

/// For each predecessor of the header of the loop:
///
/// 1. If the predecessor is not in the loop, make the predecessor a predecessor
/// of the loop instead of the header.
///
/// 2. If the predecessor *is* in the loop it must be the tail of a backedge. We
/// remove these back edges and instead represent them as unknown control flow
/// edges. This is because we rely on loop canonicalization to canonicalize
/// multiple backedge loops into separate loops.
LoopRegionFunctionInfo::RegionTy *
LoopRegionFunctionInfo::
rewriteLoopHeaderPredecessors(LoopTy *SubLoop, RegionTy *SubLoopRegion) {
  auto *SubLoopHeaderRegion = getRegion(SubLoop->getHeader());
  assert(SubLoopHeaderRegion->isBlock() && "A header must always be a block");

  DEBUG(llvm::dbgs()
        << "        Header: " << SubLoopHeaderRegion->getID() << "\n"
        << "        Rewiring Header Predecessors to be Loop Preds.\n");

  if (SubLoopHeaderRegion->IsUnknownControlFlowEdgeHead)
    SubLoopRegion->IsUnknownControlFlowEdgeHead = true;

  for (unsigned PredID : SubLoopHeaderRegion->Preds) {
    auto *PredRegion = getRegion(PredID);

    DEBUG(llvm::dbgs() << "            " << PredRegion->getID() << "\n");
    if (!SubLoopRegion->containsSubregion(PredRegion)) {
      DEBUG(llvm::dbgs() << "            Not in loop... Replacing...\n");
      // This is always a local edge since non-local edges can only have loops
      // as end points.
      PredRegion->replaceSucc(SubLoopHeaderRegion->ID, SubLoopRegion->ID,
                              /*IsNonLocal*/ false);
      propagateLivenessDownNonLocalSuccessorEdges(PredRegion);
      SubLoopRegion->addPred(PredRegion);
      continue;
    }

    DEBUG(llvm::dbgs() << "            Is in loop... Erasing...\n");
    // We are abusing the fact that a block can only be a local successor.
    PredRegion->removeLocalSucc(SubLoopHeaderRegion->getID());
    propagateLivenessDownNonLocalSuccessorEdges(PredRegion);
  }
  SubLoopHeaderRegion->Preds.clear();

  return SubLoopHeaderRegion;
}

static void
getExitingRegions(LoopRegionFunctionInfo *LRFI,
                  SILLoop *Loop,
                  LoopRegion *LRegion,
                  llvm::SmallVectorImpl<LoopRegion *> &ExitingRegions) {
  llvm::SmallVector<SILBasicBlock *, 8> ExitingBlocks;
  Loop->getExitingBlocks(ExitingBlocks);

  // Determine the outer most region that contains the exiting block that is not
  // this subloop's region. That is the *true* exiting region.
  for (auto *BB : ExitingBlocks) {
    auto *Region = LRFI->getRegion(BB);
    unsigned RegionParentID = Region->getParentID();
    while (RegionParentID != LRegion->getID()) {
      Region = LRFI->getRegion(RegionParentID);
      RegionParentID = Region->getParentID();
    }
    ExitingRegions.push_back(Region);
  }

  // We can have a loop subregion that has multiple exiting edges from the
  // current loop. We do not want to visit that loop subregion multiple
  // times. So we unique the exiting region list.
  sortUnique(ExitingRegions);
}

/// For each exiting block:
///
/// 1. Make any successors outside of the loop successors of the loop instead
/// of the exiting block.
///
/// 2. Any successors that are inside the loop but are not back edges are
/// left alone.
///
/// 3. Any successors that are inside the loop but are the head of a backedge
/// have the edge removed. This is computed by the successor having a greater
/// post order number than the exit.
///
/// *NOTE* We have to be careful here since exiting blocks may include BBs
/// that have been subsumed into a subloop already. Also to determine back
/// edges, we need to compare post order numbers potentially of loop headers,
/// not the loops themselves.
void
LoopRegionFunctionInfo::
rewriteLoopExitingBlockSuccessors(LoopTy *Loop, RegionTy *LRegion) {
  llvm::SmallVector<RegionTy *, 8> ExitingRegions;
  getExitingRegions(this, Loop, LRegion, ExitingRegions);

  DEBUG(llvm::dbgs() << "    Visiting Exit Blocks...\n");
  for (auto *ExitingRegion : ExitingRegions) {
    DEBUG(llvm::dbgs() << "        Exiting Region: "
          << ExitingRegion->getID() << "\n");
    for (auto SuccID : ExitingRegion->getSuccs()) {
      DEBUG(llvm::dbgs() << "            Succ: " << SuccID.ID
                         << ". IsNonLocal: "
                         << (SuccID.IsNonLocal ? "true" : "false") << "\n");

      auto *SuccRegion = getRegion(SuccID.ID);
      if (!LRegion->containsSubregion(SuccRegion)) {
        DEBUG(llvm::dbgs() << "            Is not a subregion, replacing.\n");
        SuccRegion->replacePred(ExitingRegion->ID, LRegion->ID);
        if (ExitingRegion->IsUnknownControlFlowEdgeTail)
          LRegion->IsUnknownControlFlowEdgeTail = true;
        // If the successor region is already in this LRegion this returns that
        // regions index. Otherwise it returns a new index.
        unsigned Index = LRegion->addSucc(SuccRegion);
        ExitingRegion->replaceSucc(SuccRegion->getID(), Index,
                                   /*IsNonLocal*/ true);
        propagateLivenessDownNonLocalSuccessorEdges(ExitingRegion);
        continue;
      }

      // If the rpo number of the successor is less than the RPO number of the
      // BB, then we know that it is not a backedge.
      if (SuccRegion->getRPONumber() > ExitingRegion->getRPONumber()) {
        DEBUG(llvm::dbgs() << "            Is a subregion, but not a "
              "backedge, not removing.\n");
        continue;
      }
      DEBUG(llvm::dbgs() << "            Is a subregion and a backedge, "
            "removing.\n");
      auto Iter =
        std::remove(SuccRegion->Preds.begin(), SuccRegion->Preds.end(),
                    ExitingRegion->getID());
      SuccRegion->Preds.erase(Iter);
    }
  }
}

/// The high level algorithm is that via the loop info we already know for
/// each subloop:
///
/// 1. predecessors.
/// 2. header.
/// 3. exiting blocks.
/// 4. successor blocks.
///
/// This means that we can simply fix up those BB regions.
///
/// *NOTE* We do not touch the successors/predecessors of the current loop. We
/// leave those connections in place for our parent loop to fix up. In the case
/// we are the function level loop, these will of course be empty anyways.
void
LoopRegionFunctionInfo::
initializeLoopFunctionRegion(RegionTy *ParentRegion,
                             iterator_range<LoopInfoTy::iterator> SubLoops) {

  DEBUG(llvm::dbgs() << "Initializing Loop Region " << ParentRegion->getID()
        << "\n");
  // For each subloop...
  for (auto *SubLoop : SubLoops) {
    // Grab the region associated with the subloop...
    auto *SubLoopRegion = getRegion(SubLoop);

    DEBUG(llvm::dbgs() << "    Visiting Subloop: " << SubLoopRegion->getID()
          << "\n");

    // First rewrite predecessors of the loop header to point at the loop.
    auto *SubLoopHeaderRegion = rewriteLoopHeaderPredecessors(SubLoop,
                                                              SubLoopRegion);

    // Then rewrite successors of all exits.
    rewriteLoopExitingBlockSuccessors(SubLoop, SubLoopRegion);

    // Add the subloop region to the subregion data.
    ParentRegion->addLoopSubregion(SubLoopRegion, SubLoopHeaderRegion);
  }

  // Now that we have finished processing this loop, sort its subregions so that
  // they are now in RPO order. This works because each BB's ID is its RPO
  // number and we represent loops by the RPO number of their preheader (with a
  // flag in the first bit to say to look in the subloop array for the *real* ID
  // of the loop).
  //
  // TODO: Is this necessary? We visit BBs in RPO order. This means that we
  // should always add BBs in RPO order to subregion lists, no? For now I am
  // going to sort just to be careful while bringing this up.
  ParentRegion->getSubregionData().sortSubregions();
}

void LoopRegionFunctionInfo::
initializeFunctionRegion(iterator_range<LoopInfoTy::iterator> SubLoops) {
  // We have already processed all of our subloops, so everything there has
  // already been properly setup.
  initializeLoopFunctionRegion(getRegion(F), SubLoops);
}

/// Recursively visit all the descendents of Parent. If there is a non-local
/// successor edge path that points to a dead edge in Parent, mark the
/// descendent non-local successor edge as dead.
void LoopRegionFunctionInfo::
propagateLivenessDownNonLocalSuccessorEdges(LoopRegion *Parent) {
  llvm::SmallVector<LoopRegion *, 4> Worklist;
  Worklist.push_back(Parent);

  while (Worklist.size()) {
    LoopRegion *R = Worklist.pop_back_val();
    for (unsigned SubregionID : R->getSubregions()) {
      LoopRegion *Subregion = getRegion(SubregionID);
      bool ShouldVisit = false;
      for (auto &SuccID : Subregion->Succs) {
        // If the successor is already dead, skip it. We should have viisted all
        // its children when we marked it as dead.
        if (SuccID.IsDead)
          continue;

        // We do not care about local IDs, we only process non-local IDs.
        if (!SuccID.IsNonLocal)
          continue;

        // Finally if the non-local successor edge points to a parent successor
        // that is not dead continue.
        if (!R->Succs[SuccID.ID].IsDead)
          continue;

        // Ok, we found a target! Mark is as dead and make sure that we visit
        // the subregion's children.
        ShouldVisit = true;
        SuccID.IsDead = unsigned(true);
      }

      if (ShouldVisit)
        Worklist.push_back(Subregion);
    }
  }
}

void LoopRegionFunctionInfo::dump() const {
  print(llvm::outs());
  llvm::outs() << "\n";
}

void LoopRegionFunctionInfo::print(raw_ostream &os) const {
  // Print out the information for each loop.
  std::vector<std::pair<LoopRegion *, LoopRegion *>> RegionWorklist;

  // Initialize the worklist with the function level region.
  RegionWorklist.push_back({nullptr, getRegion(F)});

  // Vectors that we use to sort our local successor/predecessor indices to make
  // our output deterministic.
  llvm::SmallVector<unsigned, 4> SortedPreds;
  llvm::SmallVector<unsigned, 4> SortedSuccs;

  // Then go to town...
  while (RegionWorklist.size()) {
    LoopRegion *Parent, *R;
    std::tie(Parent, R) = RegionWorklist.back();
    RegionWorklist.pop_back();

    os << *R;

    // If we have a parent, print out its id. This is not strictly necessary,
    // but it makes it easier to read the output from a dump.
    if (Parent)
      os << " parent:" << Parent->getID();
    os << "\n";

    // Print predecessors.
    SortedPreds.clear();
    for (unsigned Pred : R->Preds)
      SortedPreds.push_back(Pred);
    std::sort(SortedPreds.begin(), SortedPreds.end());
    os << "    (preds";
    for (unsigned SID : SortedPreds) {
      os << "\n        ";
      LoopRegion *PredRegion = getRegion(SID);
      PredRegion->print(os, true);
    }
    os << ")\n";

    os << "    (succs";

    // To make our output deterministic, we sort local successor indices.
    SortedSuccs.clear();
    // TODO: Investigate why std::copy does not work here. I tried to use a
    // std::copy here, but ran into miscompile issues with
    // OptionalTransformRange.
    for (unsigned I : R->getLocalSuccs()) {
      SortedSuccs.push_back(I);
    }
    std::sort(SortedSuccs.begin(), SortedSuccs.end());
    auto ARef = ArrayRef<unsigned>(SortedSuccs);
    for (unsigned SID : SortedSuccs) {
      os << "\n        ";
      LoopRegion *SuccRegion = getRegion(SID);
      SuccRegion->print(os, true);
    }
    os << ")\n";

    os << "    (subregs";
    if (!R->isBlock()) {
      // Go through the subregions.
      for (unsigned SID : R->getSubregions()) {
        os << "\n        ";
        LoopRegion *Subregion = getRegion(SID);
        Subregion->print(os, true);
        RegionWorklist.push_back({R, Subregion});
      }
    }
    os << ")\n";

    SortedSuccs.clear();
    for (unsigned I : R->getNonLocalSuccs()) {
      SortedSuccs.push_back(I);
    }
    std::sort(SortedSuccs.begin(), SortedSuccs.end());
    os << "    (non-local-succs";
    for (unsigned I : SortedSuccs) {
      os << "\n        (parentindex:" << I << ")";
    }
    os << "))\n";
  }
}

//===----------------------------------------------------------------------===//
//                              Graphing Support
//===----------------------------------------------------------------------===//

#ifndef NDEBUG

/// A lot of the code below is unfortunate and due to the inflexibility of
/// GraphUtils. But you gotta do what you gotta do.
namespace {

struct LoopRegionWrapper;

struct LoopRegionFunctionInfoGrapherWrapper {
  LoopRegionFunctionInfo *FuncInfo;
  std::vector<LoopRegionWrapper> Data;
};

struct alledge_iterator;

struct LoopRegionWrapper {
  LoopRegionFunctionInfoGrapherWrapper &FuncInfo;
  LoopRegion *Region;

  LoopRegionWrapper *getParent() const {
    unsigned ParentIndex = Region->getParentID();
    return &FuncInfo.Data[ParentIndex];
  }

  alledge_iterator begin();
  alledge_iterator end();
};

/// An iterator on Regions that first iterates over subregions and then over
/// successors.
struct alledge_iterator
    : std::iterator<std::forward_iterator_tag, LoopRegionWrapper> {
  LoopRegionWrapper *Wrapper;
  swift::LoopRegion::subregion_iterator SubregionIter;
  LoopRegion::succ_iterator SuccIter;

  alledge_iterator(LoopRegionWrapper *w,
                   swift::LoopRegion::subregion_iterator subregioniter,
                   LoopRegion::succ_iterator succiter)
      : Wrapper(w), SubregionIter(subregioniter), SuccIter(succiter) {

    // Prime the successor iterator so that we skip over any initial dead
    // successor edges.
    //
    // TODO: Refactor this to use a FilterRange.
    for (auto SuccEnd = Wrapper->Region->succ_end();
         SuccIter->IsDead && SuccIter != SuccEnd; ++SuccIter) {
    }
  }

  bool isSubregion() const {
    return SubregionIter != Wrapper->Region->subregion_end();
  }

  bool isNonLocalEdge() const {
    if (isSubregion())
      return false;
    return SuccIter->IsNonLocal;
  }

  Optional<unsigned> getSuccIndex() const {
    if (isSubregion())
      return None;
    // Since we have a bidirectional iterator, this will perform increments
    // until we get to SuccIter. This is the behavior we want so that we ensure
    // that we skip over any dead successor edges. We are just performing
    // graphing, so performance is not a concern.
    return std::distance(Wrapper->Region->succ_begin(), SuccIter);
  }

  LoopRegionWrapper *operator*() const {
    if (SubregionIter != Wrapper->Region->subregion_end()) {
      return &Wrapper->FuncInfo.Data[*SubregionIter];
    }
    // If we have a non-local id, just return the parent region's data.
    if (SuccIter->IsNonLocal)
      return &Wrapper->FuncInfo.Data[Wrapper->Region->getParentID()];
    // Otherwise return the data associated with this successor.
    return &Wrapper->FuncInfo.Data[SuccIter->ID];
  }

  alledge_iterator &operator++() {
    if (SubregionIter != Wrapper->Region->subregion_end()) {
      SubregionIter++;
    } else {
      // Make sure that we skip past any dead successors.
      auto End = Wrapper->Region->succ_end();
      do {
        SuccIter++;
      } while (SuccIter->IsDead && SuccIter != End);
    }
    return *this;
  }
  alledge_iterator operator++(int) {
    if (SubregionIter != Wrapper->Region->subregion_end()) {
      return alledge_iterator{Wrapper, SubregionIter++, SuccIter};
    }
    auto NewIter = *this;
    ++NewIter;
    return NewIter;
  }
  bool operator==(alledge_iterator rhs) {
    if (Wrapper->Region != rhs.Wrapper->Region)
      return false;
    if (SubregionIter != rhs.SubregionIter)
      return false;
    if (SuccIter != rhs.SuccIter)
      return false;
    return true;
  }
  bool operator!=(alledge_iterator rhs) { return !(*this == rhs); }
};

} // end anonymous namespace

alledge_iterator LoopRegionWrapper::begin() {
  return alledge_iterator(this, Region->subregion_begin(),
                          Region->succ_begin());
}

alledge_iterator LoopRegionWrapper::end() {
  return alledge_iterator(this, Region->subregion_end(), Region->succ_end());
}

namespace llvm {
template <> struct GraphTraits<LoopRegionWrapper> {
  using NodeType = LoopRegionWrapper;
  using ChildIteratorType = alledge_iterator;

  static NodeType *getEntryNode(NodeType *BB) { return BB; }

  static ChildIteratorType child_begin(NodeType *N) { return N->begin(); }
  static ChildIteratorType child_end(NodeType *N) { return N->end(); }
};

template <>
struct GraphTraits<LoopRegionFunctionInfoGrapherWrapper *>
    : public GraphTraits<LoopRegionWrapper> {
  using GraphType = LoopRegionFunctionInfoGrapherWrapper;

  static NodeType *getEntryNode(GraphType *F) { return &F->Data[0]; }

  using nodes_iterator = std::vector<LoopRegionWrapper>::iterator;

  static nodes_iterator nodes_begin(GraphType *F) { return F->Data.begin(); }
  static nodes_iterator nodes_end(GraphType *F) { return F->Data.end(); }
  static unsigned size(GraphType *F) { return F->Data.size(); }
};

} // end namespace llvm

static llvm::cl::opt<unsigned>
    MaxColumns("view-loop-regions-max-columns", llvm::cl::init(80),
               llvm::cl::desc("Maximum width of a printed node"));

namespace {
enum class LongLineBehavior { None, Truncate, Wrap };
} // end anonymous namespace
static llvm::cl::opt<LongLineBehavior> LLBehavior(
    "view-loop-regions-long-line-behavior",
    llvm::cl::init(LongLineBehavior::Truncate),
    llvm::cl::desc("Behavior when line width is greater than the "
                   "value provided my -view-loop-regions-max-columns "
                   "option"),
    llvm::cl::values(
        clEnumValN(LongLineBehavior::None, "none", "Print everything"),
        clEnumValN(LongLineBehavior::Truncate, "truncate",
                   "Truncate long lines"),
        clEnumValN(LongLineBehavior::Wrap, "wrap", "Wrap long lines"),
        clEnumValEnd));

static llvm::cl::opt<bool> RemoveUseListComments(
    "view-loop-regions-remove-use-list-comments", llvm::cl::init(false),
    llvm::cl::desc("Should use list comments be removed"));

namespace llvm {
template <>
struct DOTGraphTraits<LoopRegionFunctionInfoGrapherWrapper *>
    : public DefaultDOTGraphTraits {

  DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}

  static std::string
  getGraphName(const LoopRegionFunctionInfoGrapherWrapper *F) {
    return "Loop Regions for '" + F->FuncInfo->getFunction()->getName().str() +
           "' function";
  }

  static std::string
  getSimpleNodeLabel(LoopRegionWrapper *Node,
                     const LoopRegionFunctionInfoGrapherWrapper *F) {
    std::string OutStr;
    raw_string_ostream OSS(OutStr);
    Node->Region->printName(OSS);
    return OSS.str();
  }

  static std::string
  getCompleteNodeLabel(LoopRegionWrapper *Node,
                       const LoopRegionFunctionInfoGrapherWrapper *F) {
    std::string Tmp0;
    raw_string_ostream OSS(Tmp0);
    Node->Region->printName(OSS);
    if (Node->Region->isUnknownControlFlowEdgeHead())
      OSS << " Unknown CF Head";
    if (Node->Region->isUnknownControlFlowEdgeTail())
      OSS << " Unknown CF Tail";
    if (!Node->Region->isBlock())
      return OSS.str();
    OSS << "\n";
    std::string Tmp1;
    raw_string_ostream OSS2(Tmp1);
    OSS2 << *Node->Region->getBlock();
    std::string OutStr = OSS2.str();
    if (OutStr[0] == '\n')
      OutStr.erase(OutStr.begin());

    // Process string output to make it nicer...
    unsigned ColNum = 0;
    unsigned LastSpace = 0;
    for (unsigned i = 0; i != OutStr.length(); ++i) {
      if (OutStr[i] == '\n') { // Left justify
        OutStr[i] = '\\';
        OutStr.insert(OutStr.begin() + i + 1, 'l');
        ColNum = 0;
        LastSpace = 0;
      } else if (RemoveUseListComments && OutStr[i] == '/' &&
                 i != (OutStr.size() - 1) && OutStr[i + 1] == '/') {
        unsigned Idx = OutStr.find('\n', i + 1); // Find end of line
        OutStr.erase(OutStr.begin() + i, OutStr.begin() + Idx);
        --i;

      } else if (ColNum == MaxColumns) { // Handle long lines.

        if (LLBehavior == LongLineBehavior::Wrap) {
          if (!LastSpace)
            LastSpace = i;
          OutStr.insert(LastSpace, "\\l...");
          ColNum = i - LastSpace;
          LastSpace = 0;
          i += 3; // The loop will advance 'i' again.
        } else if (LLBehavior == LongLineBehavior::Truncate) {
          unsigned Idx = OutStr.find('\n', i + 1); // Find end of line
          OutStr.erase(OutStr.begin() + i, OutStr.begin() + Idx);
          --i;
        }

        // Else keep trying to find a space.
      } else
        ++ColNum;
      if (OutStr[i] == ' ')
        LastSpace = i;
    }

    return OSS.str() + OutStr;
  }

  std::string getNodeLabel(LoopRegionWrapper *Node,
                           const LoopRegionFunctionInfoGrapherWrapper *Graph) {
    if (isSimple())
      return getSimpleNodeLabel(Node, Graph);
    else
      return getCompleteNodeLabel(Node, Graph);
  }

  static bool hasEdgeDestLabels() { return true; }

  static unsigned numEdgeDestLabels(LoopRegionWrapper *Node) {
    return std::distance(Node->begin(), Node->end());
  }

  static std::string getEdgeDestLabel(LoopRegionWrapper *Node, unsigned i) {
    alledge_iterator Iter = Node->begin();
    std::advance(Iter, i);
    return getEdgeSourceLabel(Node, Iter);
  }

  static std::string getEdgeSourceLabel(LoopRegionWrapper *Node,
                                        alledge_iterator I) {
    if (I.isNonLocalEdge())
      return "NLSuc";
    if (I.isSubregion())
      return "Sub";
    return "LSuc";
  }

  /// If you want to override the dot attributes printed for a particular
  /// edge, override this method.
  static std::string
  getEdgeAttributes(const LoopRegionWrapper *Node, alledge_iterator EI,
                    const LoopRegionFunctionInfoGrapherWrapper *Graph) {
    if (EI.isSubregion())
      return "color=red";
    if (EI.isNonLocalEdge())
      return "color=blue";
    return "";
  }

  static bool edgeTargetsEdgeSource(const LoopRegionWrapper *Node,
                                    alledge_iterator I) {
    return I.isNonLocalEdge();
  }

  /// If edgeTargetsEdgeSource returns true, this method is called to determine
  /// which outgoing edge of Node is the target of this edge.
  ///
  /// Currently this can only happen given a non-local edge in which case we
  /// want the non-local edge to point at its parent's successor edge.
  static alledge_iterator getEdgeTarget(LoopRegionWrapper *Node,
                                        alledge_iterator I) {
    assert(I.isNonLocalEdge() && "This should only be called given a non "
                                 "local edge");
    auto *ParentRegion = Node->getParent();
    auto SuccIter = ParentRegion->Region->succ_begin();
    std::advance(SuccIter, I.SuccIter->ID);
    return alledge_iterator(ParentRegion, ParentRegion->Region->subregion_end(),
                            SuccIter);
  }
};
} // end llvm namespace

static llvm::cl::opt<std::string> TargetFunction(
    "view-loop-regions-only-for-function", llvm::cl::init(""),
    llvm::cl::desc("Only print out the loop regions for this function"));
#endif

void LoopRegionFunctionInfo::viewLoopRegions() const {
// This is a no-op when asserts are disabled.
#ifndef NDEBUG
  // If we have a target function, only print that function out.
  if (!TargetFunction.empty() && !(F->getName().str() == TargetFunction))
    return;
  LoopRegionFunctionInfoGrapherWrapper Wrapper;
  Wrapper.FuncInfo = const_cast<LoopRegionFunctionInfo *>(this);
  for (auto *R : IDToRegionMap) {
    Wrapper.Data.push_back({Wrapper, R});
  }
  llvm::ViewGraph(&Wrapper, "loop region function info" + F->getName().str());
#endif
}

//===----------------------------------------------------------------------===//
//                             LoopRegionAnalysis
//===----------------------------------------------------------------------===//

void LoopRegionAnalysis::initialize(SILPassManager *PM) {
  SLA = PM->getAnalysis<SILLoopAnalysis>();
  POA = PM->getAnalysis<PostOrderAnalysis>();
}

//===----------------------------------------------------------------------===//
//                              Main Entry Point
//===----------------------------------------------------------------------===//

SILAnalysis *swift::createLoopRegionAnalysis(SILModule *M) {
  return new LoopRegionAnalysis(M);
}

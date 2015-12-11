//===--- LoopUtils.cpp ----------------------------------------------------===//
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

#define DEBUG_TYPE "sil-loop-utils"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "llvm/Support/Debug.h"

using namespace swift;

static SILBasicBlock *getSingleOutsideLoopPredecessor(SILLoop *L,
                                                      SILBasicBlock *BB) {
  SmallVector<SILBasicBlock *, 8> Preds;
  for (auto *Pred : BB->getPreds())
    if (!L->contains(Pred))
      Preds.push_back(Pred);
  if (Preds.size() != 1)
    return nullptr;
  return Preds[0];
}

/// \brief Try to create a unique loop preheader.
///
/// FIXME: We should handle merging multiple loop predecessors.
static SILBasicBlock* insertPreheader(SILLoop *L, DominanceInfo *DT,
                                      SILLoopInfo *LI) {
  assert(!L->getLoopPreheader() && "Expect multiple preheaders");

  SILBasicBlock *Header = L->getHeader();
  SILBasicBlock *Preheader = nullptr;
  if (auto LoopPred = getSingleOutsideLoopPredecessor(L, Header)) {
    if (isa<CondBranchInst>(LoopPred->getTerminator())) {
      Preheader = splitIfCriticalEdge(LoopPred, Header, DT, LI);
      DEBUG(llvm::dbgs() << "Created preheader for loop: " << *L);
      assert(Preheader && "Must have a preheader now");
    }
  }
  return Preheader;
}

/// \brief Convert a loop with multiple backedges to a single backedge loop.
///
/// Create a new block as a common target for all the current loop backedges.
static SILBasicBlock *insertBackedgeBlock(SILLoop *L, DominanceInfo *DT,
                                          SILLoopInfo *LI) {
  assert(!L->getLoopLatch() && "Must have > 1 backedge.");

  // For simplicity, assume a single preheader
  SILBasicBlock *Preheader = L->getLoopPreheader();
  if (!Preheader)
    return nullptr;

  SILBasicBlock *Header = L->getHeader();
  SILFunction *F = Header->getParent();

  // Figure out which basic blocks contain back-edges to the loop header.
  SmallVector<SILBasicBlock*, 4> BackedgeBlocks;
  for (auto *Pred : Header->getPreds()) {
    if (Pred == Preheader)
      continue;
    // Branches can be handled trivially and CondBranch edges can be split.
    if (!isa<BranchInst>(Pred->getTerminator())
        && !isa<CondBranchInst>(Pred->getTerminator())) {
      return nullptr;
    }
    BackedgeBlocks.push_back(Pred);
  }

  // Create and insert the new backedge block...
  SILBasicBlock *BEBlock =
    new (F->getModule()) SILBasicBlock(F, BackedgeBlocks.back());

  DEBUG(llvm::dbgs() << "  Inserting unique backedge block " << *BEBlock
        << "\n");

  // Now that the block has been inserted into the function, create PHI nodes in
  // the backedge block which correspond to any PHI nodes in the header block.
  SmallVector<SILValue, 6> BBArgs;
  for (auto *BBArg : Header->getBBArgs()) {
    BBArgs.push_back(BEBlock->createBBArg(BBArg->getType(), /*Decl=*/nullptr));
  }

  // Arbitrarily pick one of the predecessor's branch locations.
  SILLocation BranchLoc = BackedgeBlocks.back()->getTerminator()->getLoc();

  // Create an unconditional branch that propagates the newly created BBArgs.
  SILBuilder(BEBlock).createBranch(BranchLoc, Header, BBArgs);

  // Redirect the backedge blocks to BEBlock instead of Header.
  for (auto *Pred : BackedgeBlocks) {
    auto *Terminator = Pred->getTerminator();

    if (auto *Branch = dyn_cast<BranchInst>(Terminator))
      changeBranchTarget(Branch, 0, BEBlock, /*PreserveArgs=*/true);
    else if (auto *CondBranch = dyn_cast<CondBranchInst>(Terminator)) {
      unsigned EdgeIdx = (CondBranch->getTrueBB() == Header)
        ? CondBranchInst::TrueIdx : CondBranchInst::FalseIdx;
      changeBranchTarget(CondBranch, EdgeIdx, BEBlock, /*PreserveArgs=*/true);
    }
    else {
      llvm_unreachable("Expected a branch terminator.");
    }
  }

  // Update Loop Information - we know that this block is now in the current
  // loop and all parent loops.
  L->addBasicBlockToLoop(BEBlock, LI->getBase());

  // Update dominator information
  SILBasicBlock *DomBB = BackedgeBlocks.back();
  for (auto BBIter = BackedgeBlocks.begin(),
         BBEnd = std::prev(BackedgeBlocks.end());
       BBIter != BBEnd; ++BBIter) {
    DomBB = DT->findNearestCommonDominator(DomBB, *BBIter);
  }
  DT->addNewBlock(BEBlock, DomBB);

  return BEBlock;
}

/// Canonicalize loop exit blocks so that they only have predecessors inside the
/// loop.
static bool canonicalizeLoopExitBlocks(SILLoop *L, DominanceInfo *DT,
                                       SILLoopInfo *LI) {
  assert(L && "We assume that L is not null");

  bool Changed = false;
  SmallVector<SILBasicBlock *, 8> ExitingBlocks;
  L->getExitingBlocks(ExitingBlocks);

  for (auto *ExitingBlock : ExitingBlocks) {
    for (unsigned i : indices(ExitingBlock->getSuccessors())) {
      // We have to look up our exiting blocks each time around the loop since
      // if we split a critical edge, the exiting block will have a new
      // terminator implying a new successor list. The new non-critical edge
      // though will be placed at the same spot in the new terminator where the
      // critical edge was in the old terminator. Thus as long as we use
      // indices, we will visit all exiting block edges appropriately and not
      // deal with touching stale memory.
      auto Succs = ExitingBlock->getSuccessors();
      auto *SuccBB = Succs[i].getBB();

      // Add only exit block successors by skipping blocks in the loop.
      if (LI->getLoopFor(SuccBB) == L)
        continue;

// This is unfortunate but necessary since splitCriticalEdge may change IDs.
#ifndef NDEBUG
      llvm::SmallString<5> OldExitingBlockName;
      DEBUG({
        llvm::raw_svector_ostream buffer(OldExitingBlockName);
        ExitingBlock->printAsOperand(buffer);
      });
      llvm::SmallString<5> OldSuccBBName;
      DEBUG({
        llvm::raw_svector_ostream buffer(OldSuccBBName);
        SuccBB->printAsOperand(buffer);
      });
#endif

      // Split any critical edges in between exiting block and succ iter.
      if (splitCriticalEdge(ExitingBlock->getTerminator(), i, DT, LI)) {
        DEBUG(llvm::dbgs() << "Split critical edge from " << OldExitingBlockName
                           << " NewID: ";
              ExitingBlock->printAsOperand(llvm::dbgs());
              llvm::dbgs() << " -> OldID: " << OldSuccBBName << " NewID: ";
              SuccBB->printAsOperand(llvm::dbgs()); llvm::dbgs() << "\n");
        Changed = true;
      }
    }
  }

  return Changed;
}

/// Canonicalize the loop for rotation and downstream passes.
///
/// Create a single preheader and single latch block.
///
/// FIXME: We should identify nested loops with a common header and separate
/// them before merging the latch. See LLVM's separateNestedLoop.
bool swift::canonicalizeLoop(SILLoop *L, DominanceInfo *DT, SILLoopInfo *LI) {
  bool ChangedCFG = false;
  if (!L->getLoopPreheader()) {
    if (!insertPreheader(L, DT, LI))
      // Skip further simplification with no preheader.
      return ChangedCFG;
    ChangedCFG = true;
  }

  ChangedCFG |= canonicalizeLoopExitBlocks(L, DT, LI);

  if (!L->getLoopLatch())
    ChangedCFG |= (insertBackedgeBlock(L, DT, LI) != nullptr);

  return ChangedCFG;
}

bool swift::canonicalizeAllLoops(DominanceInfo *DT, SILLoopInfo *LI) {
  // Visit the loop nest hierarchy bottom up.
  bool MadeChange = false;
  llvm::SmallVector<std::pair<SILLoop *, bool>, 16> Worklist;
  for (auto *L : LI->getTopLevelLoops())
    Worklist.push_back({L, L->empty()});

  while (Worklist.size()) {
    SILLoop *L;
    bool VisitedAlready;
    std::tie(L, VisitedAlready) = Worklist.pop_back_val();

    if (!VisitedAlready) {
      Worklist.push_back({L, true});
      for (auto *Subloop : L->getSubLoopRange()) {
        Worklist.push_back({Subloop, Subloop->empty()});
      }
      continue;
    }

    MadeChange |= canonicalizeLoop(L, DT, LI);
  }

  return MadeChange;
}

//===----------------------------------------------------------------------===//
//                                Loop Visitor
//===----------------------------------------------------------------------===//

void SILLoopVisitor::run() {
  // We visit the loop nest inside out via a depth first, post order using
  // this
  // worklist.
  llvm::SmallVector<std::pair<SILLoop *, bool>, 32> Worklist;
  for (auto *L : LI->getTopLevelLoops()) {
    Worklist.push_back({L, L->empty()});
  }

  while (Worklist.size()) {
    SILLoop *L;
    bool Visited;
    std::tie(L, Visited) = Worklist.pop_back_val();

    if (!Visited) {
      Worklist.push_back({L, true});
      for (auto *SubLoop : L->getSubLoops()) {
        Worklist.push_back({SubLoop, SubLoop->empty()});
      }
      continue;
    }
    runOnLoop(L);
  }

  runOnFunction(F);
}

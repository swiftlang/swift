//===--------- LoopSimplify.cpp - Loop structure simplify -*- C++ -*-------===//
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

#define DEBUG_TYPE "sil-looprotate"

#include "swift/SIL/Dominance.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/LoopAnalysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "swift/SILPasses/Utils/SILSSAUpdater.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"

#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

static llvm::cl::opt<bool> ShouldRotate("sil-looprotate",
                                        llvm::cl::init(true));

/// Splits the critical edges between from and to. This code assumes there is
/// only one edge between the two basic blocks.
static SILBasicBlock *splitIfCriticalEdge(SILBasicBlock *From,
                                        SILBasicBlock *To,
                                        DominanceInfo *DT,
                                        SILLoopInfo *LI) {
  auto *T = From->getTerminator();
  for (unsigned i = 0, e = T->getSuccessors().size(); i != e; ++i) {
    if (T->getSuccessors()[i] == To)
      return splitCriticalEdge(T, i, DT, LI);
  }
  llvm_unreachable("Destination block not found");
}

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

/// Check whether all operands are loop invariant.
static bool hasLoopInvariantOperands(SILInstruction *I, SILLoop *L,
                                     llvm::DenseSet<SILInstruction *> &Inv) {
  auto Opds = I->getAllOperands();

  return std::all_of(Opds.begin(), Opds.end(), [=](Operand &Op) {

    auto *Def = Op.get().getDef();
    // Operand is outside the loop or marked invariant.
    if (auto *Inst = dyn_cast<SILInstruction>(Def))
      return !L->contains(Inst->getParent()) || Inv.count(Inst);
    if (auto *Arg = dyn_cast<SILArgument>(Def))
      return !L->contains(Arg->getParent());

    return false;
  });
}

/// We can not duplicate blocks with AllocStack instructions (they need to be
/// FIFO). Other instructions can be moved to the preheader.
static bool
canDuplicateOrMoveToPreheader(SILLoop *L, SILBasicBlock *Preheader,
                              SILBasicBlock *Blk,
                              SmallVectorImpl<SILInstruction *> &Move) {
  llvm::DenseSet<SILInstruction *> Invariant;
  for (auto &I : *Blk) {
    auto *Inst = &I;
    if (auto *MI = dyn_cast<MethodInst>(Inst)) {
      if (MI->getMember().isForeign && MI->isVolatile())
        return false;
      if (MI->isVolatile() || !hasLoopInvariantOperands(Inst, L, Invariant))
        continue;
      Move.push_back(Inst);
      Invariant.insert(Inst);
    } else if (!I.isTriviallyDuplicatable())
      return false;
    else if (isa<FunctionRefInst>(Inst)) {
      Move.push_back(Inst);
      Invariant.insert(Inst);
    } else if (isa<IntegerLiteralInst>(Inst)) {
      Move.push_back(Inst);
      Invariant.insert(Inst);
    } else if (!Inst->mayHaveSideEffects() &&
               !Inst->mayReadFromMemory() &&
               !isa<TermInst>(Inst) &&
               !isa<AllocationInst>(Inst) && /* not marked mayhavesideffects */
               hasLoopInvariantOperands(Inst, L, Invariant)) {
      Move.push_back(Inst);
      Invariant.insert(Inst);
    }
  }

  return true;
}

static void mapOperands(SILInstruction *I,
                        llvm::DenseMap<ValueBase *, SILValue> ValueMap) {
  for (auto &Opd : I->getAllOperands()) {
    SILValue OrigVal = Opd.get();
    ValueBase *OrigDef = OrigVal.getDef();
    if (SILValue MappedVal = ValueMap[OrigDef]) {
      unsigned ResultIdx = OrigVal.getResultNumber();
      // All mapped instructions have their result number set to zero. Except
      // for arguments that we followed along one edge to their incoming value
      // on that edge.
      if (isa<SILArgument>(OrigDef))
        ResultIdx = MappedVal.getResultNumber();
      Opd.set(SILValue(MappedVal.getDef(), ResultIdx));
    }
  }
}

static void
updateSSAForUseOfInst(SILSSAUpdater &Updater,
                      SmallVectorImpl<SILArgument*> &InsertedPHIs,
                      llvm::DenseMap<ValueBase *, SILValue> &ValueMap,
                      SILBasicBlock *Header, SILBasicBlock *EntryCheckBlock,
                      ValueBase *Inst) {
  if (Inst->use_empty())
    return;

  // Find the mapped instruction.
  SILValue MappedValue = ValueMap[Inst];
  auto *MappedInst = MappedValue.getDef();
  assert(MappedValue);
  assert(MappedInst);

  // For each use of a specific result value of the instruction.
  for (unsigned i = 0, e = Inst->getNumTypes(); i != e; ++i) {
    SILValue Res(Inst, i);
    // For block arguments, MappedValue is already indexed to indicate the
    // single result value that feeds the argument. In this case, i==0 because
    // SILArgument only produces one value.
    SILValue MappedRes =
        isa<SILArgument>(Inst) ? MappedValue : SILValue(MappedInst, i);
    assert(Res.getType() == MappedRes.getType() && "The types must match");

    InsertedPHIs.clear();
    Updater.Initialize(Res.getType());
    Updater.AddAvailableValue(Header, Res);
    Updater.AddAvailableValue(EntryCheckBlock, MappedRes);


    // Because of the way that phi nodes are represented we have to collect all
    // uses before we update SSA. Modifying one phi node can invalidate another
    // unrelated phi nodes operands through the common branch instruction (that
    // has to be modified). This would invalidate a plain ValueUseIterator.
    // Instead we collect uses wrapping uses in branches specially so that we
    // can reconstruct the use even after the branch has been modified.
    SmallVector<UseWrapper, 8> StoredUses;
    for (auto *U : Res.getUses())
      StoredUses.push_back(UseWrapper(U));
    for (auto U : StoredUses) {
      Operand *Use = U;
      SILInstruction *User = Use->getUser();
      assert(User && "Missing user");

      // Ignore uses in the same basic block.
      if (User->getParent() == Header)
        continue;

      assert(User->getParent() != EntryCheckBlock &&
             "The entry check block should dominate the header");
      Updater.RewriteUse(*Use);
    }
    // Canonicalize inserted phis to avoid extra BB Args.
    for (SILArgument *Arg : InsertedPHIs) {
      if (SILInstruction *Inst = replaceBBArgWithCast(Arg)) {
        Arg->replaceAllUsesWith(Inst);
        // DCE+SimplifyCFG runs as a post-pass cleanup.
        // DCE replaces dead arg values with undef.
        // SimplifyCFG deletes the dead BB arg.
      }
    }
  }
}

/// Rewrite the code we just created in the preheader and update SSA form.
static void
rewriteNewLoopEntryCheckBlock(SILBasicBlock *Header,
                              SILBasicBlock *EntryCheckBlock,
                              llvm::DenseMap<ValueBase *, SILValue> ValueMap) {
  SmallVector<SILArgument*, 4> InsertedPHIs;
  SILSSAUpdater Updater(&InsertedPHIs);

  // Fix PHIs (incomming arguments).
  for (auto *Inst: Header->getBBArgs())
    updateSSAForUseOfInst(Updater, InsertedPHIs, ValueMap, Header,
                          EntryCheckBlock, Inst);

  auto InstIter = Header->begin();

  // The terminator might change from under us.
  while (InstIter != Header->end()) {
    auto &Inst = *InstIter;
    updateSSAForUseOfInst(Updater, InsertedPHIs, ValueMap, Header,
                          EntryCheckBlock, &Inst);
    InstIter++;
  }
}

/// Update the dominator tree after rotating the loop.
/// The former preheader now dominates all of the former headers children. The
/// former latch now dominates the former header.
static void updateDomTree(DominanceInfo *DT, SILBasicBlock *Preheader,
                          SILBasicBlock *Latch, SILBasicBlock *Header) {
  auto *HeaderN = DT->getNode(Header);
  SmallVector<DominanceInfoNode *, 4> Children(HeaderN->begin(),
                                               HeaderN->end());
  auto *PreheaderN = DT->getNode(Preheader);
  for (auto *Child : Children)
    DT->changeImmediateDominator(Child, PreheaderN);

  if (Header != Latch)
    DT->changeImmediateDominator(HeaderN, DT->getNode(Latch));
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
  BranchInst *Branch = BranchInst::create(BranchLoc, Header, BBArgs, *F);
  BEBlock->getInstList().insert(BEBlock->getInstList().end(), Branch);

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


/// Canonicalize the loop for rotation and downstream passes.
///
/// Create a single preheader and single latch block.
///
/// FIXME: We should identify nested loops with a common header and separate
/// them before merging the latch. See LLVM's separateNestedLoop.
static bool simplifyLoop(SILLoop *L, DominanceInfo *DT, SILLoopInfo *LI) {
  bool ChangedCFG = false;
  if (!L->getLoopPreheader()) {
    if (insertPreheader(L, DT, LI))
      ChangedCFG = true;
    else
      return ChangedCFG; // Skip further simplification with no preheader.
  }
  if (!L->getLoopLatch())
    ChangedCFG |= (insertBackedgeBlock(L, DT, LI) != nullptr);

  return ChangedCFG;
}

static bool rotateLoopAtMostUpToLatch(SILLoop *L, DominanceInfo *DT,
                                      SILLoopInfo *LI, bool ShouldVerify) {
  auto *Latch = L->getLoopLatch();
  if (!Latch) {
    DEBUG(llvm::dbgs() << *L << " does not have a single latch block\n");
    return false;
  }

  bool DidRotate = rotateLoop(L, DT, LI, false /* RotateSingleBlockLoops */,
                              Latch, ShouldVerify);

  // Keep rotating at most until we hit the original latch.
  if (DidRotate)
    while (rotateLoop(L, DT, LI, false, Latch, ShouldVerify)) {}

  return DidRotate;
}

/// We rotated a loop if it has the following properties.
///
/// * It has an exiting header with a conditional branch.
/// * It has a preheader (the function will try to create one for critical edges
///   from cond_br).
///
/// We will rotate at most up to the basic block passed as an argument.
/// We will not rotate a loop where the header is equal to the latch except is
/// RotateSingleBlockLoops is true.
///
/// Note: The code relies on the 'UpTo' basic block to stay within the rotate
/// loop for termination.
bool swift::rotateLoop(SILLoop *L, DominanceInfo *DT, SILLoopInfo *LI,
                       bool RotateSingleBlockLoops, SILBasicBlock *UpTo,
                       bool ShouldVerify) {
  assert(L != nullptr && DT != nullptr && LI != nullptr &&
         "Missing loop information");

  auto *Header = L->getHeader();
  if (!Header)
    return false;

  // We need a preheader - this is also a cannonicalization for follow-up
  // passes.
  auto *Preheader = L->getLoopPreheader();
  if (!Preheader) {
    DEBUG(llvm::dbgs() << *L << " no preheader\n");
    DEBUG(L->getHeader()->getParent()->dump());
    return false;
  }

  if (!RotateSingleBlockLoops && Header == UpTo)
    return false;

  assert(RotateSingleBlockLoops || L->getBlocks().size() != 1);

  // Need a conditional branch that guards the entry into the loop.
  auto *LoopEntryBranch = dyn_cast<CondBranchInst>(Header->getTerminator());
  if (!LoopEntryBranch)
    return false;

  // The header needs to exit the loop.
  if (!L->isLoopExiting(Header)) {
    DEBUG(llvm::dbgs() << *L << " not a exiting header\n");
    DEBUG(L->getHeader()->getParent()->dump());
    return false;
  }

  // We need a single backedge and the latch must not exit the loop if it is
  // also the header.
  auto *Latch = L->getLoopLatch();
  if (!Latch) {
    DEBUG(llvm::dbgs() << *L << " no single latch\n");
    return false;
  }

  // Make sure we can duplicate the header.
  SmallVector<SILInstruction *, 8> MoveToPreheader;
  if (!canDuplicateOrMoveToPreheader(L, Preheader, Header, MoveToPreheader)) {
    DEBUG(llvm::dbgs() << *L << " instructions in header preventing rotating\n");
    return false;
  }

  auto *NewHeader = LoopEntryBranch->getTrueBB();
  auto *Exit = LoopEntryBranch->getFalseBB();
  if (L->contains(Exit))
    std::swap(NewHeader, Exit);
  assert(L->contains(NewHeader) && !L->contains(Exit) &&
         "Could not find loop header and exit block");

  // We don't want to rotate such that we merge two headers of separate loops
  // into one. This can be turned into an assert again once we have guaranteed
  // preheader insertions.
  if (!NewHeader->getSinglePredecessor() && Header != Latch)
    return false;

  // Now that we know we can perform the rotation - move the instructions that
  // need moving.
  for (auto *Inst : MoveToPreheader)
    Inst->moveBefore(Preheader->getTerminator());

  DEBUG(llvm::dbgs() << " Rotating " << *L);

  // Map the values for the duplicated header block. We are duplicating the
  // header instructions into the end of the preheader.
  llvm::DenseMap<ValueBase *, SILValue> ValueMap;

  // The original 'phi' argument values are just the values coming from the
  // preheader edge.
  ArrayRef<SILArgument *> PHIs = Header->getBBArgs();
  OperandValueArrayRef PreheaderArgs =
      cast<BranchInst>(Preheader->getTerminator())->getArgs();
  assert(PHIs.size() == PreheaderArgs.size() &&
         "Basic block arguments and incoming edge mismatch");

  // Here we also store the value index to use into the value map (versus
  // non-argument values where the operand use decides which value index to
  // use).
  for (unsigned Idx = 0, E = PHIs.size(); Idx != E; ++Idx)
    ValueMap[PHIs[Idx]] = PreheaderArgs[Idx];

  // The other instructions are just cloned to the preheader.
  TermInst *PreheaderBranch = Preheader->getTerminator();
  for (auto &Inst : *Header) {
    SILInstruction *I = Inst.clone(PreheaderBranch);
    mapOperands(I, ValueMap);

    // The actual operand will sort out which result idx to use.
    ValueMap[&Inst] = SILValue(I, 0);
  }

  PreheaderBranch->dropAllReferences();
  PreheaderBranch->eraseFromParent();

  // If there were any uses of instructions in the duplicated loop entry check
  // block rewrite them using the ssa updater.
  rewriteNewLoopEntryCheckBlock(Header, Preheader, ValueMap);

  L->moveToHeader(NewHeader);

  // Now the original preheader dominates all of headers children and the
  // original latch dominates the header.
  updateDomTree(DT, Preheader, Latch, Header);

  assert(DT->getNode(NewHeader)->getIDom() == DT->getNode(Preheader));
  assert(!DT->dominates(Header, Exit) ||
         DT->getNode(Exit)->getIDom() == DT->getNode(Preheader));
  assert(DT->getNode(Header)->getIDom() == DT->getNode(Latch) ||
         ((Header == Latch) &&
          DT->getNode(Header)->getIDom() == DT->getNode(Preheader)));

  // Beautify the IR. Move the old header to after the old latch as it is now
  // the latch.
  Header->moveAfter(Latch);

  // Merge the the old latch with the old header if possible.
  mergeBasicBlockWithSuccessor(Latch, DT, LI);

  // Create a new preheader.
  splitIfCriticalEdge(Preheader, NewHeader, DT, LI);

  if (ShouldVerify) {
    DT->verify();
    LI->verify();
    Latch->getParent()->verify();
  }

  DEBUG(llvm::dbgs() << "  to " << *L);
  DEBUG(L->getHeader()->getParent()->dump());
  return true;
}

namespace {

class LoopRotation : public SILFunctionTransform {

  StringRef getName() override { return "SIL Loop Rotation"; }

  void run() override {
    SILLoopAnalysis *LA = PM->getAnalysis<SILLoopAnalysis>();
    assert(LA);
    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    assert(DA);

    SILFunction *F = getFunction();
    assert(F);
    SILLoopInfo *LI = LA->getLoopInfo(F);
    assert(LI);
    DominanceInfo *DT = DA->getDomInfo(F);

    if (LI->empty()) {
      DEBUG(llvm::dbgs() << "No loops in " << F->getName() << "\n");
      return;
    }
    if (!ShouldRotate) {
      DEBUG(llvm::dbgs() << "Skipping loop rotation in " << F->getName()
            << "\n");
      return;
    }
    DEBUG(llvm::dbgs() << "Rotating loops in " << F->getName() << "\n");
    bool ShouldVerify = getOptions().VerifyAll;

    bool Changed = false;
    for (auto *LoopIt : *LI) {
      // Rotate loops recursively bottom-up in the loop tree.
      SmallVector<SILLoop *, 8> Worklist;
      Worklist.push_back(LoopIt);
      for (unsigned i = 0; i < Worklist.size(); ++i) {
        auto *L = Worklist[i];
        for (auto *SubLoop : *L)
          Worklist.push_back(SubLoop);
      }

      while (!Worklist.empty()) {
        SILLoop *Loop = Worklist.pop_back_val();
        Changed |= simplifyLoop(Loop, DT, LI);
        Changed |= rotateLoopAtMostUpToLatch(Loop, DT, LI, ShouldVerify);
      }
    }

    if (Changed) {
      // We preserve loop info and the dominator tree.
      auto PreservedDT = DA->preserveDomAnalysis(F);
      auto PreservedLI = LA->preserveAnalysis(F);

      PM->invalidateAnalysis(F, SILAnalysis::InvalidationKind::CFG);

      // Update domtree and loop info.
      DA->updateAnalysis(F, std::move(PreservedDT));
      LA->updateAnalysis(F, std::move(PreservedLI));
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createLoopRotate() {
  return new LoopRotation();
}

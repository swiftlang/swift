//===--- LoopRotate.cpp - Loop structure simplify -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-looprotate"

#include "swift/SIL/Dominance.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"

#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

static llvm::cl::opt<bool> ShouldRotate("sil-looprotate",
                                        llvm::cl::init(true));

/// Check whether all operands are loop invariant.
static bool hasLoopInvariantOperands(SILInstruction *I, SILLoop *L,
                                     llvm::DenseSet<SILInstruction *> &Inv) {
  auto Opds = I->getAllOperands();

  return std::all_of(Opds.begin(), Opds.end(), [=](Operand &Op) {

    ValueBase *Def = Op.get();
    // Operand is outside the loop or marked invariant.
    if (auto *Inst = Def->getDefiningInstruction())
      return !L->contains(Inst->getParent()) || Inv.count(Inst);
    if (auto *Arg = dyn_cast<SILArgument>(Def))
      return !L->contains(Arg->getParent());

    return false;
  });
}

/// We cannot duplicate blocks with AllocStack instructions (they need to be
/// FIFO). Other instructions can be moved to the preheader.
static bool
canDuplicateOrMoveToPreheader(SILLoop *L, SILBasicBlock *Preheader,
                              SILBasicBlock *Blk,
                              SmallVectorImpl<SILInstruction *> &Move) {
  llvm::DenseSet<SILInstruction *> Invariant;
  for (auto &I : *Blk) {
    auto *Inst = &I;
    if (auto *MI = dyn_cast<MethodInst>(Inst)) {
      if (MI->getMember().isForeign)
        return false;
      if (!hasLoopInvariantOperands(Inst, L, Invariant))
        continue;
      Move.push_back(Inst);
      Invariant.insert(Inst);
    } else if (!I.isTriviallyDuplicatable())
      return false;
    else if (isa<FunctionRefInst>(Inst)) {
      Move.push_back(Inst);
      Invariant.insert(Inst);
    } else if (isa<DynamicFunctionRefInst>(Inst)) {
      Move.push_back(Inst);
      Invariant.insert(Inst);
    }
    else if (isa<PreviousDynamicFunctionRefInst>(Inst)) {
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
                        const llvm::DenseMap<ValueBase *, SILValue> &ValueMap) {
  for (auto &Opd : I->getAllOperands()) {
    SILValue OrigVal = Opd.get();
    ValueBase *OrigDef = OrigVal;
    auto Found = ValueMap.find(OrigDef);
    if (Found != ValueMap.end()) {
      SILValue MappedVal = Found->second;
      Opd.set(MappedVal);
    }
  }
}

static void updateSSAForUseOfValue(
    SILSSAUpdater &Updater, SmallVectorImpl<SILPhiArgument *> &InsertedPHIs,
    const llvm::DenseMap<ValueBase *, SILValue> &ValueMap,
    SILBasicBlock *Header, SILBasicBlock *EntryCheckBlock,
    SILValue Res) {
  // Find the mapped instruction.
  assert(ValueMap.count(Res) && "Expected to find value in map!");
  SILValue MappedValue = ValueMap.find(Res)->second;
  assert(MappedValue);
  assert(Res->getType() == MappedValue->getType() && "The types must match");

  InsertedPHIs.clear();
  Updater.Initialize(Res->getType());
  Updater.AddAvailableValue(Header, Res);
  Updater.AddAvailableValue(EntryCheckBlock, MappedValue);


  // Because of the way that phi nodes are represented we have to collect all
  // uses before we update SSA. Modifying one phi node can invalidate another
  // unrelated phi nodes operands through the common branch instruction (that
  // has to be modified). This would invalidate a plain ValueUseIterator.
  // Instead we collect uses wrapping uses in branches specially so that we
  // can reconstruct the use even after the branch has been modified.
  SmallVector<UseWrapper, 8> StoredUses;
  for (auto *U : Res->getUses())
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
  for (SILPhiArgument *Arg : InsertedPHIs) {
    if (SILValue Inst = replaceBBArgWithCast(Arg)) {
      Arg->replaceAllUsesWith(Inst);
      // DCE+SimplifyCFG runs as a post-pass cleanup.
      // DCE replaces dead arg values with undef.
      // SimplifyCFG deletes the dead BB arg.
    }
  }
}

static void updateSSAForUseOfInst(
    SILSSAUpdater &Updater, SmallVectorImpl<SILPhiArgument *> &InsertedPHIs,
    const llvm::DenseMap<ValueBase *, SILValue> &ValueMap,
    SILBasicBlock *Header, SILBasicBlock *EntryCheckBlock,
    SILInstruction *Inst) {
  for (auto result : Inst->getResults())
    updateSSAForUseOfValue(Updater, InsertedPHIs, ValueMap, Header,
                           EntryCheckBlock, result);
}

/// Rewrite the code we just created in the preheader and update SSA form.
static void
rewriteNewLoopEntryCheckBlock(SILBasicBlock *Header,
                              SILBasicBlock *EntryCheckBlock,
                        const llvm::DenseMap<ValueBase *, SILValue> &ValueMap) {
  SmallVector<SILPhiArgument *, 4> InsertedPHIs;
  SILSSAUpdater Updater(&InsertedPHIs);

  // Fix PHIs (incoming arguments).
  for (auto *Arg : Header->getArguments())
    updateSSAForUseOfValue(Updater, InsertedPHIs, ValueMap, Header,
                           EntryCheckBlock, Arg);

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

static bool rotateLoopAtMostUpToLatch(SILLoop *L, DominanceInfo *DT,
                                      SILLoopInfo *LI, bool ShouldVerify) {
  auto *Latch = L->getLoopLatch();
  if (!Latch) {
    LLVM_DEBUG(llvm::dbgs() << *L << " does not have a single latch block\n");
    return false;
  }

  bool DidRotate = rotateLoop(L, DT, LI, false /* RotateSingleBlockLoops */,
                              Latch, ShouldVerify);

  // Keep rotating at most until we hit the original latch.
  if (DidRotate)
    while (rotateLoop(L, DT, LI, false, Latch, ShouldVerify)) {}

  return DidRotate;
}

/// Check whether this a single basic block loop - ignoring split back edges.
static bool isSingleBlockLoop(SILLoop *L) {
  auto Blocks = L->getBlocks();
  auto NumBlocks = Blocks.size();
  if (NumBlocks > 2)
    return false;

  if (NumBlocks == 1)
    return true;

  auto *Header = L->getHeader();
  auto *BackEdge = Blocks[1];
  if (BackEdge == Header)
    BackEdge = Blocks[0];

  if (!BackEdge->getSingleSuccessorBlock())
    return false;

  assert(BackEdge->getSingleSuccessorBlock() == Header &&
         "Loop not well formed");

  // Check whether the back-edge block is just a split-edge.
  return ++BackEdge->begin() == BackEdge->end();
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

  // We need a preheader - this is also a canonicalization for follow-up
  // passes.
  auto *Preheader = L->getLoopPreheader();
  if (!Preheader) {
    LLVM_DEBUG(llvm::dbgs() << *L << " no preheader\n");
    LLVM_DEBUG(L->getHeader()->getParent()->dump());
    return false;
  }

  if (!RotateSingleBlockLoops && (Header == UpTo || isSingleBlockLoop(L)))
    return false;

  assert(RotateSingleBlockLoops || L->getBlocks().size() != 1);

  // Need a conditional branch that guards the entry into the loop.
  auto *LoopEntryBranch = dyn_cast<CondBranchInst>(Header->getTerminator());
  if (!LoopEntryBranch)
    return false;

  // The header needs to exit the loop.
  if (!L->isLoopExiting(Header)) {
    LLVM_DEBUG(llvm::dbgs() << *L << " not an exiting header\n");
    LLVM_DEBUG(L->getHeader()->getParent()->dump());
    return false;
  }

  // We need a single backedge and the latch must not exit the loop if it is
  // also the header.
  auto *Latch = L->getLoopLatch();
  if (!Latch) {
    LLVM_DEBUG(llvm::dbgs() << *L << " no single latch\n");
    return false;
  }

  // Make sure we can duplicate the header.
  SmallVector<SILInstruction *, 8> MoveToPreheader;
  if (!canDuplicateOrMoveToPreheader(L, Preheader, Header, MoveToPreheader)) {
    LLVM_DEBUG(llvm::dbgs() << *L
                            << " instructions in header preventing rotating\n");
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
  if (!NewHeader->getSinglePredecessorBlock() && Header != Latch)
    return false;

  // Now that we know we can perform the rotation - move the instructions that
  // need moving.
  for (auto *Inst : MoveToPreheader)
    Inst->moveBefore(Preheader->getTerminator());

  LLVM_DEBUG(llvm::dbgs() << " Rotating " << *L);

  // Map the values for the duplicated header block. We are duplicating the
  // header instructions into the end of the preheader.
  llvm::DenseMap<ValueBase *, SILValue> ValueMap;

  // The original 'phi' argument values are just the values coming from the
  // preheader edge.
  ArrayRef<SILArgument *> PHIs = Header->getArguments();
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
    if (SILInstruction *cloned = Inst.clone(PreheaderBranch)) {
      mapOperands(cloned, ValueMap);

      // The actual operand will sort out which result idx to use.
      auto instResults = Inst.getResults();
      auto clonedResults = cloned->getResults();
      assert(instResults.size() == clonedResults.size());
      for (auto i : indices(instResults))
        ValueMap[instResults[i]] = clonedResults[i];
    }
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

  // Merge the old latch with the old header if possible.
  mergeBasicBlockWithSuccessor(Latch, DT, LI);

  // Create a new preheader.
  splitIfCriticalEdge(Preheader, NewHeader, DT, LI);

  if (ShouldVerify) {
    DT->verify();
    LI->verify();
    Latch->getParent()->verify();
  }

  LLVM_DEBUG(llvm::dbgs() << "  to " << *L);
  LLVM_DEBUG(L->getHeader()->getParent()->dump());
  return true;
}

namespace {

class LoopRotation : public SILFunctionTransform {

  void run() override {
    SILLoopAnalysis *LA = PM->getAnalysis<SILLoopAnalysis>();
    assert(LA);
    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    assert(DA);

    SILFunction *F = getFunction();
    assert(F);
    SILLoopInfo *LI = LA->get(F);
    assert(LI);
    DominanceInfo *DT = DA->get(F);

    if (LI->empty()) {
      LLVM_DEBUG(llvm::dbgs() << "No loops in " << F->getName() << "\n");
      return;
    }
    if (!ShouldRotate) {
      LLVM_DEBUG(llvm::dbgs() << "Skipping loop rotation in " << F->getName()
                              << "\n");
      return;
    }
    LLVM_DEBUG(llvm::dbgs() << "Rotating loops in " << F->getName() << "\n");
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
        Changed |= canonicalizeLoop(Loop, DT, LI);
        Changed |= rotateLoopAtMostUpToLatch(Loop, DT, LI, ShouldVerify);
      }
    }

    if (Changed) {
      // We preserve loop info and the dominator tree.
      DA->lockInvalidation();
      LA->lockInvalidation();
      PM->invalidateAnalysis(F, SILAnalysis::InvalidationKind::FunctionBody);
      DA->unlockInvalidation();
      LA->unlockInvalidation();
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createLoopRotate() {
  return new LoopRotation();
}

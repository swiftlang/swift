//===--- LoopBranchHoist.cpp - Hoist conditional branches out of loops ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-loopbranchhoist"

#include "llvm/ADT/DepthFirstIterator.h"

#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/PerformanceInlinerUtils.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"

using namespace swift;
using namespace swift::PatternMatch;

using llvm::DenseMap;
using llvm::MapVector;

namespace {

  /// Clone the basic blocks in a loop.
  class ConditionalLoopCloner : public SILCloner<ConditionalLoopCloner> {
    SILLoop *Loop;
    unsigned int argNumber;

    friend class SILInstructionVisitor<ConditionalLoopCloner>;
    friend class SILCloner<ConditionalLoopCloner>;

  public:
    ConditionalLoopCloner(SILLoop *Loop)
    : SILCloner<ConditionalLoopCloner>(*Loop->getHeader()->getParent()), Loop(Loop) {}

    /// Clone the basic blocks belonging to false and shared paths through the loop.
    void cloneSharedLoopParts(DominanceInfo *domTree);

    /// Get a map from basic blocks or the original loop to the cloned loop.
    MapVector<SILBasicBlock *, SILBasicBlock *> &getBBMap() { return BBMap; }

    DenseMap<SILValue, SILValue> &getValueMap() { return ValueMap; }

  protected:
    SILValue remapValue(SILValue V) {
      if (auto *BB = V->getParentBlock()) {
        if (!Loop->contains(BB))
          return V;
      }
      return SILCloner<ConditionalLoopCloner>::remapValue(V);
    }
    void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
      SILCloner<ConditionalLoopCloner>::postProcess(Orig, Cloned);
    }
  };

} // end anonymous namespace

void ConditionalLoopCloner::cloneSharedLoopParts(DominanceInfo *domTree) {
  auto *Header = Loop->getHeader();
  auto *CurFun = Loop->getHeader()->getParent();
  auto condBr = dyn_cast<CondBranchInst>(Header->getTerminator());
  auto condTrueBB = condBr->getTrueBB();

  SmallVector<SILBasicBlock *, 4> exitBlocks;
  Loop->getExitBlocks(exitBlocks);
  for (auto *BB : exitBlocks) {
    for (auto *dest : BB->getSuccessorBlocks())
      BBMap[dest] = dest;
  }

  for (auto *BB : Loop->getBlocks()) {
    // Don't need to copy blocks that are only in the true half of the loop
    bool inTrue = domTree->dominates(&condTrueBB->front(), &BB->front());
    if (inTrue)
      BBMap[BB] = BB;
  }

  auto *ClonedHeader = CurFun->createBasicBlock();
  BBMap[Header] = ClonedHeader;

  // Clone args.
  for (auto *arg : Header->getArguments()) {
    SILValue mappedArg = ClonedHeader->createPHIArgument(
                                                         getOpType(arg->getType()), ValueOwnershipKind::Owned);
    ValueMap.insert(std::make_pair(arg, mappedArg));
  }

  // Clone the instructions in this basic block and recursively clone
  // successor blocks.
  getBuilder().setInsertionPoint(ClonedHeader);
  visitSILBasicBlock(Header);
  // Fix-up terminators.
  for (auto BBPair : BBMap) {
    if (BBPair.first != BBPair.second) {
      getBuilder().setInsertionPoint(BBPair.second);
      visit(BBPair.first->getTerminator());
    }
  }
}

/// Check whether we can duplicate the instructions in the loop.
static bool canAndShouldDuplicateLoop(SILLoop *Loop) {
  assert(Loop->getSubLoops().empty() && "Expect innermost loops");

  // We can duplicate a loop if we can duplicate the instructions it holds.
  for (auto *BB : Loop->getBlocks()) {
    for (auto &Inst : *BB) {
      if (!Loop->canDuplicate(&Inst))
        return false;
    }
  }
  return true;
}

static void redirectToBothLoops(SILBasicBlock *latch, unsigned int argNumber,
                                SILBasicBlock *trueHeader, SILBasicBlock *falseHeader) {
  auto existing = cast<BranchInst>(latch->getTerminator());
  auto dropOp = existing->getOperand(argNumber);
  SmallVector<SILValue, 4> newArgs;
  for (auto &operand : existing->getAllOperands()) {
    if (operand.get() != dropOp)
      newArgs.push_back(operand.get());
  }
  SILBuilder(existing).createCondBranch(existing->getLoc(), dropOp, trueHeader, newArgs,
                                        falseHeader, newArgs);
  existing->eraseFromParent();
}

static void redirectToSingleLoop(SILBasicBlock *latch, unsigned int argNumber,
                                 SILBasicBlock *header) {
  auto existing = cast<BranchInst>(latch->getTerminator());
  auto dropOp = existing->getOperand(argNumber);
  SmallVector<SILValue, 4> newArgs;
  for (auto &operand : existing->getAllOperands()) {
    if (operand.get() != dropOp)
      newArgs.push_back(operand.get());
  }
  SILBuilder(existing).createBranch(existing->getLoc(), header, newArgs);
  existing->eraseFromParent();
}



/// Try to hoist branch from loop, thus duplicating parts of the loop in both branches.
static bool tryToHoist(SILLoop *loop, unsigned int argNumber, DominanceInfo *domTree) {
  assert(loop->getSubLoops().empty() && "Expecting innermost loops");

  auto *preheader = loop->getLoopPreheader();
  if (!preheader || !isa<BranchInst>(preheader->getTerminator()))
    return false;

  auto *header = loop->getHeader();
  auto headerArgument = header->getArgument(argNumber);
  auto condBr = dyn_cast<CondBranchInst>(header->getTerminator());
  if (!condBr)
    return false;
  if (condBr->getCondition() != headerArgument)
    return false;

  auto trueBB = condBr->getTrueBB();
  auto trueBBFront = &trueBB->front();
  auto falseBB = condBr->getFalseBB();
  auto falseBBFront = &falseBB->front();
  auto loopFront = &header->front();
  bool modifiedInTrue = false;
  bool modifiedInFalse = false;

  SmallVector<SILBasicBlock *, 8> latchBlocks;
  loop->getLoopLatches(latchBlocks);

  for (auto latch : latchBlocks) {
    auto terminator = dyn_cast<BranchInst>(latch->getTerminator());
    if (!terminator)
      return false;

    auto arg = terminator->getAllOperands()[argNumber].get()->getDefiningInstruction();
    if (!arg)
      return false;

    bool trueDom = domTree->dominates(trueBBFront, arg);
    bool falseDom = domTree->dominates(falseBBFront, arg);
    bool inLoop = domTree->dominates(loopFront, arg);

    if (trueDom && falseDom && inLoop) // arg modified on both true and false paths
      return false;
    modifiedInTrue |= trueDom;
    modifiedInFalse |= falseDom;
  }

  if (modifiedInTrue && modifiedInFalse)
    return false;

  if (!canAndShouldDuplicateLoop(loop))
    return false;

  ConditionalLoopCloner cloner(loop);
  cloner.cloneSharedLoopParts(domTree);

  // Okay, now rethread all the branches through the two separated loop parts...
  auto clonedBB = cloner.getBBMap();
  auto clonedHeader = clonedBB[header];

  // Header cond_br becomes unconditional in each half.
  SILBuilder(condBr).createBranch(condBr->getLoc(), trueBB);
  condBr->eraseFromParent();
  auto clonedCondBr = clonedHeader->getTerminator();
  SILBuilder(clonedCondBr).createBranch(clonedCondBr->getLoc(), clonedBB[falseBB]);
  clonedCondBr->eraseFromParent();

  // And then remove the boolean phi from the header in each half.
  header->eraseArgument(argNumber);
  clonedHeader->eraseArgument(argNumber);

  // But preheader becomes conditional to select which loop initially.
  redirectToBothLoops(preheader, argNumber, header, clonedHeader);

  for (auto latch : latchBlocks) {
    auto terminator = latch->getTerminator();
    auto arg = terminator->getAllOperands()[argNumber].get()->getDefiningInstruction();
    bool trueDom = domTree->dominates(trueBBFront, arg);
    bool falseDom = domTree->dominates(falseBBFront, arg);

    auto clonedLatch = clonedBB[latch];
    if (latch == clonedLatch || !clonedLatch) {
      // No clone so latch is just in the true loop
      if (trueDom) {
        redirectToBothLoops(latch, argNumber, header, clonedHeader);
      } else {
        redirectToSingleLoop(latch, argNumber, header);
      }
    } else {
      if (falseDom) // false loop changed the loop value
        redirectToBothLoops(clonedLatch, argNumber, header, clonedHeader);
      else
        redirectToSingleLoop(clonedLatch, argNumber, clonedHeader);

      bool onlyInFalse = domTree->dominates(falseBBFront, &latch->front());
      if (onlyInFalse) {
        latch->eraseFromParent();
      } else {
        if (trueDom) // true loop changed the loop value
          redirectToBothLoops(latch, argNumber, header, clonedHeader);
        else
          redirectToSingleLoop(latch, argNumber, header);
      }
    }
  }
  return true;
}

// =============================================================================
//                                 Driver
// =============================================================================

namespace {

  class LoopBranchHoisting : public SILFunctionTransform {

    void run() override {
      bool Changed = false;

      auto *Fun = getFunction();
      auto I1Ty = SILType::getBuiltinIntegerType(1, Fun->getASTContext());

      SILLoopInfo *LoopInfo = PM->getAnalysis<SILLoopAnalysis>()->get(Fun);
      DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
      DominanceInfo *domTree = nullptr;

      // Collect innermost loops.
      SmallVector<SILLoop *, 16> InnermostLoops;
      for (auto *Loop : *LoopInfo) {
        SmallVector<SILLoop *, 8> Worklist;
        Worklist.push_back(Loop);

        for (unsigned i = 0; i < Worklist.size(); ++i) {
          auto *L = Worklist[i];
          for (auto *SubLoop : *L)
            Worklist.push_back(SubLoop);
          if (L->getSubLoops().empty())
            InnermostLoops.push_back(L);
        }
      }

      // Check innermost loops.
      for (auto *loop : InnermostLoops) {
        auto *header = loop->getHeader();
        unsigned int headerArgs = header->getNumArguments();
        for (unsigned int argNumber = 0; argNumber < headerArgs; argNumber++) {
          if (header->getArgument(argNumber)->getType() == I1Ty) {
            if (!domTree) domTree = DA->get(Fun);
            if (tryToHoist(loop, argNumber, domTree)) {
              Changed = true;
              break; // don't try to hoist out more than one bool arg per loop
            }
          }
        }
      }

      if (Changed) {
        invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
      }
    }
  };

} // end anonymous namespace

SILTransform *swift::createLoopBranchHoist() {
    return new LoopBranchHoisting();
}


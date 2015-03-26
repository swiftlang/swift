//===-- CondFailOpt.cpp -  Optimizes cond_fail instructions -*- C++ -*-----===//
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

#define DEBUG_TYPE "condfailopt"

#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILArgument.h"

#include "llvm/Support/Debug.h"

using namespace swift;

namespace {
  
/// Optimize cond_fail instructions.
///
/// The first optimization tries to simplify the control flow. The following
/// sequence
///
///     bb1:
///       cond_br %c, bb2, other_bb
///     bb2:
///       cond_fail %literal_1  // condition is a literal 1 (always true)
///
/// is replaced with
///
///     bb1:
///       cond_fail %c
///       br other_bb           // cond_br is replaced by br
///     bb2:                    // bb2 is dead if it has no other predecessors
///       cond_fail %literal_1
///
/// The second optimization is a kind of partial dead-code elimination. A
/// sequence with a cond_fail in a control-flow merge
///
///     bb1:
///       br bb3(%c)
///     bb2:
///       %i = integer_literal
///       br bb3(%i)            // at least one input argument is constant
///     bb3(%a)
///       cond_fail %a          // %a has no other uses
///
/// is replaced with
///
///     bb1:
///       cond_fail %c
///       br bb3(%c)
///     bb2:
///       %i = integer_literal
///       cond_fail %i
///       br bb3(%i)
///     bb3(%a)                 // %a is dead
///
class CondFailOpt : public SILFunctionTransform {

  void optimizeBlock(SILBasicBlock *BB);
  
  void tryToReplaceCondBr(CondFailInst *CFI);
  
  bool tryToRemoveBlockArg(CondFailInst *CFI);
  
  bool Changed = false;
  bool CfgChanged = false;

public:
  CondFailOpt() {}

  StringRef getName() override { return "Optimize cond_fail instructions"; }

  void run() override;
};

void CondFailOpt::optimizeBlock(SILBasicBlock *BB) {

  for (auto InstIt = BB->begin(), End = BB->end(); InstIt != End;) {
    auto *CurInst = &*InstIt;
    ++InstIt;
    if (CondFailInst *CFI = dyn_cast<CondFailInst>(CurInst)) {
      
      // First optimization.
      tryToReplaceCondBr(CFI);
      
      // Second optimization. If it removed the cond_fail we can continue
      // searching for other cond_fail instructions in this block.
      if (tryToRemoveBlockArg(CFI))
        continue;

      // Abort because:
      // 1. We don't want to move other cond_fails over this cond_fail.
      // 2. In case of a single-block loop, tryToReplaceCondBr() may invalidate
      //    the iterator for this block's terminator.
      return;
    }

    // We don't want to move cond_fail instructions over instructions with
    // side-effects.
    if (CurInst->mayHaveSideEffects())
      return;
  }
}

static void createCondFail(CondFailInst *Orig, SILValue Cond, bool inverted,
                           SILBuilder &Builder) {
  if (inverted) {
    auto One = Builder.createIntegerLiteral(Orig->getLoc(), Cond.getType(), 1);
    Cond = Builder.createBuiltin(Orig->getLoc(),
                             Builder.getASTContext().getIdentifier("xor_Int1"),
                             Cond.getType(), {}, { Cond, One });
  }
  Builder.createCondFail(Orig->getLoc(), Cond);
}

// The first optimization: try to convert cond_br to br instructions by copying
// the cond_fail into a predecessor block.
void CondFailOpt::tryToReplaceCondBr(CondFailInst *CFI) {
  SILBasicBlock *B = CFI->getParent();
  
  // Check if the cond_fail condition is a constant true value.
  auto *IL = dyn_cast<IntegerLiteralInst>(CFI->getOperand());
  if (!IL || !IL->getValue().isAllOnesValue())
    return;

  for (auto PredIter = B->pred_begin(), End = B->pred_end(); PredIter != End;) {
    SILBasicBlock *Pred = *PredIter;
    ++PredIter;
    
    // Does the predecessor block end with a cond_br?
    auto *PredCondBr = dyn_cast<CondBranchInst>(Pred->getTerminator());
    if (!PredCondBr)
      continue;

    SILBuilderWithScope<4> Builder(PredCondBr);

    DEBUG(llvm::dbgs() << "### replace with br: " << *PredCondBr);
    
    // Copy the cond_fail into the predecessor block (wich the cond_br's
    // condition) and replace the cond_br with a br.
    if (PredCondBr->getFalseBB() == B) {
      createCondFail(CFI, PredCondBr->getCondition(), true, Builder);
      Builder.createBranch(PredCondBr->getLoc(),
                           PredCondBr->getTrueBB(), PredCondBr->getTrueArgs());
    } else {
      createCondFail(CFI, PredCondBr->getCondition(), false, Builder);
      Builder.createBranch(PredCondBr->getLoc(),
                           PredCondBr->getFalseBB(), PredCondBr->getFalseArgs());
    }
    PredCondBr->eraseFromParent();

    Changed = true;
    CfgChanged = true;
  }
}

// The second optimization: move the cond_fail into predecessor blocks to make
// the block argument dead.
bool CondFailOpt::tryToRemoveBlockArg(CondFailInst *CFI) {
  
  // Find the underlying condition value of the cond_fail.
  SILValue cond = CFI->getOperand();
  bool inverted = false;
  while (auto *BI = dyn_cast<BuiltinInst>(cond)) {
    
    // This is not a correctness check, but we only want to to the optimization
    // if the condition gets dead after moving the cond_fail.
    if (!BI->hasOneUse())
      return false;

    OperandValueArrayRef Args = BI->getArguments();
  
    if (BI->getBuiltinInfo().ID == BuiltinValueKind::Xor) {
      // Check if it's a boolean invertion of the condition.
      if (auto *IL = dyn_cast<IntegerLiteralInst>(Args[1])) {
        if (IL->getValue().isAllOnesValue()) {
          cond = Args[0];
          inverted = !inverted;
          continue;
        }
      } else if (auto *IL = dyn_cast<IntegerLiteralInst>(Args[0])) {
        if (IL->getValue().isAllOnesValue()) {
          cond = Args[1];
          inverted = !inverted;
          continue;
        }
      }
    }
    break;
  }
  // Check if the condition is a single-used argument in the current block.
  SILArgument *condArg = dyn_cast<SILArgument>(cond);
  if (!condArg || !condArg->hasOneUse())
    return false;
  
  SILBasicBlock *B = CFI->getParent();
  if (condArg->getParent() != B)
    return false;

  // Check if some of the predecessor blocks provide a constant for the
  // cond_fail condition. So that the optimization has a positive effect.
  bool somePredsAreConst = false;
  for (auto *Pred : B->getPreds()) {
    
    // The cond_fail must post-dominate the predecessor block. We may not
    // execute the cond_fail speculatively.
    if (!Pred->getSingleSuccessor())
      return false;

    SILValue incoming = condArg->getIncomingValue(Pred);
    if (isa<IntegerLiteralInst>(incoming)) {
      somePredsAreConst = true;
      break;
    }
  }
  if (!somePredsAreConst)
    return false;
  
  DEBUG(llvm::dbgs() << "### move to predecessors: " << *CFI);
  
  // Move the cond_fail to the predecessor blocks.
  for (auto *Pred : B->getPreds()) {
    SILValue incoming = condArg->getIncomingValue(Pred);
    SILBuilderWithScope<4> Builder(Pred->getTerminator());

    createCondFail(CFI, incoming, inverted, Builder);
  }
  CFI->eraseFromParent();
  Changed = true;
  return true;
}

void CondFailOpt::run() {
  
  DEBUG(llvm::dbgs() << getName() << " in " << getFunction()->getName() << '\n');

  Changed = false;
  CfgChanged = false;
  
  for (auto &BB : *getFunction()) {
    optimizeBlock(&BB);
  }
  if (Changed) {
    PM->invalidateAnalysis(getFunction(),
                           CfgChanged ?
                             SILAnalysis::PreserveKind::Calls :
                             SILAnalysis::PreserveKind::ProgramFlow);
  }
}
  

}

SILTransform *swift::createCondFailOpt() {
  return new CondFailOpt();
}

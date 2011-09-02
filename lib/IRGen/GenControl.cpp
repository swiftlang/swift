//===--- GenControl.cpp - IR Generation for Control Flow ------------------===//
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
//
//  This file implements general IR generation for control flow.
//
//===----------------------------------------------------------------------===//

#include "llvm/Function.h"
#include "Condition.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "JumpDest.h"
#include "RValue.h"

using namespace swift;
using namespace irgen;

/// Insert the given basic block after the IP block and move the
/// insertion point to it.  Only valid if the IP is valid.
void IRBuilder::emitBlock(llvm::BasicBlock *BB) {
  assert(ClearedIP == nullptr);
  llvm::BasicBlock *CurBB = GetInsertBlock();
  assert(CurBB && "current insertion point is invalid");
  CurBB->getParent()->getBasicBlockList().insertAfter(CurBB, BB);
  IRBuilderBase::SetInsertPoint(BB);
}

/// Insert the given basic block "anywhere".  The IP may be invalid,
/// in which case the block will be inserted after the block which
/// contained the IP before the IP was invalidated.
void IRBuilder::emitBlockAnywhere(llvm::BasicBlock *BB) {
  llvm::BasicBlock *IP = GetInsertBlock();
  if (!IP) {
    assert(ClearedIP && "no insertion point and none saved, either");
    IP = ClearedIP;
    ClearedIP = nullptr;
  }
  IP->getParent()->getBasicBlockList().insertAfter(IP, BB);
  IRBuilderBase::SetInsertPoint(BB);
}

/// Emit a branch to the given jump destination, threading out through
/// any cleanups we might need to run.  Leaves the insertion point at
/// the current point.
void IRGenFunction::emitBranch(JumpDest Dest) {
  // FIXME: scope depth
  Builder.CreateBr(Dest.getBlock());
}

/// Create a new basic block with the given name.  The block is not
/// automatically inserted into the function.
llvm::BasicBlock *
IRGenFunction::createBasicBlock(const llvm::Twine &Name) {
  return llvm::BasicBlock::Create(IGM.getLLVMContext(), Name);
}

/// Emit a boolean expression as a control-flow condition.
///
/// \param hasFalseCode - true if the false branch doesn't just lead
/// to the fallthrough.
Condition IRGenFunction::emitCondition(Expr *E, bool hasFalseCode) {
  assert(Builder.hasValidIP() && "emitting condition at unreachable point");

  // TODO: special-case interesting condition expressions.
  RValue RV = emitRValue(E);

  // FIXME: hard-coding this is not ideal.
  assert(RV.isScalar(1) && "r-value has unexpected form!");
  llvm::Value *V = RV.getScalars()[0];
  assert(V->getType()->isIntegerTy(1));

  llvm::BasicBlock *trueBB, *falseBB, *contBB;

  // Check for a constant condition.
  if (llvm::ConstantInt *C = dyn_cast<llvm::ConstantInt>(V)) {
    // If the condition is constant false, ignore the true branch.  We
    // will fall into the false branch unless there is none.
    if (C->isZero()) {
      trueBB = nullptr;
      falseBB = (hasFalseCode ? Builder.GetInsertBlock() : nullptr);

    // Otherwise, ignore the false branch.  We will fall into the true
    // branch.
    } else {
      trueBB = Builder.GetInsertBlock();
      falseBB = nullptr;
    }

    // There is no separate continuation block.
    contBB = nullptr;

  // Otherwise, the condition requires a conditional branch.
  } else {
    contBB = createBasicBlock("condition.cont");
    trueBB = createBasicBlock("if.true");

    llvm::BasicBlock *falseDestBB;
    if (hasFalseCode) {
      falseBB = falseDestBB = createBasicBlock("if.false");
    } else {
      falseBB = nullptr;
      falseDestBB = contBB;
    }

    Builder.CreateCondBr(V, trueBB, falseDestBB);
  }

  return Condition(trueBB, falseBB, contBB);
}

void Condition::enterTrue(IRGenFunction &IGF) {
  assert(TrueBB);
  assert(IGF.Builder.hasValidIP());

  // We only need a separate branch if there's a continuation block.
  if (!ContBB) return;

  IGF.Builder.emitBlock(TrueBB);
}

void Condition::exitTrue(IRGenFunction &IGF) {
  // Nothing to do if there's no continuation block.
  if (!ContBB) return;
  assert(FalseBB);

  // It's quite possible for this point to be unreachable, in which
  // case there's nothing to do.
  if (!IGF.Builder.hasValidIP()) return;

  // Otherwise, resume into the continuation block.
  IGF.Builder.CreateBr(ContBB);
}

void Condition::enterFalse(IRGenFunction &IGF) {
  assert(FalseBB);

  // We only need a separate branch if there's a continuation block.
  if (!ContBB) return;

  // It's possible to have no insertion point here because the end of
  // the continuation block was unreachable.
  IGF.Builder.emitBlockAnywhere(FalseBB);
}

void Condition::exitFalse(IRGenFunction &IGF) {
  // Nothing to do if there's no continuation block.
  if (!ContBB) return;

  // If there are no uses of the continuation block, bypass it.
  // We'll just emit continuation code directly after this, if
  // we even have an insertion point.
  if (ContBB->use_empty()) {

  // Otherwise, if there's no insertion point, merge the continuation
  // block back into its single predecessor and move the IP there.
  } else if (!IGF.Builder.hasValidIP()) {
    assert(ContBB->hasOneUse());
    llvm::BranchInst *Br = cast<llvm::BranchInst>(*ContBB->use_begin());
    assert(Br->isUnconditional());

    IGF.Builder.SetInsertPoint(Br->getParent());
    Br->eraseFromParent();
    assert(ContBB->use_empty());

  // Otherwise, branch to the continuation block and start inserting there.
  } else {
    IGF.Builder.CreateBr(ContBB);
  }
}

void Condition::complete(IRGenFunction &IGF) {
  // The invariants coming in here are strong but complex.

  // The only situation where we don't have an insertion point is when
  // the continuation block is totally unreachable.
  assert(IGF.Builder.hasValidIP() || !ContBB || ContBB->use_empty());

  // Either the continuation block is unreachable or the insertion
  // point immediately follows a terminator, but not both.
  assert((!ContBB || ContBB->use_empty()) !=
         (IGF.Builder.hasValidIP() &&
          isa<llvm::TerminatorInst>(IGF.Builder.GetInsertBlock()->back())));

  // If there is no continuation block, there is nothing to do.
  if (!ContBB) return;

  // If the continuation block has no uses, just destroy it and leave
  // the insertion point wherever it currently lies.
  if (ContBB->use_empty()) {
    delete ContBB;

  // Otherwise, we need to insert the continuation block.  We know we
  // have an insertion point.
  } else {
    IGF.Builder.emitBlock(ContBB);
  }
}

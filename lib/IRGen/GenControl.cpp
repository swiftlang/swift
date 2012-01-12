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

  // Sema forces conditions to have Builtin.i1 type, which guarantees this.
  // TODO: special-case interesting condition expressions.
  llvm::Value *V = emitAsPrimitiveScalar(E);
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

  // TrueBB has already been inserted somewhere unless there's a
  // continuation block.
  if (!ContBB) return;

  IGF.Builder.emitBlock(TrueBB);
}

void Condition::exitTrue(IRGenFunction &IGF) {
  // If there's no continuation block, it's because the condition was
  // folded to true.  In that case, we just continue emitting code as
  // if we were still in the true case, and we're unreachable iff the
  // end of the true case is unreachable.  In other words, there's
  // nothing to do.
  if (!ContBB) {
    assert(!FalseBB && "no continuation");
    return;
  }

  // If there is a continuation block, we should branch to it if the
  // current point is not unreachable.
  if (!IGF.Builder.hasValidIP()) {
    // If there is no false code, the continuation block has a use
    // because the main condition jumps directly to it.
    assert(ContBB->use_empty() || !FalseBB);
    return;
  }

  // Otherwise, resume into the continuation block.  This branch might
  // be folded by exitFalse if it turns out that that point is
  // unreachable.
  IGF.Builder.CreateBr(ContBB);

  // Coming out of exitTrue, we can be in one of three states:
  //   - a valid non-terminal IP, but only if there is no continuation
  //     block, which is only possible if there is no false block;
  //   - a valid terminal IP, if the end of the true block was reachable; or
  //   - a cleared IP, if the end of the true block was not reachable.
}

void Condition::enterFalse(IRGenFunction &IGF) {
  assert(FalseBB && "entering the false branch when it was not valid");

  // FalseBB has already been inserted somewhere unless there's a
  // continuation block.
  if (!ContBB) return;

  // It's possible to have no insertion point here if the end of the
  // true case was unreachable.
  IGF.Builder.emitBlockAnywhere(FalseBB);
}

void Condition::exitFalse(IRGenFunction &IGF) {
  // If there's no continuation block, it's because the condition was
  // folded to false.  In that case, we just continue emitting code as
  // if we were still in the false case, and we're unreachable iff the
  // end of the false case is unreachable.  In other words, there's
  // nothing to do.
  if (!ContBB) return;

  // If the true case didn't need the continuation block, then
  // we don't either, regardless of whether the current location
  // is reachable.  Just keep inserting / being unreachable
  // right where we are.
  if (ContBB->use_empty()) {

  // If the true case did need the continuation block, but the false
  // case doesn't, just merge the continuation block back into its
  // single predecessor and move the IP there.
  //
  // Note that doing this tends to strand the false code after
  // everything else in the function, so maybe it's not a great idea.
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
  // If there is no continuation block, it's because we
  // constant-folded the branch.  The case-exit will have left us in a
  // normal insertion state (i.e. not a post-terminator IP) with
  // nothing to clean up after.
  if (!ContBB) {
    assert(!IGF.Builder.hasPostTerminatorIP());
    return;
  }

  // Kill the continuation block if it's not being used.  Case-exits
  // only leave themselves post-terminator if they use the
  // continuation block, so we're in an acceptable insertion state.
  if (ContBB->use_empty()) {
    assert(!IGF.Builder.hasPostTerminatorIP());
    delete ContBB;
    return;
  }

  // Okay, we need to insert the continuation block.  Usually we'll be
  // post-terminator here, but we might not be if there is no false
  // case and the end of the true case is unreachable.
  assert(IGF.Builder.hasPostTerminatorIP() ||
         (!FalseBB && !IGF.Builder.hasValidIP()));

  IGF.Builder.emitBlockAnywhere(ContBB);
}

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

#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/CallSite.h"
#include "llvm/IR/Function.h"
#include "Cleanup.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

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

/// Create a new basic block with the given name.  The block is not
/// automatically inserted into the function.
llvm::BasicBlock *
IRGenFunction::createBasicBlock(const llvm::Twine &Name) {
  return llvm::BasicBlock::Create(IGM.getLLVMContext(), Name);
}

/// Get or create the jump-destination variable.
llvm::Value *IRGenFunction::getJumpDestSlot() {
  if (JumpDestSlot) return JumpDestSlot;

  JumpDestSlot = new llvm::AllocaInst(IGM.Int32Ty, "jumpdest.var", AllocaIP);
  return JumpDestSlot;
}

/// Get or create the unreachable block.
llvm::BasicBlock *IRGenFunction::getUnreachableBlock() {
  if (UnreachableBB) return UnreachableBB;

  // Create it at the very end of the function.
  UnreachableBB = createBasicBlock("unreachable");
  new llvm::UnreachableInst(UnreachableBB->getContext(), UnreachableBB);
  CurFn->getBasicBlockList().push_back(UnreachableBB);
  
  return UnreachableBB;
}

//****************************************************************************//
//******************************* EXCEPTIONS *********************************//
//****************************************************************************//

llvm::CallSite IRGenFunction::emitInvoke(llvm::CallingConv::ID convention,
                                         llvm::Value *fn,
                                         ArrayRef<llvm::Value*> args,
                                         const llvm::AttributeSet &attrs) {
  // TODO: exceptions!
  llvm::CallInst *call = Builder.CreateCall(fn, args);
  call->setAttributes(attrs);
  call->setCallingConv(convention);
  return call;
}                                        

//****************************************************************************//
//******************************** CLEANUPS **********************************//
//****************************************************************************//



/// Remove all the dead cleanups on the top of the cleanup stack.
static void popAndEmitTopDeadCleanups(IRGenFunction &IGF,
                                      DiverseStackImpl<Cleanup> &stack,
                                      CleanupsDepth end) {
  stack.checkIterator(end);

  while (stack.stable_begin() != end && stack.begin()->isDead()) {
    assert(!stack.empty());

    // We might get better results popping them all at once.
    stack.pop();
    stack.checkIterator(end);
  }
}

/// Leave a scope, with all its cleanups.
void IRGenFunction::endScope(CleanupsDepth depth) {
  Cleanups.checkIterator(depth);

  popAndEmitTopDeadCleanups(*this, Cleanups, InnermostScope);
}


/// Initialize a just-pushed cleanup.
Cleanup &IRGenFunction::initCleanup(Cleanup &cleanup, size_t allocSize,
                                    CleanupState state) {
  cleanup.AllocatedSize = allocSize;
  cleanup.State = unsigned(state);
  cleanup.NormalEntryBB = nullptr;


  return cleanup;
}

/// Change the state of a cleanup.
void IRGenFunction::setCleanupState(CleanupsDepth depth,
                                    CleanupState newState) {
  auto iter = Cleanups.find(depth);
  assert(iter != Cleanups.end() && "changing state of end of stack");
  setCleanupState(*iter, newState);

  if (newState == CleanupState::Dead && iter == Cleanups.begin())
    popAndEmitTopDeadCleanups(*this, Cleanups, InnermostScope);
}

void IRGenFunction::setCleanupState(Cleanup &cleanup, CleanupState newState) {
  cleanup.setState(newState);
}

// Anchor the Cleanup v-table in this translation unit.
void Cleanup::_anchor() {}

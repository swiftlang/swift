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

/// Create an alloca at the top of the function whose purpose is to
/// support the code in some way.
llvm::AllocaInst *
IRGenFunction::createSupportAlloca(llvm::Type *ty, Alignment align,
                                   const llvm::Twine &name) {
  llvm::AllocaInst *alloca = new llvm::AllocaInst(ty, name, AllocaIP);
  alloca->setAlignment(align.getValue());
  return alloca;
}

/// Like emitBlock, but merge the target block into its unique
/// predecessor if possible.
void IRBuilder::emitMergeableBlock(llvm::BasicBlock *BB) {
  assert(ClearedIP == nullptr);

  // Check our special case.
  if (BB->hasOneUse()) {
    llvm::BranchInst *br = dyn_cast<llvm::BranchInst>(*BB->use_begin());
    if (br && br->isUnconditional()) {
      IRBuilderBase::SetInsertPoint(br->getParent());
      br->eraseFromParent();
      delete BB;
      return;
    }
  }

  // Do the normal thing.
  emitBlock(BB);
}

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
void IRBuilder::insertBlockAnywhere(llvm::BasicBlock *BB) {
  llvm::BasicBlock *IP = GetInsertBlock();
  if (!IP) {
    assert(ClearedIP && "no insertion point and none saved, either");
    IP = ClearedIP;
  }
  IP->getParent()->getBasicBlockList().insertAfter(IP, BB);
}

/// Insert the given basic block "anywhere" and move the insertion
/// point to it.  The IP may be invalid, in which case the block will
/// be inserted after the block which contained the IP before the IP
/// was invalidated.
void IRBuilder::emitBlockAnywhere(llvm::BasicBlock *BB) {
  insertBlockAnywhere(BB);
  SetInsertPoint(BB);
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


/// The control of a cleanup is the information dynamically recording
/// whether or not the cleanup is active.
///
/// The goal is to avoid creating and updating a flag variable
/// whenever possible.
///
/// The current design is that the control is either:
///   - the address of a flag variable which must be written
///     on every state change or
///   - a pair of stable IPs recording the range of instructions
///     covering the last state that the cleanup was in.
/// The control becomes a flag address as soon as a transition
/// is recorded which makes it clear that the cleanup has been
/// referenced while both active and inactive.  Therefore there is
/// an invariant that at least one of these properties was false
/// at the last transition point of the cleanup.
class swift::irgen::CleanupControl {
  llvm::AllocaInst *Flag;
  IRBuilder::StableIP IPBegin;
  IRBuilder::StableIP IPEnd;

public:
  CleanupControl() : Flag(nullptr) {}

  static CleanupControl forFlag(llvm::AllocaInst *flag) {
    CleanupControl control;
    control.Flag = flag;
    return control;
  }

  static CleanupControl forIPRange(IRBuilder::StableIP begin,
                                   IRBuilder::StableIP end) {
    CleanupControl control;
    control.Flag = nullptr;
    control.IPBegin = begin;
    control.IPEnd = end;
    return control;
  }

  static CleanupControl forIP(IRBuilder::StableIP begin) {
    return forIPRange(begin, begin);
  }

  bool hasFlag() const { return Flag != nullptr; }
  llvm::AllocaInst *getFlag() const { assert(hasFlag()); return Flag; }
  IRBuilder::StableIP getIPBegin() const {
    assert(!hasFlag());
    return IPBegin;
  }
  IRBuilder::StableIP getIPEnd() const {
    assert(!hasFlag());
    return IPEnd;
  }
};

CleanupControl Cleanup::getControl() const {
  if (HasControlFlag)
    return CleanupControl::forFlag(
               reinterpret_cast<llvm::AllocaInst*>(ControlBegin));

  return CleanupControl::forIPRange(
               IRBuilder::StableIP::getFromOpaqueValue(ControlBegin),
               IRBuilder::StableIP::getFromOpaqueValue(ControlEnd));
}

void Cleanup::setControl(const CleanupControl &control) {
  if (control.hasFlag()) {
    HasControlFlag = true;
    ControlBegin = control.getFlag();
  } else {
    HasControlFlag = false;
    ControlBegin = control.getIPBegin().getOpaqueValue();
    ControlEnd = control.getIPEnd().getOpaqueValue();
  }
}

/// Alter a control flag at the current insertion point (which must be valid).
static void setFlagNow(IRGenFunction &IGF, llvm::AllocaInst *flag, bool value) {
  assert(IGF.Builder.hasValidIP());
  IGF.Builder.CreateStore(IGF.Builder.getInt1(value), flag, Alignment(1));
}

/// Alter a control flag at the given stable insertion point.
static void setFlagThen(IRGenFunction &IGF, IRBuilder::StableIP ip,
                        llvm::AllocaInst *flag, bool value) {
  if (!ip.isValid()) {
    assert(value == false && "activation point is unreachable!");
    return;
  }

  llvm::StoreInst *store =
    new llvm::StoreInst(IGF.Builder.getInt1(value), flag);
  store->setAlignment(1);
  ip.insert(store);
}

/// Create a control flag and set it on a cleanup.
static llvm::AllocaInst *createControlFlag(IRGenFunction &IGF,
                                           Cleanup &cleanup) {
  llvm::AllocaInst *flag =
    IGF.createSupportAlloca(IGF.IGM.Int1Ty, Alignment(1), "cleanup.isactive");
  cleanup.setControl(CleanupControl::forFlag(flag));
  return flag;
}

/// Transition the given cleanup to using a flag for control.
static void transitionControlToFlag(IRGenFunction &IGF, Cleanup &cleanup) {
  CleanupControl control = cleanup.getControl();
  assert(!control.hasFlag());

  bool isActiveNow = cleanup.isActive();

  llvm::AllocaInst *flag = createControlFlag(IGF, cleanup);
  setFlagThen(IGF, control.getIPBegin(), flag, isActiveNow);
  setFlagThen(IGF, control.getIPEnd(), flag, !isActiveNow);
  if (IGF.Builder.hasValidIP())
    setFlagNow(IGF, flag, isActiveNow);
}

/// The top cleanup on the stack is dead.  Pop it off and perform any
/// emission or forwarding necessary.
static void popAndEmitTopCleanup(IRGenFunction &IGF,
                                 DiverseStackImpl<Cleanup> &stack) {
  Cleanup &stackCleanup = *stack.begin();
  assert(stackCleanup.isDead() && "popping a living cleanup");

  if (!stackCleanup.isUsedWhileActive()) {
    // emitBranch never directly branches to a cleanup that's currently
    // inactive, and the popping/forwarding code never branches to a
    // cleanup that's never been active.
    assert(stackCleanup.getNormalEntryBlock() == nullptr);
    
    // This is just the usual invariant about the control flag only
    // existing on cleanups with both active and inactive references.
    assert(!stackCleanup.getControl().hasFlag());

    // Therefore we have nothing to do for this cleanup and can just
    // pop it.
    stack.pop();
    return;
  }

  assert(0);
}

/// Remove all the dead cleanups on the top of the cleanup stack.
static void popAndEmitTopDeadCleanups(IRGenFunction &IGF,
                                      DiverseStackImpl<Cleanup> &stack,
                                      CleanupsDepth end) {
  stack.checkIterator(end);

  while (stack.stable_begin() != end && stack.begin()->isDead()) {
    assert(!stack.empty());

    // We might get better results popping them all at once.
    popAndEmitTopCleanup(IGF, stack);
    stack.checkIterator(end);
  }
}

/// Leave a scope, with all its cleanups.
void IRGenFunction::endScope(CleanupsDepth depth) {
  Cleanups.checkIterator(depth);

  popAndEmitTopDeadCleanups(*this, Cleanups, InnermostScope);
}

/// End the scope induced by a single cleanup.
void IRGenFunction::endSingleCleanupScope() {
  assert(!Cleanups.empty() && "popping empty stack!");
  Cleanups.checkIterator(InnermostScope);
  assert(Cleanups.stable_begin() != InnermostScope &&
         "popping past innermost scope!");
  endScope(Cleanups.stabilize(llvm::next(Cleanups.begin())));
}

/// Initialize a just-pushed cleanup.
Cleanup &IRGenFunction::initCleanup(Cleanup &cleanup, size_t allocSize,
                                    CleanupState state) {
  cleanup.AllocatedSize = allocSize;
  cleanup.State = unsigned(state);
  cleanup.UsedWhileActive = false;
  cleanup.UsedWhileInactive = false;
  cleanup.HasFallthroughOutflow = false;
  //      HasControlFlag set below
  cleanup.NextDestLabel = 0;
  cleanup.NormalEntryBB = nullptr;
  //      ControlBegin set below
  //      ControlEnd set below

  cleanup.setControl(CleanupControl::forIP(Builder.getStableIP()));

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
  assert((newState != CleanupState::Active || Builder.hasValidIP()) &&
         "activating cleanup at invalid IP");

  // Do the transition now to avoid doing it in N places below.
  CleanupState oldState = cleanup.getState();
  cleanup.setState(newState);

  assert(newState != oldState && "cleanup state is already active");
  switch (oldState) {
  case CleanupState::Dead:
    llvm_unreachable("changing state of dead cleanup");

  // We're either activating or killing off a dormant cleanup.
  case CleanupState::Dormant:
    switch (newState) {
    case CleanupState::Dormant: llvm_unreachable("no transition");

    // We're killing a dormant cleanup.  This can probably happen.
    // This isn't a state transition we need to do anything about,
    // though.
    case CleanupState::Dead:
      return;

    // We're activating a dormant cleanup.
    case CleanupState::Active: {
      CleanupControl control = cleanup.getControl();

      // If we have a control flag already, just store to it.
      if (control.hasFlag()) {
        setFlagNow(*this, control.getFlag(), true);
        return;
      }

      // Otherwise, the control is the IP range over which the cleanup
      // was in an active state.

      // If the cleanup has been referenced in both states, force it now.
      if (cleanup.isUsedWhileActive() && cleanup.isUsedWhileInactive()) {
        transitionControlToFlag(*this, cleanup);
        return;
      }

      // If the cleanup was not referenced in this most recent dormant
      // spurt, don't update the locations.
      if (!cleanup.isUsedWhileInactive())
        return;

      // Otherwise, we have uses while inactive but none while active.
      // Set the range to the range of the dormant period.
      assert(!cleanup.isUsedWhileActive());
      cleanup.setControl(CleanupControl::forIPRange(control.getIPEnd(),
                                                    Builder.getStableIP()));
      return;
    }
    }
    llvm_unreachable("bad cleanup state");

  // We're deactivating an active cleanup, either temporarily or not.
  // The code is the same either way.
  case CleanupState::Active: {
    CleanupControl control = cleanup.getControl();

    // If we have a control flag already, just store to it.
    if (control.hasFlag()) {
      // Deactivation doesn't have to happen at a valid IP.
      if (Builder.hasValidIP())
        setFlagNow(*this, control.getFlag(), false);

      return;
    }

    // Otherwise, the control is an IP range over which the cleanup
    // was in a dormant state.

    // If the cleanup has been referenced in both states, force it now.
    if (cleanup.isUsedWhileActive() && cleanup.isUsedWhileInactive()) {
      transitionControlToFlag(*this, cleanup);
      return;
    }

    // If the cleanup was not referenced in this most recent active
    // interval, don't update the locations.
    if (cleanup.isUsedWhileActive()) {
      // do nothing

    // Otherwise, we have uses while active but none while inactive.
    // Set the range to the range of the active period.
    } else {
      assert(!cleanup.isUsedWhileInactive());
      cleanup.setControl(CleanupControl::forIPRange(control.getIPEnd(),
                                                    Builder.getStableIP()));
    }
    return;
  }
  }
  llvm_unreachable("bad cleanup state");
}

/// Emit a branch to the given jump destination, threading out through
/// any cleanups we might need to run.  Leaves the insertion point in
/// the current block.
void IRGenFunction::emitBranch(llvm::BasicBlock *cblock, CleanupsDepth cdepth) {
  assert(Builder.hasValidIP());

  auto depth = Cleanups.find(cdepth);

  // Find the topmost active cleanup.
  auto it = Cleanups.begin();
  for (; it != depth; ++it) {
    if (it->isActive())
      break;
  }

  // If we got out to the destination depth, we're done.
  assert(it == depth);
  Builder.CreateBr(cblock);
}

// Anchor the Cleanup v-table in this translation unit.
void Cleanup::_anchor() {}

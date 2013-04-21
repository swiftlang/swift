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

    }

    return;
  }
  }
  llvm_unreachable("bad cleanup state");
}

// Anchor the Cleanup v-table in this translation unit.
void Cleanup::_anchor() {}

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
#include "llvm/Function.h"
#include "Cleanup.h"
#include "Condition.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "JumpDest.h"
#include "Scope.h"

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

/// Emit a boolean expression as a control-flow condition.
///
/// \param hasFalseCode - true if the false branch doesn't just lead
/// to the fallthrough.
/// \param invertValue - true if this routine should invert the value before
/// testing true/false.
Condition IRGenFunction::emitCondition(Expr *E, bool hasFalseCode,
                                       bool invertValue) {
  assert(Builder.hasValidIP() && "emitting condition at unreachable point");

  // Sema forces conditions to have Builtin.i1 type, which guarantees this.
  // TODO: special-case interesting condition expressions.
  llvm::Value *V;
  {
    FullExpr Scope(*this);
    V = emitAsPrimitiveScalar(E);
  }
  assert(V->getType()->isIntegerTy(1));

  llvm::BasicBlock *trueBB, *falseBB, *contBB;

  // Check for a constant condition.
  if (llvm::ConstantInt *C = dyn_cast<llvm::ConstantInt>(V)) {
    // If the condition is constant false, ignore the true branch.  We
    // will fall into the false branch unless there is none.
    if (C->isZero() == !invertValue) {
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
    // If requested, invert the value.
    if (invertValue)
      V = Builder.CreateXor(V,
                            llvm::Constant::getIntegerValue(IGM.Int1Ty,
                                                            llvm::APInt(1, 1)));
    
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
//******************************** CLEANUPS **********************************//
//****************************************************************************//

/// The outflows of a cleanup are branches that terminate after the
/// cleanup.  For example, if there are four cleanups on the stack:
///    top -> A | B | C | D | bottom
/// and we branch to a JumpDest at depth D, then cleanups A and B
/// get fallthrough outflows to that label and cleanup C gets
/// a branch-out outflow.
class swift::irgen::CleanupOutflows {
public:
  CleanupOutflows() : NextOutflowLabel(0) {}

  struct Outflow {
    llvm::ConstantInt *DestLabel;
    llvm::BasicBlock *DestBlock;
  };
  llvm::SmallVector<Outflow, 4> Outflows;

  /// A label larger than any of the branches out of this scope.
  unsigned NextOutflowLabel;

  void add(llvm::ConstantInt *destLabel, llvm::BasicBlock *destBlock) {
    Outflow outflow = { destLabel, destBlock };
    Outflows.push_back(outflow);
  }
};

static CleanupOutflows *getOrCreateOutflows(Cleanup &cleanup) {
  CleanupOutflows *outs = cleanup.getOutflows();
  if (!outs) cleanup.setOutflows(outs = new CleanupOutflows());
  return outs;
}

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

namespace {
  /// A CleanupBuffer is a location to which to temporarily copy a
  /// cleanup.
  class CleanupBuffer {
    llvm::SmallVector<char, sizeof(Cleanup) + 10 * sizeof(void*)> Data;

  public:
    CleanupBuffer(const Cleanup &cleanup) {
      size_t size = cleanup.allocated_size();
      Data.set_size(size);
      memcpy(Data.data(), reinterpret_cast<const void*>(&cleanup), size);
    }

    Cleanup &getCopy() { return *reinterpret_cast<Cleanup*>(Data.data()); }
  };
}

/// Get or create a normal entry block on the given cleanup.
static llvm::BasicBlock *
getOrCreateNormalEntryBlock(IRGenFunction &IGF, Cleanup &cleanup) {
  llvm::BasicBlock *block = cleanup.getNormalEntryBlock();
  if (block) return block;

  block = IGF.createBasicBlock("cleanup");
  cleanup.setNormalEntryBlock(block);
  return block;
}

/// Given that we're routing fallthrough for a cleanup that just got
/// popped, find the cleanup that we should head towards.
static Cleanup &getNextCleanupForFallthrough(DiverseStackImpl<Cleanup> &stack) {
  // Scan down the stack.
  for (Cleanup &cleanup : stack) {
    // We need to land at a cleanup if (1) it has non-fallthrough outflows
    // for any of our fallthroughs or (2) if it was active when any of the
    // fallthrough branches began.  We only add non-fallthrough outflows
    // to active cleanups, so (2) is sufficient.  We can conservatively
    // approximate (2) with the presence of any uses while active at all.
    if (cleanup.isUsedWhileActive())
      return cleanup;

    assert(!cleanup.getOutflows());
  }

  // We can't get out here because the existence of fallthroughs
  // implies the existence of an outflow *somewhere*.
  llvm_unreachable("ran out of cleanups looking for outflows!");
}

/// Get or create a normal entry block for the next meaningful
/// cleanup.  This is designed for the needs of popAndForwardCleanup:
/// it assumes that it's routing fallthroughs from a cleanup that just
/// got popped.
static llvm::BasicBlock *
getOrCreateNextNormalEntryBlock(IRGenFunction &IGF,
                                DiverseStackImpl<Cleanup> &stack) {
  return getOrCreateNormalEntryBlock(IGF, getNextCleanupForFallthrough(stack));
}

/// Emit a cleanup at the current insertion point.
static void emitCleanupHere(IRGenFunction &IGF, Cleanup &cleanup) {
  assert(IGF.Builder.hasValidIP());

  // If the cleanup requires a dynamic check for activation, we need
  // to handle that here.
  CleanupControl control = cleanup.getControl();
  llvm::BasicBlock *contBB = nullptr;
  if (control.hasFlag()) {
    contBB = IGF.createBasicBlock("cleanup.cont");
    llvm::BasicBlock *bodyBB = IGF.createBasicBlock("cleanup.body");

    // Branch on the control flag.
    llvm::Value *flagValue =
      IGF.Builder.CreateLoad(control.getFlag(), Alignment(1));
    IGF.Builder.CreateCondBr(flagValue, bodyBB, contBB);
    IGF.Builder.emitBlock(bodyBB);
  }

  // Emit the cleanup body.
  cleanup.emit(IGF);

  assert(IGF.Builder.hasValidIP());

  if (contBB) {
    IGF.Builder.CreateBr(contBB);
    IGF.Builder.emitBlock(contBB);
  }
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
    
    // We never add outflows to an inactive cleanup.
    assert(stackCleanup.getOutflows() == nullptr);

    // This is just the usual invariant about the control flag only
    // existing on cleanups with both active and inactive references.
    assert(!stackCleanup.getControl().hasFlag());

    // Therefore we have nothing to do for this cleanup and can just
    // pop it.
    stack.pop();
    return;
  }

  // Otherwise, we'll need to copy it off the cleanups stack.
  CleanupBuffer buffer(stackCleanup);
  Cleanup &cleanup = buffer.getCopy();

  // Pop now.
  stack.pop();

  // We must have an entry block.
  // TODO: avoid creating these in obvious cases.
  llvm::BasicBlock *entry = cleanup.getNormalEntryBlock();
  assert(entry && "no entry block for referenced top cleanup");
  assert(!entry->use_empty() && "unused entry block");

  llvm::BasicBlock *trueEntry;

  // If the entry block has exactly one use, and that use is an
  // unconditional branch, then merge this into its predecessor.
  if (entry->hasOneUse()) {
    llvm::TerminatorInst *term =
      cast<llvm::TerminatorInst>(*entry->use_begin());
    llvm::BranchInst *br = dyn_cast<llvm::BranchInst>(term);
    if (br && br->isUnconditional()) {
      trueEntry = br->getParent();
      br->eraseFromParent();
      delete entry;

    // Even if it's not an unconditional branch, place the entry after
    // the single predecessor.  This is likely to be the fallthrough
    // edge of a switch-out.
    } else {
      trueEntry = entry;
      entry->getParent()->getBasicBlockList()
                         .insertAfter(term->getParent(), entry);
    }

  // If the block has multiple uses, insert it at the next reasonable point.
  // Don't enter it yet, though.
  } else {
    trueEntry = entry;
    IGF.Builder.insertBlockAnywhere(trueEntry);
  }

  // Temporarily enter the entry block.
  IRBuilder::ShiftIP shiftedIP(IGF.Builder, trueEntry);

  // Emit the cleanup.
  emitCleanupHere(IGF, cleanup);

  // Set up the outflows.
  CleanupOutflows *outflows = cleanup.getOutflows();

  // This is straightforward if we have no branch outflows.
  if (!outflows) {
    // If we have no branch outflows, we must at least have a
    // fallthrough outflow.
    assert(cleanup.hasFallthroughOutflow());

    // Branch to the next cleanup.
    llvm::BasicBlock *next = getOrCreateNextNormalEntryBlock(IGF, stack);
    IGF.Builder.CreateBr(next);
    return;
  }

  assert(!outflows->Outflows.empty());
  llvm::OwningPtr<CleanupOutflows> outflowsDeleter(outflows);

  // It's also straightforward if we have one branch outflow and
  // no fallthrough.
  if (!cleanup.hasFallthroughOutflow() && outflows->Outflows.size() == 1) {
    IGF.Builder.CreateBr(outflows->Outflows[0].DestBlock);
    return;
  }

  // Okay, we need a switch.

  // The default destination is either the next entry block, if there
  // are fallthroughs, or the unreachable block if there aren't.
  llvm::BasicBlock *defaultDest;
  if (cleanup.hasFallthroughOutflow())
    defaultDest = getOrCreateNextNormalEntryBlock(IGF, stack);
  else
    defaultDest = IGF.getUnreachableBlock();

  // Switch on the value in the jump destination slot.
  llvm::Value *destValue =
    IGF.Builder.CreateLoad(IGF.getJumpDestSlot(), IGF.getJumpDestAlignment(),
                           "jumpdest.switchvalue");

  llvm::SwitchInst *sw =
    IGF.Builder.CreateSwitch(destValue, defaultDest, outflows->Outflows.size());
  for (auto &outflow : outflows->Outflows)
    sw->addCase(outflow.DestLabel, outflow.DestBlock);
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

/// Are there any active cleanups in the given range?
static bool hasAnyActiveCleanups(DiverseStackImpl<Cleanup>::iterator begin,
                                 DiverseStackImpl<Cleanup>::iterator end) {
  for (; begin != end; ++begin)
    if (begin->isActive())
      return true;
  return false;
}

/// Leave a scope, with all its cleanups.
void IRGenFunction::endScope(CleanupsDepth depth) {
  Cleanups.checkIterator(depth);

  // Fast path: no cleanups to leave in this scope.
  if (Cleanups.stable_begin() == depth) {
    popAndEmitTopDeadCleanups(*this, Cleanups, InnermostScope);
    return;
  }

  // Thread a branch through the cleanups if there are any active
  // cleanups and we have a valid insertion point.
  llvm::BasicBlock *contBB = nullptr;
  if (Builder.hasValidIP() &&
      hasAnyActiveCleanups(Cleanups.begin(), Cleanups.find(depth))) {
    contBB = createBasicBlock("cleanups.fallthrough");
    emitBranch(JumpDest(contBB, depth));
  }

  // Iteratively mark cleanups dead and pop them.
  // Maybe we'd get better results if we marked them all dead in one shot?
  while (Cleanups.stable_begin() != depth) {
    // Mark the cleanup dead.
    if (!Cleanups.begin()->isDead())
      setCleanupState(*Cleanups.begin(), CleanupState::Dead);

    // Pop it.
    popAndEmitTopCleanup(*this, Cleanups);
  }

  // Emit the continuation block if we made one.
  if (contBB) {
    Builder.emitMergeableBlock(contBB);
  }
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
  cleanup.Outflows = nullptr;
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
void IRGenFunction::emitBranch(JumpDest dest) {
  assert(Builder.hasValidIP());

  auto depth = Cleanups.find(dest.getDepth());

  // Find the topmost active cleanup.
  auto it = Cleanups.begin();
  for (; it != depth; ++it) {
    if (it->isActive())
      break;
  }

  // If we got out to the destination depth, we're done.
  if (it == depth) {
    Builder.CreateBr(dest.getBlock());
    return;
  }

  // Scan through the stack looking for the cleanup immediately prior
  // to the scope depth.
  auto innermost = it, outermost = innermost;
  unsigned destLabel = 0;
  while (true) {
    auto next = it; ++next;

    // Keep track of the outermost active cleanup we've seen.
    if (it->isActive())
      outermost = it;

    // Keep track of the biggest label that any of the cleanups we're
    // actually routing through has needed to deal with.  We don't
    // actually have to route through this cleanup if it's not active
    // and it doesn't end up inside the outermost scope, but it's not
    // really worth doing another pass just to ignore these.
    destLabel = std::max(destLabel, it->getNextDestLabel());

    // Stop if the next location is the target scope depth.
    if (next == depth) break;

    // Otherwise, on to the next.
    it = next;
  }

  assert(outermost->isActive());

  // First, look for an existing outflow for this destination block on
  // the outermost cleanup.  Note that there might be other labels
  // leading to this same block on different cleanup scopes (due to
  // the branches occuring with different cleanups active).  That's okay.
  llvm::ConstantInt *labelValue = nullptr;
  CleanupOutflows *outs = getOrCreateOutflows(*outermost);
  for (auto &outflow : outs->Outflows) {
    if (outflow.DestBlock == dest.getBlock()) {
      labelValue = outflow.DestLabel;
      break;
    }
  }

  // If we didn't find a label, create it (and remember that we did).
  bool hadExistingLabel = (labelValue != nullptr);
  if (!hadExistingLabel) {
    labelValue = llvm::ConstantInt::get(IGM.Int32Ty, destLabel);
    outs->add(labelValue, dest.getBlock());

    outermost->addActiveUse();
    outermost->setNextDestLabel(destLabel + 1);
  }

  // Set the destination and branch to the innermost cleanup.
  Builder.CreateStore(labelValue, getJumpDestSlot(), getJumpDestAlignment());
  Builder.CreateBr(getOrCreateNormalEntryBlock(*this, *innermost));

  // Walk through the intermediate cleanups again and add fallthrough outflows.
  for (it = innermost; it != outermost; ++it) {
    it->addUse();
    it->addFallthroughOutflow();

    // And tell each cleanup that the new label value has been reserved.
    if (!hadExistingLabel) it->setNextDestLabel(destLabel + 1);
  }
}

// Anchor the Cleanup v-table in this translation unit.
void Cleanup::_anchor() {}

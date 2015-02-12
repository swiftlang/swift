//===--- InOutDeshadowing.cpp - Remove non-escaping inout shadows ---------===//
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
// SILGen produces shadow variables for "inout" arguments to provide proper
// semantics for when the inout argument is closed over.  However, this shadow
// value is *only* needed when the argument is closed over (and when that
// closure isn't inlined).  This pass looks for shadow allocations and removes
// them.
//
// This is a guaranteed optimization pass, because adding additional references
// can cause algorithmic performance changes, e.g. turning amortized constant
// time string and array operations into linear time operations.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "inout-deshadow"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumShadowsRemoved, "Number of inout shadow variables removed");
STATISTIC(NumShadowsKept, "Number of inout shadow variables kept");

//===----------------------------------------------------------------------===//
//                          inout Deshadowing
//===----------------------------------------------------------------------===//

/// promoteShadow - Given an AllocStackInst that is copied to/from an inout
/// argument, completely replace the alloc_stack with that inout argument.
static void promoteShadow(AllocStackInst *Alloc, SILArgument *InOutArg) {

  // Since the allocation has already been promoted to an alloc_stack, we know
  // it doesn't escape.  Simply eliminate the allocation and any obviously
  // trivial noop copies into and out of it.
  while (!Alloc->use_empty()) {
    auto Use = *Alloc->use_begin();
    auto *User = Use->getUser();

    // If this is a use of the 0th result, not the address result, just zap the
    // instruction.  It is a dealloc_stack or something similar.
    if (Use->get().getResultNumber() == 0) {
      User->eraseFromParent();
      continue;
    }

    // Otherwise, it is a use of the argument.  If this is a copy_addr that
    // defines or destroys the value, then remove it.
    if (auto *CAI = dyn_cast<CopyAddrInst>(User)) {
      if (CAI->getSrc() == InOutArg || CAI->getDest() == InOutArg) {
        User->eraseFromParent();
        continue;
      }
    }

    // Otherwise, this is something else that is using the memory.  Remap this
    // to use the InOutArg directly instead of using the allocation.
    Use->set(InOutArg);
  }

  Alloc->eraseFromParent();
}


//===----------------------------------------------------------------------===//
//                     Candidate Variable Identification
//===----------------------------------------------------------------------===//

class StackSlotState {
  bool Failed : 1;
  bool HaveEntrySlot : 1;
  
  AllocStackInst *TheSlot = nullptr;
  llvm::SmallPtrSet<SILBasicBlock*, 1> ExitBBs;
  
public:
  StackSlotState(SILFunction *F)
    : Failed(false), HaveEntrySlot(false)
  {
    // We need to see a store back to the inout on every exit path.
    for (auto &bb : *F) {
      auto term = bb.getTerminator();
      if (isa<ReturnInst>(term)
          || isa<AutoreleaseReturnInst>(term)) {
        DEBUG(llvm::dbgs() << "     need load from stack slot on exit " << &bb
                           << '\n');
        ExitBBs.insert(&bb);
      }
    }
  }
  
  /// True if analysis has concluded that deshadowing cannot happen.
  bool failed() {
    return Failed;
  }
  
  /// Get the single stack slot we can deshadow, or null if no such slot was
  /// found.
  AllocStackInst *getDeshadowableSlot() {
    if (Failed)
      return nullptr;
    
    // We must have seen both a store to and a load from the slot to deshadow
    // on every exit BB.
    if (!HaveEntrySlot) {
      DEBUG(llvm::dbgs() << "*** Rejecting deshadow: no store to stack slot\n");
      return nullptr;
    }
    
    if (!ExitBBs.empty()) {
      DEBUG(llvm::dbgs() << "*** Rejecting deshadow: no load from stack slot "
                            "on some paths\n");
      return nullptr;
    }
    
    return TheSlot;
  }
  
  /// Add a stack slot that was copy-initialized from the inout on entry
  /// to the analysis. If we already have seen a load, or if the slot does not
  /// match the exit slot, the analysis fails.
  void addEntrySlot(AllocStackInst *slot) {
    if (Failed) return;
    
    if (HaveEntrySlot)
      return setFailed("inout is loaded multiple times");
    if (TheSlot && slot != TheSlot)
      return setFailed("inout is loaded and stored into different slots");
    
    DEBUG(llvm::dbgs() << "    found store to stack slot on entry\n");
    HaveEntrySlot = true;
    TheSlot = slot;
  }
  
  /// Add a stack slot that was take-assigned to the inout from an exit BB
  /// to the analysis. If we already have seen a store on this BB, or if the
  /// slot does not match the entry slot, the analysis fails.
  void addExitSlot(AllocStackInst *slot, SILBasicBlock *exitBB) {
    if (Failed) return;
    
    if (TheSlot && slot != TheSlot)
      return setFailed("inout is loaded and stored into different slots");
    if (!ExitBBs.erase(exitBB))
      return setFailed("inout is stored multiple times from same exit BB");
    
    DEBUG(llvm::dbgs() << "    found load from stack slot on exit "
                       << exitBB << '\n');
    TheSlot = slot;
  }
  
  /// Fail the analysis.
  void setFailed(StringRef reason) {
    DEBUG(llvm::dbgs() << "*** Rejecting deshadow: " << reason << '\n');
    Failed = true;
  }
};

/// isCopyToOrFromStack - Check to see if the specified use of an inout
/// argument is a copy_addr to/from an alloc_stack.
///
/// This returns the alloc_stack if found, or null if not.
static void analyzeUseOfInOut(Operand *UI, StackSlotState &state) {
  // Look for copy_addr instructions.
  auto CAI = dyn_cast<CopyAddrInst>(UI->getUser());
  // Non-copy_addr uses of the inout should only occur in canonical SIL, so
  // fail the analysis if we see one.
  if (!CAI)
    return state.setFailed("inout is used by a non-copy_addr instruction");
  
  // We only look at autogenerated copy_addr's.  We don't want to muck with
  // user variables, as in:
  //   func f(inout a : Int) { var b = a }
  if (!CAI->getLoc().isAutoGenerated() && !CAI->getLoc().is<SILFileLocation>())
    return;
  
  // Get a stack slot, looking through mark_uninitialized if necessary.
  auto getStackSlot = [](SILValue V) {
    // Look through mark_uninitialized.
    if (auto *MUI = dyn_cast<MarkUninitializedInst>(V))
      V = MUI->getOperand();
    
    return dyn_cast<AllocStackInst>(V);
  };
  
  // Are we the source or destination?
  switch (UI->getOperandNumber()) {
  case 0: { // Source
    // Is this copy in the entry block?
    if (CAI->getParent() != CAI->getFunction()->begin())
      // Any copy from the inout outside of the entry block fails the analysis.
      // We don't need full flow-sensitive analysis for SILGen-ed code.
      return state.setFailed("inout is loaded outside of the entry block");
    
    // Fail if this isn't a copy-initialization.
    if (CAI->isTakeOfSrc() || !CAI->isInitializationOfDest())
      return state.setFailed("inout is loaded as a non-copy-initialization");
      
    // Fail if this isn't a store to a stack slot.
    AllocStackInst *destSlot = getStackSlot(CAI->getDest());
    if (!destSlot)
      return state.setFailed("inout is loaded to a non-stack slot");
    
    // Add the entry slot to the analysis.
    return state.addEntrySlot(destSlot);
  }
  case 1: { // Destination
    // Is this copy in an exit block?
    auto term = CAI->getParent()->getTerminator();
    
    // Unreachable blocks don't matter to the analysis.
    if (isa<UnreachableInst>(term))
      return;
    
    if (!isa<ReturnInst>(term)
        && !isa<AutoreleaseReturnInst>(term))
      // Any copy from the inout outside of an exit block fails the analysis.
      // We don't need full flow-sensitive analysis for SILGen-ed code.
      return state.setFailed("inout is stored outside of an exit block");
    
    // Fail if this isn't an assignment.
    if (CAI->isInitializationOfDest())
      return state.setFailed("inout is stored as a non-assignment");
    
    // Fail if this isn't a load from a stack slot.
    AllocStackInst *srcSlot = getStackSlot(CAI->getSrc());
    if (!srcSlot)
      return state.setFailed("inout is stored from a non-stack slot");
      
    // Add the exit slot to the analysis.
    return state.addExitSlot(srcSlot, CAI->getParent());
  }
  default:
    llvm_unreachable("copy_addr only has two operands");
  }
}


/// processInOutValue - Walk the use-def list of the inout argument to find uses
/// of it.  If we find any autogenerated copies to/from an alloc_stack, then
/// remove the alloc stack in favor of loading/storing to the inout pointer
/// directly.
///
/// This returns true if it promotes away the shadow variable.
///
static bool processInOutValue(SILArgument *InOutArg) {
  assert(InOutArg->getType().isAddress() &&
         "inout arguments should always be addresses");
  
  {
    StackSlotState state(InOutArg->getFunction());
    
    for (auto UI : InOutArg->getUses()) {
      analyzeUseOfInOut(UI, state);
      if (state.failed())
        goto failed;
    }
    
    AllocStackInst *stackSlot = state.getDeshadowableSlot();
    if (!stackSlot)
      goto failed;
    
    DEBUG(llvm::dbgs() << "    Promoting shadow variable " << *stackSlot);
    promoteShadow(stackSlot, InOutArg);
    return true;
  }
failed:
  // If we fail, dump out some internal state.
  DEBUG({
    llvm::dbgs() << "*** Failed to deshadow.  Uses:\n";
    for (auto UI : InOutArg->getUses())
      llvm::dbgs() << "    " << *UI->getUser();
  });

  return false;
}

namespace {
class InOutDeshadowing : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    SILFunction &F = *getFunction();
    SILBasicBlock &EntryBlock = F.front();

    // For each function, find any inout arguments and try to optimize each of
    // them.
    SILFunctionType *FTI = F.getLoweredFunctionType();

    for (unsigned arg = 0, e = FTI->getParameters().size();
         arg != e; ++arg) {
      if (!FTI->getParameters()[arg].isIndirectInOut()) continue;

      DEBUG(llvm::dbgs()<< "  " << F.getName() << ": argument #"<< arg << "\n");

      if (processInOutValue(EntryBlock.getBBArgs()[arg]))
        ++NumShadowsRemoved;
      else {
        ++NumShadowsKept;
      }
    }
  }

  StringRef getName() override { return "InOut Deshadowing"; }
};
} // end anonymous namespace


SILTransform *swift::createInOutDeshadowing() {
  return new InOutDeshadowing();
}

//===--- AllocBoxToStack.cpp - Promote alloc_box to alloc_stack -----------===//
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

#define DEBUG_TYPE "allocbox-to-stack"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/Dominance.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumStackPromoted, "Number of alloc_box's promoted to the stack");

//===----------------------------------------------------------------------===//
//                           alloc_box Promotion
//===----------------------------------------------------------------------===//

static SILInstruction* findUnexpectedBoxUse(SILValue Box,
                                            bool examinePartialApply);

// Propagate liveness backwards from an initial set of blocks in our
// LiveIn set.
void propagateLiveness(llvm::SmallPtrSetImpl<SILBasicBlock*> &LiveIn,
                       SILBasicBlock *DefBB) {

  // First populate a worklist of predecessors.
  llvm::SmallVector<SILBasicBlock*, 64> Worklist;
  for (auto *BB : LiveIn)
    for (auto Pred : BB->getPreds())
      Worklist.push_back(Pred);

  // Now propagate liveness backwards until we hit the alloc_box.
  while (!Worklist.empty()) {
    auto *BB = Worklist.pop_back_val();

    // If it's already in the set, then we've already queued and/or
    // processed the predecessors.
    if (BB == DefBB || !LiveIn.insert(BB))
      continue;

    for (auto Pred : BB->getPreds())
      Worklist.push_back(Pred);
  }
}

// Is any successor of BB in the LiveIn set?
bool successorHasLiveIn(SILBasicBlock *BB,
                        llvm::SmallPtrSetImpl<SILBasicBlock*> &LiveIn) {
  for (auto &Succ : BB->getSuccs())
    if (LiveIn.count(Succ))
      return true;

  return false;
}

// Walk backwards in BB looking for strong_release or dealloc_box of
// the given value, and add it to Releases.
void addLastRelease(SILValue V, SILBasicBlock *BB,
                    llvm::SmallVectorImpl<SILInstruction*> &Releases) {
  for (auto I = BB->rbegin(); I != BB->rend(); ++I) {
    if (isa<StrongReleaseInst>(*I) || isa<DeallocBoxInst>(*I)) {
      if (I->getOperand(0) != V)
        continue;

      Releases.push_back(&*I);
      return;
    }
    assert((!isa<StrongRetainInst>(*I) || I->getOperand(0) != V) &&
           "Did not expect retain after final release/dealloc!");

  }

  llvm_unreachable("Did not find release/dealloc!");
}

// Find the final releases of the alloc_box along any given path.
// These can include paths from a release back to the alloc_box in a
// loop.
void getFinalReleases(AllocBoxInst *ABI,
                      llvm::SmallVectorImpl<SILInstruction*> &Releases) {
  llvm::SmallPtrSet<SILBasicBlock*, 16> LiveIn;
  llvm::SmallPtrSet<SILBasicBlock*, 16> UseBlocks;

  auto *DefBB = ABI->getParent();

  auto seenRelease = false;
  SILInstruction *OneRelease = nullptr;

  auto Box = ABI->getContainerResult();

  // We'll treat this like a liveness problem where the alloc_box is
  // the def. Each block that has a use of the owning pointer has the
  // value live-in unless it is the block with the alloc_box.
  for (auto UI : Box.getUses()) {
    auto *User = UI->getUser();
    auto *BB = User->getParent();

    if (BB != DefBB)
      LiveIn.insert(BB);

    // Also keep track of the blocks with uses.
    UseBlocks.insert(BB);

    // Try to speed up the trivial case of single release/dealloc.
    if (isa<StrongReleaseInst>(User) || isa<DeallocBoxInst>(User)) {
      if (!seenRelease)
        OneRelease = User;
      else
        OneRelease = nullptr;

      seenRelease = true;
    }
  }

  // Only a single release/dealloc? We're done!
  if (OneRelease) {
    Releases.push_back(OneRelease);
    return;
  }

  propagateLiveness(LiveIn, DefBB);

  // Now examine each block we saw a use in. If it has no successors
  // that are in LiveIn, then the last use in the block is the final
  // release/dealloc.
  for (auto *BB : UseBlocks)
    if (!successorHasLiveIn(BB, LiveIn))
      addLastRelease(Box, BB, Releases);
}

/// checkPartialApplyBody - Check the body of a partial apply to see
/// if the box pointer argument passed to it has uses that would
/// disqualify it from being protmoted to a stack location.  Return
/// true if this partial apply will not block our promoting the box.
static bool checkPartialApplyBody(PartialApplyInst *PAI,
                                  Operand *O) {
  auto ClosureType = PAI->getSubstCalleeType();

  // TODO: We currently can only handle non-polymorphic closures.
  if (PAI->hasSubstitutions() || ClosureType->isPolymorphic())
    return false;

  auto Callee = PAI->getCallee();
  if (!isa<FunctionRefInst>(Callee))
    return true;

  auto *FRI = cast<FunctionRefInst>(Callee);

  SILFunction *F = FRI->getReferencedFunction();
  if (F->empty())
    return false;

  auto Args = PAI->getArguments();
  auto Params = ClosureType->getInterfaceParameters();
  assert((Params.size() >= Args.size())
         && "Expected fewer args to function!");
  auto OperandIndex = O->getOperandNumber();
  assert((OperandIndex > 0) && "Box pointer cannot be the applied function!");

  int ParamIndex = (Params.size() - Args.size()) + OperandIndex - 1;
  auto &Entry = F->front();
  auto *Box = Entry.getBBArg(ParamIndex);

  // Examine the uses of the parameter to see if there is anything
  // that would block us from promoting the box to the stack.
  // We won't examine the bodies of partial_apply when checking.
  auto *Found = findUnexpectedBoxUse(SILValue(Box),
                                     /* examinePartialApply =*/ false);
  return !Found;
}


/// findUnexpectedBoxUse - Validate that the uses of a pointer to a
/// box do not eliminate it from consideration for promotion to a
/// stack element. Optionally examine the body of partial_apply
/// to see if there is an unexpected use inside.  Return the
/// instruction with the unexpected use if we find one.
static SILInstruction* findUnexpectedBoxUse(SILValue Box,
                                            bool examinePartialApply) {
  assert((Box.getType() ==
          SILType::getObjectPointerType(Box.getType().getASTContext())) &&
         "Expected an object pointer!");

  // Scan all of the uses of the retain count value, collecting all
  // the releases and validating that we don't have an unexpected
  // user.
  for (auto UI : Box.getUses()) {
    auto *User = UI->getUser();

    // Retains, releases, and deallocs are fine.
    if (isa<StrongRetainInst>(User) || isa<StrongReleaseInst>(User) ||
        isa<DeallocBoxInst>(User))
      continue;

    // For partial_apply, if we've been asked to examine the body, the
    // uses of the argument are okay there, and the partial_apply
    // itself cannot escape, then everything is fine.
    if (auto *PAI = dyn_cast<PartialApplyInst>(User))
      if (examinePartialApply && checkPartialApplyBody(PAI, UI) &&
          // FIXME: Change false to true to enable once the
          //        appropriate partial_apply cloning and rewrites are
          //        in place.
          !canValueEscape(PAI, /* examineApply = */ false))
        continue;

    return User;
  }

  return nullptr;
}

/// canPromoteAllocBox - Can we promote this alloc_box to an alloc_stack?
static bool canPromoteAllocBox(AllocBoxInst *ABI) {
  // Scan all of the uses of the address of the box's contained value
  // to see if any of them cause address to escape, in which case we
  // can't promote it to live on the stack.
  if (canValueEscape(ABI->getAddressResult(), /* examineApply = */ false))
    return false;

  // Scan all of the uses of the address of the box to see if any
  // disqualifies the box from being promoted tot he stack.
  if (auto *User = findUnexpectedBoxUse(ABI->getContainerResult(),
                                        /* examinePartialApply = */ true)) {
    // Otherwise, we have an unexpected use.
    DEBUG(llvm::dbgs() << "*** Failed to promote alloc_box in @"
          << ABI->getFunction()->getName() << ": " << *ABI
          << "    Due to user: " << *User << "\n");

    return false;
  }

  // Okay, it looks like this value doesn't escape.
  return true;
}

/// rewriteAllocBoxAsAllocStack - Replace uses of the alloc_box with a
/// new alloc_stack, but do not delete the alloc_box yet.
static void rewriteAllocBoxAsAllocStack(AllocBoxInst *ABI) {
  DEBUG(llvm::dbgs() << "*** Promoting alloc_box to stack: " << *ABI);

  llvm::SmallVector<SILInstruction*, 4> FinalReleases;
  getFinalReleases(ABI, FinalReleases);

  // Promote this alloc_box to an alloc_stack.  Start by inserting the
  // alloc_stack after the alloc_box.
  SILBuilder B1(ABI->getParent(), ++SILBasicBlock::iterator(ABI));
  auto *ASI = B1.createAllocStack(ABI->getLoc(), ABI->getElementType());
  ASI->setDebugScope(ABI->getDebugScope());

  // Replace all uses of the address of the box's contained value with
  // the address of the stack location.
  ABI->getAddressResult().replaceAllUsesWith(ASI->getAddressResult());

  // Check to see if the alloc_box was used by a mark_uninitialized instruction.
  // If so, any uses of the pointer result need to keep using the MUI, not the
  // alloc_stack directly.  If we don't do this, DI will miss the uses.
  SILValue PointerResult = ASI->getAddressResult();
  for (auto UI : ASI->getAddressResult().getUses())
    if (auto *MUI = dyn_cast<MarkUninitializedInst>(UI->getUser())) {
      assert(ASI->getAddressResult().hasOneUse() &&
             "alloc_stack used by mark_uninialized, but not exclusively!");
      PointerResult = MUI;
      break;
    }

  auto &Lowering = ABI->getModule().getTypeLowering(ABI->getElementType());

  for (auto LastRelease : FinalReleases) {
    SILBuilder B2(LastRelease);

    if (!Lowering.isTrivial() && !isa<DeallocBoxInst>(LastRelease))
      B2.emitDestroyAddr(CleanupLocation::getCleanupLocation(ABI->getLoc()),
                         PointerResult);

    // Reset the insertion point in case the destroy address expanded to
    // multiple blocks.
    B2.setInsertionPoint(LastRelease);
    B2.createDeallocStack(LastRelease->getLoc(), ASI->getContainerResult());
  }

  // Remove any retain and release instructions.  Since all uses of result #1
  // are gone, this only walks through uses of result #0 (the retain count
  // pointer).
  while (!ABI->use_empty()) {
    auto *User = (*ABI->use_begin())->getUser();
    assert(isa<StrongReleaseInst>(User) || isa<StrongRetainInst>(User) ||
           isa<DeallocBoxInst>(User));

    User->eraseFromParent();
  }
}

namespace {
class AllocBoxToStack : public SILFunctionTransform {
  /// The entry point to the transformation.
  void run() {
    bool Changed = false;

    for (auto &BB : *getFunction()) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        if (auto *ABI = dyn_cast<AllocBoxInst>(I))
          if (canPromoteAllocBox(ABI)) {
            rewriteAllocBoxAsAllocStack(ABI);
            ++NumStackPromoted;
            // Carefully move iterator to avoid invalidation problems.
            ++I;
            ABI->eraseFromParent();
            Changed = true;
            continue;
          }

        ++I;
      }
    }
    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "AllocBox-To-Stack Optimization"; }
};
} // end anonymous namespace

SILTransform *swift::createAllocBoxToStack() {
  return new AllocBoxToStack();
}

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
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumStackPromoted, "Number of alloc_box's promoted to the stack");

//===----------------------------------------------------------------------===//
//                           alloc_box Promotion
//===----------------------------------------------------------------------===//

typedef llvm::SmallSet<unsigned int, 4> ParamIndexSet;

static SILInstruction* findUnexpectedBoxUse(SILValue Box,
                                            bool examinePartialApply,
                                            bool inAppliedFunction,
                                            llvm::SmallVectorImpl<Operand*> &);
static bool operandEscapesApply(Operand *O);

// Propagate liveness backwards from an initial set of blocks in our
// LiveIn set.
static void propagateLiveness(llvm::SmallPtrSetImpl<SILBasicBlock*> &LiveIn,
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
static bool successorHasLiveIn(SILBasicBlock *BB,
                               llvm::SmallPtrSetImpl<SILBasicBlock*> &LiveIn) {
  for (auto &Succ : BB->getSuccs())
    if (LiveIn.count(Succ))
      return true;

  return false;
}

// Walk backwards in BB looking for strong_release or dealloc_box of
// the given value, and add it to Releases.
static void addLastRelease(SILValue V, SILBasicBlock *BB,
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
static void getFinalReleases(AllocBoxInst *ABI,
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

/// \brief Returns True if the operand or one of its users is captured.
static bool useCaptured(Operand *UI) {
  auto *User = UI->getUser();

  // These instructions do not cause the address to escape.
  if (isa<CopyAddrInst>(User) ||
      isa<LoadInst>(User) ||
      isa<ProtocolMethodInst>(User) ||
      isa<DebugValueInst>(User) ||
      isa<DebugValueAddrInst>(User) ||
      isa<StrongReleaseInst>(User) ||
      isa<StrongRetainInst>(User) ||
      isa<DeallocBoxInst>(User))
    return false;

  if (auto *Store = dyn_cast<StoreInst>(User)) {
    if (Store->getDest() == UI->get())
      return false;
  } else if (auto *Assign = dyn_cast<AssignInst>(User)) {
    if (Assign->getDest() == UI->get())
      return false;
  }

  return true;
}

static bool canValueEscape(SILValue V, bool examineApply) {
  for (auto UI : V.getUses()) {
    auto *User = UI->getUser();

    // These instructions do not cause the address to escape.
    if (!useCaptured(UI))
      continue;

    // These instructions only cause the value to escape if they are used in
    // a way that escapes.  Recursively check that the uses of the instruction
    // don't escape and collect all of the uses of the value.
    if (isa<StructElementAddrInst>(User) || isa<TupleElementAddrInst>(User) ||
        isa<ProjectExistentialInst>(User) || isa<OpenExistentialInst>(User) ||
        isa<MarkUninitializedInst>(User) || isa<AddressToPointerInst>(User) ||
        isa<PointerToAddressInst>(User)) {
      if (canValueEscape(User, examineApply))
        return true;
      continue;
    }

    if (auto apply = dyn_cast<ApplyInst>(User)) {
      // Applying a function does not cause the function to escape.
      if (UI->getOperandNumber() == 0)
        continue;

      // apply instructions do not capture the pointer when it is passed
      // indirectly
      if (apply->getSubstCalleeType()
          ->getInterfaceParameters()[UI->getOperandNumber()-1].isIndirect())
        continue;

      // Optionally drill down into an apply to see if the operand is
      // captured in or returned from the apply.
      if (examineApply && !operandEscapesApply(UI))
        continue;
    }

    // partial_apply instructions do not allow the pointer to escape
    // when it is passed indirectly, unless the partial_apply itself
    // escapes
    if (auto partialApply = dyn_cast<PartialApplyInst>(User)) {
      auto args = partialApply->getArguments();
      auto params = partialApply->getSubstCalleeType()
        ->getInterfaceParameters();
      params = params.slice(params.size() - args.size(), args.size());
      if (params[UI->getOperandNumber()-1].isIndirect()) {
        if (canValueEscape(User, /*examineApply = */ true))
          return true;
        continue;
      }
    }

    return true;
  }

  return false;
}


/// Given an apply or partial_apply, return the direct callee or
/// nullptr if this is not a direct call.
static FunctionRefInst *getDirectCallee(SILInstruction *Call) {
  if (auto *Apply = dyn_cast<ApplyInst>(Call))
    return dyn_cast<FunctionRefInst>(Apply->getCallee());
  else
    return dyn_cast<FunctionRefInst>(cast<PartialApplyInst>(Call)->getCallee());
}

static bool callIsPolymorphic(SILInstruction *Call) {
  if (auto *Apply = dyn_cast<ApplyInst>(Call))
    return Apply->hasSubstitutions();
  else
    return cast<PartialApplyInst>(Call)->hasSubstitutions();
}

/// Given an operand of a direct apply or partial_apply of a
/// non-generic function, return the index of the parameter used in
/// the body of the function to represent this operand.
static size_t getParameterIndexForOperand(Operand *O) {
  assert(isa<ApplyInst>(O->getUser()) || isa<PartialApplyInst>(O->getUser()) &&
         "Expected apply or partial_apply!");

  CanSILFunctionType Type;
  size_t ArgCount;
  if (auto *Apply = dyn_cast<ApplyInst>(O->getUser())) {
    Type = Apply->getSubstCalleeType();
    ArgCount = Apply->getArguments().size();
    assert(Type->getInterfaceParameters().size() == ArgCount &&
           "Expected all arguments to be supplied!");
  } else {
    auto *PartialApply = cast<PartialApplyInst>(O->getUser());
    Type = PartialApply->getSubstCalleeType();
    ArgCount = PartialApply->getArguments().size();
  }

  size_t ParamCount = Type->getInterfaceParameters().size();
  assert(ParamCount >= ArgCount && "Expected fewer arguments to function!");

  auto OperandIndex = O->getOperandNumber();
  assert(OperandIndex != 0 && "Operand cannot be the applied function!");

  // The applied function is the first operand.
  auto ParamIndex = (ParamCount - ArgCount) + OperandIndex - 1;

  return ParamIndex;
}

/// Given an operand of a direct apply or partial_apply of a
/// non-generic function, return the parameter used in the body of the
/// function to represent this operand. For non-direct calls and for
/// functions that we cannot examine the body of, return an empty
/// SILValue.
static SILValue getParameterForOperand(SILFunction *F, Operand *O) {
  assert(F && !F->empty() && "Expected a function with a body!");

  auto &Entry = F->front();
  size_t ParamIndex = getParameterIndexForOperand(O);

  auto *Param = Entry.getBBArg(ParamIndex);
  return SILValue(Param);
}

/// Return a pointer to the SILFunction called by Call if we can
/// determine which funciton that is, and we have a body for that
/// function. Otherwise return nullptr.
static SILFunction *getFunctionBody(SILInstruction *Call) {
  // TODO: Support generics at some point.
  if (callIsPolymorphic(Call))
    return nullptr;

  if (auto *FRI = getDirectCallee(Call))
    if (auto *F = FRI->getReferencedFunction())
      if (!F->empty())
        return F;

  return nullptr;
}

/// Could this operand to an apply escape that function by being
/// stored or returned?
static bool operandEscapesApply(Operand *O) {
  SILFunction *F = getFunctionBody(O->getUser());
  // If we cannot examine the function body, assume the worst.
  if (!F)
    return true;

  // Check the uses of the operand, but do not recurse down into other
  // apply instructions.
  auto Param = getParameterForOperand(F, O);
  return canValueEscape(Param, /* examineApply = */ false);
}

/// checkPartialApplyBody - Check the body of a partial apply to see
/// if the box pointer argument passed to it has uses that would
/// disqualify it from being protmoted to a stack location.  Return
/// true if this partial apply will not block our promoting the box.
static bool checkPartialApplyBody(Operand *O) {
  SILFunction *F = getFunctionBody(O->getUser());
  // If we cannot examine the function body, assume the worst.
  if (!F)
    return false;

  // We don't actually use these because we're not recursively
  // rewriting the partial applies we find.
  llvm::SmallVector<Operand *, 1> ElidedOperands;
  auto Param = getParameterForOperand(F, O);
  return !findUnexpectedBoxUse(Param, /* examinePartialApply = */ false,
                               /* inAppliedFunction = */ true,
                               ElidedOperands);
}


/// findUnexpectedBoxUse - Validate that the uses of a pointer to a
/// box do not eliminate it from consideration for promotion to a
/// stack element. Optionally examine the body of partial_apply
/// to see if there is an unexpected use inside.  Return the
/// instruction with the unexpected use if we find one.
static SILInstruction* findUnexpectedBoxUse(SILValue Box,
                                            bool examinePartialApply,
                                            bool inAppliedFunction,
                             llvm::SmallVectorImpl<Operand *> &ElidedOperands) {
  assert((Box.getType() ==
          SILType::getObjectPointerType(Box.getType().getASTContext())) &&
         "Expected an object pointer!");

  llvm::SmallVector<Operand *, 4> LocalElidedOperands;

  // Scan all of the uses of the retain count value, collecting all
  // the releases and validating that we don't have an unexpected
  // user.
  for (auto UI : Box.getUses()) {
    auto *User = UI->getUser();

    // Retains and releases are fine. Deallocs are fine if we're not
    // examining a function that the alloc_box was passed into.
    if (isa<StrongRetainInst>(User) || isa<StrongReleaseInst>(User) ||
        (!inAppliedFunction && isa<DeallocBoxInst>(User)))
      continue;

    // For partial_apply, if we've been asked to examine the body, the
    // uses of the argument are okay there, and the partial_apply
    // itself cannot escape, then everything is fine.
    if (auto *PAI = dyn_cast<PartialApplyInst>(User))
      if (examinePartialApply && checkPartialApplyBody(UI) &&
          !canValueEscape(PAI, /* examineApply = */ true)) {
        LocalElidedOperands.push_back(UI);
        continue;
      }

    return User;
  }

  ElidedOperands.append(LocalElidedOperands.begin(), LocalElidedOperands.end());
  return nullptr;
}

/// canPromoteAllocBox - Can we promote this alloc_box to an alloc_stack?
static bool canPromoteAllocBox(AllocBoxInst *ABI,
                             llvm::SmallVectorImpl<Operand *> &ElidedOperands) {
  // Scan all of the uses of the address of the box to see if any
  // disqualifies the box from being promoted tot he stack.
  if (auto *User = findUnexpectedBoxUse(ABI->getContainerResult(),
                                        /* examinePartialApply = */ true,
                                        /* inAppliedFunction = */ false,
                                        ElidedOperands)) {
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

/// \brief A SILCloner subclass which clones a closure function while
/// removing some of the parameters, which are either completely dead
/// or only used in retains and releases (which will not be cloned).
class DeadParamCloner : public SILCloner<DeadParamCloner> {
  public:
  friend class SILVisitor<DeadParamCloner>;
  friend class SILCloner<DeadParamCloner>;

  DeadParamCloner(SILFunction *Orig, ParamIndexSet &DeadParamIndices);

  void populateCloned();

  SILFunction *getCloned() { return &getBuilder().getFunction(); }

  private:
  static SILFunction *initCloned(SILFunction *Orig,
                                 ParamIndexSet &DeadParamIndices);

  void visitStrongReleaseInst(StrongReleaseInst *Inst);
  void visitStrongRetainInst(StrongRetainInst *Inst);

  SILFunction *Orig;
  ParamIndexSet &DeadParamIndices;

  // The values in the original function that are either dead or only
  // used in retains and releases.
  llvm::SmallSet<SILValue, 4> DeadParameters;
};
} // end anonymous namespace.

DeadParamCloner::DeadParamCloner(SILFunction *Orig,
                                 ParamIndexSet &DeadParamIndices)
  : SILCloner<DeadParamCloner>(*initCloned(Orig, DeadParamIndices)),
    Orig(Orig), DeadParamIndices(DeadParamIndices) {
}

/// \brief Create the function corresponding to the clone of the
/// original closure with the signature modified to reflect removed
/// parameters (which are specified by DeadParamIndices).
SILFunction*
DeadParamCloner::initCloned(SILFunction *Orig,
                            ParamIndexSet &DeadParamIndices) {
  SILModule &M = Orig->getModule();

  // Suffix the function name with "_specializedX", where X is the first integer
  // that does not result in a conflict.
  // TODO: come up with a good mangling for this transformation and
  // use shared linkage.
  unsigned Counter = 0;
  std::string ClonedName;
  do {
    ClonedName.clear();
    llvm::raw_string_ostream buffer(ClonedName);
    buffer << Orig->getName() << "_specialized" << Counter++;
  } while (M.lookUpFunction(ClonedName));

  SmallVector<SILParameterInfo, 4> ClonedInterfaceArgTys;

  // Generate a new parameter list with deleted parameters removed.
  SILFunctionType *OrigFTI = Orig->getLoweredFunctionType();
  unsigned Index = 0;
  for (auto &param : OrigFTI->getInterfaceParameters()) {
    if (!DeadParamIndices.count(Index))
      ClonedInterfaceArgTys.push_back(param);
    ++Index;
  }

  // Create the new function type for the cloned function with some of
  // the parameters removed.
  auto ClonedTy =
    SILFunctionType::get(OrigFTI->getGenericSignature(),
                         OrigFTI->getExtInfo(),
                         OrigFTI->getCalleeConvention(),
                         ClonedInterfaceArgTys,
                         OrigFTI->getInterfaceResult(),
                         M.getASTContext());

  assert((Orig->isTransparent() || Orig->isBare() || Orig->getLocation())
         && "SILFunction missing location");
  assert((Orig->isTransparent() || Orig->isBare() || Orig->getDebugScope())
         && "SILFunction missing DebugScope");
  return SILFunction::create(M, Orig->getLinkage(), ClonedName, ClonedTy,
                             Orig->getContextGenericParams(),
                             Orig->getLocation(), Orig->isBare(),
                             IsNotTransparent, Orig,
                             Orig->getDebugScope());
}

/// \brief Populate the body of the cloned closure, modifying instructions as
/// necessary to take into consideration the removed parameters.
void
DeadParamCloner::populateCloned() {
  SILFunction *Cloned = getCloned();
  SILModule &M = Cloned->getModule();

  // Create arguments for the entry block
  SILBasicBlock *OrigEntryBB = Orig->begin();
  SILBasicBlock *ClonedEntryBB = new (M) SILBasicBlock(Cloned);
  unsigned ArgNo = 0;
  auto I = OrigEntryBB->bbarg_begin(), E = OrigEntryBB->bbarg_end();
  while (I != E) {
    if (!DeadParamIndices.count(ArgNo)) {
      // Create a new argument which copies the original argument.
      SILValue MappedValue =
        new (M) SILArgument((*I)->getType(), ClonedEntryBB, (*I)->getDecl());
      ValueMap.insert(std::make_pair(*I, MappedValue));
    } else {
      DeadParameters.insert(SILValue(*I));
    }
    ++ArgNo;
    ++I;
  }

  getBuilder().setInsertionPoint(ClonedEntryBB);
  BBMap.insert(std::make_pair(OrigEntryBB, ClonedEntryBB));
  // Recursively visit original BBs in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(OrigEntryBB);

  // Now iterate over the BBs and fix up the terminators.
  for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
    getBuilder().setInsertionPoint(BI->second);
    visit(BI->first->getTerminator());
  }
}

/// \brief Handle a strong_release instruction during cloning of a closure; if
/// it is a strong release of a promoted box argument, then it is replaced wit
/// a ReleaseValue of the new object type argument, otherwise it is handled
/// normally.
void
DeadParamCloner::visitStrongReleaseInst(StrongReleaseInst *Inst) {
  // If it's a release of a dead parameter, just drop the instruction.
  if (DeadParameters.count(Inst->getOperand()))
    return;

  SILCloner<DeadParamCloner>::visitStrongReleaseInst(Inst);
}

void
DeadParamCloner::visitStrongRetainInst(StrongRetainInst *Inst) {
  // If it's a retain of a dead parameter, just drop the instruction.
  if (DeadParameters.count(Inst->getOperand()))
    return;

  SILCloner<DeadParamCloner>::visitStrongRetainInst(Inst);
}

/// Specialize a partial_apply by removing the parameters indicated by
/// indices. We expect these parameters to be either dead, or used
/// only by retains and releases.
static PartialApplyInst *specializePartialApply(PartialApplyInst *PartialApply,
                                              ParamIndexSet &DeadParamIndices) {
  auto *FRI = cast<FunctionRefInst>(PartialApply->getCallee());
  assert(FRI && "Expected a direct partial_apply!");
  auto *F = FRI->getReferencedFunction();
  assert(F && "Expected a referenced function!");

  // Clone the function the existing partial_apply references.
  DeadParamCloner Cloner(F, DeadParamIndices);
  Cloner.populateCloned();

  // Now create the new partial_apply using the cloned function.
  llvm::SmallVector<SILValue, 16> Args;

  // Only use the arguments that are not dead.
  for (auto &O : PartialApply->getArgumentOperands()) {
    auto ParamIndex = getParameterIndexForOperand(&O);
    if (!DeadParamIndices.count(ParamIndex))
      Args.push_back(O.get());
  }

  // Build the function_ref and partial_apply.
  SILBuilder Builder(PartialApply);
  SILValue FunctionRef = Builder.createFunctionRef(PartialApply->getLoc(),
                                                   Cloner.getCloned());
  return Builder.createPartialApply(PartialApply->getLoc(), FunctionRef,
                                    FunctionRef.getType(), {}, Args,
                                    PartialApply->getType());
}

static void
rewritePartialApplies(llvm::SmallVectorImpl<Operand *> &ElidedOperands) {
  llvm::DenseMap<PartialApplyInst *, ParamIndexSet> IndexMap;
  ParamIndexSet Indices;

  // Build a map from partial_apply to the indices of the operands
  // that will not be in our rewritten version.
  for (auto *O : ElidedOperands) {
    auto ParamIndexNumber = getParameterIndexForOperand(O);

    Indices.clear();
    Indices.insert(ParamIndexNumber);

    auto *PartialApply = cast<PartialApplyInst>(O->getUser());
    llvm::DenseMap<PartialApplyInst *, ParamIndexSet>::iterator It;
    bool Inserted;
    std::tie(It, Inserted) = IndexMap.insert(std::make_pair(PartialApply,
                                                            Indices));
    if (!Inserted)
      It->second.insert(ParamIndexNumber);
  }

  // Clone the referenced function of each partial_apply, removing the
  // operands that we will not need, and remove the existing
  // partial_apply.
  for (auto &It : IndexMap) {
    auto *PartialApply = It.first;
    auto &Indices = It.second;
    auto *Replacement = specializePartialApply(PartialApply, Indices);
    PartialApply->replaceAllUsesWith(Replacement);

    auto *FRI = cast<FunctionRefInst>(PartialApply->getCallee());
    PartialApply->eraseFromParent();

    // TODO: Erase from module if there are no more uses.
    if (FRI->use_empty())
      FRI->eraseFromParent();
  }
}

static void
rewritePromotedBoxes(llvm::SmallVectorImpl<AllocBoxInst *> &Promoted,
                     llvm::SmallVectorImpl<Operand *> &ElidedOperands) {
  // First we'll rewrite any partial applies that we can to remove the
  // box container pointer from the operands.
  rewritePartialApplies(ElidedOperands);

  for (auto *ABI : Promoted) {
    rewriteAllocBoxAsAllocStack(ABI);
    ABI->eraseFromParent();
  }
}

namespace {
class AllocBoxToStack : public SILFunctionTransform {
  /// The entry point to the transformation.
  void run() {
    llvm::SmallVector<AllocBoxInst *, 8> Promoted;
    llvm::SmallVector<Operand *, 8> ElidedOperands;

    for (auto &BB : *getFunction())
      for (auto &I : BB)
        if (auto *ABI = dyn_cast<AllocBoxInst>(&I))
          if (canPromoteAllocBox(ABI, ElidedOperands))
            Promoted.push_back(ABI);

    if (!Promoted.empty()) {
      rewritePromotedBoxes(Promoted, ElidedOperands);
      NumStackPromoted += Promoted.size();
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }

  StringRef getName() override { return "AllocBox-To-Stack Optimization"; }
};
} // end anonymous namespace

SILTransform *swift::createAllocBoxToStack() {
  return new AllocBoxToStack();
}

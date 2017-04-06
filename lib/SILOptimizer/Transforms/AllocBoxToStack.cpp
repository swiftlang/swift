//===--- AllocBoxToStack.cpp - Promote alloc_box to alloc_stack -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "allocbox-to-stack"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SIL/Dominance.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/StackNesting.h"
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

/// This is a list we use to store a set of indices. We create the set by
/// sorting, uniquing at the appropriate time. The reason why it makes sense to
/// just use a sorted vector with std::count is because generally functions do
/// not have that many arguments and even fewer promoted arguments.
using ArgIndexList = llvm::SmallVector<unsigned, 8>;

static SILInstruction* findUnexpectedBoxUse(SILValue Box,
                                            bool examinePartialApply,
                                            bool inAppliedFunction,
                                            llvm::SmallVectorImpl<Operand*> &);
static bool partialApplyArgumentEscapes(Operand *O);

// Propagate liveness backwards from an initial set of blocks in our
// LiveIn set.
static void propagateLiveness(llvm::SmallPtrSetImpl<SILBasicBlock*> &LiveIn,
                              SILBasicBlock *DefBB) {

  // First populate a worklist of predecessors.
  llvm::SmallVector<SILBasicBlock*, 64> Worklist;
  for (auto *BB : LiveIn)
    for (auto Pred : BB->getPredecessorBlocks())
      Worklist.push_back(Pred);

  // Now propagate liveness backwards until we hit the alloc_box.
  while (!Worklist.empty()) {
    auto *BB = Worklist.pop_back_val();

    // If it's already in the set, then we've already queued and/or
    // processed the predecessors.
    if (BB == DefBB || !LiveIn.insert(BB).second)
      continue;

    for (auto Pred : BB->getPredecessorBlocks())
      Worklist.push_back(Pred);
  }
}

// Is any successor of BB in the LiveIn set?
static bool successorHasLiveIn(SILBasicBlock *BB,
                               llvm::SmallPtrSetImpl<SILBasicBlock*> &LiveIn) {
  for (auto &Succ : BB->getSuccessors())
    if (LiveIn.count(Succ))
      return true;

  return false;
}

static SILValue stripOffCopyValue(SILValue V) {
  while (auto *CVI = dyn_cast<CopyValueInst>(V)) {
    V = CVI->getOperand();
  }
  return V;
}

// Walk backwards in BB looking for strong_release, destroy_value, or
// dealloc_box of the given value, and add it to releases.
static bool addLastRelease(SILValue V, SILBasicBlock *BB,
                           llvm::SmallVectorImpl<SILInstruction*> &Releases) {
  for (auto I = BB->rbegin(); I != BB->rend(); ++I) {
    if (isa<StrongReleaseInst>(*I) || isa<DeallocBoxInst>(*I) ||
        isa<DestroyValueInst>(*I)) {
      if (stripOffCopyValue(I->getOperand(0)) != V)
        continue;

      Releases.push_back(&*I);
      return true;
    }
  }

  return false;
}

// Find the final releases of the alloc_box along any given path.
// These can include paths from a release back to the alloc_box in a
// loop.
static bool getFinalReleases(AllocBoxInst *ABI,
                             llvm::SmallVectorImpl<SILInstruction*> &Releases) {
  llvm::SmallPtrSet<SILBasicBlock*, 16> LiveIn;
  llvm::SmallPtrSet<SILBasicBlock*, 16> UseBlocks;

  auto *DefBB = ABI->getParent();

  auto seenRelease = false;
  SILInstruction *OneRelease = nullptr;

  // We'll treat this like a liveness problem where the alloc_box is
  // the def. Each block that has a use of the owning pointer has the
  // value live-in unless it is the block with the alloc_box.
  llvm::SmallVector<Operand *, 32> Worklist(ABI->use_begin(), ABI->use_end());
  while (!Worklist.empty()) {
    auto *Op = Worklist.pop_back_val();
    auto *User = Op->getUser();
    auto *BB = User->getParent();

    if (isa<ProjectBoxInst>(User))
      continue;

    if (BB != DefBB)
      LiveIn.insert(BB);

    // Also keep track of the blocks with uses.
    UseBlocks.insert(BB);

    // If we have a copy value, add its uses to the work list and continue.
    //
    // We are actually performing multiple operations here. We are eliminating
    // copies of the box and the box itself.
    if (auto *CVI = dyn_cast<CopyValueInst>(User)) {
      std::copy(CVI->use_begin(), CVI->use_end(), std::back_inserter(Worklist));
      continue;
    }

    // Try to speed up the trivial case of single release/dealloc.
    if (isa<StrongReleaseInst>(User) || isa<DeallocBoxInst>(User) ||
        isa<DestroyValueInst>(User)) {
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
    return true;
  }

  propagateLiveness(LiveIn, DefBB);

  // Now examine each block we saw a use in. If it has no successors
  // that are in LiveIn, then the last use in the block is the final
  // release/dealloc.
  for (auto *BB : UseBlocks)
    if (!successorHasLiveIn(BB, LiveIn))
      if (!addLastRelease(ABI, BB, Releases))
        return false;

  return true;
}

/// \brief Returns True if the operand or one of its users is captured.
static bool useCaptured(Operand *UI) {
  auto *User = UI->getUser();

  // These instructions do not cause the address to escape.
  if (isa<DebugValueInst>(User) || isa<DebugValueAddrInst>(User) ||
      isa<StrongReleaseInst>(User) || isa<StrongRetainInst>(User) ||
      isa<DestroyValueInst>(User))
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

static bool partialApplyEscapes(SILValue V, bool examineApply) {
  SILModuleConventions ModConv(*V->getModule());
  llvm::SmallVector<Operand *, 32> Worklist(V->use_begin(), V->use_end());
  while (!Worklist.empty()) {
    auto *Op = Worklist.pop_back_val();

    // These instructions do not cause the address to escape.
    if (!useCaptured(Op))
      continue;

    auto *User = Op->getUser();

    // If we have a copy_value, the copy value does not cause an escape, but its
    // uses might do so... so add the copy_value's uses to the worklist and
    // continue.
    if (auto *Copy = dyn_cast<CopyValueInst>(User)) {
      std::copy(Copy->use_begin(), Copy->use_end(),
                std::back_inserter(Worklist));
      continue;
    }

    if (auto *Apply = dyn_cast<ApplyInst>(User)) {
      // Applying a function does not cause the function to escape.
      if (Op->getOperandNumber() == 0)
        continue;

      // apply instructions do not capture the pointer when it is passed
      // indirectly
      if (Apply->getArgumentConvention(Op->getOperandNumber() - 1)
              .isIndirectConvention())
        continue;

      // Optionally drill down into an apply to see if the operand is
      // captured in or returned from the apply.
      if (examineApply && !partialApplyArgumentEscapes(Op))
        continue;
    }

    // partial_apply instructions do not allow the pointer to escape
    // when it is passed indirectly, unless the partial_apply itself
    // escapes
    if (auto *PartialApply = dyn_cast<PartialApplyInst>(User)) {
      auto Args = PartialApply->getArguments();
      auto Params = PartialApply->getSubstCalleeType()->getParameters();
      Params = Params.slice(Params.size() - Args.size(), Args.size());
      if (ModConv.isSILIndirect(Params[Op->getOperandNumber() - 1])) {
        if (partialApplyEscapes(PartialApply, /*examineApply = */ true))
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

/// Given an operand of a direct apply or partial_apply of a function,
/// return the argument index of the parameter used in the body of the function
/// to represent this operand.
static size_t getArgIndexForOperand(Operand *O) {
  assert(isa<ApplyInst>(O->getUser()) || isa<PartialApplyInst>(O->getUser()) &&
         "Expected apply or partial_apply!");

  auto OperandIndex = O->getOperandNumber();
  assert(OperandIndex != 0 && "Operand cannot be the applied function!");

  // The applied function is the first operand.
  auto ArgIndex = OperandIndex - ApplyInst::getArgumentOperandNumber();

  if (auto *Apply = dyn_cast<ApplyInst>(O->getUser())) {
    assert(Apply->getSubstCalleeConv().getNumSILArguments()
               == Apply->getArguments().size()
           && "Expected all arguments to be supplied!");
    (void) Apply;
  } else {
    auto *PartialApply = cast<PartialApplyInst>(O->getUser());
    auto fnConv = PartialApply->getSubstCalleeConv();
    auto ArgCount = PartialApply->getArguments().size();
    assert(ArgCount <= fnConv.getNumParameters());
    ArgIndex += (fnConv.getNumSILArguments() - ArgCount);
  }

  return ArgIndex;
}

/// Given an operand of a direct apply or partial_apply of a function,
/// return the parameter used in the body of the function to represent
/// this operand.
static SILArgument *getParameterForOperand(SILFunction *F, Operand *O) {
  assert(F && !F->empty() && "Expected a function with a body!");

  auto &Entry = F->front();
  size_t ArgIndex = getArgIndexForOperand(O);
  assert(ArgIndex >= F->getConventions().getSILArgIndexOfFirstParam());

  return Entry.getArgument(ArgIndex);
}

/// Return a pointer to the SILFunction called by Call if we can
/// determine which function that is, and we have a body for that
/// function. Otherwise return nullptr.
static SILFunction *getFunctionBody(SILInstruction *Call) {
  if (auto *FRI = getDirectCallee(Call))
    if (auto *F = FRI->getReferencedFunction())
      if (!F->empty())
        return F;

  return nullptr;
}

/// Could this operand to an apply escape that function by being
/// stored or returned?
static bool partialApplyArgumentEscapes(Operand *O) {
  SILFunction *F = getFunctionBody(O->getUser());
  // If we cannot examine the function body, assume the worst.
  if (!F)
    return true;

  // Check the uses of the operand, but do not recurse down into other
  // apply instructions.
  auto Param = SILValue(getParameterForOperand(F, O));
  return partialApplyEscapes(Param, /* examineApply = */ false);
}

/// checkPartialApplyBody - Check the body of a partial apply to see
/// if the box pointer argument passed to it has uses that would
/// disqualify it from being promoted to a stack location.  Return
/// true if this partial apply will not block our promoting the box.
static bool checkPartialApplyBody(Operand *O) {
  SILFunction *F = getFunctionBody(O->getUser());
  // If we cannot examine the function body, assume the worst.
  if (!F)
    return false;

  // We don't actually use these because we're not recursively
  // rewriting the partial applies we find.
  llvm::SmallVector<Operand *, 1> PromotedOperands;
  auto Param = SILValue(getParameterForOperand(F, O));
  return !findUnexpectedBoxUse(Param, /* examinePartialApply = */ false,
                               /* inAppliedFunction = */ true,
                               PromotedOperands);
}


/// findUnexpectedBoxUse - Validate that the uses of a pointer to a
/// box do not eliminate it from consideration for promotion to a
/// stack element. Optionally examine the body of partial_apply
/// to see if there is an unexpected use inside.  Return the
/// instruction with the unexpected use if we find one.
static SILInstruction* findUnexpectedBoxUse(SILValue Box,
                                            bool examinePartialApply,
                                            bool inAppliedFunction,
                            llvm::SmallVectorImpl<Operand *> &PromotedOperands) {
  assert((Box->getType().is<SILBoxType>()
          || Box->getType()
                 == SILType::getNativeObjectType(Box->getType().getASTContext()))
         && "Expected an object pointer!");

  llvm::SmallVector<Operand *, 4> LocalPromotedOperands;

  // Scan all of the uses of the retain count value, collecting all
  // the releases and validating that we don't have an unexpected
  // user.
  llvm::SmallVector<Operand *, 32> Worklist(Box->use_begin(), Box->use_end());
  while (!Worklist.empty()) {
    auto *Op = Worklist.pop_back_val();
    auto *User = Op->getUser();

    // Retains and releases are fine. Deallocs are fine if we're not
    // examining a function that the alloc_box was passed into.
    // Projections are fine as well.
    if (isa<StrongRetainInst>(User) || isa<StrongReleaseInst>(User) ||
        isa<ProjectBoxInst>(User) || isa<DestroyValueInst>(User) ||
        (!inAppliedFunction && isa<DeallocBoxInst>(User)))
      continue;

    // If we have a copy_value, visit the users of the copy_value.
    if (auto *CVI = dyn_cast<CopyValueInst>(User)) {
      std::copy(CVI->use_begin(), CVI->use_end(), std::back_inserter(Worklist));
      continue;
    }

    // For partial_apply, if we've been asked to examine the body, the
    // uses of the argument are okay there, and the partial_apply
    // itself cannot escape, then everything is fine.
    if (auto *PAI = dyn_cast<PartialApplyInst>(User))
      if (examinePartialApply && checkPartialApplyBody(Op) &&
          !partialApplyEscapes(PAI, /* examineApply = */ true)) {
        LocalPromotedOperands.push_back(Op);
        continue;
      }

    return User;
  }

  PromotedOperands.append(LocalPromotedOperands.begin(),
                          LocalPromotedOperands.end());
  return nullptr;
}

/// canPromoteAllocBox - Can we promote this alloc_box to an alloc_stack?
static bool canPromoteAllocBox(AllocBoxInst *ABI,
                             llvm::SmallVectorImpl<Operand *> &PromotedOperands){
  // Scan all of the uses of the address of the box to see if any
  // disqualifies the box from being promoted to the stack.
  if (auto *User = findUnexpectedBoxUse(ABI,
                                        /* examinePartialApply = */ true,
                                        /* inAppliedFunction = */ false,
                                        PromotedOperands)) {
    (void)User;
    // Otherwise, we have an unexpected use.
    DEBUG(llvm::dbgs() << "*** Failed to promote alloc_box in @"
          << ABI->getFunction()->getName() << ": " << *ABI
          << "    Due to user: " << *User << "\n");

    return false;
  }

  // Okay, it looks like this value doesn't escape.
  return true;
}

static void replaceProjectBoxUsers(AllocBoxInst *ABI, AllocStackInst *ASI) {
  llvm::SmallVector<Operand *, 8> Worklist(ABI->use_begin(), ABI->use_end());
  while (!Worklist.empty()) {
    auto *Op = Worklist.pop_back_val();
    if (auto *PBI = dyn_cast<ProjectBoxInst>(Op->getUser())) {
      PBI->replaceAllUsesWith(ASI);
      continue;
    }

    auto *CVI = dyn_cast<CopyValueInst>(Op->getUser());
    if (!CVI)
      continue;
    std::copy(CVI->use_begin(), CVI->use_end(), std::back_inserter(Worklist));
  }
}

/// rewriteAllocBoxAsAllocStack - Replace uses of the alloc_box with a
/// new alloc_stack, but do not delete the alloc_box yet.
static bool rewriteAllocBoxAsAllocStack(AllocBoxInst *ABI) {
  DEBUG(llvm::dbgs() << "*** Promoting alloc_box to stack: " << *ABI);

  llvm::SmallVector<SILInstruction *, 4> FinalReleases;
  if (!getFinalReleases(ABI, FinalReleases))
    return false;

  // Promote this alloc_box to an alloc_stack. Insert the alloc_stack
  // at the beginning of the function.
  SILBuilder BuildAlloc(ABI);
  BuildAlloc.setCurrentDebugScope(ABI->getDebugScope());
  assert(ABI->getBoxType()->getLayout()->getFields().size() == 1
         && "rewriting multi-field box not implemented");
  auto *ASI = BuildAlloc.createAllocStack(ABI->getLoc(),
                          ABI->getBoxType()->getFieldType(ABI->getModule(), 0),
                          ABI->getVarInfo());

  // Replace all uses of the address of the box's contained value with
  // the address of the stack location.
  replaceProjectBoxUsers(ABI, ASI);

  // Check to see if the alloc_box was used by a mark_uninitialized instruction.
  // If so, any uses of the pointer result need to keep using the MUI, not the
  // alloc_stack directly.  If we don't do this, DI will miss the uses.
  SILValue PointerResult = ASI;
  for (auto UI : ASI->getUses()) {
    if (auto *MUI = dyn_cast<MarkUninitializedInst>(UI->getUser())) {
      assert(ASI->hasOneUse() &&
             "alloc_stack used by mark_uninitialized, but not exclusively!");
      PointerResult = MUI;
      break;
    }
  }

  assert(ABI->getBoxType()->getLayout()->getFields().size() == 1
         && "promoting multi-field box not implemented");
  auto &Lowering = ABI->getModule()
    .getTypeLowering(ABI->getBoxType()->getFieldType(ABI->getModule(), 0));
  auto Loc = CleanupLocation::get(ABI->getLoc());

  for (auto LastRelease : FinalReleases) {
    SILBuilderWithScope Builder(LastRelease);
    if (!isa<DeallocBoxInst>(LastRelease)&& !Lowering.isTrivial()) {
      // For non-trivial types, insert destroys for each final release-like
      // instruction we found that isn't an explicit dealloc_box.
      Builder.emitDestroyAddrAndFold(Loc, PointerResult);
    }
    Builder.createDeallocStack(Loc, ASI);
  }

  // Remove any retain and release instructions.  Since all uses of project_box
  // are gone, this only walks through uses of the box itself (the retain count
  // pointer).
  llvm::SmallVector<SILInstruction *, 8> Worklist;
  std::transform(ABI->use_begin(), ABI->use_end(), std::back_inserter(Worklist),
                 [](Operand *Op) -> SILInstruction * { return Op->getUser(); });
  while (!Worklist.empty()) {
    auto *User = Worklist.pop_back_val();

    if (isa<CopyValueInst>(User)) {
      std::transform(
          User->use_begin(), User->use_end(), std::back_inserter(Worklist),
          [](Operand *Op) -> SILInstruction * { return Op->getUser(); });
      User->replaceAllUsesWith(
          SILUndef::get(User->getType(), User->getModule()));
      User->eraseFromParent();
      continue;
    }

    assert(isa<StrongReleaseInst>(User) || isa<StrongRetainInst>(User) ||
           isa<DeallocBoxInst>(User) || isa<ProjectBoxInst>(User) ||
           isa<DestroyValueInst>(User));

    User->eraseFromParent();
  }

  return true;
}

namespace {

/// \brief A SILCloner subclass which clones a closure function while
/// promoting some of its box parameters to stack addresses.
class PromotedParamCloner : public SILClonerWithScopes<PromotedParamCloner> {
public:
  friend class SILVisitor<PromotedParamCloner>;
  friend class SILCloner<PromotedParamCloner>;

  PromotedParamCloner(SILFunction *Orig, IsSerialized_t Serialized,
                      ArgIndexList &PromotedArgIndices,
                      llvm::StringRef ClonedName);

  void populateCloned();

  SILFunction *getCloned() { return &getBuilder().getFunction(); }

private:
  static SILFunction *initCloned(SILFunction *Orig, IsSerialized_t Serialized,
                                 ArgIndexList &PromotedArgIndices,
                                 llvm::StringRef ClonedName);

  void visitStrongReleaseInst(StrongReleaseInst *Inst);
  void visitDestroyValueInst(DestroyValueInst *Inst);
  void visitStrongRetainInst(StrongRetainInst *Inst);
  void visitCopyValueInst(CopyValueInst *Inst);
  void visitProjectBoxInst(ProjectBoxInst *Inst);

  SILFunction *Orig;
  ArgIndexList &PromotedArgIndices;

  // The values in the original function that are promoted to stack
  // references.
  llvm::SmallSet<SILValue, 4> PromotedParameters;
};
} // end anonymous namespace

PromotedParamCloner::PromotedParamCloner(SILFunction *Orig, IsSerialized_t Serialized,
                                         ArgIndexList &PromotedArgIndices,
                                         llvm::StringRef ClonedName)
    : SILClonerWithScopes<PromotedParamCloner>(
          *initCloned(Orig, Serialized, PromotedArgIndices, ClonedName)),
      Orig(Orig), PromotedArgIndices(PromotedArgIndices) {
  assert(Orig->getDebugScope()->getParentFunction() !=
         getCloned()->getDebugScope()->getParentFunction());
}

static std::string getClonedName(SILFunction *F, IsSerialized_t Serialized,
                                 ArgIndexList &PromotedArgIndices) {
  auto P = Demangle::SpecializationPass::AllocBoxToStack;
  Mangle::FunctionSignatureSpecializationMangler Mangler(P, Serialized, F);
  for (unsigned i : PromotedArgIndices) {
    Mangler.setArgumentBoxToStack(i);
  }
  return Mangler.mangle();
}

/// \brief Create the function corresponding to the clone of the
/// original closure with the signature modified to reflect promoted
/// parameters (which are specified by PromotedArgIndices).
SILFunction *PromotedParamCloner::initCloned(SILFunction *Orig,
                                             IsSerialized_t Serialized,
                                             ArgIndexList &PromotedArgIndices,
                                             llvm::StringRef ClonedName) {
  SILModule &M = Orig->getModule();

  SmallVector<SILParameterInfo, 4> ClonedInterfaceArgTys;

  // Generate a new parameter list with deleted parameters removed.
  SILFunctionType *OrigFTI = Orig->getLoweredFunctionType();
  unsigned Index = Orig->getConventions().getSILArgIndexOfFirstParam();
  for (auto &param : OrigFTI->getParameters()) {
    if (count(PromotedArgIndices, Index)) {
      auto boxTy = param.getSILStorageType().castTo<SILBoxType>();
      assert(boxTy->getLayout()->getFields().size() == 1
             && "promoting compound box not implemented");
      SILType paramTy;
      {
        Lowering::GenericContextScope scope(Orig->getModule().Types,
                                            OrigFTI->getGenericSignature());
        paramTy = boxTy->getFieldType(Orig->getModule(), 0);
      }
      auto promotedParam = SILParameterInfo(paramTy.getSwiftRValueType(),
                                  ParameterConvention::Indirect_InoutAliasable);
      ClonedInterfaceArgTys.push_back(promotedParam);
    } else {
      ClonedInterfaceArgTys.push_back(param);
    }
    ++Index;
  }

  // Create the new function type for the cloned function with some of
  // the parameters promoted.
  auto ClonedTy = SILFunctionType::get(
      OrigFTI->getGenericSignature(), OrigFTI->getExtInfo(),
      OrigFTI->getCalleeConvention(), ClonedInterfaceArgTys,
      OrigFTI->getResults(), OrigFTI->getOptionalErrorResult(),
      M.getASTContext());

  assert((Orig->isTransparent() || Orig->isBare() || Orig->getLocation())
         && "SILFunction missing location");
  assert((Orig->isTransparent() || Orig->isBare() || Orig->getDebugScope())
         && "SILFunction missing DebugScope");
  assert(!Orig->isGlobalInit() && "Global initializer cannot be cloned");
  auto *Fn = M.createFunction(
      SILLinkage::Shared, ClonedName, ClonedTy, Orig->getGenericEnvironment(),
      Orig->getLocation(), Orig->isBare(), IsNotTransparent, Serialized,
      Orig->isThunk(), Orig->getClassVisibility(), Orig->getInlineStrategy(),
      Orig->getEffectsKind(), Orig, Orig->getDebugScope());
  for (auto &Attr : Orig->getSemanticsAttrs()) {
    Fn->addSemanticsAttr(Attr);
  }
  if (Orig->hasUnqualifiedOwnership()) {
    Fn->setUnqualifiedOwnership();
  }
  return Fn;
}

/// \brief Populate the body of the cloned closure, modifying instructions as
/// necessary to take into consideration the removed parameters.
void
PromotedParamCloner::populateCloned() {
  SILFunction *Cloned = getCloned();

  // Create arguments for the entry block
  SILBasicBlock *OrigEntryBB = &*Orig->begin();
  SILBasicBlock *ClonedEntryBB = Cloned->createBasicBlock();
  unsigned ArgNo = 0;
  auto I = OrigEntryBB->args_begin(), E = OrigEntryBB->args_end();
  while (I != E) {
    if (count(PromotedArgIndices, ArgNo)) {
      // Create a new argument with the promoted type.
      auto boxTy = (*I)->getType().castTo<SILBoxType>();
      assert(boxTy->getLayout()->getFields().size() == 1
             && "promoting multi-field boxes not implemented yet");
      auto promotedTy = boxTy->getFieldType(Cloned->getModule(), 0);
      auto *promotedArg =
          ClonedEntryBB->createFunctionArgument(promotedTy, (*I)->getDecl());
      PromotedParameters.insert(*I);
      
      // Map any projections of the box to the promoted argument.
      for (auto use : (*I)->getUses()) {
        if (auto project = dyn_cast<ProjectBoxInst>(use->getUser())) {
          ValueMap.insert(std::make_pair(project, promotedArg));
        }
      }
      
    } else {
      // Create a new argument which copies the original argument.
      SILValue MappedValue = ClonedEntryBB->createFunctionArgument(
          (*I)->getType(), (*I)->getDecl());
      ValueMap.insert(std::make_pair(*I, MappedValue));
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
/// it is a strong release of a promoted box argument, then it is replaced with
/// a ReleaseValue of the new object type argument, otherwise it is handled
/// normally.
void
PromotedParamCloner::visitStrongReleaseInst(StrongReleaseInst *Inst) {
  // If it's a release of a promoted parameter, just drop the instruction.
  if (PromotedParameters.count(Inst->getOperand()))
    return;

  SILCloner<PromotedParamCloner>::visitStrongReleaseInst(Inst);
}

/// \brief Handle a strong_release instruction during cloning of a closure; if
/// it is a strong release of a promoted box argument, then it is replaced with
/// a ReleaseValue of the new object type argument, otherwise it is handled
/// normally.
void PromotedParamCloner::visitDestroyValueInst(DestroyValueInst *Inst) {
  // If we are a destroy of a promoted parameter, just drop the instruction. We
  // look through copy_value to preserve current behavior.
  SILInstruction *Tmp = Inst;
  while (auto *CopyOp = dyn_cast<CopyValueInst>(Tmp->getOperand(0))) {
    Tmp = CopyOp;
  }

  if (PromotedParameters.count(Tmp->getOperand(0)))
    return;

  SILCloner<PromotedParamCloner>::visitDestroyValueInst(Inst);
}

void
PromotedParamCloner::visitStrongRetainInst(StrongRetainInst *Inst) {
  // If it's a retain of a promoted parameter, just drop the instruction.
  if (PromotedParameters.count(Inst->getOperand()))
    return;

  SILCloner<PromotedParamCloner>::visitStrongRetainInst(Inst);
}

void PromotedParamCloner::visitCopyValueInst(CopyValueInst *CVI) {
  // If it's a copy of a promoted parameter, just drop the instruction.
  auto *Tmp = CVI;
  while (auto *CopyOp = dyn_cast<CopyValueInst>(Tmp->getOperand())) {
    Tmp = CopyOp;
  }
  if (PromotedParameters.count(Tmp->getOperand()))
    return;

  SILCloner<PromotedParamCloner>::visitCopyValueInst(CVI);
}

void PromotedParamCloner::visitProjectBoxInst(ProjectBoxInst *Inst) {
  // If it's a projection of a promoted parameter, drop the instruction.
  // Its uses will be replaced by the promoted address.
  // and replace its uses with
  if (PromotedParameters.count(Inst->getOperand()))
    return;
  
  SILCloner<PromotedParamCloner>::visitProjectBoxInst(Inst);
}

/// Specialize a partial_apply by promoting the parameters indicated by
/// indices. We expect these parameters to be replaced by stack address
/// references.
static PartialApplyInst *
specializePartialApply(PartialApplyInst *PartialApply,
                       ArgIndexList &PromotedArgIndices, bool &CFGChanged) {
  auto *FRI = cast<FunctionRefInst>(PartialApply->getCallee());
  assert(FRI && "Expected a direct partial_apply!");
  auto *F = FRI->getReferencedFunction();
  assert(F && "Expected a referenced function!");

  IsSerialized_t Serialized = IsNotSerialized;
  if (PartialApply->getFunction()->isSerialized())
    Serialized = IsSerializable;

  std::string ClonedName = getClonedName(F, Serialized, PromotedArgIndices);

  auto &M = PartialApply->getModule();

  SILFunction *ClonedFn;
  if (auto *PrevFn = M.lookUpFunction(ClonedName)) {
    assert(PrevFn->isSerialized() == Serialized);
    ClonedFn = PrevFn;
  } else {
    // Clone the function the existing partial_apply references.
    PromotedParamCloner Cloner(F, Serialized, PromotedArgIndices, ClonedName);
    Cloner.populateCloned();
    ClonedFn = Cloner.getCloned();
  }

  // Now create the new partial_apply using the cloned function.
  llvm::SmallVector<SILValue, 16> Args;

  ValueLifetimeAnalysis::Frontier PAFrontier;

  // Promote the arguments that need promotion.
  for (auto &O : PartialApply->getArgumentOperands()) {
    auto ArgIndex = getArgIndexForOperand(&O);
    if (!count(PromotedArgIndices, ArgIndex)) {
      Args.push_back(O.get());
      continue;
    }

    // If this argument is promoted, it is a box that we're turning into an
    // address because we've proven we can keep this value on the stack. The
    // partial_apply had ownership of this box so we must now release it
    // explicitly when the partial_apply is released.
    SILInstruction *Box = cast<SILInstruction>(O.get());
    assert((isa<AllocBoxInst>(Box) || isa<CopyValueInst>(Box)) &&
           "Expected either an alloc box or a copy of an alloc box");
    SILBuilder B(Box);
    Args.push_back(B.createProjectBox(Box->getLoc(), Box, 0));

    if (PAFrontier.empty()) {
      ValueLifetimeAnalysis VLA(PartialApply);
      CFGChanged |= !VLA.computeFrontier(PAFrontier,
                                      ValueLifetimeAnalysis::AllowToModifyCFG);
      assert(!PAFrontier.empty() && "partial_apply must have at least one use "
                                    "to release the returned function");
    }

    // Insert destroys of the box at each point where the partial_apply becomes
    // dead.
    for (SILInstruction *FrontierInst : PAFrontier) {
      SILBuilderWithScope Builder(FrontierInst);
      Builder.createDestroyValue(PartialApply->getLoc(), Box);
    }
  }

  SILBuilderWithScope Builder(PartialApply);

  // Build the function_ref and partial_apply.
  SILValue FunctionRef = Builder.createFunctionRef(PartialApply->getLoc(),
                                                   ClonedFn);
  CanSILFunctionType CanFnTy = ClonedFn->getLoweredFunctionType();
  auto const &Subs = PartialApply->getSubstitutions();
  CanSILFunctionType SubstCalleeTy = CanFnTy->substGenericArgs(M, Subs);
  return Builder.createPartialApply(PartialApply->getLoc(), FunctionRef,
                                 SILType::getPrimitiveObjectType(SubstCalleeTy),
                                    PartialApply->getSubstitutions(), Args,
                                    PartialApply->getType());
}

static void
rewritePartialApplies(llvm::SmallVectorImpl<Operand *> &PromotedOperands,
                      bool &CFGChanged) {
  llvm::DenseMap<PartialApplyInst *, ArgIndexList> IndexMap;
  ArgIndexList Indices;

  // Build a map from partial_apply to the indices of the operands
  // that will be promoted in our rewritten version.
  for (auto *O : PromotedOperands) {
    auto ArgIndexNumber = getArgIndexForOperand(O);

    Indices.clear();
    Indices.push_back(ArgIndexNumber);

    auto *PartialApply = cast<PartialApplyInst>(O->getUser());
    llvm::DenseMap<PartialApplyInst *, ArgIndexList>::iterator It;
    bool Inserted;
    std::tie(It, Inserted) = IndexMap.insert(std::make_pair(PartialApply,
                                                            Indices));
    if (!Inserted)
      It->second.push_back(ArgIndexNumber);
  }

  // Clone the referenced function of each partial_apply, removing the
  // operands that we will not need, and remove the existing
  // partial_apply.
  for (auto &It : IndexMap) {
    auto *PartialApply = It.first;
    auto &Indices = It.second;

    // Sort the indices and unique them.
    std::sort(Indices.begin(), Indices.end());
    Indices.erase(std::unique(Indices.begin(), Indices.end()), Indices.end());

    auto *Replacement = specializePartialApply(PartialApply, Indices,
                                               CFGChanged);
    PartialApply->replaceAllUsesWith(Replacement);

    auto *FRI = cast<FunctionRefInst>(PartialApply->getCallee());
    PartialApply->eraseFromParent();

    // TODO: Erase from module if there are no more uses.
    if (FRI->use_empty())
      FRI->eraseFromParent();
  }
}

static unsigned
rewritePromotedBoxes(llvm::SmallVectorImpl<AllocBoxInst *> &Promoted,
                     llvm::SmallVectorImpl<Operand *> &PromotedOperands,
                     bool &CFGChanged) {
  // First we'll rewrite any partial applies that we can to remove the
  // box container pointer from the operands.
  rewritePartialApplies(PromotedOperands, CFGChanged);

  unsigned Count = 0;
  auto rend = Promoted.rend();
  for (auto I = Promoted.rbegin(); I != rend; ++I) {
    auto *ABI = *I;
    if (rewriteAllocBoxAsAllocStack(ABI)) {
      ++Count;
      ABI->eraseFromParent();
    }
  }
  return Count;
}

namespace {
class AllocBoxToStack : public SILFunctionTransform {
  /// The entry point to the transformation.
  void run() override {
    llvm::SmallVector<AllocBoxInst *, 8> Promotable;
    llvm::SmallVector<Operand *, 8> PromotedOperands;

    for (auto &BB : *getFunction()) {
      for (auto &I : BB)
        if (auto *ABI = dyn_cast<AllocBoxInst>(&I))
          if (canPromoteAllocBox(ABI, PromotedOperands))
            Promotable.push_back(ABI);
    }

    if (!Promotable.empty()) {
      bool CFGChanged = false;
      auto Count = rewritePromotedBoxes(Promotable, PromotedOperands,
                                        CFGChanged);
      NumStackPromoted += Count;
      if (Count) {
        StackNesting SN;
        if (SN.correctStackNesting(getFunction()) == StackNesting::Changes::CFG)
          CFGChanged = true;
      }
      
      invalidateAnalysis(CFGChanged ?
                         SILAnalysis::InvalidationKind::FunctionBody :
                         SILAnalysis::InvalidationKind::CallsAndInstructions);
    }
  }

  StringRef getName() override { return "AllocBox-To-Stack Optimization"; }
};
} // end anonymous namespace

SILTransform *swift::createAllocBoxToStack() {
  return new AllocBoxToStack();
}

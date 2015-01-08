//===-- ClosureSpecializer.cpp ------ Performs Closure Specialization----===//
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

#define DEBUG_TYPE "closure-specialization"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/Mangle.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILAnalysis/LoopAnalysis.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/SILInliner.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumClosureSpecialized,
          "Number of functions with closures specialized");

//===----------------------------------------------------------------------===//
//                            Closure Spec Cloner
//===----------------------------------------------------------------------===//

namespace {

/// \brief A SILCloner subclass which clones a function that takes a closure
/// argument. We update the parameter list to remove the parameter for the
/// closure argument and to append the variables captured in the closure.
/// We also need to replace the closure parameter with the partial apply
/// on the closure. We need to update the callsite to pass in the correct
/// arguments.
class ClosureSpecCloner : public SILClonerWithScopes<ClosureSpecCloner> {
public:
  using SuperTy = SILClonerWithScopes<ClosureSpecCloner>;
  friend class SILVisitor<ClosureSpecCloner>;
  friend class SILCloner<ClosureSpecCloner>;

  ClosureSpecCloner(SILFunction *PAIUser, PartialApplyInst *PAI,
                     unsigned ClosureIndex, StringRef ClonedName)
    : SuperTy(*initCloned(PAIUser, PAI, ClosureIndex, ClonedName)),
      PAIUser(PAIUser), ClosureIndex(ClosureIndex), PAI(PAI) {
  }

  void populateCloned();

  SILFunction *getCloned() { return &getBuilder().getFunction(); }
  static SILFunction *cloneFunction(SILFunction *F, PartialApplyInst *PAI,
                                    unsigned ClosureIndex, StringRef NewName) {
    ClosureSpecCloner C(F, PAI, ClosureIndex, NewName);
    C.populateCloned();
    ++NumClosureSpecialized;
    return C.getCloned();
  };

private:
  static SILFunction *initCloned(SILFunction *PAIUser, PartialApplyInst *PAI,
                                 unsigned ClosureIndex, StringRef ClonedName);
  SILFunction *PAIUser;
  unsigned ClosureIndex;
  PartialApplyInst *PAI;
};

} // end anonymous namespace

/// In this function we create the actual cloned function and its proper cloned
/// type. But we do not create any body. This implies that the creation of the
/// actual arguments in the function is in populateCloned.
///
/// \arg PAUser The function that is being passed the partial apply.
/// \arg PAI The partial apply that is being passed to PAUser.
/// \arg ClosureIndex The index of the partial apply in PAUser's function
///                   signature.
/// \arg ClonedName The name of the cloned function that we will create.
SILFunction *ClosureSpecCloner::initCloned(SILFunction *PAIUser,
                                           PartialApplyInst *PAI,
                                           unsigned ClosureIndex,
                                           StringRef ClonedName) {
  // This is the list of new interface parameters of the cloned function.
  llvm::SmallVector<SILParameterInfo, 4> NewParameterInfoList;

  // First add to NewParameterInfoList all of the SILParameterInfo in the
  // original function except for the closure.
  CanSILFunctionType PAIUserFunTy = PAIUser->getLoweredFunctionType();
  unsigned Index = 0;
  for (auto &param : PAIUserFunTy->getParameters()) {
    if (Index != ClosureIndex)
      NewParameterInfoList.push_back(param);
    ++Index;
  }

  // Then add any arguments that are captured in the closure to the function's
  // argument type. Since they are captured, we need to pass them directly into
  // the new specialized function.
  auto *ClosedOverFunFRI = cast<FunctionRefInst>(PAI->getCallee());
  auto ClosedOverFunTy = ClosedOverFunFRI->getFunctionType();

  // Captured parameters are always appended to the function signature. So grab the
  unsigned NumTotalParams = ClosedOverFunTy->getParameters().size();
  unsigned NumNotCaptured = NumTotalParams - PAI->getNumArguments();
  for (auto &PInfo : ClosedOverFunTy->getParameters().slice(NumNotCaptured))
    NewParameterInfoList.push_back(PInfo);

  SILModule &M = PAIUser->getModule();
  auto ClonedTy =
    SILFunctionType::get(PAIUserFunTy->getGenericSignature(),
                         PAIUserFunTy->getExtInfo(),
                         PAIUserFunTy->getCalleeConvention(),
                         NewParameterInfoList,
                         PAIUserFunTy->getResult(),
                         M.getASTContext());

  // We make this function bare so we don't have to worry about decls in the
  // SILArgument.
  auto Fn = SILFunction::create(M, PAIUser->getLinkage(), ClonedName, ClonedTy,
                                PAIUser->getContextGenericParams(),
                                PAIUser->getLocation(), IsBare,
                                PAIUser->isTransparent(), PAIUser->isFragile(),
                                PAIUser->getClassVisibility(),
                                PAIUser->getInlineStrategy(),
                                PAIUser->getEffectsInfo(),
                                PAIUser, PAIUser->getDebugScope());
  Fn->setSemanticsAttr(PAIUser->getSemanticsAttr());
  return Fn;
}

/// \brief Populate the body of the cloned closure, modifying instructions as
/// necessary. This is where we create the actual specialized BB Arguments.
void ClosureSpecCloner::populateCloned() {
  SILFunction *Cloned = getCloned();
  SILModule &M = Cloned->getModule();

  // Create arguments for the entry block.
  SILBasicBlock *PAIUserEntryBB = PAIUser->begin();
  SILBasicBlock *ClonedEntryBB = new (M) SILBasicBlock(Cloned);

  // Remove the closure argument.
  SILArgument *ClosureArg = nullptr;
  for (size_t i = 0, e = PAIUserEntryBB->bbarg_size(); i != e; ++i) {
    SILArgument *Arg = PAIUserEntryBB->getBBArg(i);
    if (i == ClosureIndex) {
      ClosureArg = Arg;
      continue;
    }

    // Otherwise, create a new argument which copies the original argument
    SILValue MappedValue =
      new (M) SILArgument(ClonedEntryBB, Arg->getType(), Arg->getDecl());

    ValueMap.insert(std::make_pair(Arg, MappedValue));
  }

  // Next we need to add in any arguments that are not captured as arguments to
  // the cloned function.
  //
  // We do not insert the new mapped arugments into the value map since there by
  // definition is nothing in the partial apply user function that references
  // such arguments. After this pass is done the only thing that will reference
  // the arguments is the partial apply that we will create.

  auto *ClosedOverFunFRI = cast<FunctionRefInst>(PAI->getCallee());
  auto *ClosedOverFun = ClosedOverFunFRI->getReferencedFunction();
  auto ClosedOverFunTy = ClosedOverFunFRI->getFunctionType();
  unsigned NumTotalParams = ClosedOverFunTy->getParameters().size();
  unsigned NumNotCaptured = NumTotalParams - PAI->getNumArguments();
  llvm::SmallVector<SILValue, 4> NewPAIArgs;
  for (auto &PInfo : ClosedOverFunTy->getParameters().slice(NumNotCaptured)) {
    SILValue MappedValue =
      new (M) SILArgument(ClonedEntryBB, PInfo.getSILType());
    NewPAIArgs.push_back(MappedValue);
  }

  getBuilder().setInsertionPoint(ClonedEntryBB);
  // Clone FRI and PAI, and replace usage of the removed closure argument
  // with result of cloned PAI.
  SILValue FnVal = getBuilder().createFunctionRef(ClosedOverFunFRI->getLoc(),
                                                  ClosedOverFun);
  auto *NewPAI = getBuilder().createPartialApply(PAI->getLoc(), FnVal,
                                                 FnVal.getType(), {},
                                                 NewPAIArgs, PAI->getType());
  ValueMap.insert(std::make_pair(ClosureArg, SILValue(NewPAI, 0)));

  BBMap.insert(std::make_pair(PAIUserEntryBB, ClonedEntryBB));
  // Recursively visit original BBs in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(PAIUserEntryBB);

  // Now iterate over the BBs and fix up the terminators.
  for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
    getBuilder().setInsertionPoint(BI->second);
    visit(BI->first->getTerminator());
  }
}

//===----------------------------------------------------------------------===//
//                            Arg Spec Descriptor
//===----------------------------------------------------------------------===//

namespace {

struct ArgDescriptor {
  PartialApplyInst *PAI;
  ApplyInst *AI;
  unsigned ClosureIndex;
  ArgDescriptor(PartialApplyInst *PAI, ApplyInst *AI,
                    unsigned ClosureIndex) :
    PAI(PAI), AI(AI), ClosureIndex(ClosureIndex) {
  }
};

} // end anonymous namespace

/// Update the callsite to pass in the correct arguments.
static void rewriteApplyInst(ArgDescriptor &AD, SILFunction *NewF) {
  SILBuilderWithScope<2> Builder(AD.AI);
  FunctionRefInst *FRI = Builder.createFunctionRef(AD.AI->getLoc(), NewF);

  // Create the args for the new apply by removing the closure argument and
  // appending the captured argument.
  llvm::SmallVector<SILValue, 8> NewArgs;
  unsigned Index = 0;
  for (auto Arg : AD.AI->getArguments()) {
    if (Index != AD.ClosureIndex)
      NewArgs.push_back(Arg);
    Index++;
  }
  for (auto Arg : AD.PAI->getArguments())
    NewArgs.push_back(Arg);

  SILType LoweredType = NewF->getLoweredType();
  SILType ResultType = LoweredType.castTo<SILFunctionType>()->getSILResult();
  ApplyInst *NewAI = Builder.createApply(AD.AI->getLoc(), FRI, LoweredType,
                                         ResultType, ArrayRef<Substitution>(),
                                         NewArgs, NewF->isTransparent());

  // Replace all uses of the old apply with the new apply.
  AD.AI->replaceAllUsesWith(NewAI);
  // Erase the old apply.
  AD.AI->eraseFromParent();

  if (AD.PAI->use_empty())
    AD.PAI->eraseFromParent();
}

//===----------------------------------------------------------------------===//
//                            Closure Specializer
//===----------------------------------------------------------------------===//

namespace {

struct ClosureSpecializer {
  SILLoopAnalysis *LA;

  ClosureSpecializer(SILLoopAnalysis *LA)
    : LA(LA) {
  }

  void gatherCallSites(SILFunction *Caller,
                       llvm::SmallVectorImpl<ArgDescriptor> &CallSites,
                       llvm::SmallPtrSet<ApplyInst *, 4> &MultipleClosureAI);
  bool specialize(SILFunction *Caller);
};

} // end anonymous namespace

static void createName(SILFunction *Callee, SILArgument *Arg,
                       PartialApplyInst *PAI,
                       llvm::SmallString<64> &Name) {
  llvm::raw_svector_ostream buffer(Name);
  Mangle::Mangler M(buffer);
  auto P = Mangle::SpecializationPass::ClosureSpecializer;
  Mangle::FunctionSignatureSpecializationMangler FSSM(P, M, Callee);
  FSSM.setArgumentClosureProp(Arg->getIndex(), PAI);
  FSSM.mangle();
}

void
ClosureSpecializer::
gatherCallSites(SILFunction *Caller,
                llvm::SmallVectorImpl<ArgDescriptor> &CallSites,
                llvm::SmallPtrSet<ApplyInst *, 4> &MultipleClosureAI) {

  // A set of apply inst that we have associated with a closure. We use this to
  // make sure that we do not handle call sites with multiple closure arguments.
  llvm::SmallPtrSet<ApplyInst *, 4> VisitedAI;

  // For each basic block BB in Caller...
  for (auto &BB : *Caller) {
    // For each instruction II in BB...
    for (auto &II : BB) {
      // If II is not a partial apply or is a partial apply with substitutions,
      // we are not interested in it... Skip it.
      auto *PAI = dyn_cast<PartialApplyInst>(&II);
      if (!PAI || PAI->hasSubstitutions())
        continue;

      // If II is a partial apply, make sure that it is a simple partial apply
      // (i.e. its callee is a function_ref). We also do not handle indirect
      // results currently in the closure so make sure that does not happen at
      // this point.
      //
      // TODO: We can probably handle other partial applies here.
      auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());
      if (!FRI || FRI->getFunctionType()->hasIndirectResult())
        continue;

      // If our partial apply has more than one use, bail.
      //
      // TODO: Handle multiple apply insts.
      if (!PAI->hasOneUse())
        continue;

      // Grab the use of our partial apply. If that use is not an apply inst or
      // an apply inst with substitutions, there is nothing interesting for us
      // to do, so continue...
      auto *AI = dyn_cast<ApplyInst>(PAI->use_begin().getUser());
      if (!AI || AI->hasSubstitutions())
        continue;

      // Check if we have already associated this apply inst with a closure to
      // be specialized. We do not handle applies that take in multiple
      // closures at this time.
      if (!VisitedAI.insert(AI).second) {
        MultipleClosureAI.insert(AI);
        continue;
      }

      // If AI does not have a function_ref defintion as its callee, we can not
      // do anything here... so continue...
      auto *CalleeFRI = dyn_cast<FunctionRefInst>(AI->getCallee());
      if (!CalleeFRI ||
          CalleeFRI->getReferencedFunction()->isExternalDeclaration())
        continue;

      // Ok, we know that we can perform the optimization but not whether or not
      // the optimization is profitable. Find the index of the argument
      // corresponding to our partial apply.
      Optional<unsigned> PAIIndex;
      for (unsigned i = 0, e = AI->getNumArguments(); i != e; ++i) {
        if (AI->getArgument(i) != SILValue(PAI))
          continue;
        PAIIndex = i;
        DEBUG(llvm::dbgs() << "    Found callsite with closure argument at "
              << i << ": " << *AI);
        break;
      }

      // If we did not find an index, there is nothing further to do, continue.
      if (!PAIIndex.hasValue())
        continue;

      // Now we know that AD is profitable to specialize. Add it to our call
      // site list.
      CallSites.push_back(ArgDescriptor(PAI, AI, PAIIndex.getValue()));
    }
  }
}

bool ClosureSpecializer::specialize(SILFunction *Caller) {
  DEBUG(llvm::dbgs() << "Optimizing callsites that take closure argument in "
                     << Caller->getName() << '\n');

  // Collect all of the PartialApplyInsts that are used as arguments to
  // ApplyInsts. Check the profitability of specializing the closure argument.
  llvm::SmallVector<ArgDescriptor, 8> CallSites;
  llvm::SmallPtrSet<ApplyInst *, 4> MultipleClosureAI;
  gatherCallSites(Caller, CallSites, MultipleClosureAI);

  bool Changed = false;
  for (auto &AD : CallSites) {
    // Do not specialize apply insts that take in multiple closures. This pass
    // does not know how to do this yet.
    if (MultipleClosureAI.count(AD.AI))
      continue;

    auto *CalleeFRI = cast<FunctionRefInst>(AD.AI->getCallee());
    auto *Callee = CalleeFRI->getReferencedFunction();

    llvm::SmallString<64> NewFName;
    createName(Callee, Callee->getArgument(AD.ClosureIndex), AD.PAI,
               NewFName);
    DEBUG(llvm::dbgs() << "    Perform optimizations with new name "
                       << NewFName << '\n');

    SILFunction *NewF = Callee->getModule().lookUpFunction(NewFName);
    if (!NewF)
      NewF = ClosureSpecCloner::cloneFunction(Callee, AD.PAI, AD.ClosureIndex,
                                              NewFName);

    rewriteApplyInst(AD, NewF);
    Changed = true;
  }

  return Changed;
}

//===----------------------------------------------------------------------===//
//                               Top Level Code
//===----------------------------------------------------------------------===//

namespace {
class SILClosureSpecializerTransform : public SILModuleTransform {
public:
  SILClosureSpecializerTransform() {}

  virtual void run() {
    auto *CGA = getAnalysis<CallGraphAnalysis>();
    auto *LA = getAnalysis<SILLoopAnalysis>();

    bool Changed = false;
    // Specialize going bottom-up in the call graph.
    for (auto *F : CGA->getCallGraph().getBottomUpFunctionOrder()) {
      // If F is an external declaration, attempt to link in its definition. If
      // we fail to do so, there is nothing further that we can do.
      if (F->isExternalDeclaration() &&
          !getModule()->linkFunction(F, SILModule::LinkingMode::LinkAll))
        continue;

      Changed |= ClosureSpecializer(LA).specialize(F);
    }

    // Invalidate the call graph.
    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
  }

  StringRef getName() override { return "Closure Specialization"; }
};
} // end anonymous namespace


SILTransform *swift::createClosureSpecializer() {
  return new SILClosureSpecializerTransform();
}

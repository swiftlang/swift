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
//                       Closure Spec Cloner Interface
//===----------------------------------------------------------------------===//

namespace {

class CallSiteDescriptor;

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

  ClosureSpecCloner(const CallSiteDescriptor &CallSiteDesc,
                    StringRef ClonedName)
      : SuperTy(*initCloned(CallSiteDesc, ClonedName)),
        CallSiteDesc(CallSiteDesc) {}

  void populateCloned();

  SILFunction *getCloned() { return &getBuilder().getFunction(); }
  static SILFunction *cloneFunction(const CallSiteDescriptor &CallSiteDesc,
                                    StringRef NewName) {
    ClosureSpecCloner C(CallSiteDesc, NewName);
    C.populateCloned();
    ++NumClosureSpecialized;
    return C.getCloned();
  };

private:
  static SILFunction *initCloned(const CallSiteDescriptor &CallSiteDesc,
                                 StringRef ClonedName);
  const CallSiteDescriptor &CallSiteDesc;
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                            Arg Spec Descriptor
//===----------------------------------------------------------------------===//

namespace {

class CallSiteDescriptor {
  SILInstruction *Closure;
  ApplyInst *AI;
  unsigned ClosureIndex;

public:
  CallSiteDescriptor(SILInstruction *ClosureInst, ApplyInst *AI,
                     unsigned ClosureIndex)
      : Closure(ClosureInst), AI(AI), ClosureIndex(ClosureIndex) {}

  SILFunction *getApplyCallee() const {
    return cast<FunctionRefInst>(AI->getCallee())->getReferencedFunction();
  }

  SILFunction *getClosureCallee() const {
    auto *PAI = cast<PartialApplyInst>(Closure);
    return cast<FunctionRefInst>(PAI->getCallee())->getReferencedFunction();
  }

  unsigned getClosureIndex() const { return ClosureIndex; }

  SILInstruction *
  createNewClosure(SILBuilder &B, SILValue V,
                   llvm::SmallVectorImpl<SILValue> &Args) const {
    return B.createPartialApply(Closure->getLoc(), V, V.getType(), {}, Args,
                                Closure->getType(0));
  }

  ApplyInst *getApplyInst() const { return AI; }

  void specializeClosure() const;

  void createName(llvm::SmallString<64> &NewName) const;

  OperandValueArrayRef getArguments() const {
    if (auto *PAI = dyn_cast<PartialApplyInst>(Closure))
      return PAI->getArguments();
    return OperandValueArrayRef(ArrayRef<Operand>());
  }

  SILInstruction *getClosure() const { return Closure; }

  unsigned getNumArguments() const {
    auto *PAI = cast<PartialApplyInst>(Closure);
    return PAI->getNumArguments();
  }

  SILLocation getLoc() const { return Closure->getLoc(); }

  SILModule &getModule() const { return AI->getModule(); }
};

} // end anonymous namespace

/// Update the callsite to pass in the correct arguments.
static void rewriteApplyInst(const CallSiteDescriptor &CSDesc,
                             SILFunction *NewF) {
  ApplyInst *AI = CSDesc.getApplyInst();
  SILBuilderWithScope<2> Builder(AI);
  FunctionRefInst *FRI = Builder.createFunctionRef(AI->getLoc(), NewF);

  // Create the args for the new apply by removing the closure argument and
  // appending the captured argument.
  llvm::SmallVector<SILValue, 8> NewArgs;
  unsigned Index = 0;
  for (auto Arg : AI->getArguments()) {
    if (Index != CSDesc.getClosureIndex())
      NewArgs.push_back(Arg);
    Index++;
  }
  for (auto Arg : CSDesc.getArguments())
    NewArgs.push_back(Arg);

  SILType LoweredType = NewF->getLoweredType();
  SILType ResultType = LoweredType.castTo<SILFunctionType>()->getSILResult();
  ApplyInst *NewAI = Builder.createApply(AI->getLoc(), FRI, LoweredType,
                                         ResultType, ArrayRef<Substitution>(),
                                         NewArgs, NewF->isTransparent());

  // Replace all uses of the old apply with the new apply.
  AI->replaceAllUsesWith(NewAI);
  // Erase the old apply.
  AI->eraseFromParent();

  SILInstruction *Closure = CSDesc.getClosure();
  if (Closure->use_empty())
    Closure->eraseFromParent();
}

void CallSiteDescriptor::createName(llvm::SmallString<64> &NewName) const {
  llvm::raw_svector_ostream buffer(NewName);
  Mangle::Mangler M(buffer);
  auto P = Mangle::SpecializationPass::ClosureSpecializer;
  Mangle::FunctionSignatureSpecializationMangler FSSM(P, M, getApplyCallee());
  auto *PAI = cast<PartialApplyInst>(Closure);
  FSSM.setArgumentClosureProp(getClosureIndex(), PAI);
  FSSM.mangle();
}

void CallSiteDescriptor::specializeClosure() const {
  llvm::SmallString<64> NewFName;
  createName(NewFName);
  DEBUG(llvm::dbgs() << "    Perform optimizations with new name " << NewFName
                     << '\n');

  // Then see if we already have a specialized version of this function in our
  // module.
  SILFunction *NewF = getModule().lookUpFunction(NewFName);

  // If not, create a specialized version of ApplyCallee calling the closure
  // directly.
  if (!NewF)
    NewF = ClosureSpecCloner::cloneFunction(*this, NewFName);

  // Rewrite the call
  rewriteApplyInst(*this, NewF);
}

//===----------------------------------------------------------------------===//
//                     Closure Spec Cloner Implementation
//===----------------------------------------------------------------------===//

/// In this function we create the actual cloned function and its proper cloned
/// type. But we do not create any body. This implies that the creation of the
/// actual arguments in the function is in populateCloned.
///
/// \arg PAUser The function that is being passed the partial apply.
/// \arg PAI The partial apply that is being passed to PAUser.
/// \arg ClosureIndex The index of the partial apply in PAUser's function
///                   signature.
/// \arg ClonedName The name of the cloned function that we will create.
SILFunction *
ClosureSpecCloner::initCloned(const CallSiteDescriptor &CallSiteDesc,
                              StringRef ClonedName) {
  SILFunction *ClosureUser = CallSiteDesc.getApplyCallee();

  // This is the list of new interface parameters of the cloned function.
  llvm::SmallVector<SILParameterInfo, 4> NewParameterInfoList;

  // First add to NewParameterInfoList all of the SILParameterInfo in the
  // original function except for the closure.
  CanSILFunctionType ClosureUserFunTy = ClosureUser->getLoweredFunctionType();
  unsigned Index = 0;
  for (auto &param : ClosureUserFunTy->getParameters()) {
    if (Index != CallSiteDesc.getClosureIndex())
      NewParameterInfoList.push_back(param);
    ++Index;
  }

  // Then add any arguments that are captured in the closure to the function's
  // argument type. Since they are captured, we need to pass them directly into
  // the new specialized function.
  SILFunction *ClosedOverFun = CallSiteDesc.getClosureCallee();
  CanSILFunctionType ClosedOverFunTy = ClosedOverFun->getLoweredFunctionType();

  // Captured parameters are always appended to the function signature. So grab the
  unsigned NumTotalParams = ClosedOverFunTy->getParameters().size();
  unsigned NumNotCaptured = NumTotalParams - CallSiteDesc.getNumArguments();
  for (auto &PInfo : ClosedOverFunTy->getParameters().slice(NumNotCaptured))
    NewParameterInfoList.push_back(PInfo);

  SILModule &M = ClosureUser->getModule();
  auto ClonedTy = SILFunctionType::get(
      ClosureUserFunTy->getGenericSignature(), ClosureUserFunTy->getExtInfo(),
      ClosureUserFunTy->getCalleeConvention(), NewParameterInfoList,
      ClosureUserFunTy->getResult(), M.getASTContext());

  // We make this function bare so we don't have to worry about decls in the
  // SILArgument.
  auto Fn = SILFunction::create(
      M, ClosureUser->getLinkage(), ClonedName, ClonedTy,
      ClosureUser->getContextGenericParams(), ClosureUser->getLocation(),
      IsBare, ClosureUser->isTransparent(), ClosureUser->isFragile(),
      ClosureUser->getClassVisibility(), ClosureUser->getInlineStrategy(),
      ClosureUser->getEffectsKind(), ClosureUser, ClosureUser->getDebugScope());
  Fn->setSemanticsAttr(ClosureUser->getSemanticsAttr());
  return Fn;
}

/// \brief Populate the body of the cloned closure, modifying instructions as
/// necessary. This is where we create the actual specialized BB Arguments.
void ClosureSpecCloner::populateCloned() {
  SILFunction *Cloned = getCloned();
  SILModule &M = Cloned->getModule();

  SILFunction *ClosureUser = CallSiteDesc.getApplyCallee();

  // Create arguments for the entry block.
  SILBasicBlock *ClosureUserEntryBB = ClosureUser->begin();
  SILBasicBlock *ClonedEntryBB = new (M) SILBasicBlock(Cloned);

  // Remove the closure argument.
  SILArgument *ClosureArg = nullptr;
  for (size_t i = 0, e = ClosureUserEntryBB->bbarg_size(); i != e; ++i) {
    SILArgument *Arg = ClosureUserEntryBB->getBBArg(i);
    if (i == CallSiteDesc.getClosureIndex()) {
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
  SILFunction *ClosedOverFun = CallSiteDesc.getClosureCallee();
  CanSILFunctionType ClosedOverFunTy = ClosedOverFun->getLoweredFunctionType();
  unsigned NumTotalParams = ClosedOverFunTy->getParameters().size();
  unsigned NumNotCaptured = NumTotalParams - CallSiteDesc.getNumArguments();
  llvm::SmallVector<SILValue, 4> NewPAIArgs;
  for (auto &PInfo : ClosedOverFunTy->getParameters().slice(NumNotCaptured)) {
    SILValue MappedValue =
      new (M) SILArgument(ClonedEntryBB, PInfo.getSILType());
    NewPAIArgs.push_back(MappedValue);
  }

  getBuilder().setInsertionPoint(ClonedEntryBB);
  // Clone FRI and PAI, and replace usage of the removed closure argument
  // with result of cloned PAI.
  SILValue FnVal =
      getBuilder().createFunctionRef(CallSiteDesc.getLoc(), ClosedOverFun);
  auto *NewClosure =
      CallSiteDesc.createNewClosure(getBuilder(), FnVal, NewPAIArgs);
  ValueMap.insert(std::make_pair(ClosureArg, SILValue(NewClosure)));

  BBMap.insert(std::make_pair(ClosureUserEntryBB, ClonedEntryBB));
  // Recursively visit original BBs in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(ClosureUserEntryBB);

  // Now iterate over the BBs and fix up the terminators.
  for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
    getBuilder().setInsertionPoint(BI->second);
    visit(BI->first->getTerminator());
  }
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
                       llvm::SmallVectorImpl<CallSiteDescriptor> &CallSites,
                       llvm::SmallPtrSet<ApplyInst *, 4> &MultipleClosureAI);
  bool specialize(SILFunction *Caller);
};

} // end anonymous namespace

void ClosureSpecializer::gatherCallSites(
    SILFunction *Caller, llvm::SmallVectorImpl<CallSiteDescriptor> &CallSites,
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

      // Now we know that CSDesc is profitable to specialize. Add it to our call
      // site list.
      CallSites.push_back(CallSiteDescriptor(PAI, AI, PAIIndex.getValue()));
    }
  }
}

bool ClosureSpecializer::specialize(SILFunction *Caller) {
  DEBUG(llvm::dbgs() << "Optimizing callsites that take closure argument in "
                     << Caller->getName() << '\n');

  // Collect all of the PartialApplyInsts that are used as arguments to
  // ApplyInsts. Check the profitability of specializing the closure argument.
  llvm::SmallVector<CallSiteDescriptor, 8> CallSites;
  llvm::SmallPtrSet<ApplyInst *, 4> MultipleClosureAI;
  gatherCallSites(Caller, CallSites, MultipleClosureAI);

  bool Changed = false;
  for (auto &CSDesc : CallSites) {
    // Do not specialize apply insts that take in multiple closures. This pass
    // does not know how to do this yet.
    if (MultipleClosureAI.count(CSDesc.getApplyInst()))
      continue;

    CSDesc.specializeClosure();
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

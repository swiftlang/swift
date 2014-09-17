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
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILAnalysis/LoopAnalysis.h"
#include "swift/SILPasses/Passes.h"
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

  ClosureSpecCloner(SILFunction *Orig, PartialApplyInst *PAI,
                     unsigned ClosureIndex, StringRef ClonedName)
    : SuperTy(*initCloned(Orig, PAI, ClosureIndex, ClonedName)),
      Orig(Orig), ClosureIndex(ClosureIndex), PAI(PAI) {
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
  static SILFunction *initCloned(SILFunction *Orig, PartialApplyInst *PAI,
                                 unsigned ClosureIndex, StringRef ClonedName);
  SILFunction *Orig;
  unsigned ClosureIndex;
  PartialApplyInst *PAI;
};

} // end anonymous namespace

SILFunction *ClosureSpecCloner::initCloned(SILFunction *Orig,
                                            PartialApplyInst *PAI,
                                            unsigned ClosureIndex,
                                            StringRef ClonedName) {
  SmallVector<SILParameterInfo, 4> ClonedInterfaceArgTys;

  SILFunctionType *OrigFTI = Orig->getLoweredFunctionType();
  unsigned Index = 0;
  // Remove the parameter for the closure argument.
  for (auto &param : OrigFTI->getParameters()) {
    if (Index != ClosureIndex)
      ClonedInterfaceArgTys.push_back(param);
    ++Index;
  }

  // Append the variables captured in the closure.
  auto *FRI = cast<FunctionRefInst>(PAI->getCallee());
  SILFunction *ClosureWithCaptured = FRI->getReferencedFunction();
  SILFunctionType *ClosureWithCapturedFTI =
    ClosureWithCaptured->getLoweredFunctionType();
  auto ClosureType = PAI->getType().castTo<SILFunctionType>();
  // Add the parameters of ClosureWithCapturedFTI starting from
  // ClosureType->getParameters().size().
  for (auto I = ClosureWithCapturedFTI->getParameters().begin() +
                ClosureType->getParameters().size(),
       E = ClosureWithCapturedFTI->getParameters().end(); I != E; I++)
    ClonedInterfaceArgTys.push_back(*I);

  SILModule &M = Orig->getModule();
  auto ClonedTy =
    SILFunctionType::get(OrigFTI->getGenericSignature(),
                         OrigFTI->getExtInfo(),
                         OrigFTI->getCalleeConvention(),
                         ClonedInterfaceArgTys,
                         OrigFTI->getResult(),
                         M.getASTContext());

  auto Fn = SILFunction::create(M, Orig->getLinkage(), ClonedName, ClonedTy,
                                Orig->getContextGenericParams(),
                                Orig->getLocation(), Orig->isBare(),
                                Orig->isTransparent(), Orig->getInlineStrategy(),
                                Orig->getEffectsInfo(),
                                Orig, Orig->getDebugScope());
  Fn->setSemanticsAttr(Orig->getSemanticsAttr());
  return Fn;
}

/// \brief Populate the body of the cloned closure, modifying instructions as
/// necessary.
void ClosureSpecCloner::populateCloned() {
  SILFunction *Cloned = getCloned();
  SILModule &M = Cloned->getModule();

  // Create arguments for the entry block.
  SILBasicBlock *OrigEntryBB = Orig->begin();
  SILBasicBlock *ClonedEntryBB = new (M) SILBasicBlock(Cloned);

  // Remove the closure argument.
  SILArgument *ClosureArg = nullptr;
  for (size_t i = 0, e = OrigEntryBB->bbarg_size(); i != e; ++i) {
    SILArgument *Arg = OrigEntryBB->getBBArg(i);
    if (i == ClosureIndex) {
      ClosureArg = Arg;
      continue;
    }

    // Otherwise, create a new argument which copies the original argument
    SILValue MappedValue =
      new (M) SILArgument(Arg->getType(), ClonedEntryBB, Arg->getDecl());
    ValueMap.insert(std::make_pair(Arg, MappedValue));
  }

  auto *FRI = cast<FunctionRefInst>(PAI->getCallee());
  SILFunction *ClosureWithCaptured = FRI->getReferencedFunction();
  SILBasicBlock *ClosureEntryBB = ClosureWithCaptured->begin();
  auto ClosureType = PAI->getType().castTo<SILFunctionType>();
  // Add the parameters of ClosureWithCapturedFTI starting from
  // ClosureType->getParameters().size().
  SmallVector<SILValue, 16> NewPAIArgs;
  for (auto I = ClosureEntryBB->bbarg_begin() +
                ClosureType->getParameters().size(),
       E = ClosureEntryBB->bbarg_end(); I != E; I++) {
    SILValue MappedValue =
      new (M) SILArgument((*I)->getType(), ClonedEntryBB, (*I)->getDecl());
    NewPAIArgs.push_back(MappedValue);
    ValueMap.insert(std::make_pair(*I, MappedValue));
  }

  getBuilder().setInsertionPoint(ClonedEntryBB);
  // Clone FRI and PAI, and replace usage of the removed closure argument
  // with result of cloned PAI.
  SILValue FnVal = getBuilder().createFunctionRef(FRI->getLoc(),
                                                  ClosureWithCaptured);
  auto *NewPAI = getBuilder().createPartialApply(PAI->getLoc(), FnVal,
                                                 FnVal.getType(), {},
                                                 NewPAIArgs, PAI->getType());
  ValueMap.insert(std::make_pair(ClosureArg, SILValue(NewPAI, 0)));

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
  SILBuilder Builder(AD.AI);
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
  SILType ResultType = LoweredType.getFunctionInterfaceResultType();
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

  bool isProfitable(ArgDescriptor &AD);
  void gatherCallSites(SILFunction *Caller,
                       llvm::SmallVectorImpl<ArgDescriptor> &CallSites);
  bool specialize(SILFunction *Caller);
};

} // end anonymous namespace

bool ClosureSpecializer::isProfitable(ArgDescriptor &AD) {
  // First check if our callee is a function_ref. We currently only handle such
  // cases.
  auto *CalleeFRI = dyn_cast<FunctionRefInst>(AD.AI->getCallee());
  if (!CalleeFRI)
    return false;
  auto *Callee = CalleeFRI->getReferencedFunction();

  // Check the relative size of the callee and the closure.
  auto *ClosureFRI = cast<FunctionRefInst>(AD.PAI->getCallee());

  // We pass in nullptr for the caller since passing in the caller is only
  // interesting if we are actually going to inline. If we are deciding whether
  // or not to specialize a partial apply is a different issue.
  unsigned ClosureCount = getFunctionCost(ClosureFRI->getReferencedFunction(),
                                          nullptr, UINT_MAX);
  unsigned CalleeCount = getFunctionCost(Callee, nullptr, UINT_MAX);
  if (CalleeCount < 2 * ClosureCount) {
    DEBUG(llvm::dbgs() << "    Callsite is not profitable: " << ClosureCount
                       << ", " << CalleeCount << "\n");
    return false;
  }
  DEBUG(llvm::dbgs() << "    Callsite is profitable: " << ClosureCount
                     << ", " << CalleeCount << "\n");

  // Collect callsites to the closure.
  SmallVector<ApplyInst*, 8> CallSites;
  SILArgument *ClosureArg = Callee->begin()->getBBArg(AD.ClosureIndex);
  for (auto U : ClosureArg->getUses())
    if (auto CS = dyn_cast<ApplyInst>(U->getUser()))
      CallSites.push_back(CS);

  if (CallSites.empty())
    return false;

  // Check hotness of the callsite (AI) and callsites to closure inside callee.
  // For now, if closure is called inside a loop, we think it is profitable.
  SILLoopInfo *LI = LA->getLoopInfo(Callee);
  for (auto AI : CallSites) {
    if (LI->getLoopFor(AI->getParent()))
      return true;
  }

  DEBUG(llvm::dbgs() << "    Callsite is not profitable: closure not called "
                        "inside a loop\n");
  return false;
}

static void createName(SILFunction *Callee, SILFunction *Closure,
                       unsigned ClosureIndex,
                       llvm::SmallString<64> &Name) {
  llvm::raw_svector_ostream buffer(Name);
  buffer << "_TTS";
  buffer << Closure->getName() << "_as" << ClosureIndex
         << '_' << Callee->getName();
}

void
ClosureSpecializer::
gatherCallSites(SILFunction *Caller,
                llvm::SmallVectorImpl<ArgDescriptor> &CallSites) {
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
      // (i.e. its callee is a function_ref).
      //
      // TODO: We can probably handle other partial applies here.
      auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());
      if (!FRI)
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

      // If AI does not have a function_ref as its callee, we can not do
      // anything here... so continue...
      if (!isa<FunctionRefInst>(AI->getCallee()))
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

      // Ok. We now have the data we need to form an ArgSpecDescriptor and
      // determine if the ArgSpecDescriptor is profitable. If it is not
      // profitable, continue.
      ArgDescriptor AD(PAI, AI, PAIIndex.getValue());
      if (!isProfitable(AD))
        continue;

      // Now we know that AD is profitable to specialize. Add it to our call
      // site list.
      CallSites.push_back(AD);
    }
  }
}

bool ClosureSpecializer::specialize(SILFunction *Caller) {
  DEBUG(llvm::dbgs() << "Optimizing callsites that take closure argument in "
                     << Caller->getName() << '\n');

  // Collect all of the PartialApplyInsts that are used as arguments to
  // ApplyInsts. Check the profitability of specializing the closure argument.
  llvm::SmallVector<ArgDescriptor, 8> CallSites;
  gatherCallSites(Caller, CallSites);

  bool Changed = false;
  for (auto &AD : CallSites) {
    auto *ClosureFRI = cast<FunctionRefInst>(AD.PAI->getCallee());
    auto *CalleeFRI = cast<FunctionRefInst>(AD.AI->getCallee());
    auto *Callee = CalleeFRI->getReferencedFunction();

    llvm::SmallString<64> NewFName;
    createName(Callee, ClosureFRI->getReferencedFunction(), AD.ClosureIndex,
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

llvm::cl::opt<bool>
EnableClosureSpecialization("enable-closure-spec", llvm::cl::init(false));

namespace {
class SILClosureSpecializerTransform : public SILModuleTransform {
public:
  SILClosureSpecializerTransform() {}

  virtual void run() {
    if (!EnableClosureSpecialization)
      return;

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

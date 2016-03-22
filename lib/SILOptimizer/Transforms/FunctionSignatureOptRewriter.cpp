//===--- FunctionSignatureOptRewriter.cpp - Rewrite function callsites ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-function-signature-opt-rewriter"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/FunctionSignatureAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/FunctionSignatureOptUtils.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumCallSitesOptimized, "Total call sites optimized");

//===----------------------------------------------------------------------===//
//                                Main Routine
//===----------------------------------------------------------------------===//

static void
addCallerArgs(const ArgumentDescriptor &AD, SILBuilder &B, FullApplySite FAS,
              llvm::SmallVectorImpl<SILValue> &NewArgs) {
  if (AD.IsEntirelyDead)
    return;

  SILValue Arg = FAS.getArgument(AD.Index);
  if (!AD.Explode) {
    NewArgs.push_back(Arg);
    return;
  }

  AD.ProjTree.createTreeFromValue(B, FAS.getLoc(), Arg, NewArgs);
}

/// This function takes in OldF and a callsite of OldF and rewrites the
/// callsite to call the new function.
static void
rewriteApplyInstToCallNewFunction(FunctionSignatureInfo *FSI,
                                  SILFunction *NewF,
                                  const FullApplySite &FAS,
                                  llvm::DenseSet<SILInstruction *> &DeadInsts) {
  auto *AI = FAS.getInstruction();

  SILBuilderWithScope Builder(AI);

  FunctionRefInst *FRI = Builder.createFunctionRef(AI->getLoc(), NewF);

  // Create the args for the new apply, ignoring any dead arguments.
  llvm::SmallVector<SILValue, 8> NewArgs;
  ArrayRef<ArgumentDescriptor> ArgDescs = FSI->getArgDescList();
  for (auto &ArgDesc : ArgDescs) {
    addCallerArgs(ArgDesc, Builder, FAS, NewArgs);
  }

  // We are ignoring generic functions and functions with out parameters for
  // now.
  SILType LoweredType = NewF->getLoweredType();
  SILType ResultType = LoweredType.getFunctionInterfaceResultType();
  SILLocation Loc = AI->getLoc();
  SILValue ReturnValue = SILValue();

  // Create the new apply.
  if (ApplyInst *RealAI = dyn_cast<ApplyInst>(AI)) {
    auto *NewAI = Builder.createApply(Loc, FRI, LoweredType, ResultType,
                                      ArrayRef<Substitution>(), NewArgs,
                                      RealAI->isNonThrowing());
    // This is the return value.
    ReturnValue = SILValue(NewAI);
    // Replace all uses of the old apply with the new apply.
    AI->replaceAllUsesWith(NewAI);
  } else {
    auto *TAI = cast<TryApplyInst>(AI);
    Builder.createTryApply(Loc, FRI, LoweredType,
                           ArrayRef<Substitution>(), NewArgs,
                           TAI->getNormalBB(), TAI->getErrorBB());

    // This is the return value.
    ReturnValue = TAI->getNormalBB()->getBBArg(0);
    Builder.setInsertionPoint(TAI->getErrorBB(), TAI->getErrorBB()->begin());
    // If we have any arguments that were consumed but are now guaranteed,
    // insert a release_value in the error block.
    for (auto &ArgDesc : ArgDescs) {
      if (ArgDesc.CalleeRelease.empty())
        continue;
      Builder.createReleaseValue(Loc, FAS.getArgument(ArgDesc.Index));
    }
    // Also insert release_value in the normal block (done below).
    Builder.setInsertionPoint(TAI->getNormalBB(),
                              TAI->getNormalBB()->begin());
  }

  // Add releases for the converted @owned to @guaranteed parameter.
  addReleasesForConvertedOwnedParameter(Builder, Loc, FAS.getArguments(),
                                        ArgDescs);

  // If we have converted the return value from @owned to @guaranteed,
  // insert a retain_value at the callsite.
  addRetainsForConvertedDirectResults(Builder, Loc, ReturnValue, AI,
                                      FSI->getResultDescList());

  ++NumCallSitesOptimized;
  // Make sure we remove this apply in the end.
  DeadInsts.insert(AI);
}

/// Look up the optimized version of the function and rewrite the callsite.
static bool
optimizeCallSite(SILModule *Mod, RCIdentityFunctionInfo *RCIA,
                 FunctionSignatureInfo *FSI, AliasAnalysis *AA, 
                 const FullApplySite &CallSite,
                 llvm::DenseSet<SILInstruction *> &DeadInsts) {
  // Analyze function arguments. If there is no work to be done, exit early.
  if (!FSI->analyze())
    return false;

  // Get the name of the optimized function.
  auto NewFName = FSI->getOptimizedName();

  // Check whether we have already created the optimized function.
  SILFunction *NewFn = Mod->lookUpFunction(NewFName);
  if (!NewFn)
    return false;

  // Rewrite all apply insts calling F to call NewF. Update each call site as
  // appropriate given the form of function signature optimization performed.
  rewriteApplyInstToCallNewFunction(FSI, NewFn, CallSite, DeadInsts);
  return true;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//
namespace {

class FunctionSignatureOptRewriter : public SILFunctionTransform {
public:
  FunctionSignatureOptRewriter() {}

  void run() override {
    auto *RCIA = getAnalysis<RCIdentityAnalysis>();
    auto *FSA = getAnalysis<FunctionSignatureAnalysis>();
    auto *AA = PM->getAnalysis<AliasAnalysis>();

    SILFunction *F = getFunction();
    DEBUG(llvm::dbgs() << "*** FSO on function: " << F->getName() << " ***\n");

    // Don't optimize function that should not be optimized.
    if (!F->shouldOptimize())
      return;

    llvm::SmallVector<FullApplySite, 4> Sites;
    llvm::DenseSet<SILInstruction *> DeadInsts;

    bool Changed = false;
    // Scan the whole function and search Apply sites.
    for (auto &BB : *F) {
      for (auto &II : BB) {
        if (auto Apply = FullApplySite::isa(&II)) {
           SILValue Callee = Apply.getCallee();

           //  Strip ThinToThickFunctionInst.
           if (auto TTTF = dyn_cast<ThinToThickFunctionInst>(Callee)) {
             Callee = TTTF->getOperand();
           }
           // Find the target function.
           auto *FRI = dyn_cast<FunctionRefInst>(Callee);
           if (!FRI)
             continue;
           // This is a callsite we can optimize.
           Sites.push_back(Apply);
        }
      }
    }

    // These are the callsites to optimize.
    for (auto &Apply : Sites) {
      SILValue Fn = Apply.getCallee();

      //  Strip ThinToThickFunctionInst.
      if (auto TTTF = dyn_cast<ThinToThickFunctionInst>(Fn)) {
        Fn = TTTF->getOperand();
      }

      // Find the target function.
      auto *FRI = dyn_cast<FunctionRefInst>(Fn);
      if (!FRI)
        continue;

      SILFunction *Callee = FRI->getReferencedFunction();

      // Check the signature of Callee to make sure that it is a function that
      // we can specialize. These are conditions independent of the call graph.
      // If this function can't be specialized, that means we have not created
      // the function signature specialized version for it.
      if (!canSpecializeFunction(Callee))
        continue;

      // We may have a specialized version of this function, try to rewrite
      // the callsite to it.
      Changed |= optimizeCallSite(&Callee->getModule(), RCIA->get(Callee),
                                  FSA->get(Callee), AA, Apply, DeadInsts);
    }

    // Lastly, we have rewritten all the callsites, erase the old applys and
    // its callee.
    for (auto AI : DeadInsts) {
      recursivelyDeleteTriviallyDeadInstructions(AI, true,
                                                [](SILInstruction *) {});
    }

    // If we changed anything, invalidate the call graph.
    if (Changed) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Calls);
    }
  }

  StringRef getName() override {
    return "Function Signature Optimization Rewriter";
  }
};

} // end anonymous namespace

SILTransform *swift::createFunctionSignatureOptRewriter() {
  return new FunctionSignatureOptRewriter();
}

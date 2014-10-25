//===-- FunctionSignatureOpts.cpp - Optimizes function signatures ---------===//
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

#define DEBUG_TYPE "sil-function-signature-opts"
#include "swift/SILPasses/Passes.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILAnalysis/RCIdentityAnalysis.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILDebugScope.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include <type_traits>

using namespace swift;

STATISTIC(NumFunctionSignaturesOptimized, "Total func sig optimized");
STATISTIC(NumDeadArgsEliminated, "Total dead args eliminated");
STATISTIC(NumOwnedConvertedToGuaranteed, "Total owned args -> guaranteed args");
STATISTIC(NumCallSitesOptimized, "Total call sites optimized");

//===----------------------------------------------------------------------===//
//                             Argument Analysis
//===----------------------------------------------------------------------===//

namespace {
struct ArgDescriptor {
  SILArgument *Arg;
  SILParameterInfo ParameterInfo;
  bool IsDead;
  SILInstruction *CalleeRelease;

  ArgDescriptor() = default;
  ArgDescriptor(SILArgument *A)
    : Arg(A), ParameterInfo(A->getParameterInfo()), IsDead(A->use_empty()),
      CalleeRelease() {}

};
} // end anonymous namespace

/// See if we can find a release on Arg in the exiting BB of F without any side
/// effects in between the release and the return statement.
///
/// We only do this in the last basic block for now since if the ARC optimizer
/// had meaningfully reduced the lifetime of an object, we don't want to extend
/// the life if for instance we had moved releases over loops.
static SILInstruction *searchForCalleeRelease(SILFunction *F,
                                              SILArgument *Arg,
                                              RCIdentityAnalysis *RCIA) {
  auto ReturnBB = F->findReturnBB();
  if (ReturnBB == F->end())
    return nullptr;

  for (auto II = ReturnBB->rbegin(), IE = ReturnBB->rend(); II != IE; ++II) {
    // TODO: Use ARC infrastructure here.
    SILInstruction *Target = nullptr;
    if (isa<ReleaseValueInst>(*II) || isa<StrongReleaseInst>(*II))
      Target = &*II;

    if (!Target) {
      if (II->mayHaveSideEffects())
        return nullptr;
      continue;
    }

    SILValue Op = RCIA->getRCIdentityRoot(Target->getOperand(0));
    return Op == SILValue(Arg) ? Target : nullptr;
  }

  return nullptr;
}

/// This function goes through the arguments of F and sees if we have anything
/// to optimize in which case it returns true. If we have nothing to optimize,
/// it returns false.
static bool
analyzeArguments(SILFunction *F, SmallVectorImpl<ArgDescriptor> &ArgDescList,
                 RCIdentityAnalysis *RCIA) {
  // For now ignore functions with indirect results.
  if (F->getLoweredFunctionType()->hasIndirectResult())
    return false;

  bool ShouldOptimize = false;

  for (SILArgument *Arg : F->begin()->getBBArgs()) {
    ArgDescriptor A{Arg};

    if (A.IsDead) {
      ShouldOptimize = true;
      ++NumDeadArgsEliminated;
    }

    // See if we can find a ref count equivalent strong_release or release_value
    // at the end of this function if our argument is an @owned parameter.
    if (A.ParameterInfo.isConsumed()) {
      if ((A.CalleeRelease = searchForCalleeRelease(F, Arg, RCIA))) {
        ShouldOptimize = true;
        ++NumOwnedConvertedToGuaranteed;
      }
    }

    ArgDescList.push_back(A);
  }

  return ShouldOptimize;
}

//===----------------------------------------------------------------------===//
//                                  Mangling
//===----------------------------------------------------------------------===//

static bool isSpecializedFunction(SILFunction &F) {
  return F.getName().startswith("_TTOS");
}

static void createNewName(SILFunction &OldF, ArrayRef<ArgDescriptor> Args,
                          llvm::SmallString<64> &Name) {
  llvm::raw_svector_ostream buffer(Name);

  // OS for optimized signature.
  buffer << "_TTOS_";

  // For every argument, put in what we are going to do to that arg in the
  // signature. The key is:
  //
  // 'n'   => We did nothing to the argument.
  // 'd'   => The argument was dead and will be removed.
  // 'a2v' => Was a loadable address and we promoted it to a value.
  // 'o2g' => Was an @owned argument, but we changed it to be a guaranteed
  //          parameter.
  // 's'   => Was a loadable value that we exploded into multiple arguments.
  //
  // Currently we only use 'n' and 'd' since we do not perform the other
  // optimizations.
  //
  // *NOTE* The guaranteed optimization requires knowledge to be taught to the
  // ARC optimizer among other passes in order to guarantee safety. That or you
  // need to insert a fix_lifetime call to make sure we do not eliminate the
  // retain, release surrounding the call site in the caller.
  //
  // Additionally we use a packed signature since at this point we don't need
  // any '_'. The fact that we can run this optimization multiple times makes me
  // worried about long symbol names so I am trying to keep the symbol names as
  // short as possible especially in light of this being applied to specialized
  // functions.
  for (const ArgDescriptor &Arg : Args) {
    // If this arg is dead, add 'd' to the packed signature and continue.
    if (Arg.IsDead) {
      buffer << 'd';
      continue;
    }

    // If we have an @owned argument and found a callee release for it, convert
    // the argument to guaranteed.
    if (Arg.CalleeRelease) {
      buffer << "o2g";
      continue;
    }

    // Otherwise we are doing nothing so add 'n' to the packed signature.
    buffer << 'n';
  }

  buffer << '_' << OldF.getName();
}

//===----------------------------------------------------------------------===//
//                                Main Routine
//===----------------------------------------------------------------------===//

/// This function takes in OldF and all callsites of OldF and rewrites the
/// callsites to call the new function.
static void
rewriteApplyInstToCallNewFunction(SILFunction *OldF, SILFunction *NewF,
                                  ArrayRef<ArgDescriptor> ArgDescs,
                                  CallGraphNode::CallerCallSiteList CallSites) {
  for (ApplyInst *AI : CallSites) {
    SILBuilder Builder(AI);

    FunctionRefInst *FRI = Builder.createFunctionRef(AI->getLoc(), NewF);

    // Create the args for the new apply, ignoring any dead arguments.
    llvm::SmallVector<SILValue, 8> NewArgs;
    for (unsigned i = 0, e = ArgDescs.size(); i != e; ++i) {
      if (ArgDescs[i].IsDead)
        continue;
      NewArgs.push_back(AI->getArgument(i));
    }

    // We are ignoring generic functions and functions with out parameters for
    // now.
    SILType LoweredType = NewF->getLoweredType();
    SILType ResultType = LoweredType.getFunctionInterfaceResultType();

    // Create the new apply.
    ApplyInst *NewAI = Builder.createApply(AI->getLoc(), FRI, LoweredType,
                                           ResultType, ArrayRef<Substitution>(),
                                           NewArgs, NewF->isTransparent());

    // Replace all uses of the old apply with the new apply.
    AI->replaceAllUsesWith(NewAI);

    // If we have any arguments that were consumed but are now guaranteed,
    // insert a fix lifetime instruction and a release_value.
    for (unsigned i = 0, e = ArgDescs.size(); i != e; ++i) {
      if (ArgDescs[i].CalleeRelease) {
        Builder.createFixLifetime(AI->getLoc(), AI->getArgument(i));
        Builder.createReleaseValue(AI->getLoc(), AI->getArgument(i));
      }
    }

    // Erase the old apply.
    AI->eraseFromParent();

    ++NumCallSitesOptimized;
  }
}

static void
computeOptimizedInterfaceParams(CanSILFunctionType OldFTy,
                                ArrayRef<ArgDescriptor> Args,
                                SmallVectorImpl<unsigned> &DeadArgs,
                                SmallVectorImpl<SILParameterInfo> &OutArray) {
  ArrayRef<SILParameterInfo> ParameterInfo = OldFTy->getParameters();
  for (unsigned i = 0, e = ParameterInfo.size(); i != e; ++i) {
    // If we have a dead argument, skip it.
    if (Args[i].IsDead) {
      DeadArgs.push_back(i);
      continue;
    }

    // If we found a release in the callee in the last BB on an @owned
    // parameter, change the parameter to @guaranteed and continue...
    if (Args[i].CalleeRelease) {
      const SILParameterInfo &OldInfo = ParameterInfo[i];
      assert(OldInfo.getConvention() == ParameterConvention::Direct_Owned &&
             "Can only transform @owned => @guaranteed in this code path");
      SILParameterInfo NewInfo(OldInfo.getType(),
                               ParameterConvention::Direct_Guaranteed);
      OutArray.push_back(NewInfo);
      continue;
    }

    // Otherwise just propagate through the parameter info.
    OutArray.push_back(ParameterInfo[i]);
  }
}

static SILFunction *
createEmptyFunctionWithOptimizedSig(SILFunction *OldF,
                                    llvm::SmallString<64> &NewFName,
                                    ArrayRef<ArgDescriptor> Args,
                                    SmallVectorImpl<unsigned> &DeadArgs) {
  SILModule &M = OldF->getModule();

  // Create the new optimized function type.
  CanSILFunctionType OldFTy = OldF->getLoweredFunctionType();
  const ASTContext &Ctx = M.getASTContext();

  // The only way that we modify the arity of function parameters is here for
  // dead arguments. Doing anything else is unsafe since by definition non-dead
  // arguments will have SSA uses in the function. We would need to be smarter
  // in our moving to handle such cases.
  SmallVector<SILParameterInfo, 4> InterfaceParams;
  computeOptimizedInterfaceParams(OldFTy, Args, DeadArgs, InterfaceParams);

  SILResultInfo InterfaceResult = OldFTy->getResult();
  CanSILFunctionType NewFTy = SILFunctionType::get(
      OldFTy->getGenericSignature(), OldFTy->getExtInfo(),
      OldFTy->getCalleeConvention(), InterfaceParams, InterfaceResult, Ctx);

  // Create the new function.
  auto *NewDebugScope = new (M) SILDebugScope(*OldF->getDebugScope());
  SILFunction *NewF = SILFunction::create(
      M, OldF->getLinkage(), NewFName, NewFTy, nullptr, OldF->getLocation(),
      OldF->isBare(), OldF->isTransparent(), OldF->isFragile(),
      OldF->getInlineStrategy(), OldF->getEffectsInfo(), 0,
      NewDebugScope, OldF->getDeclContext());
  NewF->setSemanticsAttr(OldF->getSemanticsAttr());
  NewDebugScope->SILFn = NewF;

  return NewF;
}

static void createThunkBody(SILBasicBlock *BB, SILFunction *NewF,
                            ArrayRef<ArgDescriptor> Args,
                            ArrayRef<unsigned> DeadArgs) {
  // TODO: What is the proper location to use here?
  SILLocation Loc = BB->getParent()->getLocation();
  SILBuilder Builder(BB);

  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  // Create the args for the thunk's apply, ignoring any dead arguments.
  llvm::SmallVector<SILValue, 8> ThunkArgs;
  llvm::SmallVector<SILValue, 4> OwnedToGuaranteedArgs;

  unsigned deadarg_i = 0, deadarg_e = DeadArgs.size();
  for (unsigned i = 0, e = BB->getNumBBArg(); i < e; ++i) {
    if (deadarg_i < deadarg_e && DeadArgs[deadarg_i] == i) {
      deadarg_i++;
      continue;
    }

    if (Args[i].CalleeRelease) {
      OwnedToGuaranteedArgs.push_back(BB->getBBArg(i));
    }

    ThunkArgs.push_back(BB->getBBArg(i));
  }

  // We are ignoring generic functions and functions with out parameters for
  // now.
  SILType LoweredType = NewF->getLoweredType();
  SILType ResultType = LoweredType.getFunctionInterfaceResultType();
  SILValue ReturnValue = Builder.createApply(Loc, FRI, LoweredType, ResultType,
                                             ArrayRef<Substitution>(),
                                             ThunkArgs, NewF->isTransparent());

  // If we have any arguments that were consumed but are now guaranteed,
  // insert a fix lifetime instruction and a release_value.
  for (SILValue V : OwnedToGuaranteedArgs) {
    Builder.createFixLifetime(Loc, V);
    Builder.createReleaseValue(Loc, V);
  }

  Builder.createReturn(Loc, ReturnValue);
}

static SILFunction *
moveFunctionBodyToNewFunctionWithName(SILFunction *F,
                                      llvm::SmallString<64> &NewFName,
                                      ArrayRef<ArgDescriptor> Args) {
  // A list of the indices of our dead args.
  llvm::SmallVector<unsigned, 4> DeadArgs;

  // First we create an empty function (i.e. no BB) whose function signature has
  // had its arity modified.
  //
  // We only do this to remove dead arguments. All other function signature
  // optimization is done later by modifying the function signature elements
  // themselves.
  SILFunction *NewF = createEmptyFunctionWithOptimizedSig(F, NewFName, Args,
                                                          DeadArgs);

  // Then we transfer the body of F to NewF. At this point, the arguments of the
  // first BB will not match.
  NewF->spliceBody(F);

  // Create a new BB called ThunkBB to use to create F's new body and use NewFs
  // first BB's arguments as a template for the BB's args.
  SILBasicBlock *ThunkBody = F->createBasicBlock();
  SILBasicBlock *NewFEntryBB = &*NewF->begin();
  for (auto *A : NewFEntryBB->getBBArgs()) {
    ThunkBody->createArgument(A->getType(), A->getDecl());
  }

  // Then erase the dead arguments from NewF using DeadArgs. We go backwards so
  // we remove arg elements by decreasing index so we don't invalidate our
  // indices.
  for (unsigned i : reversed(DeadArgs)) {
    NewFEntryBB->eraseArgument(i);
  }

  // Intrusively optimize the function signature of NewF.

  // Then we create a new body for ThunkBody that calls NewF.
  createThunkBody(ThunkBody, NewF, Args, DeadArgs);

  return NewF;
}

/// This function takes in a SILFunction F and its callsites in the current
/// module and produces a new SILFunction that has the body of F but with
/// optimized function arguments. F is changed to be a thunk that calls NewF to
/// reduce code duplication in cases where we missed a callsite to F. The
/// function returns true if we were successful in creating the new function and
/// returns false otherwise.
static bool
optimizeFunctionSignature(SILFunction *F,
                          CallGraphNode::CallerCallSiteList CallSites,
                          RCIdentityAnalysis *RCIA) {
  DEBUG(llvm::dbgs() << "Optimizing Function Signature of " << F->getName()
                     << "\n");

  // An array containing our ArgDescriptor objects that contain information
  // from our analysis.
  llvm::SmallVector<ArgDescriptor, 8> Arguments;

  // Analyze function arguments. If there is no work to be done, exit early.
  if (!analyzeArguments(F, Arguments, RCIA)) {
    DEBUG(llvm::dbgs() << "    Has no optimizable arguments... "
                          "bailing...\n");
    return false;
  }

  DEBUG(llvm::dbgs() << "    Has optimizable arguments... Performing "
                        "optimizations...\n");

  ++NumFunctionSignaturesOptimized;

  DEBUG(for (auto *AI : CallSites) {
    llvm::dbgs()  << "        CALLSITE: " << *AI;
  });

  llvm::SmallString<64> NewFName;
  createNewName(*F, Arguments, NewFName);

  // If we already have a specialized version of this function, do not
  // respecialize. For now just bail.
  //
  // TODO: Improve this. I do not expect this to occur often so I am fine for
  // now avoiding this issue. The main things I am worried about are assumptions
  // that we make about the callee and caller being violated. That said, this is
  // just a fear.
  if (F->getModule().lookUpFunction(NewFName))
    return false;

  // Otherwise, move F over to NewF.
  SILFunction *NewF = moveFunctionBodyToNewFunctionWithName(F, NewFName,
                                                            Arguments);

  // And remove all Callee releases that we found and made redundent via owned
  // to guaranteed conversion.
  //
  // TODO: If more stuff needs to be placed here, refactor into its own method.
  for (auto &A : Arguments) {
    if (A.CalleeRelease) {
      A.CalleeRelease->eraseFromParent();
    }
  }

  // Rewrite all apply insts calling F to call NewF. Update each call site as
  // appropriate given the form of function signature optimization performed.
  rewriteApplyInstToCallNewFunction(F, NewF, Arguments, CallSites);

  return true;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static bool isSpecializableCC(AbstractCC CC) {
  switch (CC) {
  case AbstractCC::Method:
  case AbstractCC::Freestanding:
  case AbstractCC::C:
    return true;
  case AbstractCC::WitnessMethod:
  case AbstractCC::ObjCMethod:
    return false;
  }
}

static bool canSpecializeFunction(SILFunction &F) {
  // Do not specialize the signature of SILFunctions that are external
  // declarations since there is no body to optimize.
  if (F.isExternalDeclaration())
    return false;

  // Do not specialize functions that are available externally. If an external
  // function was able to be specialized, it would have been specialized in its
  // own module. We will inline the original function as a thunk. The thunk will
  // call the specialized function.
  if (F.isAvailableExternally())
    return false;

  // Do not specialize functions that we already specialized.
  if (isSpecializedFunction(F))
    return false;

  // Do not specialize the signature of transparent functions or always inline
  // functions, we will just inline them and specialize each one of the
  // individual functions that these sorts of functions are inlined into.
  if (F.isTransparent() || F.getInlineStrategy() == Inline_t::AlwaysInline)
    return false;

  // For now ignore generic functions to keep things simple...
  if (F.getLoweredFunctionType()->isPolymorphic())
    return false;

  // Make sure F has a linkage that we can optimize.
  if (!isSpecializableCC(F.getAbstractCC()))
    return false;

  return true;
}

namespace {

class FunctionSignatureOpts : public SILModuleTransform {
public:
  FunctionSignatureOpts() {}

  void run() {
    SILModule *M = getModule();
    auto *CGA = getAnalysis<CallGraphAnalysis>();
    auto *RCIA = getAnalysis<RCIdentityAnalysis>();

    DEBUG(llvm::dbgs() << "**** Optimizing Function Signatures ****\n\n");

    CallGraph &CG = CGA->getCallGraph();

    // Construct a map from Callee -> Call Site Set.

    // Process each function in the callgraph that we are able to optimize.
    //
    // TODO: Determine if it is profitable to always perform this optimization
    // even if a function is not called locally. As far as we can tell. Down the
    // line more calls may be exposed and the inliner might be able to handle
    // those calls.
    bool Changed = false;

    for (auto &F : *M) {
      // Check the signature of F to make sure that it is a function that we can
      // specialize. These are conditions independent of the call graph.
      if (!canSpecializeFunction(F))
        continue;

      // Then try and grab F's call graph node.
      CallGraphNode *FNode = CG.getCallGraphNode(&F);

      // If we don't have any call graph information for F, skip F.
      if (!FNode)
        continue;

      // Now that we have our call graph, grab the CallSites of F.
      auto CallSites = FNode->getKnownCallerCallSites();

      // If this function is not called anywhere, for now don't do anything.
      //
      // TODO: If it is public, it may still make sense to specialize since if
      // we link in the public function in another module, we may be able to
      // inline it and access the specialized version.
      if (CallSites.empty())
        continue;

      // Otherwise, try to optimize the function signature of F.
      Changed |= optimizeFunctionSignature(&F, CallSites, RCIA);
    }

    // If we changed anything, invalidate the call graph.
    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
  }

  StringRef getName() override { return "Function Signature Optimization"; }
};
} // end anonymous namespace

SILTransform *swift::createFunctionSignatureOpts() {
  return new FunctionSignatureOpts();
}

//===--- FunctionSignatureOpts.cpp - Optimizes function signatures --------===//
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

#define DEBUG_TYPE "sil-function-signature-opts"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/FunctionSignatureAnalysis.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/FunctionOrder.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/Basic/BlotMapVector.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/Mangle.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILDebugScope.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumFunctionSignaturesOptimized, "Total func sig optimized");
STATISTIC(NumDeadArgsEliminated, "Total dead args eliminated");
STATISTIC(NumOwnedConvertedToGuaranteed, "Total owned args -> guaranteed args");
STATISTIC(NumOwnedConvertedToNotOwnedResult, "Total owned result -> not owned result");
STATISTIC(NumCallSitesOptimized, "Total call sites optimized");
STATISTIC(NumSROAArguments, "Total SROA arguments optimized");

typedef SmallVector<FullApplySite, 8> ApplyList;
//===----------------------------------------------------------------------===//
//                     Argument and Result Optimzer
//===----------------------------------------------------------------------===//

static void
computeOptimizedInterfaceParams(ArgumentDescriptor &AD,
                                SmallVectorImpl<SILParameterInfo> &Out) {
  DEBUG(llvm::dbgs() << "        Computing Interface Params\n");
  // If we have a dead argument, bail.
  if (AD.IsEntirelyDead) {
    DEBUG(llvm::dbgs() << "            Dead!\n");
    ++NumDeadArgsEliminated;
    return;
  }

  // If we have an indirect result, bail.
  if (AD.IsIndirectResult) {
    DEBUG(llvm::dbgs() << "            Indirect result.\n");
    return;
  }

  auto ParameterInfo = AD.Arg->getKnownParameterInfo();

  // If this argument is live, but we cannot optimize it.
  if (!AD.canOptimizeLiveArg()) {
    DEBUG(llvm::dbgs() << "            Cannot optimize live arg!\n");
    Out.push_back(ParameterInfo);
    return;
  }

  // If we cannot explode this value, handle callee release and return.
  if (!AD.Explode) {
    DEBUG(llvm::dbgs() << "            ProjTree cannot explode arg.\n");
    // If we found releases in the callee in the last BB on an @owned
    // parameter, change the parameter to @guaranteed and continue...
    if (!AD.CalleeRelease.empty()) {
      DEBUG(llvm::dbgs() << "            Has callee release.\n");
      assert(ParameterInfo.getConvention() ==
                 ParameterConvention::Direct_Owned &&
             "Can only transform @owned => @guaranteed in this code path");
      SILParameterInfo NewInfo(ParameterInfo.getType(),
                               ParameterConvention::Direct_Guaranteed);
      Out.push_back(NewInfo);
      ++NumOwnedConvertedToGuaranteed;
      return;
    }

    DEBUG(llvm::dbgs() << "            Does not have callee release.\n");
    // Otherwise just propagate through the parameter info.
    Out.push_back(ParameterInfo);
    return;
  }

  ++NumSROAArguments;
  DEBUG(llvm::dbgs() << "            ProjTree can explode arg.\n");
  // Ok, we need to use the projection tree. Iterate over the leafs of the
  // tree...
  llvm::SmallVector<const ProjectionTreeNode*, 8> LeafNodes;
  AD.ProjTree.getLeafNodes(LeafNodes);
  DEBUG(llvm::dbgs() << "            Leafs:\n");
  for (auto Node : LeafNodes) {
    // Node type.
    SILType Ty = Node->getType();
    DEBUG(llvm::dbgs() << "                " << Ty << "\n");
    // If Ty is trivial, just pass it directly.
    if (Ty.isTrivial(AD.Arg->getModule())) {
      SILParameterInfo NewInfo(Ty.getSwiftRValueType(),
                               ParameterConvention::Direct_Unowned);
      Out.push_back(NewInfo);
      continue;
    }

    // If Ty is guaranteed, just pass it through.
    ParameterConvention Conv = ParameterInfo.getConvention();
    if (Conv == ParameterConvention::Direct_Guaranteed) {
      assert(AD.CalleeRelease.empty() && "Guaranteed parameter should not have a "
                                      "callee release.");
      SILParameterInfo NewInfo(Ty.getSwiftRValueType(),
                               ParameterConvention::Direct_Guaranteed);
      Out.push_back(NewInfo);
      continue;
    }

    // If Ty is not trivial and we found a callee release, pass it as
    // guaranteed.
    assert(ParameterInfo.getConvention() == ParameterConvention::Direct_Owned &&
           "Can only transform @owned => @guaranteed in this code path");
    if (!AD.CalleeRelease.empty()) {
      SILParameterInfo NewInfo(Ty.getSwiftRValueType(),
                               ParameterConvention::Direct_Guaranteed);
      Out.push_back(NewInfo);
      ++NumOwnedConvertedToGuaranteed;
      continue;
    }

    // Otherwise, just add Ty as an @owned parameter.
    SILParameterInfo NewInfo(Ty.getSwiftRValueType(),
                             ParameterConvention::Direct_Owned);
    Out.push_back(NewInfo);
  }
}

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

static void
addThunkArgs(const ArgumentDescriptor &AD, SILBuilder &Builder, SILBasicBlock *BB,
             llvm::SmallVectorImpl<SILValue> &NewArgs) {
  if (AD.IsEntirelyDead)
    return;

  if (!AD.Explode) {
    NewArgs.push_back(BB->getBBArg(AD.Index));
    return;
  }

  AD.ProjTree.createTreeFromValue(Builder, BB->getParent()->getLocation(),
                                  BB->getBBArg(AD.Index), NewArgs);
}

static unsigned
updateOptimizedBBArgs(ArgumentDescriptor &AD, SILBuilder &Builder,
                      SILBasicBlock *BB, unsigned ArgOffset) {
  // If this argument is completely dead, delete this argument and return
  // ArgOffset.
  if (AD.IsEntirelyDead) {
    // If we have a callee release and we are dead, set the callee release's
    // operand to undef. We do not need it to have the argument anymore, but we
    // do need the instruction to be non-null.
    //
    // TODO: This should not be necessary.
    for (auto &X : AD.CalleeRelease) {
      SILType CalleeReleaseTy = X->getOperand(0)->getType();
      X->setOperand(
          0, SILUndef::get(CalleeReleaseTy, Builder.getModule()));
    }

    // We should be able to recursively delete all of the remaining
    // instructions.
    SILArgument *Arg = BB->getBBArg(ArgOffset);
    eraseUsesOfValue(Arg);
    BB->eraseBBArg(ArgOffset);
    return ArgOffset;
  }

  // If this argument is not dead and we did not perform SROA, increment the
  // offset and return.
  if (!AD.Explode) {
    return ArgOffset + 1;
  }

  // Create values for the leaf types.
  llvm::SmallVector<SILValue, 8> LeafValues;

  // Create a reference to the old arg offset and increment arg offset so we can
  // create the new arguments.
  unsigned OldArgOffset = ArgOffset++;

  // We do this in the same order as leaf types since ProjTree expects that the
  // order of leaf values matches the order of leaf types.
  {
    llvm::SmallVector<const ProjectionTreeNode*, 8> LeafNodes;
    AD.ProjTree.getLeafNodes(LeafNodes);
    for (auto Node : LeafNodes) {
      LeafValues.push_back(BB->insertBBArg(
          ArgOffset++, Node->getType(), BB->getBBArg(OldArgOffset)->getDecl()));
    }
  }

  // Then go through the projection tree constructing aggregates and replacing
  // uses.
  //
  // TODO: What is the right location to use here?
  AD.ProjTree.replaceValueUsesWithLeafUses(Builder, BB->getParent()->getLocation(),
                                        LeafValues);

  // We ignored debugvalue uses when we constructed the new arguments, in order
  // to preserve as much information as possible, we construct a new value for
  // OrigArg from the leaf values and use that in place of the OrigArg.
  SILValue NewOrigArgValue = AD.ProjTree.computeExplodedArgumentValue(Builder,
                                           BB->getParent()->getLocation(),
                                           LeafValues);

  // Replace all uses of the original arg with the new value.
  SILArgument *OrigArg = BB->getBBArg(OldArgOffset);
  OrigArg->replaceAllUsesWith(NewOrigArgValue);

  // Now erase the old argument since it does not have any uses. We also
  // decrement ArgOffset since we have one less argument now.
  BB->eraseBBArg(OldArgOffset);
  --ArgOffset;

  return ArgOffset;
}

//===----------------------------------------------------------------------===//
//                             Signature Optimizer
//===----------------------------------------------------------------------===//

namespace {

/// A class that contains all analysis information we gather about our
/// function. Also provides utility methods for creating the new empty function.
class SignatureOptimizer {
  FunctionSignatureFunctionInfo *FSFI;

public:
  SignatureOptimizer() = delete;
  SignatureOptimizer(const SignatureOptimizer &) = delete;
  SignatureOptimizer(SignatureOptimizer &&) = delete;

  SignatureOptimizer(FunctionSignatureFunctionInfo *FSFI) : FSFI(FSFI) {}

  ArrayRef<ArgumentDescriptor> getArgDescList() const {
    return FSFI->getArgDescList();
  }

  MutableArrayRef<ArgumentDescriptor> getArgDescList() {
    return FSFI->getArgDescList();
  }

  MutableArrayRef<ResultDescriptor> getResultDescList() {
    return FSFI->getResultDescList();
  }

  /// Create a new empty function with the optimized signature found by this
  /// analysis.
  ///
  /// *NOTE* This occurs in the same module as F.
  SILFunction *createEmptyFunctionWithOptimizedSig(const std::string &Name);

private:
  /// Compute the CanSILFunctionType for the optimized function.
  CanSILFunctionType createOptimizedSILFunctionType();
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                         Creating the New Function
//===----------------------------------------------------------------------===//

CanSILFunctionType SignatureOptimizer::createOptimizedSILFunctionType() {
  auto *F = FSFI->getAnalyzedFunction();

  const ASTContext &Ctx = F->getModule().getASTContext();
  CanSILFunctionType FTy = F->getLoweredFunctionType();

  // The only way that we modify the arity of function parameters is here for
  // dead arguments. Doing anything else is unsafe since by definition non-dead
  // arguments will have SSA uses in the function. We would need to be smarter
  // in our moving to handle such cases.
  llvm::SmallVector<SILParameterInfo, 8> InterfaceParams;
  for (auto &ArgDesc : getArgDescList()) {
    computeOptimizedInterfaceParams(ArgDesc, InterfaceParams);
  }

  // ResultDescs only covers the direct results; we currently can't ever
  // change an indirect result.  Piece the modified direct result information
  // back into the all-results list.
  llvm::SmallVector<SILResultInfo, 8> InterfaceResults;
  auto ResultDescs = getResultDescList();
  for (SILResultInfo InterfaceResult : FTy->getAllResults()) {
    if (InterfaceResult.isDirect()) {
      auto &RV = ResultDescs[0];
      ResultDescs = ResultDescs.slice(0);
      if (!RV.CalleeRetain.empty()) {
        InterfaceResults.push_back(SILResultInfo(InterfaceResult.getType(),
                                                 ResultConvention::Unowned));
        ++NumOwnedConvertedToNotOwnedResult;
        continue;
      }
    }

    InterfaceResults.push_back(InterfaceResult);
  }

  auto InterfaceErrorResult = FTy->getOptionalErrorResult();
  auto ExtInfo = FTy->getExtInfo();

  // Don't use a method representation if we modified self.
  if (FSFI->shouldModifySelfArgument())
    ExtInfo = ExtInfo.withRepresentation(SILFunctionTypeRepresentation::Thin);

  return SILFunctionType::get(FTy->getGenericSignature(), ExtInfo,
                              FTy->getCalleeConvention(), InterfaceParams,
                              InterfaceResults, InterfaceErrorResult, Ctx);
}

SILFunction *SignatureOptimizer::createEmptyFunctionWithOptimizedSig(
    const std::string &NewFName) {

  auto *F = FSFI->getAnalyzedFunction();
  SILModule &M = F->getModule();

  // Create the new optimized function type.
  CanSILFunctionType NewFTy = createOptimizedSILFunctionType();

  // Create the new function.
  auto *NewF = M.getOrCreateFunction(
      F->getLinkage(), NewFName, NewFTy, nullptr, F->getLocation(), F->isBare(),
      F->isTransparent(), F->isFragile(), F->isThunk(), F->getClassVisibility(),
      F->getInlineStrategy(), F->getEffectsKind(), 0, F->getDebugScope(),
      F->getDeclContext());

  NewF->setDeclCtx(F->getDeclContext());

  // Array semantic clients rely on the signature being as in the original
  // version.
  for (auto &Attr : F->getSemanticsAttrs())
    if (!StringRef(Attr).startswith("array."))
      NewF->addSemanticsAttr(Attr);

  return NewF;
}

static void 
addReleasesForConvertedOwnedParameter(SILBuilder &Builder,
                                      SILLocation Loc,
                                      ArrayRef<SILArgument*> Parameters,
                                      ArrayRef<ArgumentDescriptor> &ArgDescs) {
  // If we have any arguments that were consumed but are now guaranteed,
  // insert a release_value.
  for (auto &ArgDesc : ArgDescs) {
    if (ArgDesc.CalleeRelease.empty())
      continue;
    Builder.createReleaseValue(Loc, Parameters[ArgDesc.Index]);
  }
}

static void 
addReleasesForConvertedOwnedParameter(SILBuilder &Builder,
                                      SILLocation Loc,
                                      OperandValueArrayRef Parameters,
                                      ArrayRef<ArgumentDescriptor> &ArgDescs) {
  // If we have any arguments that were consumed but are now guaranteed,
  // insert a release_value.
  for (auto &ArgDesc : ArgDescs) {
    if (ArgDesc.CalleeRelease.empty())
      continue;
    Builder.createReleaseValue(Loc, Parameters[ArgDesc.Index]);
  }
}

static void
addRetainsForConvertedDirectResults(SILBuilder &Builder,
                                    SILLocation Loc,
                                    SILValue ReturnValue,
                                    SILInstruction *AI,
                                    ArrayRef<ResultDescriptor> DirectResults) {
  for (auto I : indices(DirectResults)) {
    auto &RV = DirectResults[I];
    if (RV.CalleeRetain.empty()) continue;

    bool IsSelfRecursionEpilogueRetain = false;
    for (auto &X : RV.CalleeRetain) {
      IsSelfRecursionEpilogueRetain |= (AI == X);
    }

    // We do not create a retain if this ApplyInst is a self-recursion.
    if (IsSelfRecursionEpilogueRetain)
      continue;

    // Extract the return value if necessary.
    SILValue SpecificResultValue = ReturnValue;
    if (DirectResults.size() != 1)
      SpecificResultValue = Builder.createTupleExtract(Loc, ReturnValue, I);

    Builder.createRetainValue(Loc, SpecificResultValue);
  }
}

//===----------------------------------------------------------------------===//
//                                Main Routine
//===----------------------------------------------------------------------===//

/// This function takes in OldF and all callsites of OldF and rewrites the
/// callsites to call the new function.
static void rewriteApplyInstToCallNewFunction(SignatureOptimizer &Optimizer,
                                              SILFunction *NewF,
                                              const ApplyList &CallSites) {
  llvm::DenseSet<SILInstruction *> ApplysToRemove;
  for (auto FAS : CallSites) {
    auto *AI = FAS.getInstruction();

    SILBuilderWithScope Builder(AI);

    FunctionRefInst *FRI = Builder.createFunctionRef(AI->getLoc(), NewF);

    // Create the args for the new apply, ignoring any dead arguments.
    llvm::SmallVector<SILValue, 8> NewArgs;
    ArrayRef<ArgumentDescriptor> ArgDescs = Optimizer.getArgDescList();
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
                                        Optimizer.getResultDescList());

    // Make sure we remove this apply in the end.
    ApplysToRemove.insert(AI);
    ++NumCallSitesOptimized;
  }

  // Lastly, we have rewritten all the callsites, erase the old applys and
  // its callee.
  for (auto *AI : ApplysToRemove) {
    recursivelyDeleteTriviallyDeadInstructions(AI, true,
                                               [](SILInstruction *) {});
  }
}

static void createThunkBody(SILBasicBlock *BB, SILFunction *NewF,
                            SignatureOptimizer &Optimizer) {
  // TODO: What is the proper location to use here?
  SILLocation Loc = BB->getParent()->getLocation();
  SILBuilder Builder(BB);
  Builder.setCurrentDebugScope(BB->getParent()->getDebugScope());

  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  // Create the args for the thunk's apply, ignoring any dead arguments.
  llvm::SmallVector<SILValue, 8> ThunkArgs;
  ArrayRef<ArgumentDescriptor> ArgDescs = Optimizer.getArgDescList();
  for (auto &ArgDesc : ArgDescs) {
    addThunkArgs(ArgDesc, Builder, BB, ThunkArgs);
  }

  // We are ignoring generic functions and functions with out parameters for
  // now.
  SILType LoweredType = NewF->getLoweredType();
  SILType ResultType = LoweredType.getFunctionInterfaceResultType();
  SILValue ReturnValue;
  auto FunctionTy = LoweredType.castTo<SILFunctionType>();
  if (FunctionTy->hasErrorResult()) {
    // We need a try_apply to call a function with an error result.
    SILFunction *Thunk = BB->getParent();
    SILBasicBlock *NormalBlock = Thunk->createBasicBlock();
    ReturnValue = NormalBlock->createBBArg(ResultType, 0);
    SILBasicBlock *ErrorBlock = Thunk->createBasicBlock();
    SILType ErrorProtocol =
        SILType::getPrimitiveObjectType(FunctionTy->getErrorResult().getType());
    auto *ErrorArg = ErrorBlock->createBBArg(ErrorProtocol, 0);
    Builder.createTryApply(Loc, FRI, LoweredType, ArrayRef<Substitution>(),
                           ThunkArgs, NormalBlock, ErrorBlock);

    // If we have any arguments that were consumed but are now guaranteed,
    // insert a release_value in the error block.
    Builder.setInsertionPoint(ErrorBlock);
    for (auto &ArgDesc : ArgDescs) {
      if (ArgDesc.CalleeRelease.empty())
        continue;
      Builder.createReleaseValue(Loc, BB->getBBArg(ArgDesc.Index));
    }
    Builder.createThrow(Loc, ErrorArg);

    // Also insert release_value in the normal block (done below).
    Builder.setInsertionPoint(NormalBlock);
  } else {
    ReturnValue =
        Builder.createApply(Loc, FRI, LoweredType, ResultType,
                            ArrayRef<Substitution>(), ThunkArgs, false);
  }

  // Add releases for the converted @owned to @guaranteed parameter.
  addReleasesForConvertedOwnedParameter(Builder, Loc, BB->getBBArgs(),
                                        ArgDescs);

  // Handle @owned to @unowned return value conversion.
  addRetainsForConvertedDirectResults(Builder, Loc, ReturnValue, nullptr,
                                      Optimizer.getResultDescList());

  // Function that are marked as @NoReturn must be followed by an 'unreachable'
  // instruction.
  if (NewF->getLoweredFunctionType()->isNoReturn()) {
    Builder.createUnreachable(Loc);
    return;
  }

  Builder.createReturn(Loc, ReturnValue);
}

static SILFunction *
moveFunctionBodyToNewFunctionWithName(SILFunction *F,
                                      const std::string &NewFName,
                                      SignatureOptimizer &Optimizer) {
  // First we create an empty function (i.e. no BB) whose function signature has
  // had its arity modified.
  //
  // We only do this to remove dead arguments. All other function signature
  // optimization is done later by modifying the function signature elements
  // themselves.
  SILFunction *NewF = Optimizer.createEmptyFunctionWithOptimizedSig(NewFName);

  // Then we transfer the body of F to NewF. At this point, the arguments of the
  // first BB will not match.
  NewF->spliceBody(F);
  // Do the same with the call graph.

  // Then perform any updates to the arguments of NewF.
  SILBasicBlock *NewFEntryBB = &*NewF->begin();
  MutableArrayRef<ArgumentDescriptor> ArgDescs = Optimizer.getArgDescList();
  unsigned ArgOffset = 0;
  SILBuilder Builder(NewFEntryBB->begin());
  Builder.setCurrentDebugScope(NewFEntryBB->getParent()->getDebugScope());
  for (auto &ArgDesc : ArgDescs) {
    // We always need to reset the insertion point in case we delete the first
    // instruction.
    Builder.setInsertionPoint(NewFEntryBB->begin());
    DEBUG(llvm::dbgs() << "Updating arguments at ArgOffset: " << ArgOffset
                       << " for: " << *ArgDesc.Arg);
    ArgOffset = updateOptimizedBBArgs(ArgDesc, Builder, NewFEntryBB, ArgOffset);
  }

  // Otherwise generate the thunk body just in case.
  SILBasicBlock *ThunkBody = F->createBasicBlock();
  for (auto &ArgDesc : ArgDescs) {
    ThunkBody->createBBArg(ArgDesc.Arg->getType(), ArgDesc.Decl);
  }
  createThunkBody(ThunkBody, NewF, Optimizer);

  F->setThunk(IsThunk);
  assert(F->getDebugScope()->Parent != NewF->getDebugScope()->Parent);

  return NewF;
}

/// This function takes in a SILFunction F and its callsites in the current
/// module and produces a new SILFunction that has the body of F but with
/// optimized function arguments. F is changed to be a thunk that calls NewF to
/// reduce code duplication in cases where we missed a callsite to F. The
/// function returns true if we were successful in creating the new function and
/// returns false otherwise.
static bool optimizeFunctionSignature(llvm::BumpPtrAllocator &BPA,
                                      RCIdentityFunctionInfo *RCIA,
                                      FunctionSignatureFunctionInfo *FSFI,
                                      AliasAnalysis *AA, 
                                      SILFunction *F,
                                      const ApplyList &CallSites) {
  DEBUG(llvm::dbgs() << "Optimizing Function Signature of " << F->getName()
                     << "\n");

  assert(!CallSites.empty() && "Unexpected empty set of call sites!");

  // Analyze function arguments. If there is no work to be done, exit early.
  if (!FSFI->analyze()) {
    DEBUG(llvm::dbgs() << "    Has no optimizable arguments... "
                          "bailing...\n");
    return false;
  }

  DEBUG(llvm::dbgs() << "    Has optimizable arguments... Performing "
                        "optimizations...\n");

  ++NumFunctionSignaturesOptimized;

  auto NewFName = FSFI->getOptimizedName();

  // If we already have a specialized version of this function, do not
  // respecialize. For now just bail.
  //
  // TODO: Improve this. I do not expect this to occur often so I am fine for
  // now avoiding this issue. The main things I am worried about are assumptions
  // that we make about the callee and caller being violated. That said, this is
  // just a fear.
  if (F->getModule().lookUpFunction(NewFName))
    return false;

  SignatureOptimizer Optimizer(FSFI);

  // Otherwise, move F over to NewF.
  SILFunction *NewF =
      moveFunctionBodyToNewFunctionWithName(F, NewFName, Optimizer);

  // And remove all Callee releases that we found and made redundant via owned
  // to guaranteed conversion.
  //
  // TODO: If more stuff needs to be placed here, refactor into its own method.
  for (auto &A : Optimizer.getArgDescList()) {
    for (auto &X : A.CalleeRelease) 
      X->eraseFromParent();
    for (auto &X : A.CalleeReleaseInThrowBlock) 
      X->eraseFromParent();
  }

  // And remove all callee retains that we found and made redundant via owned
  // to unowned conversion.
  for (ResultDescriptor &RD : Optimizer.getResultDescList()) {
    for (auto &X : RD.CalleeRetain) {
      if (!isa<StrongRetainInst>(X) && !isa<RetainValueInst>(X))
        continue;
      X->eraseFromParent();
    }
  }

  // Rewrite all apply insts calling F to call NewF. Update each call site as
  // appropriate given the form of function signature optimization performed.
  rewriteApplyInstToCallNewFunction(Optimizer, NewF, CallSites);

  return true;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static bool isSpecializableRepresentation(SILFunctionTypeRepresentation Rep) {
  switch (Rep) {
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::CFunctionPointer:
    return true;
  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::Block:
    return false;
  }
}

/// Returns true if F is a function which the pass know show to specialize
/// function signatures for.
static bool canSpecializeFunction(SILFunction *F) {
  // Do not specialize the signature of SILFunctions that are external
  // declarations since there is no body to optimize.
  if (F->isExternalDeclaration())
    return false;

  // Do not specialize functions that are available externally. If an external
  // function was able to be specialized, it would have been specialized in its
  // own module. We will inline the original function as a thunk. The thunk will
  // call the specialized function.
  if (F->isAvailableExternally())
    return false;

  // Do not specialize the signature of always inline functions. We
  // will just inline them and specialize each one of the individual
  // functions that these sorts of functions are inlined into.
  if (F->getInlineStrategy() == Inline_t::AlwaysInline)
    return false;

  // For now ignore generic functions to keep things simple...
  if (F->getLoweredFunctionType()->isPolymorphic())
    return false;

  // Make sure F has a linkage that we can optimize.
  if (!isSpecializableRepresentation(F->getRepresentation()))
    return false;

  return true;
}


//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//
namespace {

class FunctionSignatureOpts : public SILModuleTransform {
public:
  FunctionSignatureOpts() {}

  void run() override {
    SILModule *M = getModule();
    auto *BCA = getAnalysis<BasicCalleeAnalysis>();
    auto *RCIA = getAnalysis<RCIdentityAnalysis>();
    auto *FSA = getAnalysis<FunctionSignatureAnalysis>();
    auto *AA = PM->getAnalysis<AliasAnalysis>();
    llvm::BumpPtrAllocator Allocator;

    DEBUG(llvm::dbgs() << "**** Optimizing Function Signatures ****\n\n");

    // Construct a map from Callee -> Call Site Set.

    // Process each function in the callgraph that we are able to optimize.
    //
    // TODO: Determine if it is profitable to always perform this optimization
    // even if a function is not called locally. As far as we can tell. Down the
    // line more calls may be exposed and the inliner might be able to handle
    // those calls.
    bool Changed = false;

    // The CallerMap maps functions to the list of call sites that call that
    // function..
    llvm::DenseMap<SILFunction *, ApplyList> CallerMap;

    for (auto &F : *M) {
      // Don't optimize callers that are marked as 'no.optimize'.
      if (!F.shouldOptimize())
        continue;

      // Scan the whole module and search Apply sites.
      for (auto &BB : F) {
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

            SILFunction *F = FRI->getReferencedFunction();
            CallerMap[F].push_back(Apply);
          }
        }
      }
    }

    BottomUpFunctionOrder BottomUpOrder(*M, BCA);
    for (auto *F : BottomUpOrder.getFunctions()) {
      // Don't optimize callees that should not be optimized.
      if (!F->shouldOptimize())
        continue;

      // Check the signature of F to make sure that it is a function that we
      // can specialize. These are conditions independent of the call graph.
      if (!canSpecializeFunction(F))
        continue;

      // Now that we have our call graph, grab the CallSites of F.
      ApplyList &CallSites = CallerMap[F];

      // If this function is not called anywhere, for now don't do anything.
      //
      // TODO: If it is public, it may still make sense to specialize since if
      // we link in the public function in another module, we may be able to
      // inline it and access the specialized version.
      if (CallSites.empty())
        continue;

      // Otherwise, try to optimize the function signature of F.
      Changed |=
          optimizeFunctionSignature(Allocator, RCIA->get(F), FSA->get(F), AA, F, CallSites);
    }

    // If we changed anything, invalidate the call graph.
    if (Changed) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Everything);
    }
  }

  StringRef getName() override { return "Function Signature Optimization"; }
};

} // end anonymous namespace

SILTransform *swift::createFunctionSignatureOpts() {
  return new FunctionSignatureOpts();
}


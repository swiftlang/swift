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

#define DEBUG_TYPE "sil-function-signature-opt"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/CallerAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/FunctionSignatureOptUtils.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumFunctionSignaturesOptimized, "Total func sig optimized");
STATISTIC(NumDeadArgsEliminated, "Total dead args eliminated");
STATISTIC(NumOwnedConvertedToGuaranteed, "Total owned args -> guaranteed args");
STATISTIC(NumOwnedConvertedToNotOwnedResult, "Total owned result -> not owned result");
STATISTIC(NumSROAArguments, "Total SROA arguments optimized");


/// Creates a decrement on \p Ptr at insertion point \p InsertPt that creates a
/// strong_release if \p Ptr has reference semantics itself or a release_value
/// if \p Ptr is a non-trivial value without reference-semantics.
static SILInstruction *createDecrement(SILValue Ptr, SILInstruction *InsertPt) {
  // Setup the builder we will use to insert at our insertion point.
  SILBuilder B(InsertPt);
  auto Loc = RegularLocation(SourceLoc());

  // If Ptr has reference semantics itself, create a strong_release.
  if (Ptr->getType().isReferenceCounted(B.getModule()))
    return B.createStrongRelease(Loc, Ptr, Atomicity::Atomic);

  // Otherwise create a release value.
  return B.createReleaseValue(Loc, Ptr, Atomicity::Atomic);
}

//===----------------------------------------------------------------------===//
//                     Argument and Result Optimizer
//===----------------------------------------------------------------------===//

static void
computeOptimizedInterfaceParams(const ArgumentDescriptor &AD,
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
addThunkArgs(const ArgumentDescriptor &AD, SILBuilder &Builder,
             SILBasicBlock *BB, llvm::SmallVectorImpl<SILValue> &NewArgs) {
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
updateOptimizedBBArgs(const ArgumentDescriptor &AD, SILBuilder &Builder,
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

  // We have built a projection tree and filled it with liveness information.
  //
  // Use this as a base to replace values in current function with their leaf
  // values.
  //
  // NOTE: this also allows us to NOT modify the results of an analysis pass.
  llvm::BumpPtrAllocator Allocator;
  ProjectionTree PT(BB->getModule(), Allocator);
  PT.initializeWithExistingTree(AD.ProjTree);

  // Then go through the projection tree constructing aggregates and replacing
  // uses.
  //
  // TODO: What is the right location to use here?
  PT.replaceValueUsesWithLeafUses(Builder, BB->getParent()->getLocation(),
                                  LeafValues);

  // We ignored debugvalue uses when we constructed the new arguments, in order
  // to preserve as much information as possible, we construct a new value for
  // OrigArg from the leaf values and use that in place of the OrigArg.
  SILValue NewOrigArgValue = PT.computeExplodedArgumentValue(Builder,
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

namespace {

/// A class that contains all analysis information we gather about our
/// function. Also provides utility methods for creating the new empty function.
class SignatureOptimizer {
  FunctionSignatureInfo &FSI;

public:
  SignatureOptimizer() = delete;
  SignatureOptimizer(const SignatureOptimizer &) = delete;
  SignatureOptimizer(SignatureOptimizer &&) = delete;

  SignatureOptimizer(FunctionSignatureInfo &FSI) : FSI(FSI) {}

  ArrayRef<ArgumentDescriptor> getArgDescList() const {
    return FSI.getArgDescList();
  }

  ArrayRef<ArgumentDescriptor> getArgDescList() {
    return FSI.getArgDescList();
  }

  ArrayRef<ResultDescriptor> getResultDescList() {
    return FSI.getResultDescList();
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

CanSILFunctionType SignatureOptimizer::createOptimizedSILFunctionType() {
  auto *F = FSI.getAnalyzedFunction();

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
  if (FSI.shouldModifySelfArgument())
    ExtInfo = ExtInfo.withRepresentation(SILFunctionTypeRepresentation::Thin);

  return SILFunctionType::get(FTy->getGenericSignature(), ExtInfo,
                              FTy->getCalleeConvention(), InterfaceParams,
                              InterfaceResults, InterfaceErrorResult, Ctx);
}

SILFunction *
SignatureOptimizer::
createEmptyFunctionWithOptimizedSig(const std::string &NewFName) {

  auto *F = FSI.getAnalyzedFunction();
  SILModule &M = F->getModule();

  // Create the new optimized function type.
  CanSILFunctionType NewFTy = createOptimizedSILFunctionType();

  // Create the new function.
  auto *NewF = M.createFunction(
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
      Builder.createReleaseValue(Loc, BB->getBBArg(ArgDesc.Index),
                                 Atomicity::Atomic);
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
createOptimizedFunctionBody(SILFunction *F, const std::string &NewFName,
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
  ArrayRef<ArgumentDescriptor> ArgDescs = Optimizer.getArgDescList();
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

/// Create an optimized version of the current function.
static SILFunction* 
createOptimizedFunction(RCIdentityFunctionInfo *RCIA,
                        FunctionSignatureInfo *FSI,
                        AliasAnalysis *AA, SILFunction *F) {
  // This is the new function name.
  auto NewFName = FSI->getOptimizedName();

  // If we already have a specialized version of this function, do not
  // respecialize. For now just bail.
  if (F->getModule().lookUpFunction(NewFName))
    return nullptr;

  ++NumFunctionSignaturesOptimized;
  SignatureOptimizer Optimizer(*FSI);

  // Otherwise, move F over to NewF.
  SILFunction *NewF = createOptimizedFunctionBody(F, NewFName, Optimizer);

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
  for (const ResultDescriptor &RD : Optimizer.getResultDescList()) {
    for (auto &X : RD.CalleeRetain) {
      if (isa<StrongRetainInst>(X) || isa<RetainValueInst>(X)) {
        X->eraseFromParent();
        continue;
      }
      assert(isa<ApplyInst>(X) && "Unknown epilogue retain");
      // Create a release to balance it out.
      createDecrement(X, dyn_cast<ApplyInst>(X)->getParent()->getTerminator());
    }
  }
  return NewF;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//
namespace {
class FunctionSignatureOpts : public SILFunctionTransform {
public:
  void run() override {
    auto *RCIA = getAnalysis<RCIdentityAnalysis>();
    auto *AA = PM->getAnalysis<AliasAnalysis>();
    auto *CA = PM->getAnalysis<CallerAnalysis>();

    SILFunction *F = getFunction();
    llvm::BumpPtrAllocator Allocator;
    FunctionSignatureInfo FSI(F, Allocator, AA, RCIA->get(F));
    DEBUG(llvm::dbgs() << "*** FSO on function: " << F->getName() << " ***\n");

    // Don't optimize callees that should not be optimized.
    if (!F->shouldOptimize())
      return;

    // If there is no opportunity on the signature, simply return.
    if (!FSI.shouldOptimize())
     return;

    // If this function does not have a caller in the current module.
    if (!CA->hasCaller(F)) {
      // If this function maybe called indirectly, e.g. from virtual table
      // do not function signature specialize it, as this will introduce a thunk.
      if (canBeCalledIndirectly(F->getRepresentation()))
        return;
      // if its not highly profitable to optimize this function. We do not
      // function signature specialize it.
      if (!FSI.profitableOptimize())
        return;
    }

    // Check the signature of F to make sure that it is a function that we
    // can specialize. These are conditions independent of the call graph.
    if (!canSpecializeFunction(F))
      return;

    // Try to create an optimized function based on the signature analysis.
    SILFunction *NewF = 
                 createOptimizedFunction(RCIA->get(F), &FSI, AA, F);
  
    if (NewF) { 
      // The thunk now carries the information on how the signature is
      // optimized. If we inline the thunk, we will get the benefit of calling
      // the signature optimized function without additional setup on the
      // caller side.
      F->setInlineStrategy(AlwaysInline);
      // Make sure the PM knows about this function. This will also help us
      // with self-recursion.
      notifyPassManagerOfFunction(NewF);
      invalidateAnalysis(SILAnalysis::InvalidationKind::Everything);
    }
  }

  StringRef getName() override {
    return "Function Signature Optimization";
  }
};

} // end anonymous namespace

SILTransform *swift::createFunctionSignatureOpts() {
  return new FunctionSignatureOpts();
}

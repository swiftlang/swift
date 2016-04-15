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
///
/// \file
///
/// This pass defines an interface as well as a few transformations (implementing
/// the interface) on the signature of the function.
///
/// Everytime, a function is transformed, the old function is turned into a
/// thunk and a new function with the optimized signature is generated.
/// Before the pass finishes, it collapses the chain of generated thunks and
/// remove them.
///
/// TODO: We could run the sequnece of optimization in a loop until
/// convergence, but we send the final optimized function to the compilation
/// pipeline anyways, i.e. notifyPassManagerOfFunction of new function.
///
/// TODO: Optimize function with generic parameters.
///
/// TODO: Improve epilogue release matcher, i.e. do a data flow instead of
/// only  finding releases in the return block. 
///
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
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/Mangle.h"
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

using SILParameterInfoList = llvm::SmallVector<SILParameterInfo, 8>;
using SILResultInfoList =  llvm::SmallVector<SILResultInfo, 8>;
using FunctionSignatureMangler = FunctionSignatureSpecializationMangler;

//===----------------------------------------------------------------------===//
//                           Utilties 
//===----------------------------------------------------------------------===//

/// Return the single apply found in this function.
static SILInstruction *findOnlyApply(SILFunction *F) {
  SILInstruction *OnlyApply = nullptr;
  for (auto &B : *F) {
    for (auto &X : B) {
      if (!isa<ApplyInst>(X))
        continue;
      assert(!OnlyApply && "There are more than 1 function calls");
      OnlyApply = &X;
    }
  }
  assert(OnlyApply && "There is no function calls");
  return OnlyApply;
}

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

/// Walk down a chain of thunks and collapse them. Break when we reach the
/// end of the thunks.
static void collapseThunkChain(SILFunction *FirstFun, SILFunction *LastFun,
                               SILPassManager *PM) {
  // Keep walking down the chain of thunks and inline every one of them.
  // break when we hit the last function.
  SILModule *Module = &FirstFun->getModule();
  while (true) {
    FullApplySite AI = FullApplySite(findOnlyApply(FirstFun)); 
    SILFunction *Callee = AI.getCalleeFunction();

    // Reached the end of the chain of functions.
    if (Callee == LastFun)
      break;

    assert(Callee->isThunk() && "Try to inline callee which is not a thunk");
    SmallVector<SILValue, 8> Args;
    for (const auto &Arg : AI.getArguments())
      Args.push_back(Arg);

    TypeSubstitutionMap ContextSubs;
    SILInliner Inliner(*FirstFun, *Callee,
                        SILInliner::InlineKind::PerformanceInline, ContextSubs,
                        AI.getSubstitutions());

    auto Success = Inliner.inlineFunction(AI, Args);
    assert(Success && "Failed to inline thunks");
    recursivelyDeleteTriviallyDeadInstructions(AI.getInstruction(), true);

    // Invalidate the information about the thunk and remove the thunk.
    PM->invalidateAnalysisForDeadFunction(Callee,
                                    SILAnalysis::InvalidationKind::Everything);
    Module->eraseFunction(Callee);
  }
}

//===----------------------------------------------------------------------===//
//                     Function Signature Transformation 
//===----------------------------------------------------------------------===//

/// FunctionSignatureTransform - This is the base class for all function
/// signature transformations. All other transformations inherit from this.
class FunctionSignatureTransform {
protected:
  /// The actual function to analyze and transform.
  SILFunction *F;

  /// Optimized function.
  SILFunction *NewF;

  /// The allocator we are using.
  llvm::BumpPtrAllocator &Allocator;

  /// The alias analysis we are using.
  AliasAnalysis *AA;

  /// The RC identity analysis we are using.
  RCIdentityAnalysis *RCIA;

  // The function signature mangler we are using.
  FunctionSignatureMangler &FM;

  // Self arument is modified.
  bool shouldModifySelfArgument;

  /// Keep a "view" of precompiled information on argumentis that we use 
  /// during our optimization.
  llvm::SmallVector<ArgumentDescriptor, 4> ArgDescList;

  /// Keep a "view" of precompiled information on the direct results that we
  /// will use during our optimization.
  llvm::SmallVector<ResultDescriptor, 4> ResultDescList;

  /// Return a function name based on ArgDescList and ResultDescList.
  std::string createOptimizedSILFunctionName();

  /// Return a function type based on ArgDescList and ResultDescList.
  CanSILFunctionType createOptimizedSILFunctionType();

  /// Take ArgDescList and ResultDescList and create an optimized function
  /// based on the current function we are analyzing. This also has
  /// the side effect of turning the current function into a thunk.
  /// If function specialization is successful, the optimized function is
  /// returned, otherwise nullptr is returned.
  SILFunction *createOptimizedSILFunction();

private:
  /// Set up the parameter descriptor list.
  void initializeParameters() {
    ArrayRef<SILArgument *> Args = F->begin()->getBBArgs();
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
      ArgumentDescriptor A(Allocator, Args[i]);
      ArgDescList.push_back(std::move(A));
    }
  }

  /// Set up the result descriptor list.
  void initializeResults() {
    auto FTy = F->getLoweredFunctionType();
    for (SILResultInfo InterfaceResult : FTy->getAllResults()) {
      ResultDescriptor R(InterfaceResult);
      ResultDescList.push_back(std::move(R));
    }
  } 

  /// ----------------------------------------------------------///
  /// Function to implemented for specific FSO transformations. ///
  /// ----------------------------------------------------------///

  /// Compute what the function name will be based on the given result decriptor.
  /// Default implementation simply passes it through.
  virtual void
  computeOptimizedName(ResultDescriptor &RV, FunctionSignatureMangler &M) {}

  /// Compute what the function name will be based on the given argument decriptor.
  /// Default implementation simply passes it through.
  virtual void
  computeOptimizedName(ArgumentDescriptor &AD, FunctionSignatureMangler &M) {}

  /// Compute what the function interface will look like based on the
  /// optimization we are doing on the given result descriptor. Default
  /// implemenation simply passes it through.
  virtual void
  computeOptimizedInterface(ResultDescriptor &RV, SILResultInfoList &Out){
    Out.push_back(RV.ResultInfo);
  }

  /// Compute what the function interface will look like based on the
  /// optimization we are doing on the given argument descriptor. Default
  /// implemenation simply passes it through.
  virtual void
  computeOptimizedInterface(ArgumentDescriptor &AD, SILParameterInfoList &Out){
    Out.push_back(AD.Arg->getKnownParameterInfo());
  }

  /// Setup the thunk arguments based on the given argument and result
  /// descriptor info. Every transformation must defines this interface. Default
  /// implementation simply passes it through.
  virtual void
  addThunkArgument(ArgumentDescriptor &AD, SILBuilder &Builder,
                   SILBasicBlock *BB, 
                   llvm::SmallVectorImpl<SILValue> &NewArgs) {
    NewArgs.push_back(BB->getBBArg(AD.Index));
  } 

  /// Set up epilogue work for the thunk arguments based in the given argument.
  /// Default implementation simply passes it through.
  virtual void
  completeThunkArgument(ArgumentDescriptor &AD, SILBuilder &Builder,
                        SILFunction *F) {}

  /// Set up epilogue work for the thunk result based in the given argument.
  /// Default implementation simply passes it through.
  virtual void
  completeThunkResult(ResultDescriptor &RD, SILBuilder &Builder,
                      SILFunction *F) {}

  /// Analyze the function and decide whether to optimize based on the function
  /// signature. Default implementation returns false.
  virtual bool analyzeResults()  { return false; }
  virtual bool analyzeParameters() { return false; }

  /// Do the actual transformations and return the transformed function, not the
  /// thunk. Default implementation simply passes through.
  virtual void transformResults() {}
  virtual void transformParameters() {}

public:
  /// Constructor.
  FunctionSignatureTransform(SILFunction *F, llvm::BumpPtrAllocator &BPA,
                             AliasAnalysis *AA, RCIdentityAnalysis *RCIA,
                             FunctionSignatureMangler &FM)
    : F(F), NewF(nullptr), Allocator(BPA), AA(AA), RCIA(RCIA), FM(FM),
      shouldModifySelfArgument(false) {}

  /// virtual destructor.
  virtual ~FunctionSignatureTransform() {}

  ArrayRef<ArgumentDescriptor> getArgDescList() { return ArgDescList; }

  ArrayRef<ResultDescriptor> getResultDescList() { return ResultDescList; }

  /// Do the actual transformations.
  SILFunction *transform() {
    // Create the new function.
    NewF = createOptimizedSILFunction();
    assert(NewF && "Failed to create optimized function");
    // Optimize the new function.
    transformResults();
    transformParameters();
    return NewF;
  }

  /// Find any owned to guaranteed opportunities.
  bool analyze() {
    // Set up result and parameter for analysis.
    initializeResults();
    initializeParameters();

    // Analyze the function for result and parameter optimizations.
    bool Result = analyzeResults();
    bool Params = analyzeParameters();

    // We are extremely unlucky to have a collision on the function name.
    if (F->getModule().lookUpFunction(createOptimizedSILFunctionName()))
      return false;

    return Params || Result;
  }
};

std::string FunctionSignatureTransform::createOptimizedSILFunctionName() {
  // Compute the argument name.
  for (auto &ArgDesc : ArgDescList) {
    computeOptimizedName(ArgDesc, FM);
  }

  // Compute the result name.
  for (auto &ResultDesc : ResultDescList) {
    computeOptimizedName(ResultDesc, FM);
  }

  FM.mangle();
  return FM.getMangler().finalize();
}

CanSILFunctionType FunctionSignatureTransform::createOptimizedSILFunctionType() {
  // Compute the argument interface parameters.
  SILParameterInfoList InterfaceParams;
  for (auto &ArgDesc : ArgDescList) {
    computeOptimizedInterface(ArgDesc, InterfaceParams);
  }

  // Compute the result interface parameters.
  SILResultInfoList InterfaceResults;
  for (auto &ResultDesc : ResultDescList) {
    computeOptimizedInterface(ResultDesc, InterfaceResults);
  }

  // Don't use a method representation if we modified self.
  CanSILFunctionType FTy = F->getLoweredFunctionType();
  auto ExtInfo = FTy->getExtInfo();
  if (shouldModifySelfArgument)
    ExtInfo = ExtInfo.withRepresentation(SILFunctionTypeRepresentation::Thin);

  return SILFunctionType::get(FTy->getGenericSignature(), ExtInfo,
                              FTy->getCalleeConvention(), InterfaceParams,
                              InterfaceResults, FTy->getOptionalErrorResult(),
                              F->getModule().getASTContext());
}

SILFunction *FunctionSignatureTransform::createOptimizedSILFunction() {
  // Create the optimized function !
  //
  // Create the name of the optimized function.
  std::string NewFName = createOptimizedSILFunctionName();
  // Create the type of the optimized function.
  SILModule &M = F->getModule();
  CanSILFunctionType NewFTy = createOptimizedSILFunctionType();
 
  // Create the optimized function.
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

  // Then we transfer the body of F to NewF. At this point, the arguments of the
  // first BB will not match.
  NewF->spliceBody(F);

  // Create the thunk body !
  F->setThunk(IsThunk);
  SILBasicBlock *ThunkBody = F->createBasicBlock();
  for (auto &ArgDesc : ArgDescList) {
    ThunkBody->createBBArg(ArgDesc.Arg->getType(), ArgDesc.Decl);
  }

  SILLocation Loc = ThunkBody->getParent()->getLocation();
  SILBuilder Builder(ThunkBody);
  Builder.setCurrentDebugScope(ThunkBody->getParent()->getDebugScope());

  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  // Create the args for the thunk's apply, ignoring any dead arguments.
  llvm::SmallVector<SILValue, 8> ThunkArgs;
  for (auto &ArgDesc : ArgDescList) {
    addThunkArgument(ArgDesc, Builder, ThunkBody, ThunkArgs);
  }

  // We are ignoring generic functions and functions with out parameters for
  // now.
  SILType LoweredType = NewF->getLoweredType();
  SILType ResultType = LoweredType.getFunctionInterfaceResultType();
  SILValue ReturnValue = Builder.createApply(Loc, FRI, LoweredType, ResultType,
                                             ArrayRef<Substitution>(),
                                             ThunkArgs, false);

  // Finish the epilogue work for the argument as well as result.
  for (auto &ArgDesc : ArgDescList) {
    completeThunkArgument(ArgDesc, Builder, F);
  }
  for (auto &ResDesc : ResultDescList) {
    completeThunkResult(ResDesc, Builder, F);
  }

  // Function that are marked as @NoReturn must be followed by an 'unreachable'
  // instruction.
  if (NewF->getLoweredFunctionType()->isNoReturn()) {
    Builder.createUnreachable(Loc);
    return NewF;
  }

  Builder.createReturn(Loc, ReturnValue);

  assert(F->getDebugScope()->Parent != NewF->getDebugScope()->Parent);
  return NewF;
}

//===----------------------------------------------------------------------===//
//                      Owned to Guaranteed Optimization 
//===----------------------------------------------------------------------===//

/// OwnedToGuaranteedTransform - Owned to Guanranteed optimization.
class OwnedToGuaranteedTransform : public FunctionSignatureTransform {
public:
   OwnedToGuaranteedTransform(SILFunction *F, llvm::BumpPtrAllocator &BPA,
                              AliasAnalysis *AA, RCIdentityAnalysis *RCIA,
                              FunctionSignatureMangler &FM)
     : FunctionSignatureTransform(F, BPA, AA, RCIA, FM) {}
   /// virtual destructor.
   virtual ~OwnedToGuaranteedTransform() {}

  /// Analyze the function and decide whether to optimize based on the function
  /// signature.
  bool analyzeParameters();
  bool analyzeResults();

  /// Transform the parameters and result of the function.
  void transformParameters();
  void transformResults();

  virtual void completeThunkArgument(ArgumentDescriptor &AD,
                                     SILBuilder &Builder, SILFunction *F) {
    // If we have any arguments that were consumed but are now guaranteed,
    // insert a release_value.
    if (!AD.OwnedToGuaranteed)
      return;
    Builder.createReleaseValue(RegularLocation(SourceLoc()),
                               F->getArguments()[AD.Index],
                               Atomicity::Atomic);
  }

  virtual void completeThunkResult(ResultDescriptor &RD,
                                   SILBuilder &Builder, SILFunction *F) {
    if (!RD.OwnedToGuaranteed)
      return;
    Builder.createRetainValue(RegularLocation(SourceLoc()), findOnlyApply(F),
                              Atomicity::Atomic);
  }

  /// Compute what the function name will be based on the given result decriptor.
  void computeOptimizedName(ResultDescriptor &RV, FunctionSignatureMangler &M) {
    if (!RV.OwnedToGuaranteed)
      return;
    M.setReturnValueOwnedToUnowned();
  }

  /// Compute what the function name will be based on the given result decriptor.
  void computeOptimizedName(ArgumentDescriptor &AD, FunctionSignatureMangler &M) {
    if (!AD.OwnedToGuaranteed)
      return;
    M.setArgumentOwnedToGuaranteed(AD.Index);
  }

  void computeOptimizedInterface(ResultDescriptor &R, SILResultInfoList &Out);
  void computeOptimizedInterface(ArgumentDescriptor &A, SILParameterInfoList &Out);
};

bool OwnedToGuaranteedTransform::analyzeParameters() {
  ArrayRef<SILArgument *> Args = F->begin()->getBBArgs();
  // A map from consumed SILArguments to the release associated with an
  // argument.
  // TODO: The return block and throw block should really be abstracted away.
  ConsumedArgToEpilogueReleaseMatcher ArgToReturnReleaseMap(RCIA->get(F), F);
  ConsumedArgToEpilogueReleaseMatcher ArgToThrowReleaseMap(
      RCIA->get(F), F, ConsumedArgToEpilogueReleaseMatcher::ExitKind::Throw);

  // Did we decide we should optimize any parameter?
  bool SignatureOptimize = false;

  // Analyze the argument information.
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgumentDescriptor &A = ArgDescList[i];

    // See if we can find a ref count equivalent strong_release or release_value
    // at the end of this function if our argument is an @owned parameter.
    if (A.hasConvention(SILArgumentConvention::Direct_Owned)) {
      auto Releases = ArgToReturnReleaseMap.getReleasesForArgument(A.Arg);
      if (!Releases.empty()) {
        // If the function has a throw block we must also find a matching
        // release in the throw block.
        auto ReleasesInThrow = ArgToThrowReleaseMap.getReleasesForArgument(A.Arg);
        if (!ArgToThrowReleaseMap.hasBlock() || !ReleasesInThrow.empty()) {
          A.CalleeRelease = Releases;
          A.CalleeReleaseInThrowBlock = ReleasesInThrow;
          // We can convert this parameter to a @guaranteed.
          A.OwnedToGuaranteed = true;
          SignatureOptimize = true;
        }
      }
    }
  }
  return SignatureOptimize;
}

bool OwnedToGuaranteedTransform::analyzeResults() {
  auto FTy = F->getLoweredFunctionType();
  // Did we decide we should optimize any parameter?
  // Analyze return result information.
  if (FTy->getIndirectResults().size())
    return false;

  // For now, only do anything if there's a single direct result.
  if (FTy->getDirectResults().size() != 1)
    return false; 

  bool SignatureOptimize = false;
  if (ResultDescList[0].hasConvention(ResultConvention::Owned)) {
    auto &RI = ResultDescList[0];
    // We have an @owned return value, find the epilogue retains now.
    ConsumedResultToEpilogueRetainMatcher ReturnRetainMap(RCIA->get(F), AA, F);
    auto Retains = ReturnRetainMap.getEpilogueRetains();
    // We do not need to worry about the throw block, as the return value is only
    // going to be used in the return block/normal block of the try_apply
    // instruction.
    if (!Retains.empty()) {
      RI.CalleeRetain = Retains;
      SignatureOptimize = true;
      RI.OwnedToGuaranteed = true;
    }
  }
  return SignatureOptimize;
}

void OwnedToGuaranteedTransform::transformParameters() {
  // And remove all Callee releases that we found and made redundant via owned
  // to guaranteed conversion.
  for (const ArgumentDescriptor &AD : ArgDescList) {
    if (!AD.OwnedToGuaranteed)
      continue;
    ++ NumOwnedConvertedToGuaranteed;
    for (auto &X : AD.CalleeRelease) 
      X->eraseFromParent();
    for (auto &X : AD.CalleeReleaseInThrowBlock) 
      X->eraseFromParent();
  }
}

void OwnedToGuaranteedTransform::transformResults() {
  // And remove all callee retains that we found and made redundant via owned
  // to unowned conversion.
  for (const ResultDescriptor &RD : ResultDescList) {
    if (!RD.OwnedToGuaranteed)
      continue;
    ++NumOwnedConvertedToNotOwnedResult; 
    for (auto &X : RD.CalleeRetain) {
      if (isa<StrongRetainInst>(X) || isa<RetainValueInst>(X)) {
        X->eraseFromParent();
        continue;
      }
      // Create a release to balance it out.
      assert(isa<ApplyInst>(X) && "Unknown epilogue retain");
      createDecrement(X, dyn_cast<ApplyInst>(X)->getParent()->getTerminator());
    }
  }
}

void OwnedToGuaranteedTransform::
computeOptimizedInterface(ArgumentDescriptor &AD, SILParameterInfoList &Out) {
  auto ParameterInfo = AD.Arg->getKnownParameterInfo();
  // If we cannot explode this value, handle callee release and return.
  // If we found releases in the callee in the last BB on an @owned
  // parameter, change the parameter to @guaranteed and continue...
  if (AD.OwnedToGuaranteed) {
    assert(ParameterInfo.getConvention() == ParameterConvention::Direct_Owned &&
           "Can only transform @owned => @guaranteed in this code path");
    SILParameterInfo NewInfo(ParameterInfo.getType(),
                             ParameterConvention::Direct_Guaranteed);
    Out.push_back(NewInfo);
    ++NumOwnedConvertedToGuaranteed;
    return;
  }

  // Otherwise just propagate through the parameter info.
  Out.push_back(ParameterInfo);
}

void OwnedToGuaranteedTransform::
computeOptimizedInterface(ResultDescriptor &RV, SILResultInfoList &Out) {
  // ResultDescs only covers the direct results; we currently can't ever
  // change an indirect result.  Piece the modified direct result information
  // back into the all-results list.
  if (RV.OwnedToGuaranteed) {
    Out.push_back(SILResultInfo(RV.ResultInfo.getType(), ResultConvention::Unowned));
    ++NumOwnedConvertedToNotOwnedResult;
    return;
  }

  Out.push_back(RV.ResultInfo);
}

//===----------------------------------------------------------------------===//
//                        Dead Argument Optimization 
//===----------------------------------------------------------------------===//

/// DeadArgumentTransform - Owned to Guanranteed optimization.
class DeadArgumentTransform : public FunctionSignatureTransform {
  /// Does any call inside the given function may bind dynamic 'Self' to a
  /// generic argument of the callee.
  bool MayBindDynamicSelf;

  /// Return true if this argument is used in a non-trivial way.
  bool hasNonTrivialNonDebugUse(SILArgument *Arg); 

public:
   DeadArgumentTransform(SILFunction *F, llvm::BumpPtrAllocator &BPA,
                           AliasAnalysis *AA, RCIdentityAnalysis *RCIA,
                         FunctionSignatureMangler &FM)
     : FunctionSignatureTransform(F, BPA, AA, RCIA, FM),
       MayBindDynamicSelf(computeMayBindDynamicSelf(F)) {}

  /// virtual destructor.
  virtual ~DeadArgumentTransform() {}

  bool isArgumentABIRequired(SILArgument *Arg) {
    // This implicitly asserts that a function binding dynamic self has a self
   // metadata argument or object from which self metadata can be obtained.
   return MayBindDynamicSelf && (F->getSelfMetadataArgument() == Arg);
  }

  /// Analyze the function and decide whether to optimize based on the function
  /// signature.
  bool analyzeParameters();

  /// Transform the parameters and result of the function.
  void transformParameters();

  /// Simply add the function argument.
  void addThunkArgument(ArgumentDescriptor &AD, SILBuilder &Builder,
                        SILBasicBlock *BB,
                        llvm::SmallVectorImpl<SILValue> &NewArgs) {
    if (AD.IsEntirelyDead) {
      ++NumDeadArgsEliminated;
      return;
    }
    NewArgs.push_back(BB->getBBArg(AD.Index));
  }

  /// Compute what the function name will be based on the given result decriptor.
  void computeOptimizedName(ArgumentDescriptor &AD, FunctionSignatureMangler &M) {
    if (!AD.IsEntirelyDead)
      return;
    M.setArgumentDead(AD.Index);
  }

  void computeOptimizedInterface(ArgumentDescriptor &A, SILParameterInfoList &Out);
};

bool DeadArgumentTransform::hasNonTrivialNonDebugUse(SILArgument *Arg) {
  llvm::SmallVector<SILInstruction *, 8> Worklist;
  llvm::SmallPtrSet<SILInstruction *, 8> SeenInsts;

  for (Operand *I : getNonDebugUses(SILValue(Arg)))
    Worklist.push_back(I->getUser());

  while (!Worklist.empty()) {
    SILInstruction *U = Worklist.pop_back_val();
    if (!SeenInsts.insert(U).second)
      continue;

    // If U is a terminator inst, return false.
    if (isa<TermInst>(U))
      return true;

    // If U has side effects...
    if (U->mayHaveSideEffects()) 
      return true;

    // Otherwise add all non-debug uses of I to the worklist.
    for (Operand *I : getNonDebugUses(SILValue(U)))
      Worklist.push_back(I->getUser());
  }
  return false;
}

bool DeadArgumentTransform::analyzeParameters() {
  // Did we decide we should optimize any parameter?
  bool SignatureOptimize = false;
  ArrayRef<SILArgument *> Args = F->begin()->getBBArgs();

  // Analyze the argument information.
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgumentDescriptor &A = ArgDescList[i];

    // Check whether argument is dead.
    A.IsEntirelyDead = true;
    A.IsEntirelyDead &= !isArgumentABIRequired(Args[i]);
    A.IsEntirelyDead &= !hasNonTrivialNonDebugUse(Args[i]); 
    SignatureOptimize |= A.IsEntirelyDead;

    if (A.IsEntirelyDead && Args[i]->isSelf())
     shouldModifySelfArgument = true;

  }
  return SignatureOptimize;
}

void DeadArgumentTransform::transformParameters() {
  SILBasicBlock *BB = &*NewF->begin();
  // Remove any dead argument starting from the last argument to the first.
  for (const ArgumentDescriptor &AD : reverse(ArgDescList)) {
    if (!AD.IsEntirelyDead)
      continue;
    SILArgument *Arg = BB->getBBArg(AD.Index);
    eraseUsesOfValue(Arg);
    BB->eraseBBArg(AD.Index);
  }
}

void DeadArgumentTransform::
computeOptimizedInterface(ArgumentDescriptor &AD, SILParameterInfoList &Out) {
  // If this argument is live, but we cannot optimize it.
  if (AD.IsEntirelyDead)
    return;

  // Otherwise just propagate through the parameter info.
  auto ParameterInfo = AD.Arg->getKnownParameterInfo();
  Out.push_back(ParameterInfo);
}

//===----------------------------------------------------------------------===//
//                        Dead Argument Optimization 
//===----------------------------------------------------------------------===//

/// ArgumentExplosionTransform - Owned to Guanranteed optimization.
class ArgumentExplosionTransform : public FunctionSignatureTransform {
public:
  ArgumentExplosionTransform(SILFunction *F, llvm::BumpPtrAllocator &BPA,
                             AliasAnalysis *AA, RCIdentityAnalysis *RCIA,
                             FunctionSignatureMangler &FM)
    : FunctionSignatureTransform(F, BPA, AA, RCIA, FM) {}

  /// virtual destructor.
  virtual ~ArgumentExplosionTransform() {}

  /// Analyze the function and decide whether to optimize based on the function
  /// signature.
  bool analyzeParameters();

  /// Transform the parameters and result of the function.
  void transformParameters();

  /// Simply add the function argument.
  void addThunkArgument(ArgumentDescriptor &AD, SILBuilder &Builder,
                        SILBasicBlock *BB,
                        llvm::SmallVectorImpl<SILValue> &NewArgs) {
    if (!AD.Explode) {
       NewArgs.push_back(BB->getBBArg(AD.Index));
       return;
    }

    // Explode the argument.
    ++NumSROAArguments;
    AD.ProjTree.createTreeFromValue(Builder, BB->getParent()->getLocation(),
                                    BB->getBBArg(AD.Index), NewArgs);
  }

  /// Compute what the function name will be based on the given result decriptor.
  void computeOptimizedName(ArgumentDescriptor &AD, FunctionSignatureMangler &M) {
    if (!AD.Explode)
      return;
    M.setArgumentSROA(AD.Index);
  }

  void computeOptimizedInterface(ArgumentDescriptor &A, SILParameterInfoList &Out);
};

bool ArgumentExplosionTransform::analyzeParameters() {
  // Did we decide we should optimize any parameter?
  bool SignatureOptimize = false;
  ArrayRef<SILArgument *> Args = F->begin()->getBBArgs();

  // Analyze the argument information.
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgumentDescriptor &A = ArgDescList[i];

    A.ProjTree.computeUsesAndLiveness(A.Arg);
    A.Explode = A.shouldExplode();

    SignatureOptimize |= A.Explode;
  }
  return SignatureOptimize;
}

void ArgumentExplosionTransform::transformParameters() {
  SILBasicBlock *BB = &*NewF->begin();
  SILBuilder Builder(BB->begin());
  Builder.setCurrentDebugScope(BB->getParent()->getDebugScope());
  for (ArgumentDescriptor &AD : reverse(ArgDescList)) {
    // Simply continue if do not explode.
    if (!AD.Explode)
      continue;

    // OK, we need to explode this argument.
    unsigned ArgOffset = AD.Index + 1;
    llvm::SmallVector<SILValue, 8> LeafValues;

    // We do this in the same order as leaf types since ProjTree expects that the
    // order of leaf values matches the order of leaf types.
    llvm::SmallVector<const ProjectionTreeNode*, 8> LeafNodes;
    AD.ProjTree.getLeafNodes(LeafNodes);
    for (auto Node : LeafNodes) {
      LeafValues.push_back(BB->insertBBArg(
          ArgOffset++, Node->getType(), BB->getBBArg(AD.Index)->getDecl()));
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
    SILArgument *OrigArg = BB->getBBArg(AD.Index);
    OrigArg->replaceAllUsesWith(NewOrigArgValue);

    // Now erase the old argument since it does not have any uses. We also
    // decrement ArgOffset since we have one less argument now.
    BB->eraseBBArg(AD.Index);
  }
}

void ArgumentExplosionTransform::
computeOptimizedInterface(ArgumentDescriptor &AD, SILParameterInfoList &Out) {
  auto PInfo = AD.Arg->getKnownParameterInfo();
  // We are not exploding the argument.
  if (!AD.Explode) {
    Out.push_back(PInfo);
    return;
  }

  llvm::SmallVector<const ProjectionTreeNode*, 8> LeafNodes;
  AD.ProjTree.getLeafNodes(LeafNodes);
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

    // Ty is not trivial, pass it through as the original calling convention.
    SILParameterInfo NewInfo(Ty.getSwiftRValueType(), PInfo.getConvention());
    Out.push_back(NewInfo);
  }
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//
namespace {
class FunctionSignatureOpts : public SILFunctionTransform {
  /// This is the function to analyze and optimize.
  SILFunction *OptFun;
  SILFunction *getFunctionToOptimize() { return OptFun; }
  void setFunctionToOptimize(SILFunction *F) { OptFun = F; }
public:
  /// constructor.
  FunctionSignatureOpts() : OptFun(nullptr) {}
  void run() override {
    auto *F = getFunction();
    DEBUG(llvm::dbgs() << "*** FSO on function: " << F->getName() << " ***\n");

    // Don't optimize callees that should not be optimized.
    if (!F->shouldOptimize())
      return;

    // Check the signature of F to make sure that it is a function that we
    // can specialize. These are conditions independent of the call graph.
    if (!canSpecializeFunction(F))
      return;

    // Does this function have a caller inside this module.
    auto *CA = PM->getAnalysis<CallerAnalysis>();
    bool hasCaller = CA->hasCaller(F);

    // If this function does not have a direct caller in the current module
    // and maybe called indirectly, e.g. from virtual table do not function
    // signature specialize it, as this will introduce a thunk.
    if (!hasCaller && canBeCalledIndirectly(F->getRepresentation()))
      return; 

    // This is the function to optimize.
    setFunctionToOptimize(getFunction());

    // As we optimize the function more and more, the name of the function is
    // going to change, make sure the mangler is aware of all the changes done
    // to the function.
    Mangle::Mangler M;
    auto P = SpecializationPass::FunctionSignatureOpts;
    FunctionSignatureMangler FM(P, M, F->isFragile(), F);

    llvm::BumpPtrAllocator BPA;
    auto *RCIA = getAnalysis<RCIdentityAnalysis>();
    auto *AA = PM->getAnalysis<AliasAnalysis>();

    // We always consider function for owned-to-guaranteed transformation
    // even if the function has no caller, because owned-to-guaranteed is
    // considered as a highly profitable optimization.
    bool OwnedToGuaranteed = false;
    bool Changed = false;

    // We run function signature optimization in the following sequence.
    //
    // OwnedToGuaranteed enables dead argument elimination, and dead argument
    // elimination gives opportunities for argument explosion.
    //
    // Owned to Guaranteed optimization.
    OwnedToGuaranteedTransform OG(getFunctionToOptimize(), BPA, AA, RCIA, FM);
    if (OG.analyze()) {
      Changed = true;
      OwnedToGuaranteed = true;
      setFunctionToOptimize(OG.transform());
    }

    // From this point hence, we only optimize if the function has a caller or
    // we have owned-to-guaranteed this function already, i.e. we have
    // introduced a thunk.
    //
    // Dead argument elimination optimization.
    DeadArgumentTransform DA(getFunctionToOptimize(), BPA, AA, RCIA, FM);
    if ((OwnedToGuaranteed || hasCaller) && DA.analyze()) {
      Changed = true;
      setFunctionToOptimize(DA.transform());
    }

    // Argument explosion optimization.
    ArgumentExplosionTransform AE(getFunctionToOptimize(), BPA, AA, RCIA, FM);
    if ((OwnedToGuaranteed || hasCaller) && AE.analyze()) {
      Changed = true;
      setFunctionToOptimize(AE.transform());
    }

    // If we manage to optimize the function ...
    if (Changed) { 
      // The old function must be a thunk now.
      assert(F->isThunk() && "Old function should have been turned into a thunk");

      // Collapse the chain of thunks.
      collapseThunkChain(F, OptFun, PM);

      // The thunk now carries the information on how the signature is
      // optimized. If we inline the thunk, we will get the benefit of calling
      // the signature optimized function without additional setup on the
      // caller side.
      F->setInlineStrategy(AlwaysInline);

      // Make sure the PM knows about this function. This will also help us
      // with self-recursion.
      notifyPassManagerOfFunction(OptFun);

      // Invalidate analyses on old function.
      PM->invalidateAnalysis(F, SILAnalysis::InvalidationKind::Everything);
      PM->invalidateAnalysis(OptFun, SILAnalysis::InvalidationKind::Everything);
      F->verify();
      OptFun->verify();
      ++ NumFunctionSignaturesOptimized;
    }
  }

  StringRef getName() override { return "Function Signature Optimization"; }
};

} // end anonymous namespace

SILTransform *swift::createFunctionSignatureOpts() {
  return new FunctionSignatureOpts();
}

//===--- FunctionSignatureOpts.cpp - Optimizes function signatures --------===//
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
///
/// \file
///
/// This pass defines function signature related optimizations.
/// When a function signature optimization is performed, changes are made to
/// the original function and after all function signature optimizations are
/// finished, a new function is created and the old function is turned into
/// a thunk.
///
/// Another possibility is to implement these optimizations as separate passes,
/// but then we would send slightly different functions to the pass pipeline
/// multiple times through notifyPassManagerOfFunction. 
///
/// TODO: Optimize function with generic parameters.
///
/// TODO: Improve epilogue release matcher, i.e. do a data flow instead of
/// only finding releases in the return block. 
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-function-signature-opt"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/CallerAnalysis.h"
#include "swift/SILOptimizer/Analysis/EpilogueARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/FunctionSignatureOptUtils.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
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
using ArgumentIndexMap = llvm::SmallDenseMap<int, int>;

//===----------------------------------------------------------------------===//
//                           Utilities
//===----------------------------------------------------------------------===//

/// Return the single return value of the function.
static SILValue findReturnValue(SILFunction *F) {
  auto RBB = F->findReturnBB();
  if (RBB == F->end())
    return SILValue();
  auto Term = dyn_cast<ReturnInst>(RBB->getTerminator());
  return Term->getOperand();
}

/// Return the single apply found in this function.
static SILInstruction *findOnlyApply(SILFunction *F) {
  SILInstruction *OnlyApply = nullptr;
  for (auto &B : *F) {
    for (auto &X : B) {
      if (!isa<ApplyInst>(X) && !isa<TryApplyInst>(X))
        continue;
      assert(!OnlyApply && "There are more than 1 function calls");
      OnlyApply = &X;
    }
  }
  assert(OnlyApply && "There is no function calls");
  return OnlyApply;
}

/// Return a unique name in the current module. We should not be blocked
/// from being able to FSO a function just because we have a name conflict.
///
/// TODO: we should teach the demangler to understand this suffix.
static std::string getUniqueName(std::string Name, SILModule &M) {
  if (!M.hasFunction(Name))
    return Name;
  return getUniqueName(Name + "_unique_suffix", M);
}

//===----------------------------------------------------------------------===//
//                     Function Signature Transformation 
//===----------------------------------------------------------------------===//
class FunctionSignatureTransform {
  /// The actual function to analyze and transform.
  SILFunction *F;

  /// The newly created function.
  SILFunction *NewF;

  /// The RC identity analysis we are using.
  RCIdentityAnalysis *RCIA;

  /// Post order analysis we are using.
  EpilogueARCAnalysis *EA;

  // The function signature mangler we are using.
  FunctionSignatureSpecializationMangler &OldFM;
  NewMangling::FunctionSignatureSpecializationMangler &NewFM;

  // Keep tracks to argument mapping.
  ArgumentIndexMap &AIM;

  // Self argument is modified.
  bool shouldModifySelfArgument;

  /// Keep a "view" of precompiled information on arguments that we use 
  /// during our optimization.
  llvm::SmallVector<ArgumentDescriptor, 4> &ArgumentDescList;

  /// Keep a "view" of precompiled information on the direct results that we
  /// will use during our optimization.
  llvm::SmallVector<ResultDescriptor, 4> &ResultDescList;

  /// Return a function name based on ArgumentDescList and ResultDescList.
  std::string createOptimizedSILFunctionName();

  /// Return a function type based on ArgumentDescList and ResultDescList.
  CanSILFunctionType createOptimizedSILFunctionType();

private:
  /// ----------------------------------------------------------///
  /// Dead argument transformation.                             ///
  /// ----------------------------------------------------------///
  /// Find any dead argument opportunities.
  bool DeadArgumentAnalyzeParameters();
  /// Modify the current function so that later function signature analysis
  /// are more effective.
  void DeadArgumentTransformFunction();
  /// Remove the dead argument once the new function is created.
  void DeadArgumentFinalizeOptimizedFunction();

  /// ----------------------------------------------------------///
  /// Owned to guaranteed transformation.                       ///
  /// ----------------------------------------------------------///
  bool OwnedToGuaranteedAnalyzeResults();
  bool OwnedToGuaranteedAnalyzeParameters();

  /// Modify the current function so that later function signature analysis
  /// are more effective.
  void OwnedToGuaranteedTransformFunctionResults();
  void OwnedToGuaranteedTransformFunctionParameters();

  /// Find any owned to guaranteed opportunities.
  bool OwnedToGuaranteedAnalyze() {
    bool Result = OwnedToGuaranteedAnalyzeResults();
    bool Params = OwnedToGuaranteedAnalyzeParameters();
    return Params || Result;
  }

  /// Do the actual owned to guaranteed transformations.
  void OwnedToGuaranteedTransform() {
    OwnedToGuaranteedTransformFunctionResults();
    OwnedToGuaranteedTransformFunctionParameters();
  }

  /// Set up epilogue work for the thunk result based in the given argument.
  void OwnedToGuaranteedAddResultRelease(ResultDescriptor &RD,
                                         SILBuilder &Builder,
                                         SILFunction *F);

  /// Set up epilogue work for the thunk argument based in the given argument.
  void OwnedToGuaranteedAddArgumentRelease(ArgumentDescriptor &AD,
                                           SILBuilder &Builder,
                                           SILFunction *F); 

  /// Add the release for converted arguments and result.
  void OwnedToGuaranteedFinalizeThunkFunction(SILBuilder &B, SILFunction *F);

  /// ----------------------------------------------------------///
  /// Argument explosion transformation.                        ///
  /// ----------------------------------------------------------///
  /// Find any argument explosion opportunities.
  bool ArgumentExplosionAnalyzeParameters();
  /// Explode the argument in the optimized function and replace the uses of
  /// the original argument.
  void ArgumentExplosionFinalizeOptimizedFunction();

  /// Setup the thunk arguments based on the given argument descriptor info.
  /// Every transformation must defines this interface. Default implementation
  /// simply passes it through.
  void addThunkArgument(ArgumentDescriptor &AD, SILBuilder &Builder,
                        SILBasicBlock *BB, 
                        llvm::SmallVectorImpl<SILValue> &NewArgs) {
    // Dead argument.
    if (AD.IsEntirelyDead) {
      return;
    }

    // Explode the argument.
    if (AD.Explode) {
      llvm::SmallVector<SILValue, 4> LeafValues;
      AD.ProjTree.createTreeFromValue(Builder, BB->getParent()->getLocation(),
                                      BB->getArgument(AD.Index), LeafValues);
      NewArgs.append(LeafValues.begin(), LeafValues.end());
      return;
    }

    // All other arguments get pushed as what they are.
    NewArgs.push_back(BB->getArgument(AD.Index));
  } 

  /// Take ArgumentDescList and ResultDescList and create an optimized function
  /// based on the current function we are analyzing. This also has the side effect
  /// of turning the current function into a thunk.
  void createFunctionSignatureOptimizedFunction();

  /// Compute the optimized function type based on the given argument descriptor.
  void computeOptimizedArgInterface(ArgumentDescriptor &A, SILParameterInfoList &O);

public:
  /// Constructor.
  FunctionSignatureTransform(SILFunction *F,
                             RCIdentityAnalysis *RCIA, EpilogueARCAnalysis *EA,
                             FunctionSignatureSpecializationMangler &OldFM,
                    NewMangling::FunctionSignatureSpecializationMangler &NewFM,
                             ArgumentIndexMap &AIM,
                             llvm::SmallVector<ArgumentDescriptor, 4> &ADL,
                             llvm::SmallVector<ResultDescriptor, 4> &RDL)
    : F(F), NewF(nullptr), RCIA(RCIA), EA(EA), OldFM(OldFM), NewFM(NewFM),
      AIM(AIM), shouldModifySelfArgument(false), ArgumentDescList(ADL),
      ResultDescList(RDL) {}

  /// Return the optimized function.
  SILFunction *getOptimizedFunction() { return NewF; }

  /// Run the optimization.
  bool run(bool hasCaller) {
    bool Changed = false;

    if (!hasCaller && canBeCalledIndirectly(F->getRepresentation())) {
      DEBUG(llvm::dbgs() << "  function has no caller -> abort\n");
      return false;
    }

    // Run OwnedToGuaranteed optimization.
    if (OwnedToGuaranteedAnalyze()) {
      Changed = true;
      DEBUG(llvm::dbgs() << "  transform owned-to-guaranteed\n");
      OwnedToGuaranteedTransform();
    }

    // Run DeadArgument elimination transformation. We only specialize
    // if this function has a caller inside the current module or we have
    // already created a thunk.
    if ((hasCaller || Changed) && DeadArgumentAnalyzeParameters()) {
      Changed = true;
      DEBUG(llvm::dbgs() << "  remove dead arguments\n");
      DeadArgumentTransformFunction();
    }

    // Run ArgumentExplosion transformation. We only specialize
    // if this function has a caller inside the current module or we have
    // already created a thunk.
    //
    // NOTE: we run argument explosion last because we've already initialized
    // the ArgumentDescList to have unexploded number of arguments. Exploding
    // it without changing the argument count is not going to help with
    // owned-to-guaranteed transformation. 
    // 
    // In order to not miss any opportunity, we send the optimized function
    // to the passmanager to optimize any opportunities exposed by argument
    // explosion.
    if ((hasCaller || Changed) && ArgumentExplosionAnalyzeParameters()) {
      Changed = true;
    }

    // Create the specialized function and invalidate the old function.
    if (Changed) {
      createFunctionSignatureOptimizedFunction();
    }
    return Changed;
  }

  /// Run dead argument elimination of partially applied functions.
  /// After this optimization CapturePropagation can replace the partial_apply
  /// by a direct reference to the specialized function.
  bool removeDeadArgs(int minPartialAppliedArgs) {
    if (minPartialAppliedArgs < 1)
      return false;

    if (!DeadArgumentAnalyzeParameters())
      return false;

    // Check if at least the minimum number of partially applied arguments
    // are dead. Otherwise no partial_apply can be removed anyway.
    for (unsigned Idx = 0, Num = ArgumentDescList.size(); Idx < Num; ++Idx) {
      if (Idx < Num - minPartialAppliedArgs) {
        // Don't remove arguments other than the partial applied ones, even if
        // they are dead.
        ArgumentDescList[Idx].IsEntirelyDead = false;
      } else {
        // Is the partially applied argument dead?
        if (!ArgumentDescList[Idx].IsEntirelyDead)
          return false;
        
        // Currently we require that all dead parameters have trivial types.
        // The reason is that it's very hard to find places where we can release
        // those parameters (as a replacement for the removed partial_apply).
        // TODO: maybe we can skip this restriction when we have semantic ARC.
        if (!ArgumentDescList[Idx].Arg->getType().isTrivial(F->getModule()))
          return false;
      }
    }

    DEBUG(llvm::dbgs() << "  remove dead arguments for partial_apply\n");
    DeadArgumentTransformFunction();
    createFunctionSignatureOptimizedFunction();
    return true;
  }
};

std::string FunctionSignatureTransform::createOptimizedSILFunctionName() {
  // Handle arguments' changes.
  for (unsigned i : indices(ArgumentDescList)) {
    const ArgumentDescriptor &Arg = ArgumentDescList[i];
    if (Arg.IsEntirelyDead) {
      OldFM.setArgumentDead(i);
      NewFM.setArgumentDead(i);
      // No point setting other attribute if argument is dead.
      continue;
    }   

    // If we have an @owned argument and found a callee release for it,
    // convert the argument to guaranteed.
    if (Arg.OwnedToGuaranteed) {
      OldFM.setArgumentOwnedToGuaranteed(i);
      NewFM.setArgumentOwnedToGuaranteed(i);
    }

    // If this argument is not dead and we can explode it, add 's' to the
    // mangling.
    if (Arg.Explode) {
      OldFM.setArgumentSROA(i);
      NewFM.setArgumentSROA(i);
    }
  }

  // Handle return value's change.
  // FIXME: handle multiple direct results here
  if (ResultDescList.size() == 1 && !ResultDescList[0].CalleeRetain.empty()) {
    OldFM.setReturnValueOwnedToUnowned();
    NewFM.setReturnValueOwnedToUnowned();
  }

  OldFM.mangle();
  SILModule &M = F->getModule();
  std::string Old = getUniqueName(OldFM.getMangler().finalize(), M);

  int UniqueID = 0;
  std::string New;
  do {
    New = NewFM.mangle(UniqueID);
    ++UniqueID;
  } while (M.hasFunction(New));

  return NewMangling::selectMangling(Old, New);
}

/// Compute what the function interface will look like based on the
/// optimization we are doing on the given argument descriptor. Default
/// implementation simply passes it through.
void
FunctionSignatureTransform::
computeOptimizedArgInterface(ArgumentDescriptor &AD, SILParameterInfoList &Out) {
  // If this argument is live, but we cannot optimize it.
  if (!AD.canOptimizeLiveArg()) {
    Out.push_back(AD.PInfo);
    return;
  }

  // If we have a dead argument, bail.
  if (AD.IsEntirelyDead) {
    ++NumDeadArgsEliminated;
    return;
  }

  // Explode the argument or not ?
  if (AD.Explode) {
    ++NumSROAArguments;
    llvm::SmallVector<const ProjectionTreeNode*, 8> LeafNodes;
    AD.ProjTree.getLeafNodes(LeafNodes);
    for (auto Node : LeafNodes) {
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
      SILParameterInfo NewInfo(Ty.getSwiftRValueType(), AD.OwnedToGuaranteed ? 
                               ParameterConvention::Direct_Guaranteed : 
                               AD.PInfo.getConvention());
      Out.push_back(NewInfo);
    }
    return;
  }

  // If we cannot explode this value, handle callee release and return.
  // If we found releases in the callee in the last BB on an @owned
  // parameter, change the parameter to @guaranteed and continue...
  if (AD.OwnedToGuaranteed) {
    ++NumOwnedConvertedToGuaranteed;
    SILParameterInfo NewInfo(AD.PInfo.getType(),
                             ParameterConvention::Direct_Guaranteed);
    Out.push_back(NewInfo);
    return;
  }

  // Otherwise just propagate through the parameter info.
  Out.push_back(AD.PInfo);
}

CanSILFunctionType FunctionSignatureTransform::createOptimizedSILFunctionType() {
  CanSILFunctionType FTy = F->getLoweredFunctionType();
  // The only way that we modify the arity of function parameters is here for
  // dead arguments. Doing anything else is unsafe since by definition non-dead
  // arguments will have SSA uses in the function. We would need to be smarter
  // in our moving to handle such cases.
  llvm::SmallVector<SILParameterInfo, 8> InterfaceParams;
  for (auto &ArgDesc : ArgumentDescList) {
    computeOptimizedArgInterface(ArgDesc, InterfaceParams);
  }

  // ResultDescs only covers the direct results; we currently can't ever
  // change an indirect result.  Piece the modified direct result information
  // back into the all-results list.
  llvm::SmallVector<SILResultInfo, 8> InterfaceResults;
  auto &ResultDescs = ResultDescList;
  for (SILResultInfo InterfaceResult : FTy->getResults()) {
    if (InterfaceResult.isFormalDirect()) {
      auto &RV = ResultDescs[0];
      if (!RV.CalleeRetain.empty()) {
        ++NumOwnedConvertedToNotOwnedResult;
        InterfaceResults.push_back(SILResultInfo(InterfaceResult.getType(),
                                                 ResultConvention::Unowned));
        continue;
      }
    }

    InterfaceResults.push_back(InterfaceResult);
  }

  // Don't use a method representation if we modified self.
  auto ExtInfo = FTy->getExtInfo();
  if (shouldModifySelfArgument) {
    ExtInfo = ExtInfo.withRepresentation(SILFunctionTypeRepresentation::Thin);
  }

  return SILFunctionType::get(FTy->getGenericSignature(), ExtInfo,
                              FTy->getCalleeConvention(), InterfaceParams,
                              InterfaceResults, FTy->getOptionalErrorResult(),
                              F->getModule().getASTContext());
}

void FunctionSignatureTransform::createFunctionSignatureOptimizedFunction() {
  // Create the optimized function !
  SILModule &M = F->getModule();
  std::string Name = createOptimizedSILFunctionName();
  SILLinkage linkage = F->getLinkage();
  if (isAvailableExternally(linkage))
    linkage = SILLinkage::Shared;

  DEBUG(llvm::dbgs() << "  -> create specialized function " << Name << "\n");

  NewF = M.createFunction(linkage, Name, createOptimizedSILFunctionType(),
                          nullptr, F->getLocation(), F->isBare(),
                          F->isTransparent(), F->isFragile(), F->isThunk(),
                          F->getClassVisibility(), F->getInlineStrategy(),
                          F->getEffectsKind(), nullptr, F->getDebugScope());
  if (F->hasUnqualifiedOwnership()) {
    NewF->setUnqualifiedOwnership();
  }

  // Then we transfer the body of F to NewF.
  NewF->spliceBody(F);

  // Array semantic clients rely on the signature being as in the original
  // version.
  for (auto &Attr : F->getSemanticsAttrs()) {
    if (!StringRef(Attr).startswith("array."))
      NewF->addSemanticsAttr(Attr);
  }

  // Do the last bit of work to the newly created optimized function.
  ArgumentExplosionFinalizeOptimizedFunction();
  DeadArgumentFinalizeOptimizedFunction();

  // Create the thunk body !
  F->setThunk(IsThunk);
  // The thunk now carries the information on how the signature is
  // optimized. If we inline the thunk, we will get the benefit of calling
  // the signature optimized function without additional setup on the
  // caller side.
  F->setInlineStrategy(AlwaysInline);
  SILBasicBlock *ThunkBody = F->createBasicBlock();
  for (auto &ArgDesc : ArgumentDescList) {
    ThunkBody->createFunctionArgument(ArgDesc.Arg->getType(), ArgDesc.Decl);
  }

  SILLocation Loc = ThunkBody->getParent()->getLocation();
  SILBuilder Builder(ThunkBody);
  Builder.setCurrentDebugScope(ThunkBody->getParent()->getDebugScope());

  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  // Create the args for the thunk's apply, ignoring any dead arguments.
  llvm::SmallVector<SILValue, 8> ThunkArgs;
  for (auto &ArgDesc : ArgumentDescList) {
    addThunkArgument(ArgDesc, Builder, ThunkBody, ThunkArgs);
  }

  // We are ignoring generic functions and functions with out parameters for
  // now.
  SILValue ReturnValue;
  SILType LoweredType = NewF->getLoweredType();
  SILType ResultType = NewF->getConventions().getSILResultType();
  auto FunctionTy = LoweredType.castTo<SILFunctionType>();
  if (FunctionTy->hasErrorResult()) {
    // We need a try_apply to call a function with an error result.
    SILFunction *Thunk = ThunkBody->getParent();
    SILBasicBlock *NormalBlock = Thunk->createBasicBlock();
    ReturnValue =
        NormalBlock->createPHIArgument(ResultType, ValueOwnershipKind::Owned);
    SILBasicBlock *ErrorBlock = Thunk->createBasicBlock();
    SILType Error =
        SILType::getPrimitiveObjectType(FunctionTy->getErrorResult().getType());
    auto *ErrorArg =
        ErrorBlock->createPHIArgument(Error, ValueOwnershipKind::Owned);
    Builder.createTryApply(Loc, FRI, LoweredType, SubstitutionList(),
                           ThunkArgs, NormalBlock, ErrorBlock);

    Builder.setInsertionPoint(ErrorBlock);
    Builder.createThrow(Loc, ErrorArg);
    Builder.setInsertionPoint(NormalBlock);
  } else {
    ReturnValue = Builder.createApply(Loc, FRI, LoweredType, ResultType,
                                      SubstitutionList(), ThunkArgs,
                                      false);
  }

  // Set up the return results.
  if (NewF->isNoReturnFunction()) {
    Builder.createUnreachable(Loc);
  } else {
    Builder.createReturn(Loc, ReturnValue);
  }

  // Do the last bit work to finalize the thunk.
  OwnedToGuaranteedFinalizeThunkFunction(Builder, F);
  assert(F->getDebugScope()->Parent != NewF->getDebugScope()->Parent);
}

/// ----------------------------------------------------------///
/// Dead argument transformation.                             ///
/// ----------------------------------------------------------///
bool FunctionSignatureTransform::DeadArgumentAnalyzeParameters() {
  // Did we decide we should optimize any parameter?
  bool SignatureOptimize = false;
  auto Args = F->begin()->getFunctionArguments();

  // Analyze the argument information.
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgumentDescriptor &A = ArgumentDescList[i];
    if (!A.canOptimizeLiveArg()) {
      continue;
    }

    // Check whether argument is dead.
    if (!hasNonTrivialNonDebugUse(Args[i])) {
      A.IsEntirelyDead = true;
      SignatureOptimize = true;
      if (Args[i]->isSelf())
        shouldModifySelfArgument = true;
    }
  }
  return SignatureOptimize;
}

void FunctionSignatureTransform::DeadArgumentTransformFunction() {
  SILBasicBlock *BB = &*F->begin();
  for (const ArgumentDescriptor &AD : ArgumentDescList) {
    if (!AD.IsEntirelyDead)
      continue;
    eraseUsesOfValue(BB->getArgument(AD.Index));
  }
}

void FunctionSignatureTransform::DeadArgumentFinalizeOptimizedFunction() {
  auto *BB = &*NewF->begin();
  // Remove any dead argument starting from the last argument to the first.
  for (const ArgumentDescriptor &AD : reverse(ArgumentDescList)) {
    if (!AD.IsEntirelyDead)
      continue;
    BB->eraseArgument(AD.Arg->getIndex());
  }
}

/// ----------------------------------------------------------///
/// Owned to Guaranteed transformation.                       ///
/// ----------------------------------------------------------///
bool FunctionSignatureTransform::OwnedToGuaranteedAnalyzeParameters() {
  auto Args = F->begin()->getFunctionArguments();
  // A map from consumed SILArguments to the release associated with an
  // argument.
  //
  // TODO: The return block and throw block should really be abstracted away.
  ConsumedArgToEpilogueReleaseMatcher ArgToReturnReleaseMap(RCIA->get(F), F);
  ConsumedArgToEpilogueReleaseMatcher ArgToThrowReleaseMap(
      RCIA->get(F), F, ConsumedArgToEpilogueReleaseMatcher::ExitKind::Throw);

  // Did we decide we should optimize any parameter?
  bool SignatureOptimize = false;

  // Analyze the argument information.
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgumentDescriptor &A = ArgumentDescList[i];
    if (!A.canOptimizeLiveArg()) {
      continue;
    }

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

    // Modified self argument.
    if (A.OwnedToGuaranteed && Args[i]->isSelf()) {
      shouldModifySelfArgument = true;
    }
  }
  return SignatureOptimize;
}

bool FunctionSignatureTransform::OwnedToGuaranteedAnalyzeResults() {
  auto fnConv = F->getConventions();
  // For now, only do anything if there's a single direct result.
  if (fnConv.getNumDirectSILResults() != 1)
    return false; 

  bool SignatureOptimize = false;
  if (ResultDescList[0].hasConvention(ResultConvention::Owned)) {
    auto RV = findReturnValue(F);
    if (!RV)
      return false;
    auto &RI = ResultDescList[0];
    // We have an @owned return value, find the epilogue retains now.
    auto Retains = EA->get(F)->computeEpilogueARCInstructions(EpilogueARCContext::EpilogueARCKind::Retain, RV); 
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

void FunctionSignatureTransform::OwnedToGuaranteedTransformFunctionParameters() {
  // And remove all Callee releases that we found and made redundant via owned
  // to guaranteed conversion.
  for (const ArgumentDescriptor &AD : ArgumentDescList) {
    if (!AD.OwnedToGuaranteed)
      continue;
    for (auto &X : AD.CalleeRelease) { 
      X->eraseFromParent();
    }
    for (auto &X : AD.CalleeReleaseInThrowBlock) { 
      X->eraseFromParent();
    }

    // Now we need to replace the FunctionArgument so that we have the correct
    // ValueOwnershipKind.
    AD.Arg->setOwnershipKind(ValueOwnershipKind::Guaranteed);
  }
}

void FunctionSignatureTransform::OwnedToGuaranteedTransformFunctionResults() {
  // And remove all callee retains that we found and made redundant via owned
  // to unowned conversion.
  for (const ResultDescriptor &RD : ResultDescList) {
    if (!RD.OwnedToGuaranteed)
      continue;
    for (auto &X : RD.CalleeRetain) {
      if (isa<StrongRetainInst>(X) || isa<RetainValueInst>(X)) {
        X->eraseFromParent();
        continue;
      }
      // Create a release to balance it out.
      assert(isa<ApplyInst>(X) && "Unknown epilogue retain");
      createDecrementBefore(X, dyn_cast<ApplyInst>(X)->getParent()->getTerminator());
    }
  }
}

void FunctionSignatureTransform::
OwnedToGuaranteedFinalizeThunkFunction(SILBuilder &Builder, SILFunction *F) {
  // Finish the epilogue work for the argument as well as result.
  for (auto &ArgDesc : ArgumentDescList) {
    OwnedToGuaranteedAddArgumentRelease(ArgDesc, Builder, F);
  }
  for (auto &ResDesc : ResultDescList) {
    OwnedToGuaranteedAddResultRelease(ResDesc, Builder, F);
  }
}

/// Set up epilogue work for the thunk arguments based in the given argument.
/// Default implementation simply passes it through.
void
FunctionSignatureTransform::
OwnedToGuaranteedAddArgumentRelease(ArgumentDescriptor &AD, SILBuilder &Builder,
                                    SILFunction *F) {
  // If we have any arguments that were consumed but are now guaranteed,
  // insert a release_value.
  if (!AD.OwnedToGuaranteed) {
    return;
  }

  SILInstruction *Call = findOnlyApply(F);
  if (isa<ApplyInst>(Call)) {
    Builder.setInsertionPoint(&*std::next(SILBasicBlock::iterator(Call)));
    Builder.createReleaseValue(RegularLocation(SourceLoc()),
                               F->getArguments()[AD.Index],
                               Builder.getDefaultAtomicity());
  } else {
    SILBasicBlock *NormalBB = dyn_cast<TryApplyInst>(Call)->getNormalBB();
    Builder.setInsertionPoint(&*NormalBB->begin());
    Builder.createReleaseValue(RegularLocation(SourceLoc()),
                               F->getArguments()[AD.Index],
                               Builder.getDefaultAtomicity());

    SILBasicBlock *ErrorBB = dyn_cast<TryApplyInst>(Call)->getErrorBB();
    Builder.setInsertionPoint(&*ErrorBB->begin());
    Builder.createReleaseValue(RegularLocation(SourceLoc()),
                               F->getArguments()[AD.Index],
                               Builder.getDefaultAtomicity());
  }
}

void
FunctionSignatureTransform::
OwnedToGuaranteedAddResultRelease(ResultDescriptor &RD, SILBuilder &Builder,
                                  SILFunction *F) {
 // If we have any result that were consumed but are now guaranteed,
  // insert a release_value.
  if (!RD.OwnedToGuaranteed) {
    return;
  }

  SILInstruction *Call = findOnlyApply(F);
  if (isa<ApplyInst>(Call)) {
    Builder.setInsertionPoint(&*std::next(SILBasicBlock::iterator(Call)));
    Builder.createRetainValue(RegularLocation(SourceLoc()), Call,
                              Builder.getDefaultAtomicity());
  } else {
    SILBasicBlock *NormalBB = dyn_cast<TryApplyInst>(Call)->getNormalBB();
    Builder.setInsertionPoint(&*NormalBB->begin());
    Builder.createRetainValue(RegularLocation(SourceLoc()),
                              NormalBB->getArgument(0), Builder.getDefaultAtomicity());
  }
}

/// ----------------------------------------------------------///
/// Argument Explosion transformation.                        ///
/// ----------------------------------------------------------///
bool FunctionSignatureTransform::ArgumentExplosionAnalyzeParameters() {
  // Did we decide we should optimize any parameter?
  bool SignatureOptimize = false;
  auto Args = F->begin()->getFunctionArguments();
  ConsumedArgToEpilogueReleaseMatcher ArgToReturnReleaseMap(RCIA->get(F), F);

  // Analyze the argument information.
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgumentDescriptor &A = ArgumentDescList[i];
    // Do not optimize argument.
    if (!A.canOptimizeLiveArg()) {
      continue;
    }

    A.ProjTree.computeUsesAndLiveness(A.Arg);
    A.Explode = A.shouldExplode(ArgToReturnReleaseMap);

    // Modified self argument.
    if (A.Explode && Args[i]->isSelf()) {
      shouldModifySelfArgument = true;
    }

    SignatureOptimize |= A.Explode;
  }
  return SignatureOptimize;
}

void FunctionSignatureTransform::ArgumentExplosionFinalizeOptimizedFunction() {
  SILBasicBlock *BB = &*NewF->begin();
  SILBuilder Builder(BB->begin());
  Builder.setCurrentDebugScope(BB->getParent()->getDebugScope());
  unsigned TotalArgIndex = 0;
  for (ArgumentDescriptor &AD : ArgumentDescList) {
    // Simply continue if do not explode.
    if (!AD.Explode) {
      AIM[TotalArgIndex] = AD.Index;
      TotalArgIndex ++;
      continue;
    }

    // OK, we need to explode this argument.
    unsigned ArgOffset = ++TotalArgIndex;
    unsigned OldArgIndex = ArgOffset - 1; 
    llvm::SmallVector<SILValue, 8> LeafValues;

    // We do this in the same order as leaf types since ProjTree expects that the
    // order of leaf values matches the order of leaf types.
    llvm::SmallVector<const ProjectionTreeNode*, 8> LeafNodes;
    AD.ProjTree.getLeafNodes(LeafNodes);

    for (auto *Node : LeafNodes) {
      auto OwnershipKind = *AD.getTransformedOwnershipKind(Node->getType());
      LeafValues.push_back(BB->insertFunctionArgument(
          ArgOffset++, Node->getType(), OwnershipKind,
          BB->getArgument(OldArgIndex)->getDecl()));
      AIM[TotalArgIndex - 1] = AD.Index;
      TotalArgIndex ++;
    }

    // Then go through the projection tree constructing aggregates and replacing
    // uses.
    AD.ProjTree.replaceValueUsesWithLeafUses(Builder, BB->getParent()->getLocation(),
                                             LeafValues);

    // We ignored debugvalue uses when we constructed the new arguments, in order
    // to preserve as much information as possible, we construct a new value for
    // OrigArg from the leaf values and use that in place of the OrigArg.
    SILValue NewOrigArgValue = AD.ProjTree.computeExplodedArgumentValue(Builder,
                                             BB->getParent()->getLocation(),
                                             LeafValues);

    // Replace all uses of the original arg with the new value.
    SILArgument *OrigArg = BB->getArgument(OldArgIndex);
    OrigArg->replaceAllUsesWith(NewOrigArgValue);

    // Now erase the old argument since it does not have any uses. We also
    // decrement ArgOffset since we have one less argument now.
    BB->eraseArgument(OldArgIndex);
    TotalArgIndex --;
  }
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//
namespace {
class FunctionSignatureOpts : public SILFunctionTransform {
  
  /// If true, perform a special kind of dead argument elimination to enable
  /// removal of partial_apply instructions where all partially applied
  /// arguments are dead.
  bool OptForPartialApply;

public:

  FunctionSignatureOpts(bool OptForPartialApply) :
     OptForPartialApply(OptForPartialApply) { }

  void run() override {
    auto *F = getFunction();

    // Don't optimize callees that should not be optimized.
    if (!F->shouldOptimize())
      return;

    // This is the function to optimize.
    DEBUG(llvm::dbgs() << "*** FSO on function: " << F->getName() << " ***\n");

    // Check the signature of F to make sure that it is a function that we
    // can specialize. These are conditions independent of the call graph.
    if (!canSpecializeFunction(F)) {
      DEBUG(llvm::dbgs() << "  cannot specialize function -> abort\n");
      return;
    }

    auto *RCIA = getAnalysis<RCIdentityAnalysis>();
    CallerAnalysis *CA = PM->getAnalysis<CallerAnalysis>();
    auto *EA = PM->getAnalysis<EpilogueARCAnalysis>();

    const CallerAnalysis::FunctionInfo &FuncInfo = CA->getCallerInfo(F);

    // Lock BCA so it's not invalidated along with the rest of the call graph.
    AnalysisPreserver BCAP(PM->getAnalysis<BasicCalleeAnalysis>());

    // As we optimize the function more and more, the name of the function is
    // going to change, make sure the mangler is aware of all the changes done
    // to the function.
    Mangle::Mangler M;
    auto P = Demangle::SpecializationPass::FunctionSignatureOpts;
    FunctionSignatureSpecializationMangler OldFM(P, M, F->isFragile(), F);
    NewMangling::FunctionSignatureSpecializationMangler NewFM(P, F->isFragile(),
                                                              F);

    /// Keep a map between the exploded argument index and the original argument
    /// index.
    llvm::SmallDenseMap<int, int> AIM;
    int asize = F->begin()->getArguments().size();
    for (auto i = 0; i < asize; ++i) {
      AIM[i] = i;
    }

    // Allocate the argument and result descriptors.
    llvm::SmallVector<ArgumentDescriptor, 4> ArgumentDescList;
    llvm::SmallVector<ResultDescriptor, 4> ResultDescList;
    auto Args = F->begin()->getFunctionArguments();
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
      ArgumentDescList.emplace_back(Args[i]);
    }
    for (SILResultInfo IR : F->getLoweredFunctionType()->getResults()) {
      ResultDescList.emplace_back(IR);
    }

    // Owned to guaranteed optimization.
    FunctionSignatureTransform FST(F, RCIA, EA, OldFM, NewFM, AIM,
                                   ArgumentDescList, ResultDescList);

    bool Changed = false;
    if (OptForPartialApply) {
      Changed = FST.removeDeadArgs(FuncInfo.getMinPartialAppliedArgs());
    } else {
      Changed = FST.run(FuncInfo.hasCaller());
    }
    if (Changed) {
      ++ NumFunctionSignaturesOptimized;
      // The old function must be a thunk now.
      assert(F->isThunk() && "Old function should have been turned into a thunk");

      PM->invalidateAnalysis(F, SILAnalysis::InvalidationKind::Everything);

      // Make sure the PM knows about this function. This will also help us
      // with self-recursion.
      notifyPassManagerOfFunction(FST.getOptimizedFunction(), F);

      if (!OptForPartialApply) {
        // We have to restart the pipeline for this thunk in order to run the
        // inliner (and other opts) again. This is important if the new
        // specialized function (which is called from this thunk) is
        // function-signature-optimized again and also becomes an
        // always-inline-thunk.
        restartPassPipeline();
      }
    }
  }

  StringRef getName() override { return "Function Signature Optimization"; }
};

} // end anonymous namespace

SILTransform *swift::createFunctionSignatureOpts() {
  return new FunctionSignatureOpts(/* OptForPartialApply */ false);
}

SILTransform *swift::createDeadArgSignatureOpt() {
  return new FunctionSignatureOpts(/* OptForPartialApply */ true);
}

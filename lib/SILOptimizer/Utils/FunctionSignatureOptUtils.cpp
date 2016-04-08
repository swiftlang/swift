//===--- FunctionSignatureOptUtils.cpp ------------------------------------===//
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

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SILOptimizer/Utils/FunctionSignatureOptUtils.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SIL/Mangle.h"

using namespace swift;

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
bool swift::canSpecializeFunction(SILFunction *F) {
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

void swift::
addReleasesForConvertedOwnedParameter(SILBuilder &Builder,
                                      SILLocation Loc,
                                      ArrayRef<SILArgument*> Parameters,
                                      ArrayRef<ArgumentDescriptor> &ArgDescs) {
  // If we have any arguments that were consumed but are now guaranteed,
  // insert a release_value.
  for (auto &ArgDesc : ArgDescs) {
    if (ArgDesc.CalleeRelease.empty())
      continue;
    Builder.createReleaseValue(Loc, Parameters[ArgDesc.Index],
                               Atomicity::Atomic);
  }
}

void swift::
addReleasesForConvertedOwnedParameter(SILBuilder &Builder,
                                      SILLocation Loc,
                                      OperandValueArrayRef Parameters,
                                      ArrayRef<ArgumentDescriptor> &ArgDescs) {
  // If we have any arguments that were consumed but are now guaranteed,
  // insert a release_value.
  for (auto &ArgDesc : ArgDescs) {
    // The argument is dead. Make sure we have a release to balance out
    // the retain for creating the @owned parameter.
    if (ArgDesc.IsEntirelyDead && 
        ArgDesc.Arg->getKnownParameterInfo().getConvention() ==
        ParameterConvention::Direct_Owned) {
      Builder.createReleaseValue(Loc, Parameters[ArgDesc.Index],
                                 Atomicity::Atomic);
      continue;
    }
    if (ArgDesc.CalleeRelease.empty())
      continue;
    Builder.createReleaseValue(Loc, Parameters[ArgDesc.Index],
                               Atomicity::Atomic);
  }
}

void swift::
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

    Builder.createRetainValue(Loc, SpecificResultValue, Atomicity::Atomic);
  }
}

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

/// Returns true if I is a release instruction.
static bool isRelease(SILInstruction *I) {
  switch (I->getKind()) {
  case ValueKind::StrongReleaseInst:
  case ValueKind::ReleaseValueInst:
    return true;
  default:
    return false;
  }
}

/// Returns true if LHS and RHS contain identical set of releases.
static bool hasIdenticalReleases(ReleaseList LHS, ReleaseList RHS) {
  llvm::DenseSet<SILInstruction *> Releases;
  if (LHS.size() != RHS.size())
    return false;
  for (auto &X : LHS) 
    Releases.insert(X);
  for (auto &X : RHS) 
    if (Releases.find(X) == Releases.end())
      return false;
  return true;
}

static ReleaseSet
collectEpilogueReleases(ConsumedArgToEpilogueReleaseMatcher &Return,
                        ConsumedArgToEpilogueReleaseMatcher &Throw,
                        SILArgument *Arg) {
  ReleaseSet EpilogueReleases;
  // Handle return block.
  auto ReturnReleases = Return.getReleasesForArgument(Arg);
  if (ReturnReleases.empty())
    return EpilogueReleases;
  for (auto &X : Return.getReleasesForArgument(Arg)) 
    EpilogueReleases.insert(X);

  // Handle throw block.
  if (!Throw.hasBlock())
    return EpilogueReleases;
  auto ThrowReleases = Throw.getReleasesForArgument(Arg);
  if (ThrowReleases.empty()) {
    EpilogueReleases.clear();
    return EpilogueReleases;
  }
  for (auto &X : ThrowReleases) 
    EpilogueReleases.insert(X);

  return EpilogueReleases;
}

/// Returns .Some(I) if I is a release that is the only non-debug instruction
/// with side-effects in the use-def graph originating from Arg. Returns
/// .Some(nullptr), if all uses from the arg were either debug insts or do not
/// have side-effects. Returns .None if there were any non-release instructions
/// with side-effects in the use-def graph from Arg or if there were multiple
/// release instructions with side-effects in the use-def graph from Arg.
static llvm::Optional<ReleaseList>
getNonTrivialNonDebugReleaseUse(SILArgument *Arg) {
  llvm::SmallVector<SILInstruction *, 8> Worklist;
  llvm::SmallPtrSet<SILInstruction *, 8> SeenInsts;
  ReleaseList Result;

  for (Operand *I : getNonDebugUses(SILValue(Arg)))
    Worklist.push_back(I->getUser());

  while (!Worklist.empty()) {
    SILInstruction *U = Worklist.pop_back_val();
    if (!SeenInsts.insert(U).second)
      continue;

    // If U is a terminator inst, return false.
    if (isa<TermInst>(U))
      return None;

    // If U has side effects...
    if (U->mayHaveSideEffects()) {
      // And is not a release_value, return None.
      if (!isRelease(U))
        return None;

      // Otherwise, set result to that value.
      Result.push_back(U);
      continue;
    }

    // Otherwise add all non-debug uses of I to the worklist.
    for (Operand *I : getNonDebugUses(U))
      Worklist.push_back(I->getUser());
  }

  return Result;
}

bool FunctionSignatureInfo::analyzeParameters() {
  // For now ignore functions with indirect results.
  if (F->getLoweredFunctionType()->hasIndirectResults())
    return false;

  ArrayRef<SILArgument *> Args = F->begin()->getBBArgs();

  // A map from consumed SILArguments to the release associated with an
  // argument.
  ConsumedArgToEpilogueReleaseMatcher ArgToReturnReleaseMap(RCFI, F);
  ConsumedArgToEpilogueReleaseMatcher ArgToThrowReleaseMap(
      RCFI, F, ConsumedArgToEpilogueReleaseMatcher::ExitKind::Throw);

  // Did we decide we should optimize any parameter?
  bool SignatureOptimize = false;

  // Analyze the argument information.
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    // Find all the epilogue releases for this argument.
    auto EpilogueReleases = collectEpilogueReleases(ArgToReturnReleaseMap,
                                                    ArgToThrowReleaseMap,
                                                    Args[i]);
    ArgumentDescriptor A(Allocator, Args[i], EpilogueReleases);
    bool HaveOptimizedArg = false;

    // Whether we will explode the argument or not.
    A.Explode = A.shouldExplode();

    bool isABIRequired = isArgumentABIRequired(Args[i]);
    auto OnlyRelease = getNonTrivialNonDebugReleaseUse(Args[i]);

    // If this argument is not ABI required and has no uses except for debug
    // instructions, remove it.
    if (!isABIRequired && OnlyRelease && OnlyRelease.getValue().empty()) {
      A.IsEntirelyDead = true;
      HaveOptimizedArg = true;
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

          // TODO: accept a second release in the throw block to let the
          // argument be dead.
          if (OnlyRelease && hasIdenticalReleases(OnlyRelease.getValue(), Releases)) {
            A.IsEntirelyDead = true;
          }

          A.CalleeRelease = Releases;
          A.CalleeReleaseInThrowBlock = ReleasesInThrow;
          HaveOptimizedArg = true;
        }
      }
    }

    if (A.Explode) {
      HaveOptimizedArg = true;
    }

    if (HaveOptimizedArg) {
      SignatureOptimize = true;
      // Store that we have modified the self argument. We need to change the
      // calling convention later.
      if (Args[i]->isSelf())
        ShouldModifySelfArgument = true;
    }

    // Add the argument to our list.
    ArgDescList.push_back(std::move(A));
  }
 
  return SignatureOptimize;
}

bool FunctionSignatureInfo::analyzeResult() {
  // For now ignore functions with indirect results.
  if (F->getLoweredFunctionType()->hasIndirectResults())
    return false;

  // Did we decide we should optimize any parameter?
  bool SignatureOptimize = false;

  // Analyze return result information.
  auto DirectResults = F->getLoweredFunctionType()->getDirectResults();
  for (SILResultInfo DirectResult : DirectResults) {
    ResultDescList.emplace_back(DirectResult);
  }
  // For now, only do anything if there's a single direct result.
  if (DirectResults.size() == 1 &&
      ResultDescList[0].hasConvention(ResultConvention::Owned)) {
    auto &RI = ResultDescList[0];
    // We have an @owned return value, find the epilogue retains now.
    ConsumedResultToEpilogueRetainMatcher RVToReturnRetainMap(RCFI, AA, F);
    auto Retains = RVToReturnRetainMap.getEpilogueRetains();
    // We do not need to worry about the throw block, as the return value is only
    // going to be used in the return block/normal block of the try_apply instruction.
    if (!Retains.empty()) {
      RI.CalleeRetain = Retains;
      SignatureOptimize = true;
    }
  }
  return SignatureOptimize;
}

/// This function goes through the arguments of F and sees if we have anything
/// to optimize in which case it returns true. If we have nothing to optimize,
/// it returns false.
void FunctionSignatureInfo::analyze() {
  // Compute the signature optimization.
  bool OptimizedParams = analyzeParameters();
  bool OptimizedResult = analyzeResult();
  ShouldOptimize = OptimizedParams || OptimizedResult;
  // We set this function to highly profitable if we have a O2G on one of its
  // parameters or results.
  for (auto &X : ArgDescList) {
    HighlyProfitable |= !X.CalleeRelease.empty();
  }
  for (auto &X : ResultDescList) {
    HighlyProfitable |= !X.CalleeRetain.empty();
  }
}

//===----------------------------------------------------------------------===//
//                                  Mangling
//===----------------------------------------------------------------------===//

std::string FunctionSignatureInfo::getOptimizedName() const {
  Mangle::Mangler M;
  auto P = SpecializationPass::FunctionSignatureOpts;
  FunctionSignatureSpecializationMangler FSSM(P, M, F->isFragile(), F);

  // Handle arguments' changes.
  for (unsigned i : indices(ArgDescList)) {
    const ArgumentDescriptor &Arg = ArgDescList[i];
    if (Arg.IsEntirelyDead) {
      FSSM.setArgumentDead(i);
    }

    // If we have an @owned argument and found a callee release for it,
    // convert the argument to guaranteed.
    if (!Arg.CalleeRelease.empty()) {
      FSSM.setArgumentOwnedToGuaranteed(i);
    }

    // If this argument is not dead and we can explode it, add 's' to the
    // mangling.
    if (Arg.Explode && !Arg.IsEntirelyDead) {
      FSSM.setArgumentSROA(i);
    }
  }

  // Handle return value's change.
  // FIXME: handle multiple direct results here
  if (ResultDescList.size() == 1 &&
      !ResultDescList[0].CalleeRetain.empty())
    FSSM.setReturnValueOwnedToUnowned();

  FSSM.mangle();

  return M.finalize();
}

//===--- FunctionSignatureAnalysis.cpp ------------------------------------===//
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

#include "swift/SILOptimizer/Analysis/FunctionSignatureAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/Mangle.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/Support/CommandLine.h"

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
bool FunctionSignatureInfo::analyze() {
  if (SignatureComputed)
    return SignatureOptimize;

  // Compute the signature optimization.
  bool OptimizedParams = analyzeParameters();
  bool OptimizedResult = analyzeResult();

  SignatureComputed = true;
  SignatureOptimize = OptimizedParams || OptimizedResult;
  return SignatureOptimize;
}

//===----------------------------------------------------------------------===//
//                                  Mangling
//===----------------------------------------------------------------------===//

std::string FunctionSignatureInfo::getOptimizedName() const {
  Mangle::Mangler M;
  auto P = SpecializationPass::FunctionSignatureOpts;
  FunctionSignatureSpecializationMangler FSSM(P, M, F);

  std::string ArgEnc;

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
      // Generate a string of encoding for the argument projection tree.
      // TODO: we can put this into the mangler itself.
      ArgEnc += "_arg" + std::to_string(i) + "_" + Arg.ProjTree.getNameEncoding();
    }
  }

  // Handle return value's change.
  // FIXME: handle multiple direct results here
  if (ResultDescList.size() == 1 &&
      !ResultDescList[0].CalleeRetain.empty())
    FSSM.setReturnValueOwnedToUnowned();

  FSSM.mangle();

  return M.finalize() + ArgEnc;
}



//===----------------------------------------------------------------------===//
//                              Main Entry Point
//===----------------------------------------------------------------------===//

void FunctionSignatureAnalysis::initialize(SILPassManager *PM) {
  AA = PM->getAnalysis<AliasAnalysis>();
  RCIA = PM->getAnalysis<RCIdentityAnalysis>();
}

SILAnalysis *swift::createFunctionSignatureAnalysis(SILModule *M) {
  return new FunctionSignatureAnalysis(M);
}

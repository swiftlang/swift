//===--- ArgumentExplosionTransform.cpp -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "fso-argument-explosion-transform"
#include "FunctionSignatureOpts.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

static llvm::cl::opt<bool> FSODisableArgExplosion(
    "sil-fso-disable-arg-explosion",
    llvm::cl::desc("Do not perform argument explosion during FSO. Intended "
                   "only for testing purposes"));

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

/// Return true if it's both legal and a good idea to explode this argument.
static bool shouldExplode(ArgumentDescriptor &argDesc,
                          ConsumedArgToEpilogueReleaseMatcher &ERM) {
  // We cannot optimize the argument.
  if (!argDesc.canOptimizeLiveArg())
    return false;

  // See if the projection tree consists of potentially multiple levels of
  // structs containing one field. In such a case, there is no point in
  // exploding the argument.
  //
  // Also, in case of a type can not be exploded, e.g an enum, we treat it
  // as a singleton.
  if (argDesc.ProjTree.isSingleton())
    return false;

  auto *arg = argDesc.Arg;
  if (!shouldExpand(arg->getModule(), arg->getType().getObjectType())) {
    return false;
  }

  // If this argument is @owned and we can not find all the releases for it
  // try to explode it, maybe we can find some of the releases and O2G some
  // of its components.
  //
  // This is a potentially a very profitable optimization. Ignore other
  // heuristics.
  if (arg->hasConvention(SILArgumentConvention::Direct_Owned) &&
      ERM.hasSomeReleasesForArgument(arg))
    return true;

  unsigned explosionSize = argDesc.ProjTree.getLiveLeafCount();
  return explosionSize >= 1 && explosionSize <= 3;
}

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

bool FunctionSignatureTransform::ArgumentExplosionAnalyzeParameters() {
  // If we are not supposed to perform argument explosion, bail.
  if (FSODisableArgExplosion)
    return false;

  SILFunction *F = TransformDescriptor.OriginalFunction;
  // Did we decide we should optimize any parameter?
  bool SignatureOptimize = false;
  auto Args = F->begin()->getFunctionArguments();
  ConsumedArgToEpilogueReleaseMatcher ArgToReturnReleaseMap(
      RCIA->get(F), F, {SILArgumentConvention::Direct_Owned});

  // Analyze the argument information.
  for (unsigned i : indices(Args)) {
    ArgumentDescriptor &A = TransformDescriptor.ArgumentDescList[i];
    // If the argument is dead, there is no point in trying to explode it. The
    // dead argument pass will get it.
    if (A.IsEntirelyDead) {
      continue;
    }

    // Do not optimize argument.
    if (!A.canOptimizeLiveArg()) {
      continue;
    }

    // Explosion of generic parameters is not supported yet.
    if (A.Arg->getType().hasArchetype())
      continue;

    A.ProjTree.computeUsesAndLiveness(A.Arg);
    A.Explode = shouldExplode(A, ArgToReturnReleaseMap);

    // Modified self argument.
    if (A.Explode && Args[i]->isSelf()) {
      TransformDescriptor.shouldModifySelfArgument = true;
    }

    SignatureOptimize |= A.Explode;
  }
  return SignatureOptimize;
}

void FunctionSignatureTransform::ArgumentExplosionFinalizeOptimizedFunction() {
  SILFunction *NewF = TransformDescriptor.OptimizedFunction.get();
  SILBasicBlock *BB = &*NewF->begin();
  SILBuilder Builder(BB->begin());
  Builder.setCurrentDebugScope(BB->getParent()->getDebugScope());
  unsigned TotalArgIndex = 0;
  for (ArgumentDescriptor &AD : TransformDescriptor.ArgumentDescList) {
    // If this argument descriptor was dead and we removed it, just skip it. Do
    // not increment the argument index.
    if (AD.WasErased) {
      continue;
    }

    // Simply continue if do not explode.
    if (!AD.Explode) {
      TransformDescriptor.AIM[TotalArgIndex] = AD.Index;
      ++TotalArgIndex;
      continue;
    }

    assert(!AD.IsEntirelyDead &&
           "Should never see completely dead values here");

    // OK, we need to explode this argument.
    unsigned ArgOffset = ++TotalArgIndex;
    unsigned OldArgIndex = ArgOffset - 1;
    llvm::SmallVector<SILValue, 8> LeafValues;

    // We do this in the same order as leaf types since ProjTree expects that
    // the order of leaf values matches the order of leaf types.
    llvm::SmallVector<const ProjectionTreeNode *, 8> LeafNodes;
    AD.ProjTree.getLiveLeafNodes(LeafNodes);

    for (auto *Node : LeafNodes) {
      auto OwnershipKind = *AD.getTransformedOwnershipKind(Node->getType());
      LeafValues.push_back(
          BB->insertFunctionArgument(ArgOffset, Node->getType(), OwnershipKind,
                                     BB->getArgument(OldArgIndex)->getDecl()));
      TransformDescriptor.AIM[TotalArgIndex - 1] = AD.Index;
      ++ArgOffset;
      ++TotalArgIndex;
    }

    // Then go through the projection tree constructing aggregates and replacing
    // uses.
    AD.ProjTree.replaceValueUsesWithLeafUses(
        Builder, BB->getParent()->getLocation(), LeafValues);

    // We ignored debugvalue uses when we constructed the new arguments, in
    // order to preserve as much information as possible, we construct a new
    // value for OrigArg from the leaf values and use that in place of the
    // OrigArg.
    SILValue NewOrigArgValue = AD.ProjTree.computeExplodedArgumentValue(
        Builder, BB->getParent()->getLocation(), LeafValues);

    // Replace all uses of the original arg with the new value.
    SILArgument *OrigArg = BB->getArgument(OldArgIndex);
    OrigArg->replaceAllUsesWith(NewOrigArgValue);

    // Now erase the old argument since it does not have any uses. We also
    // decrement ArgOffset since we have one less argument now.
    BB->eraseArgument(OldArgIndex);
    --TotalArgIndex;
  }
}

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
///
/// \file
///
/// This file contains an implementation of the partial dead argument
/// elimination optimization. We do this to attempt to remove non-trivial
/// arguments of callees to eliminate lifetime constraints of a large argument
/// on values in the caller.
///
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

static bool
shouldExplodeTrivial(FunctionSignatureTransformDescriptor &transformDesc,
                     ArgumentDescriptor &argDesc, SILType ty,
                     unsigned maxExplosionSize) {
  // Just blow up parameters if we will reduce the size of arguments.
  //
  // FIXME: In the future we should attempt to only do this if we can generate a
  // thunk. This was tried with the current heuristic and it resulted in a 1%
  // increase in code-size in the standard library.
  unsigned explosionSize = argDesc.ProjTree.getLiveLeafCount();
  return explosionSize <= maxExplosionSize;
}

/// Return true if it's both legal and a good idea to explode this argument.
///
/// Our main interest here is to expose more opportunities for ARC. This means
/// that we are not interested in exploding (and partially DCEing) structs in
/// the following cases:
///
/// 1. Completely dead arguments. This is handled by dead argument elimination.
///
/// 2. Values that are completely trivial. By splitting these up we create
///    more register pressure during argument marshalling and do not really add
///    any advantage. We only eliminate them
///
/// 3. Structs with many live leaf nodes. Our heuristic is 1-3 live leaf
///    nodes. Otherwise again we run into register pressure/spilling issues.
///
/// One important thing to note here is that the last two cases could be dealt
/// with more effectively by having FSO consider the number of arguments
/// created in total instead of not reasoning about this and hoping the
/// heuristic works.
///
/// With that in mind, we want to perform argument exploding in the following
/// cases (assuming our live leaf restriction):
///
/// 1. Non-trivial structs that only have live trivial parts. This at the SIL
///    level eliminates ARC restrictions on the caller by the callee.
///
/// 2. Splitting non-trivial structs that have multiple non-trivial live leaf
///    nodes. This is useful because it enables the low level ARC optimizer to
///    consider the arguments as having different RC identities and thus pair
///    retains/releases in an easier way.
///
/// What is important to notice here is that we do not want to explode
/// arguments if.
static bool
shouldExplode(FunctionSignatureTransformDescriptor &transformDesc,
              ArgumentDescriptor &argDesc,
              ConsumedArgToEpilogueReleaseMatcher &epilogueReleaseMatcher) {
  // No passes can optimize this argument, so just bail.
  if (!argDesc.canOptimizeLiveArg()) {
    return false;
  }

  // We do not explode parameters that are completely dead. This is so we can
  // rely on normal dead argument elimination to eliminate such parameters.
  //
  // We compute this early since it is already computed at this point.
  unsigned naiveExplosionSize = argDesc.ProjTree.getLiveLeafCount();
  if (naiveExplosionSize == 0) {
    return false;
  }

  // See if the projection tree consists of potentially multiple levels of
  // structs containing one field. In such a case, there is no point in
  // exploding the argument.
  //
  // Also, in case of a type can not be exploded, e.g an enum, we treat it
  // as a singleton.
  if (argDesc.ProjTree.isSingleton()) {
    return false;
  }

  // Ok, we have a case that we may be able to handle. First make sure that the
  // current global size expansion heuristic does not ban us from expanding this
  // type.
  auto *arg = argDesc.Arg;
  auto &module = arg->getModule();
  auto ty = arg->getType().getObjectType();
#if false
  if (!shouldExpand(module, ty)) {
    return false;
  }
#endif

  // If we have a singular argument, be more aggressive about our max explosion
  // size. If we were unable to expand the value we know that it will be
  // exploded so use UINT_MAX.
  unsigned maxExplosionSize = 3;
  if (transformDesc.ArgumentDescList.size() == 1) {
    maxExplosionSize = UINT_MAX;
  }

  // Ok, this is something that globally we are not forbidden from
  // expanded. First check if our type is completely trivial. We never want to
  // explode arguments that are trivial so return false. See comment above.
  if (ty.isTrivial(module)) {
    return shouldExplodeTrivial(transformDesc, argDesc, ty, maxExplosionSize);
  }

  // Ok, we think that this /may/ be profitable to optimize. Grab our leaf node
  // types. We already know that we have a strictily non-trivial type. If by
  // performing partial DCE we will eliminate a non-trivial argument, we want to
  // eliminate that argument to eliminate an ARC lifetime restriction in our
  // caller scope.
  llvm::SmallVector<SILType, 32> allTypes;
  argDesc.ProjTree.getAllLeafTypes(allTypes);
  llvm::SmallVector<const ProjectionTreeNode *, 32> liveNodes;
  argDesc.ProjTree.getLiveLeafNodes(liveNodes);

  unsigned numInputNonTrivialLeafNodes =
      llvm::count_if(allTypes, [&](SILType t) { return !t.isTrivial(module); });
  unsigned numNonTrivialLiveLeafNodes =
      llvm::count_if(liveNodes, [&](const ProjectionTreeNode *n) {
        return n->getType().isTrivial(module);
      });

  // If we reduced the number of non-trivial leaf types, we want to split this
  // given that we already know that we are not going to drastically change the
  // number of arguments.
  if (naiveExplosionSize <= maxExplosionSize &&
      numNonTrivialLiveLeafNodes < numInputNonTrivialLeafNodes) {
    return true;
  }

  // Ok, this is an argument with more than 3 live leaf nodes. See if after
  // performing o2g we will be able to reduce our number of non-trivial nodes.
  //
  // *NOTE* This does not create a phase ordering issue since we re-run the
  // pipeline after we run FSO a first time.
  if (numNonTrivialLiveLeafNodes > 1 &&
      argDesc.hasConvention(SILArgumentConvention::Direct_Owned)) {
    if (auto releases =
            epilogueReleaseMatcher.getPartiallyPostDomReleaseSet(arg)) {
      llvm::SmallPtrSet<SILInstruction *, 8> users;
      for (auto *i : *releases)
        users.insert(i);

      // *NOTE* This will still include trivial parameters. We only
      // will delete non-trivial parameters.
      unsigned newExplosionSize = naiveExplosionSize;
      for (auto *node : liveNodes) {
        // If all of our users are epilogue releases, reduce the explosion size.
        if (llvm::all_of(node->getNonProjUsers(), [&](Operand *op) {
              return users.count(op->getUser());
            })) {
          --newExplosionSize;
        }
      }

      // See if newExplosionSize is less than our max allowed explosion size. If
      // we reduce this value then we know we will reduce the number of
      // non-trivial nodes. We just don't want to expand the number of arguments
      // too much.
      return newExplosionSize <= maxExplosionSize;
    }
  }

  // Otherwise, we are not reducing the number of live non-trivial values
  return false;
}

//===----------------------------------------------------------------------===//
//                          Top Level Implementation
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
    A.Explode = shouldExplode(TransformDescriptor, A, ArgToReturnReleaseMap);

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

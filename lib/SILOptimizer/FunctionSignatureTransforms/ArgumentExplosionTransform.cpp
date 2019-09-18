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
///    any advantage. We only eliminate them if they are completely dead;
///    otherwise, they are left unmodified.
///
/// 3. Structs with many live leaf nodes. Our heuristic is 1-3 live leaf
///    nodes. Otherwise again we run into register pressure/spilling issues.
///
/// TODO: The last two cases could be dealt with more effectively by having FSO
///       consider the number of arguments created in total instead of not
///       reasoning about this and hoping the heuristic works.
///
/// Perform argument exploding as long as the following conditions hold:
///
/// 1. The total live leaf count should be in 0>..3.
///
/// 2. Some of the non-trivial leaves are dead.  Those fields being passed
///    results in superfluous ARC traffic.
static bool
shouldExplode(FunctionSignatureTransformDescriptor &transformDesc,
              ArgumentDescriptor &argDesc,
              ConsumedArgToEpilogueReleaseMatcher &epilogueReleaseMatcher) {
  // No passes can optimize this argument, so just bail.
  if (!argDesc.canOptimizeLiveArg()) {
    LLVM_DEBUG(llvm::dbgs() << "The argument is of a type that cannot be "
                               "exploded.");
    return false;
  }

  auto *argument = argDesc.Arg;
  auto &module = argument->getModule();
  auto type = argument->getType().getObjectType();

  // If the global type expansion heuristic does not allow the type to be
  // expanded, it will not be exploded.
  if (!shouldExpand(module, type)) {
    LLVM_DEBUG(llvm::dbgs() << "The argument is of a type which should not be "
                               "expanded.");
    return false;
  }

  auto *function = argument->getFunction();

  // If the argument is completely trivial, it will not be exploded.
  if (type.isTrivial(*function)) {
    LLVM_DEBUG(llvm::dbgs() << "The argument's type is trivial.");
    return false;
  }

  // If the argument is a singleton, it will not be exploded.
  //
  // Explosion makes sense only if some but not all of the leaves are live.
  //
  // Note that ProjectionTree::isSingleton returns true for enums since they are
  // sums and not products and so only have a single top-level node.
  if (argDesc.ProjTree.isSingleton()) {
    LLVM_DEBUG(llvm::dbgs() << "The argument's type is a singleton.");
    return false;
  }

  // NOTE: The value obtained here is an upper bound because the
  //       owned-to-guaranteed transformation may eliminate some live leaves,
  //       leaving the count lower.
  unsigned const liveLeafCountUpperBound = argDesc.ProjTree.getLiveLeafCount();

  assert(liveLeafCountUpperBound >= 0 &&
         "Counting live leaves should result in "
         "a non-negative number.");

  // If the argument is completely dead, it will not be exploded.
  //
  // Explosion makes sense only if some but not all of the leaves are live.  The
  // dead argument transformation will try to eliminate the argument.
  if (liveLeafCountUpperBound == 0) {
    LLVM_DEBUG(llvm::dbgs() << "The argument has no live leaves.");
    return false;
  }

  // To determine whether some but not all of the leaves are used, the total
  // leaf count must be retrieved.
  llvm::SmallVector<SILType, 32> allLeaves;
  argDesc.ProjTree.getAllLeafTypes(allLeaves);
  unsigned leafCount = allLeaves.size();

  assert(
      liveLeafCountUpperBound <= leafCount &&
      "There should be no more *live* leaves than there are *total* leaves.");

  // NOTE: Exiting early at this point in the case where liveLeafCountUpperBound
  // ==
  //       leafCount misses cases where leaves are only live until the
  //       owned-to-guaranteed transform eliminates their liveness.

  unsigned const nontrivialLeafCount = llvm::count_if(
      allLeaves, [&](SILType type) { return !type.isTrivial(*function); });

  assert(nontrivialLeafCount >= 0 && "Counting non-trivial leaves should "
                                     " result in a non-negative number.");

  // If the argument is completely trivial, it will not be exploded.
  if (nontrivialLeafCount == 0) {
    llvm_unreachable("There should be at least one nontrivial leaf since our "
                     "type is not trivial.");
    return false;
  }

  llvm::SmallVector<const ProjectionTreeNode *, 32> liveLeaves;
  argDesc.ProjTree.getLiveLeafNodes(liveLeaves);
  // NOTE: The value obtained here is an upper bound because the
  //       owned-to-guaranteed transformation may eliminate some live leaves,
  //       leaving the count lower.
  unsigned liveNontrivialLeafCountUpperBound =
      llvm::count_if(liveLeaves, [&](const ProjectionTreeNode *leaf) {
        return !leaf->getType().isTrivial(*function);
      });

  assert(liveNontrivialLeafCountUpperBound >= 0 &&
         "Counting live non-trivial "
         "leaves should result in a non-negative number.");

  assert(liveNontrivialLeafCountUpperBound <= nontrivialLeafCount &&
         "There "
         "should be no more *live* non-trivial leaves than there are *total* "
         "non-trivial leaves.");
  assert(nontrivialLeafCount <= leafCount &&
         "There should be no more "
         "*non-trivial* leaves than there are *total* leaves.");

  // The heuristic max explosion size for a single argument.
  unsigned const maxExplosionSize = 3;

  // If it is known without taking the owned-to-guaranteed transformation into
  // account both that exploding will reduce ARC traffic (because an upper bound
  // for the number of live non-trivial leaves is less thann the non-trivial
  // leaf count) and also that the explosion will fit within the heuristic upper
  // bound (because an upper bound for the total live leaf count falls within
  // the limit imposed by the heuristic), then explode now.
  if (liveNontrivialLeafCountUpperBound < nontrivialLeafCount &&
      liveLeafCountUpperBound <= maxExplosionSize) {
    LLVM_DEBUG(llvm::dbgs()
               << "Without considering the expected results of "
                  "the owned-to-guaranteed transformation, there "
                  "are already fewer ("
               << liveNontrivialLeafCountUpperBound
               << ") live non-trivial leaves than total leaves ("
               << nontrivialLeafCount << ") and no more total live leaves ("
               << liveLeafCountUpperBound << ") than the heuristic permist ("
               << maxExplosionSize << ").  Exploding.");
    return true;
  }

  unsigned liveLeafCount = liveLeafCountUpperBound;
  unsigned liveNontrivialLeafCount = liveNontrivialLeafCountUpperBound;

  // Exploding the argument would reduce ARC traffic.  At the moment, however,
  // there are more live leaves than should be exploded.  The owned-to
  // -guaranteed transformation may eliminate some live leaves.  If some live
  // leaves are eliminated, the number of live leaves may fall beneath the max.
  //
  // The owned-to-guaranteed will only be applied to the argumehnt if its
  // convention is Direct_Owned.  Additionally, it only applies to non-trivial
  // leaves, which it may kill, so if it is already known that there are no live
  // non-trivial leaves, owned-to-guaranteed will not eliminate anything.
  if (argDesc.hasConvention(SILArgumentConvention::Direct_Owned) &&
      liveNontrivialLeafCountUpperBound > 0) {
    if (auto maybeReleases =
            epilogueReleaseMatcher.getPartiallyPostDomReleaseSet(argument)) {
      auto releases = maybeReleases.getValue();
      llvm::SmallPtrSet<SILInstruction *, 8> users;
      users.insert(std::begin(releases), std::end(releases));

      for (auto *leaf : liveLeaves) {
        if (llvm::all_of(leaf->getNonProjUsers(), [&](Operand *operand) {
              return users.count(operand->getUser());
            })) {
          // Every non-projection user of the leaf is an epilogue release.  The
          // owned-to-guaranteed transformation will eliminate this usage.  With
          // the expectation of that usage being eliminated, stop considering
          // this leaf to be live for the purposes of deciding whether the
          // argument should be exploded.
          --liveLeafCount;
          --liveNontrivialLeafCount;
        }
      }
    }
  }

  if (liveNontrivialLeafCount == nontrivialLeafCount) {
    LLVM_DEBUG(llvm::dbgs()
               << "Even considering the results of the "
                  "owned-to-guaranteed transformation, every non-trivial leaf "
                  "is live.  The argument will not be exploded.");
    return false;
  }

  // Established condition:
  // - liveNontrivialLeafCountUpperBound < nontrivialLeafCount
  //
  // At this point, it is known that exploding the argument would result in
  // reduced ARC traffic.  Whether the argument is exploded now depends only on
  // the max explosion size.
  return liveLeafCount <= maxExplosionSize;
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

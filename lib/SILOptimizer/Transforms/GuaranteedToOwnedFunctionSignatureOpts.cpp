//===--- GuaranteedToOwnedFunctionSignatureOpts.cpp -----------------------===//
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
/// This file implements the @guaranteed -> @owned transformation
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-function-signature-opt"
#include "FunctionSignatureOpts.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILArgument.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                         Implicit Owned Parameters
//===----------------------------------------------------------------------===//

static void
partitionNonTrivialUses(SILFunctionArgument *arg,
                        ArrayRef<Operand *> nonTrivialUsers,
                        SmallVectorImpl<BranchPropagatedUser> &consumingUsers,
                        SmallVectorImpl<BranchPropagatedUser> &regularUsers) {

  // TODO: Put in an assert to make sure nonTrivialUsers is sorted/uniqued. We
  // assume this.

  // Go through our uses and partition our uses. We allow for an apply to take a
  // value as a consuming and a non-consuming parameter. In such a case, we want
  // to fail, so we do not want to check for instructions appearing multiple
  // times. We just care about operands.
  for (auto *Op : nonTrivialUsers) {
    SILInstruction *I = Op->getUser();

    if (isa<ReturnInst>(I)) {
      consumingUsers.push_back(I);
      continue;
    }

    // We only handle direct_owned call sites right now.
    if (auto FAS = FullApplySite::isa(I)) {
      // For now, we do not support callees. The reason why is that we would
      // need to deal with function conversions and thunking.
      if (FAS.getCallee() != Op->get()) {
        auto Conv = FAS.getArgumentConvention(FAS.getCalleeArgIndex(*Op));
        if (Conv == SILArgumentConvention::Direct_Owned) {
          consumingUsers.push_back(Op->getUser());
          continue;
        }
      }
    }

    // TODO: Partial Apply.

    if (auto *SI = dyn_cast<StoreInst>(I)) {
      // Make sure that we are only looking at what we stored.
      if (SI->getSrc() == Op->get()) {
        consumingUsers.push_back(I);
        continue;
      }
    }

    regularUsers.push_back(I);
  }
}

static bool isImplicitOwnedParameter(RCIdentityFunctionInfo *rcfi,
                                     DeadEndBlocks &deBlocks,
                                     SILFunctionArgument *arg) {
  assert(arg->getArgumentConvention() == SILArgumentConvention::Direct_Guaranteed &&
         "We only accept direct guaranteed function arguments");

  SmallVector<Operand *, 32> nonTrivialUsers;
  rcfi->getRCUses(arg, nonTrivialUsers);

  // If we do not have any non-trivial uses, there is no reason to convert this
  // to an owned parameter... it is dead and it will be removed by dead argument
  // elimination.
  if (nonTrivialUsers.empty())
    return false;

  // Unique our non trivial users.
  sortUnique(nonTrivialUsers);

  // Then partition our nonTrivialUsers into a consuming and non-consuming set.
  SmallVector<BranchPropagatedUser, 32> consumingUsers; // FIXME: Merge w/arg?
  SmallVector<BranchPropagatedUser, 32> nonConsumingUsers;
  partitionNonTrivialUses(arg, nonTrivialUsers, consumingUsers,
                          nonConsumingUsers);

  // If we do not have any consuming uses, we do not want to perform the
  // optimization since there are no reasons to want to forward a value.
  if (consumingUsers.empty())
    return false;

  // Then add all of our users to the linear lifetime checker. If we can prove
  // that there is a unique post-dominating set of consuming values that are all
  // unreachable from each other.
  llvm::SmallPtrSet<SILBasicBlock *, 8> visitedBlocks;
  return valueHasLinearLifetime(SILValue(arg), consumingUsers,
                                nonConsumingUsers, visitedBlocks, deBlocks,
                                ownership::ErrorBehaviorKind::ReturnFalse);
}

//===----------------------------------------------------------------------===//
//                           Top Level Entrypoints
//===----------------------------------------------------------------------===//

bool FunctionSignatureTransform::GuaranteedToOwnedAnalyze() {
  if (!FSOEnableGuaranteedToOwned)
    return false;

  SILFunction *f = TransformDescriptor.OriginalFunction;
  auto *rcfi = RCIA->get(f);
  // TODO: Should deBlocks be cached?
  DeadEndBlocks deBlocks(f);
  auto args = f->begin()->getFunctionArguments();

  // Did we decide we should optimize any parameter?
  bool signatureOptimize = false;

  // A dummy array that we pass to implicit owned parameter for its
  // own use. We don't care about it really.
  llvm::SmallVector<Operand *, 32> consumingUses;

  // Analyze the argument information.
  for (unsigned i : indices(args)) {
    ArgumentDescriptor &ad = TransformDescriptor.ArgumentDescList[i];
    if (!ad.canOptimizeLiveArg()) {
      continue;
    }

    // See if we can find a set of consuming uses.
    if (ad.hasConvention(SILArgumentConvention::Direct_Guaranteed)) {
      if (isImplicitOwnedParameter(rcfi, deBlocks,
                                   cast<SILFunctionArgument>(ad.Arg))) {
        ad.GuaranteedToOwned = true;
        signatureOptimize = true;
      }
      consumingUses.clear();
    }

    if (ad.GuaranteedToOwned && args[i]->isSelf()) {
      TransformDescriptor.shouldModifySelfArgument = true;
    }
  }

  return signatureOptimize;
}

void FunctionSignatureTransform::GuaranteedToOwnedTransform() {
  auto *f = TransformDescriptor.OriginalFunction;

  auto loc = RegularLocation::getAutoGeneratedLocation();
  for (const ArgumentDescriptor &ad : TransformDescriptor.ArgumentDescList) {
    if (!ad.GuaranteedToOwned)
      continue;

    // First change the argument to owned...
    ad.Arg->setOwnershipKind(ValueOwnershipKind::Owned);

    // And then add a release at each exit that we have.
    auto iter = f->findReturnBB();
    if (iter != f->end()) {
      SILBuilderWithScope(iter->getTerminator())
          .emitDestroyValueOperation(loc, ad.Arg);
    }

    iter = f->findThrowBB();
    if (iter != f->end()) {
      SILBuilderWithScope(iter->getTerminator())
          .emitDestroyValueOperation(loc, ad.Arg);
    }
  }
}

void FunctionSignatureTransform::GuaranteedToOwnedFinalizeThunkFunction(
    SILBuilder &builder, SILFunction *f) {
  for (auto &ad : TransformDescriptor.ArgumentDescList) {
    GuaranteedToOwnedAddArgumentRetain(ad, builder);
  }
}

static void createArgumentRetain(SILBuilder &Builder, ArgumentDescriptor &AD) {
  auto &F = Builder.getFunction();
  SILArgument *Arg = F.getArguments()[AD.Index];
  Builder.emitCopyValueOperation(RegularLocation::getAutoGeneratedLocation(),
                                 Arg);
}

/// Set up epilogue work for the thunk arguments based in the given argument.
/// Default implementation simply passes it through.
void FunctionSignatureTransform::GuaranteedToOwnedAddArgumentRetain(
    ArgumentDescriptor &AD, SILBuilder &Builder) {
  // If we have any arguments that were consumed but are now guaranteed,
  // insert a retain RC instruction.
  if (!AD.GuaranteedToOwned) {
    return;
  }

  SILInstruction *Call = TransformDescriptor.findOnlyApplyInThunk();
  Builder.setInsertionPoint(Call->getIterator());
  createArgumentRetain(Builder, AD);
}

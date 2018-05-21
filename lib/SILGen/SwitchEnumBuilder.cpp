//===--- SwitchEnumBuilder.cpp --------------------------------------------===//
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

#include "SwitchEnumBuilder.h"
#include "SILGenFunction.h"
#include "swift/SIL/SILLocation.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
//                     SwitchCaseFullExpr Implementation
//===----------------------------------------------------------------------===//

SwitchCaseFullExpr::SwitchCaseFullExpr(SILGenFunction &SGF, CleanupLocation loc)
    : SGF(SGF), scope(SGF.Cleanups, loc), loc(loc), branchDest() {}

SwitchCaseFullExpr::SwitchCaseFullExpr(SILGenFunction &SGF, CleanupLocation loc,
                                       SwitchCaseBranchDest branchDest)
    : SGF(SGF), scope(SGF.Cleanups, loc), loc(loc), branchDest(branchDest) {}

void SwitchCaseFullExpr::exitAndBranch(SILLocation loc,
                                       ArrayRef<SILValue> branchArgs) {
  assert(bool(branchDest) && "Must have a branch destination!");
  assert(SGF.B.hasValidInsertionPoint());
  scope.pop();

  // Then either do a direct branch or a branch + cleanups.
  if (SILBasicBlock *block = branchDest.getBlock()) {
    SGF.B.createBranch(loc, block, branchArgs);
    return;
  }

  SGF.Cleanups.emitBranchAndCleanups(branchDest.getJumpDest(), loc, branchArgs);
}

void SwitchCaseFullExpr::exit() {
  assert(!bool(branchDest) &&
         "Should not call this if we do have a continuation block");
  assert(SGF.B.hasValidInsertionPoint());
  scope.pop();
}

SwitchCaseFullExpr::~SwitchCaseFullExpr() {
  assert(!scope.isValid() && "Did not pop scope?!");
}

void SwitchCaseFullExpr::unreachableExit() {
  // This is important to ensure that we do not actually emit any cleanups since
  // we already know that an unreachable was emitted.
  assert(!SGF.B.hasValidInsertionPoint() && "Expected to pop scope without a "
                                            "valid insertion point!");
  scope.pop();
}

//===----------------------------------------------------------------------===//
//                      SwitchEnumBuilder Implementation
//===----------------------------------------------------------------------===//

void SwitchEnumBuilder::emit() && {
  bool isAddressOnly = optional.getType().isAddressOnly(builder.getModule()) &&
                       getSGF().silConv.useLoweredAddresses();
  using DeclBlockPair = std::pair<EnumElementDecl *, SILBasicBlock *>;
  {
    // TODO: We could store the data in CaseBB form and not have to do this.
    llvm::SmallVector<DeclBlockPair, 8> caseBlocks;
    llvm::SmallVector<ProfileCounter, 8> caseBlockCounts;
    std::transform(caseDataArray.begin(), caseDataArray.end(),
                   std::back_inserter(caseBlocks),
                   [](NormalCaseData &caseData) -> DeclBlockPair {
                     return {caseData.decl, caseData.block};
                   });
    std::transform(caseDataArray.begin(), caseDataArray.end(),
                   std::back_inserter(caseBlockCounts),
                   [](NormalCaseData &caseData) -> ProfileCounter {
                     return caseData.count;
                   });
    SILBasicBlock *defaultBlock =
        defaultBlockData ? defaultBlockData->block : nullptr;
    ProfileCounter defaultBlockCount =
        defaultBlockData ? defaultBlockData->count : ProfileCounter();
    ArrayRef<ProfileCounter> caseBlockCountsRef = caseBlockCounts;
    if (isAddressOnly) {
      builder.createSwitchEnumAddr(loc, optional.getValue(), defaultBlock,
                                   caseBlocks, caseBlockCountsRef,
                                   defaultBlockCount);
    } else {
      if (optional.getType().isAddress()) {
        // TODO: Refactor this into a maybe load.
        if (optional.hasCleanup()) {
          optional = builder.createLoadTake(loc, optional);
        } else {
          optional = builder.createLoadCopy(loc, optional);
        }
      }
      builder.createSwitchEnum(loc, optional.forward(getSGF()), defaultBlock,
                               caseBlocks, caseBlockCountsRef,
                               defaultBlockCount);
    }
  }

  // If we are asked to create a default block and it is specified that the
  // default block should be emitted before normal cases, emit it now.
  if (defaultBlockData &&
      defaultBlockData->dispatchTime ==
          DefaultDispatchTime::BeforeNormalCases) {
    SILBasicBlock *defaultBlock = defaultBlockData->block;
    SwitchCaseBranchDest branchDest = defaultBlockData->branchDest;
    DefaultCaseHandler handler = defaultBlockData->handler;

    // Don't allow cleanups to escape the conditional block.
    SwitchCaseFullExpr presentScope(builder.getSILGenFunction(),
                                    CleanupLocation::get(loc), branchDest);
    builder.emitBlock(defaultBlock);
    ManagedValue input = optional;
    if (!isAddressOnly) {
      input = builder.createOwnedPHIArgument(optional.getType());
    }
    handler(input, std::move(presentScope));
    builder.clearInsertionPoint();
  }

  for (NormalCaseData &caseData : caseDataArray) {
    EnumElementDecl *decl = caseData.decl;
    SILBasicBlock *caseBlock = caseData.block;
    SwitchCaseBranchDest branchDest = caseData.branchDest;
    NormalCaseHandler handler = caseData.handler;

    // Don't allow cleanups to escape the conditional block.
    SwitchCaseFullExpr presentScope(builder.getSILGenFunction(),
                                    CleanupLocation::get(loc), branchDest);

    builder.emitBlock(caseBlock);

    ManagedValue input;
    if (decl->hasAssociatedValues()) {
      // Pull the payload out if we have one.
      SILType inputType =
          optional.getType().getEnumElementType(decl, builder.getModule());
      input = optional;
      if (!isAddressOnly) {
        input = builder.createOwnedPHIArgument(inputType);
      }
    }
    handler(input, std::move(presentScope));
    builder.clearInsertionPoint();
  }

  // If we are asked to create a default block and it is specified that the
  // default block should be emitted after normal cases, emit it now.
  if (defaultBlockData &&
      defaultBlockData->dispatchTime == DefaultDispatchTime::AfterNormalCases) {
    SILBasicBlock *defaultBlock = defaultBlockData->block;
    auto branchDest = defaultBlockData->branchDest;
    DefaultCaseHandler handler = defaultBlockData->handler;

    // Don't allow cleanups to escape the conditional block.
    SwitchCaseFullExpr presentScope(builder.getSILGenFunction(),
                                    CleanupLocation::get(loc), branchDest);
    builder.emitBlock(defaultBlock);
    ManagedValue input = optional;
    if (!isAddressOnly) {
      input = builder.createOwnedPHIArgument(optional.getType());
    }
    handler(input, std::move(presentScope));
    builder.clearInsertionPoint();
  }
}

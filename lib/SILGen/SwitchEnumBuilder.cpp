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

SwitchCaseFullExpr::SwitchCaseFullExpr(SILGenFunction &SGF, CleanupLocation loc,
                                       SILBasicBlock *contBlock)
    : SGF(SGF), scope(SGF.Cleanups, loc), loc(loc), contBlock(contBlock) {}

void SwitchCaseFullExpr::exitAndBranch(SILLocation loc,
                                       ArrayRef<SILValue> branchArgs) {
  assert(contBlock &&
         "Should not call this if we do not have a continuation block");
  assert(SGF.B.hasValidInsertionPoint());
  scope.pop();
  SGF.B.createBranch(loc, contBlock.get(), branchArgs);
}

void SwitchCaseFullExpr::exit() {
  assert(!contBlock &&
         "Should not call this if we do have a continuation block");
  assert(SGF.B.hasValidInsertionPoint());
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
    NullablePtr<SILBasicBlock> contBB = defaultBlockData->contBlock;
    DefaultCaseHandler handler = defaultBlockData->handler;

    // Don't allow cleanups to escape the conditional block.
    SwitchCaseFullExpr presentScope(builder.getSILGenFunction(),
                                    CleanupLocation::get(loc),
                                    contBB.getPtrOrNull());
    builder.emitBlock(defaultBlock);
    ManagedValue input = optional;
    if (!isAddressOnly) {
      input = builder.createOwnedPHIArgument(optional.getType());
    }
    handler(input, presentScope);
    assert(!builder.hasValidInsertionPoint());
  }

  for (NormalCaseData &caseData : caseDataArray) {
    EnumElementDecl *decl = caseData.decl;
    SILBasicBlock *caseBlock = caseData.block;
    NullablePtr<SILBasicBlock> contBlock = caseData.contBlock;
    NormalCaseHandler handler = caseData.handler;

    // Don't allow cleanups to escape the conditional block.
    SwitchCaseFullExpr presentScope(builder.getSILGenFunction(),
                                    CleanupLocation::get(loc),
                                    contBlock.getPtrOrNull());

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
    handler(input, presentScope);
    assert(!builder.hasValidInsertionPoint());
  }

  // If we are asked to create a default block and it is specified that the
  // default block should be emitted after normal cases, emit it now.
  if (defaultBlockData &&
      defaultBlockData->dispatchTime == DefaultDispatchTime::AfterNormalCases) {
    SILBasicBlock *defaultBlock = defaultBlockData->block;
    NullablePtr<SILBasicBlock> contBB = defaultBlockData->contBlock;
    DefaultCaseHandler handler = defaultBlockData->handler;

    // Don't allow cleanups to escape the conditional block.
    SwitchCaseFullExpr presentScope(builder.getSILGenFunction(),
                                    CleanupLocation::get(loc),
                                    contBB.getPtrOrNull());
    builder.emitBlock(defaultBlock);
    ManagedValue input = optional;
    if (!isAddressOnly) {
      input = builder.createOwnedPHIArgument(optional.getType());
    }
    handler(input, presentScope);
    assert(!builder.hasValidInsertionPoint());
  }
}

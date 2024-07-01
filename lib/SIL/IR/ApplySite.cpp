//===--- ApplySite.cpp - Wrapper around apply instructions ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/ApplySite.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILBuilder.h"


using namespace swift;

void ApplySite::insertAfterInvocation(function_ref<void(SILBuilder &)> func) const {
  SILBuilderWithScope::insertAfter(getInstruction(), func);
}

void ApplySite::visitFinalApplications(
    function_ref<void(SILInstruction *)> visitor) const {
  switch (getKind()) {
  case ApplySiteKind::ApplyInst:
  case ApplySiteKind::TryApplyInst:
  case ApplySiteKind::PartialApplyInst:
    visitor(getInstruction());
    return;
  case ApplySiteKind::BeginApplyInst:
    for (auto *tokenUse :
         cast<BeginApplyInst>(getInstruction())->getTokenResult()->getUses()) {
      visitor(tokenUse->getUser());
    }
    return;
  }
  llvm_unreachable("covered switch isn't covered");
}

void ApplySite::insertAfterApplication(
    function_ref<void(SILBuilder &)> func) const {
  visitFinalApplications(
      [&](auto *inst) { SILBuilderWithScope::insertAfter(inst, func); });
}

void ApplySite::computeApplicationBoundary(
    PrunedLivenessBoundary &boundary,
    SmallVectorImpl<SILBasicBlock *> &discoveredBlocks) const {
  auto *inst = getInstruction();
  SSAPrunedLiveness liveness(inst->getFunction(), &discoveredBlocks);
  liveness.initializeDef(getCallee());
  visitFinalApplications([&](auto *inst) {
    liveness.updateForUse(getInstruction(), /*lifetimeEnding=*/false);
  });
  if (auto *pai = dyn_cast<PartialApplyInst>(getInstruction())) {
    pai->visitApplyUses([&](auto *use) {
      liveness.updateForUse(use->getUser(), /*lifetimeEnding=*/false);
      return true;
    });
  }
  liveness.computeBoundary(boundary);
}

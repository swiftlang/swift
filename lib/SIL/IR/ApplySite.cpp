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
#include "swift/SIL/SILBuilder.h"


using namespace swift;

void ApplySite::insertAfterInvocation(function_ref<void(SILBuilder &)> func) const {
  SILBuilderWithScope::insertAfter(getInstruction(), func);
}

void ApplySite::insertAfterApplication(
    function_ref<void(SILBuilder &)> func) const {
  switch (getKind()) {
  case ApplySiteKind::ApplyInst:
  case ApplySiteKind::TryApplyInst:
  case ApplySiteKind::PartialApplyInst:
    return insertAfterInvocation(func);
  case ApplySiteKind::BeginApplyInst:
    SmallVector<EndApplyInst *, 2> endApplies;
    SmallVector<AbortApplyInst *, 2> abortApplies;
    auto *bai = cast<BeginApplyInst>(getInstruction());
    bai->getCoroutineEndPoints(endApplies, abortApplies);
    for (auto *eai : endApplies) {
      SILBuilderWithScope builder(std::next(eai->getIterator()));
      func(builder);
    }
    for (auto *aai : abortApplies) {
      SILBuilderWithScope builder(std::next(aai->getIterator()));
      func(builder);
    }
    return;
  }
  llvm_unreachable("covered switch isn't covered");
}

bool ApplySite::isAddressable(const Operand &operand) const {
  unsigned calleeArgIndex = getCalleeArgIndex(operand);
  assert(calleeArgIndex >= getSubstCalleeConv().getSILArgIndexOfFirstParam());
  unsigned paramIdx =
    calleeArgIndex - getSubstCalleeConv().getSILArgIndexOfFirstParam();

  CanSILFunctionType calleeType = getSubstCalleeType();
  return calleeType->isAddressable(paramIdx, getFunction());
}

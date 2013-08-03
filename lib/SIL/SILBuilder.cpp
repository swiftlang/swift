//===--- SILBuilder.h - Class for creating SIL Constructs --------*- C++ -*-==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILBuilder.h"

using namespace swift;
using namespace Lowering;

SILValue SILBuilder::emitGeneralizedValue(SILLocation loc, SILValue v) {
  // Thicken thin functions.
  if (v.getType().is<AnyFunctionType>() &&
      v.getType().castTo<AnyFunctionType>()->isThin()) {
    // Thunk functions to the standard "freestanding" calling convention.
    if (v.getType().getAbstractCC() != AbstractCC::Freestanding) {
      auto freestandingType = getThinFunctionType(v.getType().getSwiftType(),
                                                  AbstractCC::Freestanding);
      SILType freestandingSILType =
        F.getParent()->Types.getLoweredLoadableType(freestandingType, 0);
      v = createConvertCC(loc, v, freestandingSILType);
    }

    Type thickTy = getThickFunctionType(v.getType().getSwiftType(),
                                        AbstractCC::Freestanding);

    v = createThinToThickFunction(loc, v,
                       F.getParent()->Types.getLoweredLoadableType(thickTy));
  }

  return v;
}

BranchInst *SILBuilder::createBranch(SILLocation Loc,
                                     SILBasicBlock *TargetBlock,
                                     OperandValueArrayRef Args) {
  llvm::SmallVector<SILValue, 6> ArgsCopy;
  ArgsCopy.reserve(Args.size());
  for (auto I = Args.begin(), E = Args.end(); I != E; ++I) {
    ArgsCopy.push_back(*I);
  }
  return createBranch(Loc, TargetBlock, ArgsCopy);
}

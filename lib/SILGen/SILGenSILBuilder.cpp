//===--- SILGenSILBuilder.cpp ---------------------------------------------===//
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

#include "SILGenSILBuilder.h"
#include "SILGenFunction.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
//                              Utility Methods
//===----------------------------------------------------------------------===//

SILGenModule &SILGenSILBuilder::getSILGenModule() const { return SGF.SGM; }

//===----------------------------------------------------------------------===//
//                                Constructors
//===----------------------------------------------------------------------===//

SILGenSILBuilder::SILGenSILBuilder(SILGenFunction &SGF)
    : SILBuilder(SGF.F), SGF(SGF) {}

SILGenSILBuilder::SILGenSILBuilder(
    SILGenFunction &SGF, SILBasicBlock *insertBB,
    SmallVectorImpl<SILInstruction *> *insertedInsts)
    : SILBuilder(insertBB, insertedInsts), SGF(SGF) {}

SILGenSILBuilder::SILGenSILBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB,
                                   SILBasicBlock::iterator insertInst)
    : SILBuilder(insertBB, insertInst), SGF(SGF) {}

//===----------------------------------------------------------------------===//
//              Instruction Emission + Conformance Endowed APIs
//===----------------------------------------------------------------------===//
//
// This section contains wrappers around SILBuilder SILValue APIs that add extra
// conformances. These are the only places where we should be accessing
// SILBuilder APIs directly.
//

ApplyInst *SILGenSILBuilder::createApply(SILLocation loc, SILValue fn,
                                         SILType substFnTy, SILType result,
                                         SubstitutionMap subs,
                                         ArrayRef<SILValue> args) {
  return SILBuilder::createApply(loc, fn, subs, args, false);
}

TryApplyInst *SILGenSILBuilder::createTryApply(
    SILLocation loc, SILValue fn, SILType substFnTy, SubstitutionMap subs,
    ArrayRef<SILValue> args, SILBasicBlock *normalBB, SILBasicBlock *errorBB) {
  return SILBuilder::createTryApply(loc, fn, subs, args, normalBB, errorBB);
}

BeginApplyInst *SILGenSILBuilder::createBeginApply(SILLocation loc, SILValue fn,
                                                   SubstitutionMap subs,
                                                   ArrayRef<SILValue> args) {
  return SILBuilder::createBeginApply(loc, fn, subs, args, false);
}

PartialApplyInst *SILGenSILBuilder::createPartialApply(
    SILLocation loc, SILValue fn, SILType substFnTy, SubstitutionMap subs,
    ArrayRef<SILValue> args, SILType closureTy) {
  return SILBuilder::createPartialApply(
      loc, fn, subs, args,
      closureTy.getAs<SILFunctionType>()->getCalleeConvention());
}

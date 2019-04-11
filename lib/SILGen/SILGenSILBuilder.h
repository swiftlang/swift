//===--- SILGenSILBuilder.h -----------------------------------------------===//
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

#ifndef SWIFT_SILGEN_SILGENSILBUILDER_H
#define SWIFT_SILGEN_SILGENSILBUILDER_H

#include "swift/SIL/SILBuilder.h"

namespace swift {
namespace Lowering {

class SILGenFunction;

class SILGenSILBuilder : public SILBuilder {
  SILGenFunction &SGF;

public:
  SILGenSILBuilder(SILGenFunction &SGF);
  SILGenSILBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB,
                   SmallVectorImpl<SILInstruction *> *insertedInsts = nullptr);
  SILGenSILBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB,
                   SILBasicBlock::iterator insertInst);

  // Create a new builder, inheriting the given builder's context and debug
  // scope.
  SILGenSILBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB,
                   const SILDebugScope *debugScope,
                   SILBuilderContext &builderCtx)
      : SILBuilder(insertBB, debugScope, builderCtx), SGF(SGF) {}

  SILGenModule &getSILGenModule() const;

  // Metatype instructions use the conformances necessary to instantiate the
  // type.

  MetatypeInst *createMetatype(SILLocation loc, SILType metatype);
  // Generic apply instructions use the conformances necessary to form the call.

  using SILBuilder::createApply;

  ApplyInst *createApply(SILLocation loc, SILValue fn, SILType SubstFnTy,
                         SILType result, SubstitutionMap subs,
                         ArrayRef<SILValue> args);

  TryApplyInst *createTryApply(SILLocation loc, SILValue fn, SILType substFnTy,
                               SubstitutionMap subs, ArrayRef<SILValue> args,
                               SILBasicBlock *normalBB, SILBasicBlock *errorBB);

  BeginApplyInst *createBeginApply(SILLocation loc, SILValue fn,
                                   SubstitutionMap subs,
                                   ArrayRef<SILValue> args);

  PartialApplyInst *createPartialApply(SILLocation loc, SILValue fn,
                                       SILType substFnTy, SubstitutionMap subs,
                                       ArrayRef<SILValue> args,
                                       SILType closureTy);
  BuiltinInst *createBuiltin(SILLocation loc, Identifier name, SILType resultTy,
                             SubstitutionMap subs, ArrayRef<SILValue> args);

  // Existential containers use the conformances needed by the existential
  // box.

  InitExistentialAddrInst *
  createInitExistentialAddr(SILLocation loc, SILValue existential,
                            CanType formalConcreteType,
                            SILType loweredConcreteType,
                            ArrayRef<ProtocolConformanceRef> conformances);

  InitExistentialValueInst *
  createInitExistentialValue(SILLocation loc, SILType existentialType,
                             CanType formalConcreteType, SILValue concrete,
                             ArrayRef<ProtocolConformanceRef> conformances);

  InitExistentialMetatypeInst *
  createInitExistentialMetatype(SILLocation loc, SILValue metatype,
                                SILType existentialType,
                                ArrayRef<ProtocolConformanceRef> conformances);

  InitExistentialRefInst *
  createInitExistentialRef(SILLocation loc, SILType existentialType,
                           CanType formalConcreteType, SILValue concreteValue,
                           ArrayRef<ProtocolConformanceRef> conformances);

  AllocExistentialBoxInst *
  createAllocExistentialBox(SILLocation loc, SILType existentialType,
                            CanType concreteType,
                            ArrayRef<ProtocolConformanceRef> conformances);
};

} // namespace Lowering
} // namespace swift

#endif

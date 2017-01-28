//===--- SILGenBuilder.h ----------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILGEN_SILGENBUILDER_H
#define SWIFT_SILGEN_SILGENBUILDER_H

#include "ManagedValue.h"
#include "swift/SIL/SILBuilder.h"

namespace swift {
namespace Lowering {

class SILGenFunction;

/// A subclass of SILBuilder that tracks used protocol conformances and will
/// eventually only traffic in ownership endowed APIs.
///
/// The goal is to make this eventually composed with SILBuilder so that all
/// APIs only vend ManagedValues.
class SILGenBuilder : public SILBuilder {
  SILGenFunction &gen;

public:
  SILGenBuilder(SILGenFunction &gen);
  SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB);
  SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB,
                SmallVectorImpl<SILInstruction *> *insertedInsts);
  SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB,
                SILBasicBlock::iterator insertInst);

  SILGenBuilder(SILGenFunction &gen, SILFunction::iterator insertBB)
      : SILGenBuilder(gen, &*insertBB) {}
  SILGenBuilder(SILGenFunction &gen, SILFunction::iterator insertBB,
                SmallVectorImpl<SILInstruction *> *insertedInsts)
      : SILGenBuilder(gen, &*insertBB, insertedInsts) {}
  SILGenBuilder(SILGenFunction &gen, SILFunction::iterator insertBB,
                SILInstruction *insertInst)
      : SILGenBuilder(gen, &*insertBB, insertInst->getIterator()) {}
  SILGenBuilder(SILGenFunction &gen, SILFunction::iterator insertBB,
                SILBasicBlock::iterator insertInst)
      : SILGenBuilder(gen, &*insertBB, insertInst) {}

  SILGenModule &getSILGenModule() const;

  // Metatype instructions use the conformances necessary to instantiate the
  // type.

  MetatypeInst *createMetatype(SILLocation Loc, SILType Metatype);

  // Generic apply instructions use the conformances necessary to form the call.

  using SILBuilder::createApply;

  ApplyInst *createApply(SILLocation Loc, SILValue Fn, SILType SubstFnTy,
                         SILType Result, ArrayRef<Substitution> Subs,
                         ArrayRef<SILValue> Args);

  TryApplyInst *createTryApply(SILLocation loc, SILValue fn, SILType substFnTy,
                               ArrayRef<Substitution> subs,
                               ArrayRef<SILValue> args, SILBasicBlock *normalBB,
                               SILBasicBlock *errorBB);

  PartialApplyInst *createPartialApply(SILLocation Loc, SILValue Fn,
                                       SILType SubstFnTy,
                                       ArrayRef<Substitution> Subs,
                                       ArrayRef<SILValue> Args,
                                       SILType ClosureTy);

  BuiltinInst *createBuiltin(SILLocation Loc, Identifier Name, SILType ResultTy,
                             ArrayRef<Substitution> Subs,
                             ArrayRef<SILValue> Args);

  // Existential containers use the conformances needed by the existential
  // box.

  InitExistentialAddrInst *
  createInitExistentialAddr(SILLocation Loc, SILValue Existential,
                            CanType FormalConcreteType,
                            SILType LoweredConcreteType,
                            ArrayRef<ProtocolConformanceRef> Conformances);

  InitExistentialMetatypeInst *
  createInitExistentialMetatype(SILLocation loc, SILValue metatype,
                                SILType existentialType,
                                ArrayRef<ProtocolConformanceRef> conformances);

  InitExistentialRefInst *
  createInitExistentialRef(SILLocation Loc, SILType ExistentialType,
                           CanType FormalConcreteType, SILValue Concrete,
                           ArrayRef<ProtocolConformanceRef> Conformances);

  AllocExistentialBoxInst *
  createAllocExistentialBox(SILLocation Loc, SILType ExistentialType,
                            CanType ConcreteType,
                            ArrayRef<ProtocolConformanceRef> Conformances);

  //===---
  // Ownership Endowed APIs
  //

  using SILBuilder::createStructExtract;
  using SILBuilder::createCopyValue;
  using SILBuilder::createCopyUnownedValue;
  ManagedValue createStructExtract(SILLocation Loc, ManagedValue Base,
                                   VarDecl *Decl);

  ManagedValue createCopyValue(SILLocation Loc, ManagedValue OriginalValue);

  ManagedValue createCopyUnownedValue(SILLocation Loc,
                                      ManagedValue OriginalValue);

  ManagedValue createUnsafeCopyUnownedValue(SILLocation Loc,
                                            ManagedValue OriginalValue);
  ManagedValue createOwnedPHIArgument(SILType Type);

  using SILBuilder::createAllocRef;
  ManagedValue createAllocRef(SILLocation Loc, SILType RefType, bool objc, bool canAllocOnStack,
                              ArrayRef<SILType> ElementTypes,
                              ArrayRef<ManagedValue> ElementCountOperands);
  using SILBuilder::createAllocRefDynamic;
  ManagedValue createAllocRefDynamic(SILLocation Loc, ManagedValue Operand, SILType RefType, bool objc,
                                     ArrayRef<SILType> ElementTypes,
                                     ArrayRef<ManagedValue> ElementCountOperands);
};

} // namespace Lowering
} // namespace swift

#endif

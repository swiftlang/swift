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
class SGFContext;

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

  MetatypeInst *createMetatype(SILLocation loc, SILType metatype);

  // Generic apply instructions use the conformances necessary to form the call.

  using SILBuilder::createApply;

  ApplyInst *createApply(SILLocation loc, SILValue fn, SILType SubstFnTy,
                         SILType result, SubstitutionList subs,
                         ArrayRef<SILValue> args);

  TryApplyInst *createTryApply(SILLocation loc, SILValue fn, SILType substFnTy,
                               SubstitutionList subs,
                               ArrayRef<SILValue> args, SILBasicBlock *normalBB,
                               SILBasicBlock *errorBB);

  PartialApplyInst *createPartialApply(SILLocation loc, SILValue fn,
                                       SILType substFnTy, SubstitutionList subs,
                                       ArrayRef<SILValue> args,
                                       SILType closureTy);

  BuiltinInst *createBuiltin(SILLocation loc, Identifier name, SILType resultTy,
                             SubstitutionList subs, ArrayRef<SILValue> args);

  // Existential containers use the conformances needed by the existential
  // box.

  InitExistentialAddrInst *
  createInitExistentialAddr(SILLocation loc, SILValue existential,
                            CanType formalConcreteType,
                            SILType loweredConcreteType,
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

  //===---
  // Ownership Endowed APIs
  //

  using SILBuilder::createStructExtract;
  using SILBuilder::createCopyValue;
  using SILBuilder::createCopyUnownedValue;
  ManagedValue createStructExtract(SILLocation loc, ManagedValue base,
                                   VarDecl *decl);

  /// Emit a +1 copy on \p originalValue that lives until the end of the current
  /// lexical scope.
  ManagedValue createCopyValue(SILLocation loc, ManagedValue originalValue);

  /// Emit a +1 copy on \p originalValue that lives until the end of the current
  /// lexical scope.
  ///
  /// This reuses a passed in lowering.
  ManagedValue createCopyValue(SILLocation loc, ManagedValue originalValue,
                               const TypeLowering &lowering);

  /// Emit a +1 copy of \p originalValue into newAddr that lives until the end
  /// of the current Formal Evaluation Scope.
  ManagedValue createFormalAccessCopyAddr(SILLocation loc,
                                          ManagedValue originalAddr,
                                          SILValue newAddr, IsTake_t isTake,
                                          IsInitialization_t isInit);

  /// Emit a +1 copy of \p originalValue into newAddr that lives until the end
  /// Formal Evaluation Scope.
  ManagedValue createFormalAccessCopyValue(SILLocation loc,
                                           ManagedValue originalValue);

  ManagedValue createCopyUnownedValue(SILLocation loc,
                                      ManagedValue originalValue);

  ManagedValue createUnsafeCopyUnownedValue(SILLocation loc,
                                            ManagedValue originalValue);
  ManagedValue createOwnedPHIArgument(SILType type);

  using SILBuilder::createAllocRef;
  ManagedValue createAllocRef(SILLocation loc, SILType refType, bool objc,
                              bool canAllocOnStack,
                              ArrayRef<SILType> elementTypes,
                              ArrayRef<ManagedValue> elementCountOperands);
  using SILBuilder::createAllocRefDynamic;
  ManagedValue
  createAllocRefDynamic(SILLocation loc, ManagedValue operand, SILType refType,
                        bool objc, ArrayRef<SILType> elementTypes,
                        ArrayRef<ManagedValue> elementCountOperands);

  using SILBuilder::createTupleExtract;

  ManagedValue createTupleExtract(SILLocation loc, ManagedValue value,
                                  unsigned index, SILType type);
  ManagedValue createTupleExtract(SILLocation loc, ManagedValue value,
                                  unsigned index);

  using SILBuilder::createLoadBorrow;
  ManagedValue createLoadBorrow(SILLocation loc, ManagedValue base);
  ManagedValue createFormalAccessLoadBorrow(SILLocation loc, ManagedValue base);

  /// Prepares a buffer to receive the result of an expression, either using the
  /// 'emit into' initialization buffer if available, or allocating a temporary
  /// allocation if not. After the buffer has been prepared, the rvalueEmitter
  /// closure will be called with the buffer ready for initialization. After the
  /// emitter has been called, the buffer will complete its initialization.
  ///
  /// \return an empty value if the buffer was taken from the context.
  ManagedValue bufferForExpr(SILLocation loc, SILType ty,
                             const TypeLowering &lowering,
                             SGFContext context,
                             std::function<void(SILValue)> rvalueEmitter);
};

} // namespace Lowering
} // namespace swift

#endif

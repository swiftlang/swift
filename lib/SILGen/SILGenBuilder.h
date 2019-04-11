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
///
/// \file
///
/// This file defines SILGenBuilder, a subclass of SILBuilder that provides APIs
/// that traffic in ManagedValue. The intention is that if one is using a
/// SILGenBuilder, the SILGenBuilder will handle preserving ownership invariants
/// (or assert upon failure) freeing the implementor of such concerns.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILGEN_SILGENBUILDER_H
#define SWIFT_SILGEN_SILGENBUILDER_H

#include "Cleanup.h"
#include "JumpDest.h"
#include "ManagedValue.h"
#include "RValue.h"
#include "SILGenSILBuilder.h"
#include "swift/Basic/ProfileCounter.h"

namespace swift {
namespace Lowering {

class SILGenFunction;
class SGFContext;

/// A subclass of SILBuilder that tracks used protocol conformances and will
/// eventually only traffic in ownership endowed APIs.
///
/// The goal is to make this eventually composed with SILBuilder so that all
/// APIs only vend ManagedValues.
class SILGenBuilder : public SILGenSILBuilder {
  SILGenFunction &SGF;

public:
  SILGenBuilder(SILGenFunction &SGF);
  SILGenBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB,
                SmallVectorImpl<SILInstruction *> *insertedInsts = nullptr);
  SILGenBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB,
                SILBasicBlock::iterator insertInst);

  // Create a new builder, inheriting the given builder's context and debug
  // scope.
  SILGenBuilder(SILGenBuilder &builder, SILBasicBlock *insertBB)
      : SILGenSILBuilder(builder.SGF, insertBB, builder.getCurrentDebugScope(),
                         builder.getBuilderContext()),
        SGF(builder.SGF) {}

  SILGenModule &getSILGenModule() const;
  SILGenFunction &getSILGenFunction() const { return SGF; }

  using SILGenSILBuilder::createInitExistentialValue;
  ManagedValue
  createInitExistentialValue(SILLocation loc, SILType existentialType,
                             CanType formalConcreteType, ManagedValue concrete,
                             ArrayRef<ProtocolConformanceRef> conformances);
  using SILGenSILBuilder::createInitExistentialRef;
  ManagedValue
  createInitExistentialRef(SILLocation loc, SILType existentialType,
                           CanType formalConcreteType, ManagedValue concrete,
                           ArrayRef<ProtocolConformanceRef> conformances);

  using SILGenSILBuilder::createPartialApply;
  ManagedValue createPartialApply(SILLocation loc, SILValue fn,
                                  SILType substFnTy, SubstitutionMap subs,
                                  ArrayRef<ManagedValue> args,
                                  SILType closureTy);
  ManagedValue createPartialApply(SILLocation loc, ManagedValue fn,
                                  SILType substFnTy, SubstitutionMap subs,
                                  ArrayRef<ManagedValue> args,
                                  SILType closureTy) {
    return createPartialApply(loc, fn.getValue(), substFnTy, subs, args,
                              closureTy);
  }

  using SILGenSILBuilder::createStructExtract;
  ManagedValue createStructExtract(SILLocation loc, ManagedValue base,
                                   VarDecl *decl);

  using SILGenSILBuilder::createRefElementAddr;
  ManagedValue createRefElementAddr(SILLocation loc, ManagedValue operand,
                                    VarDecl *field, SILType resultTy);

  using SILGenSILBuilder::createCopyValue;

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

#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)            \
  using SILGenSILBuilder::createCopy##Name##Value;                             \
  ManagedValue createCopy##Name##Value(SILLocation loc,                        \
                                       ManagedValue originalValue);
#define UNCHECKED_REF_STORAGE(Name, ...) \
  ManagedValue createUnsafeCopy##Name##Value(SILLocation loc, \
                                             ManagedValue originalValue);
#include "swift/AST/ReferenceStorage.def"

  ManagedValue createOwnedPhiArgument(SILType type);
  ManagedValue createGuaranteedPhiArgument(SILType type);

  using SILGenSILBuilder::createMarkUninitialized;
  ManagedValue createMarkUninitialized(ValueDecl *decl, ManagedValue operand,
                                       MarkUninitializedInst::Kind muKind);

  using SILGenSILBuilder::createAllocRef;
  ManagedValue createAllocRef(SILLocation loc, SILType refType, bool objc,
                              bool canAllocOnStack,
                              ArrayRef<SILType> elementTypes,
                              ArrayRef<ManagedValue> elementCountOperands);
  using SILGenSILBuilder::createAllocRefDynamic;
  ManagedValue
  createAllocRefDynamic(SILLocation loc, ManagedValue operand, SILType refType,
                        bool objc, ArrayRef<SILType> elementTypes,
                        ArrayRef<ManagedValue> elementCountOperands);

  using SILGenSILBuilder::createTuple;
  ManagedValue createTuple(SILLocation loc, SILType type,
                           ArrayRef<ManagedValue> elements);
  using SILGenSILBuilder::createTupleExtract;
  ManagedValue createTupleExtract(SILLocation loc, ManagedValue value,
                                  unsigned index, SILType type);
  ManagedValue createTupleExtract(SILLocation loc, ManagedValue value,
                                  unsigned index);
  using SILGenSILBuilder::createTupleElementAddr;
  ManagedValue createTupleElementAddr(SILLocation loc, ManagedValue addr,
                                      unsigned index, SILType type);
  ManagedValue createTupleElementAddr(SILLocation loc, ManagedValue addr,
                                      unsigned index);

  using SILGenSILBuilder::createLoadBorrow;
  ManagedValue createLoadBorrow(SILLocation loc, ManagedValue base);
  ManagedValue createFormalAccessLoadBorrow(SILLocation loc, ManagedValue base);

  using SILGenSILBuilder::createStoreBorrow;
  void createStoreBorrow(SILLocation loc, ManagedValue value, SILValue address);

  /// Create a store_borrow if we have a non-trivial value and a store [trivial]
  /// otherwise.
  void createStoreBorrowOrTrivial(SILLocation loc, ManagedValue value,
                                  SILValue address);

  /// Prepares a buffer to receive the result of an expression, either using the
  /// 'emit into' initialization buffer if available, or allocating a temporary
  /// allocation if not. After the buffer has been prepared, the rvalueEmitter
  /// closure will be called with the buffer ready for initialization. After the
  /// emitter has been called, the buffer will complete its initialization.
  ///
  /// \return an empty value if the buffer was taken from the context.
  ManagedValue bufferForExpr(SILLocation loc, SILType ty,
                             const TypeLowering &lowering, SGFContext context,
                             llvm::function_ref<void(SILValue)> rvalueEmitter);

  using SILGenSILBuilder::createUncheckedEnumData;
  ManagedValue createUncheckedEnumData(SILLocation loc, ManagedValue operand,
                                       EnumElementDecl *element);

  using SILGenSILBuilder::createUncheckedTakeEnumDataAddr;
  ManagedValue createUncheckedTakeEnumDataAddr(SILLocation loc, ManagedValue operand,
                                               EnumElementDecl *element, SILType ty);

  ManagedValue createLoadTake(SILLocation loc, ManagedValue addr);
  ManagedValue createLoadTake(SILLocation loc, ManagedValue addr,
                              const TypeLowering &lowering);
  ManagedValue createLoadCopy(SILLocation loc, ManagedValue addr);
  ManagedValue createLoadCopy(SILLocation loc, ManagedValue addr,
                              const TypeLowering &lowering);

  /// Create a SILArgument for an input parameter. Asserts if used to create a
  /// function argument for an out parameter.
  ManagedValue createInputFunctionArgument(SILType type, ValueDecl *decl);

  /// Create a SILArgument for an input parameter. Uses \p loc to create any
  /// copies necessary. Asserts if used to create a function argument for an out
  /// parameter.
  ///
  /// *NOTE* This API purposely used an Optional<SILLocation> to distinguish
  /// this API from the ValueDecl * API in C++. This is necessary since
  /// ValueDecl * can implicitly convert to SILLocation. The optional forces the
  /// user to be explicit that they want to use this API.
  ManagedValue createInputFunctionArgument(SILType type,
                                           Optional<SILLocation> loc);

  using SILGenSILBuilder::createEnum;
  ManagedValue createEnum(SILLocation loc, ManagedValue payload,
                          EnumElementDecl *decl, SILType type);

  ManagedValue createSemanticLoadBorrow(SILLocation loc, ManagedValue addr);

  ManagedValue
  formalAccessBufferForExpr(SILLocation loc, SILType ty,
                            const TypeLowering &lowering, SGFContext context,
                            llvm::function_ref<void(SILValue)> rvalueEmitter);

  using SILGenSILBuilder::createUnconditionalCheckedCastValue;
  ManagedValue
  createUnconditionalCheckedCastValue(SILLocation loc,
                                      ManagedValue operand, SILType type);
  using SILGenSILBuilder::createUnconditionalCheckedCast;
  ManagedValue createUnconditionalCheckedCast(SILLocation loc,
                                              ManagedValue operand,
                                              SILType type);

  using SILGenSILBuilder::createCheckedCastBranch;
  void createCheckedCastBranch(SILLocation loc, bool isExact,
                               ManagedValue operand, SILType type,
                               SILBasicBlock *trueBlock,
                               SILBasicBlock *falseBlock,
                               ProfileCounter Target1Count,
                               ProfileCounter Target2Count);

  using SILGenSILBuilder::createCheckedCastValueBranch;
  void createCheckedCastValueBranch(SILLocation loc, ManagedValue operand,
                                    SILType type, SILBasicBlock *trueBlock,
                                    SILBasicBlock *falseBlock);

  using SILGenSILBuilder::createUpcast;
  ManagedValue createUpcast(SILLocation loc, ManagedValue original,
                            SILType type);

  using SILGenSILBuilder::tryCreateUncheckedRefCast;
  ManagedValue tryCreateUncheckedRefCast(SILLocation loc, ManagedValue original,
                                         SILType type);

  using SILGenSILBuilder::createUncheckedTrivialBitCast;
  ManagedValue createUncheckedTrivialBitCast(SILLocation loc,
                                             ManagedValue original,
                                             SILType type);

  using SILGenSILBuilder::createUncheckedRefCast;
  ManagedValue createUncheckedRefCast(SILLocation loc, ManagedValue original,
                                      SILType type);

  using SILGenSILBuilder::createUncheckedAddrCast;
  ManagedValue createUncheckedAddrCast(SILLocation loc, ManagedValue op,
                                       SILType resultTy);

  using SILGenSILBuilder::createUncheckedBitCast;
  ManagedValue createUncheckedBitCast(SILLocation loc, ManagedValue original,
                                      SILType type);

  using SILGenSILBuilder::createOpenExistentialRef;
  ManagedValue createOpenExistentialRef(SILLocation loc, ManagedValue arg,
                                        SILType openedType);

  using SILGenSILBuilder::createOpenExistentialValue;
  ManagedValue createOpenExistentialValue(SILLocation loc,
                                          ManagedValue original, SILType type);

  using SILGenSILBuilder::createOpenExistentialBoxValue;
  ManagedValue createOpenExistentialBoxValue(SILLocation loc,
                                          ManagedValue original, SILType type);

  using SILGenSILBuilder::createOpenExistentialMetatype;
  ManagedValue createOpenExistentialMetatype(SILLocation loc,
                                             ManagedValue value,
                                             SILType openedType);

  /// Convert a @convention(block) value to AnyObject.
  ManagedValue createBlockToAnyObject(SILLocation loc, ManagedValue block,
                                      SILType type);

  using SILGenSILBuilder::createOptionalSome;
  ManagedValue createOptionalSome(SILLocation Loc, ManagedValue Arg);
  ManagedValue createManagedOptionalNone(SILLocation Loc, SILType Type);

  // TODO: Rename this to createFunctionRef once all calls to createFunctionRef
  // are removed.
  ManagedValue createManagedFunctionRef(SILLocation loc, SILFunction *f);

  using SILGenSILBuilder::createConvertFunction;
  ManagedValue createConvertFunction(SILLocation loc, ManagedValue fn,
                                     SILType resultTy,
                                     bool WithoutActuallyEscaping = false);

  using SILGenSILBuilder::createConvertEscapeToNoEscape;
  ManagedValue
  createConvertEscapeToNoEscape(SILLocation loc, ManagedValue fn,
                                SILType resultTy);

  using SILGenSILBuilder::createStore;
  /// Forward \p value into \p address.
  ///
  /// This will forward value's cleanup (if it has one) into the equivalent
  /// cleanup on address. In practice this means if the value is non-trivial,
  /// the memory location will at end of scope have a destroy_addr applied to
  /// it.
  ManagedValue createStore(SILLocation loc, ManagedValue value,
                           SILValue address, StoreOwnershipQualifier qualifier);

  using SILGenSILBuilder::createSuperMethod;
  ManagedValue createSuperMethod(SILLocation loc, ManagedValue operand,
                                 SILDeclRef member, SILType methodTy);

  using SILGenSILBuilder::createObjCSuperMethod;
  ManagedValue createObjCSuperMethod(SILLocation loc, ManagedValue operand,
                                     SILDeclRef member, SILType methodTy);

  using SILGenSILBuilder::createValueMetatype;
  ManagedValue createValueMetatype(SILLocation loc, SILType metatype,
                                   ManagedValue base);

  using SILGenSILBuilder::createBridgeObjectToRef;
  ManagedValue createBridgeObjectToRef(SILLocation loc, ManagedValue mv,
                                       SILType destType);

  using SILGenSILBuilder::createRefToBridgeObject;
  ManagedValue createRefToBridgeObject(SILLocation loc, ManagedValue mv,
                                       SILValue bits);

  using SILGenSILBuilder::createBranch;
  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock,
                           ArrayRef<ManagedValue> Args);

  using SILGenSILBuilder::createReturn;
  ReturnInst *createReturn(SILLocation Loc, ManagedValue ReturnValue);

  using SILGenSILBuilder::emitDestructureValueOperation;
  /// Perform either a tuple or struct destructure and then pass its components
  /// as managed value one by one with an index to the closure.
  void emitDestructureValueOperation(
      SILLocation loc, ManagedValue value,
      function_ref<void(unsigned, ManagedValue)> func);

  using SILGenSILBuilder::createProjectBox;
  ManagedValue createProjectBox(SILLocation loc, ManagedValue mv,
                                unsigned index);
};

} // namespace Lowering
} // namespace swift

#endif

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
#include "swift/Basic/ProfileCounter.h"
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
  SILGenFunction &SGF;

public:
  SILGenBuilder(SILGenFunction &SGF);
  SILGenBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB);
  SILGenBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB,
                SmallVectorImpl<SILInstruction *> *insertedInsts);
  SILGenBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB,
                SILBasicBlock::iterator insertInst);

  SILGenBuilder(SILGenFunction &SGF, SILFunction::iterator insertBB)
      : SILGenBuilder(SGF, &*insertBB) {}
  SILGenBuilder(SILGenFunction &SGF, SILFunction::iterator insertBB,
                SmallVectorImpl<SILInstruction *> *insertedInsts)
      : SILGenBuilder(SGF, &*insertBB, insertedInsts) {}
  SILGenBuilder(SILGenFunction &SGF, SILFunction::iterator insertBB,
                SILInstruction *insertInst)
      : SILGenBuilder(SGF, &*insertBB, insertInst->getIterator()) {}
  SILGenBuilder(SILGenFunction &SGF, SILFunction::iterator insertBB,
                SILBasicBlock::iterator insertInst)
      : SILGenBuilder(SGF, &*insertBB, insertInst) {}

  SILGenModule &getSILGenModule() const;
  SILGenFunction &getSILGenFunction() const { return SGF; }

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

  InitExistentialValueInst *
  createInitExistentialValue(SILLocation loc, SILType existentialType,
                             CanType formalConcreteType, SILValue concrete,
                             ArrayRef<ProtocolConformanceRef> conformances);
  ManagedValue
  createInitExistentialValue(SILLocation loc, SILType existentialType,
                             CanType formalConcreteType, ManagedValue concrete,
                             ArrayRef<ProtocolConformanceRef> conformances);

  InitExistentialMetatypeInst *
  createInitExistentialMetatype(SILLocation loc, SILValue metatype,
                                SILType existentialType,
                                ArrayRef<ProtocolConformanceRef> conformances);

  InitExistentialRefInst *
  createInitExistentialRef(SILLocation loc, SILType existentialType,
                           CanType formalConcreteType, SILValue concreteValue,
                           ArrayRef<ProtocolConformanceRef> conformances);

  ManagedValue
  createInitExistentialRef(SILLocation loc, SILType existentialType,
                           CanType formalConcreteType, ManagedValue concrete,
                           ArrayRef<ProtocolConformanceRef> conformances);

  AllocExistentialBoxInst *
  createAllocExistentialBox(SILLocation loc, SILType existentialType,
                            CanType concreteType,
                            ArrayRef<ProtocolConformanceRef> conformances);

  //===---
  // Ownership Endowed APIs
  //

  using SILBuilder::createStructExtract;
  ManagedValue createStructExtract(SILLocation loc, ManagedValue base,
                                   VarDecl *decl);

  using SILBuilder::createRefElementAddr;
  ManagedValue createRefElementAddr(SILLocation loc, ManagedValue operand,
                                    VarDecl *field, SILType resultTy);

  using SILBuilder::createCopyValue;
  using SILBuilder::createCopyUnownedValue;

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
  ManagedValue createGuaranteedPHIArgument(SILType type);

  using SILBuilder::createMarkUninitialized;
  ManagedValue createMarkUninitialized(ValueDecl *decl, ManagedValue operand,
                                       MarkUninitializedInst::Kind muKind);

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

  using SILBuilder::createTuple;
  ManagedValue createTuple(SILLocation loc, SILType type,
                           ArrayRef<ManagedValue> elements);
  using SILBuilder::createTupleExtract;
  ManagedValue createTupleExtract(SILLocation loc, ManagedValue value,
                                  unsigned index, SILType type);
  ManagedValue createTupleExtract(SILLocation loc, ManagedValue value,
                                  unsigned index);
  using SILBuilder::createTupleElementAddr;
  ManagedValue createTupleElementAddr(SILLocation loc, ManagedValue addr,
                                      unsigned index, SILType type);
  ManagedValue createTupleElementAddr(SILLocation loc, ManagedValue addr,
                                      unsigned index);

  using SILBuilder::createLoadBorrow;
  ManagedValue createLoadBorrow(SILLocation loc, ManagedValue base);
  ManagedValue createFormalAccessLoadBorrow(SILLocation loc, ManagedValue base);

  using SILBuilder::createStoreBorrow;
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
                             const TypeLowering &lowering,
                             SGFContext context,
                             std::function<void(SILValue)> rvalueEmitter);

  using SILBuilder::createUncheckedEnumData;
  ManagedValue createUncheckedEnumData(SILLocation loc, ManagedValue operand,
                                       EnumElementDecl *element);

  using SILBuilder::createUncheckedTakeEnumDataAddr;
  ManagedValue createUncheckedTakeEnumDataAddr(SILLocation loc, ManagedValue operand,
                                               EnumElementDecl *element, SILType ty);

  ManagedValue createLoadTake(SILLocation loc, ManagedValue addr);
  ManagedValue createLoadTake(SILLocation loc, ManagedValue addr,
                              const TypeLowering &lowering);
  ManagedValue createLoadCopy(SILLocation loc, ManagedValue addr);
  ManagedValue createLoadCopy(SILLocation loc, ManagedValue addr,
                              const TypeLowering &lowering);

  ManagedValue createFunctionArgument(SILType type, ValueDecl *decl);

  using SILBuilder::createEnum;
  ManagedValue createEnum(SILLocation loc, ManagedValue payload,
                          EnumElementDecl *decl, SILType type);

  ManagedValue createSemanticLoadBorrow(SILLocation loc, ManagedValue addr);

  ManagedValue formalAccessBufferForExpr(
      SILLocation loc, SILType ty, const TypeLowering &lowering,
      SGFContext context, std::function<void(SILValue)> rvalueEmitter);

  using SILBuilder::createUnconditionalCheckedCastValue;
  ManagedValue
  createUnconditionalCheckedCastValue(SILLocation loc,
                                      ManagedValue operand, SILType type);
  using SILBuilder::createUnconditionalCheckedCast;
  ManagedValue createUnconditionalCheckedCast(SILLocation loc,
                                              ManagedValue operand,
                                              SILType type);

  using SILBuilder::createCheckedCastBranch;
  void createCheckedCastBranch(SILLocation loc, bool isExact,
                               ManagedValue operand, SILType type,
                               SILBasicBlock *trueBlock,
                               SILBasicBlock *falseBlock,
                               ProfileCounter Target1Count,
                               ProfileCounter Target2Count);

  using SILBuilder::createCheckedCastValueBranch;
  void createCheckedCastValueBranch(SILLocation loc, ManagedValue operand,
                                    SILType type, SILBasicBlock *trueBlock,
                                    SILBasicBlock *falseBlock);

  using SILBuilder::createUpcast;
  ManagedValue createUpcast(SILLocation loc, ManagedValue original,
                            SILType type);

  using SILBuilder::createUncheckedRefCast;
  ManagedValue createUncheckedRefCast(SILLocation loc, ManagedValue original,
                                      SILType type);

  using SILBuilder::createUncheckedBitCast;
  ManagedValue createUncheckedBitCast(SILLocation loc, ManagedValue original,
                                      SILType type);

  using SILBuilder::createOpenExistentialRef;
  ManagedValue createOpenExistentialRef(SILLocation loc, ManagedValue arg,
                                        SILType openedType);

  using SILBuilder::createOpenExistentialValue;
  ManagedValue createOpenExistentialValue(SILLocation loc,
                                          ManagedValue original, SILType type);

  using SILBuilder::createOpenExistentialBoxValue;
  ManagedValue createOpenExistentialBoxValue(SILLocation loc,
                                          ManagedValue original, SILType type);

  /// Convert a @convention(block) value to AnyObject.
  ManagedValue createBlockToAnyObject(SILLocation loc, ManagedValue block,
                                      SILType type);

  using SILBuilder::createOptionalSome;
  ManagedValue createOptionalSome(SILLocation Loc, ManagedValue Arg);
  ManagedValue createManagedOptionalNone(SILLocation Loc, SILType Type);

  /// Forward \p value into \p address.
  ///
  /// This will forward value's cleanup (if it has one) into the equivalent
  /// cleanup on address. In practice this means if the value is non-trivial,
  /// the memory location will at end of scope have a destroy_addr applied to
  /// it.
  ManagedValue createStore(SILLocation loc, ManagedValue value,
                           SILValue address, StoreOwnershipQualifier qualifier);

  using SILBuilder::createSuperMethod;
  ManagedValue createSuperMethod(SILLocation loc, ManagedValue operand,
                                 SILDeclRef member, SILType methodTy);

  using SILBuilder::createObjCSuperMethod;
  ManagedValue createObjCSuperMethod(SILLocation loc, ManagedValue operand,
                                     SILDeclRef member, SILType methodTy);

  using SILBuilder::createValueMetatype;
  ManagedValue createValueMetatype(SILLocation loc, SILType metatype,
                                   ManagedValue base);

  using SILBuilder::createBridgeObjectToRef;
  ManagedValue createBridgeObjectToRef(SILLocation loc, ManagedValue mv,
                                       SILType destType);

  using SILBuilder::createBranch;
  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock,
                           ArrayRef<ManagedValue> Args);

  using SILBuilder::createReturn;
  ReturnInst *createReturn(SILLocation Loc, ManagedValue ReturnValue);
};

class SwitchCaseFullExpr;

/// A class for building switch enums that handles all of the ownership
/// requirements for the user.
///
/// It assumes that the user passes in a block that takes in a ManagedValue and
/// returns a ManagedValue for the blocks exit argument. Should return an empty
/// ManagedValue to signal no result.
class SwitchEnumBuilder {
public:
  using NormalCaseHandler =
      std::function<void(ManagedValue, SwitchCaseFullExpr &)>;
  using DefaultCaseHandler =
      std::function<void(ManagedValue, SwitchCaseFullExpr &)>;

  enum class DefaultDispatchTime { BeforeNormalCases, AfterNormalCases };

private:
  struct NormalCaseData {
    EnumElementDecl *decl;
    SILBasicBlock *block;
    NullablePtr<SILBasicBlock> contBlock;
    NormalCaseHandler handler;
    ProfileCounter count;

    NormalCaseData(EnumElementDecl *decl, SILBasicBlock *block,
                   NullablePtr<SILBasicBlock> contBlock,
                   NormalCaseHandler handler, ProfileCounter count)
        : decl(decl), block(block), contBlock(contBlock), handler(handler),
          count(count) {}
    ~NormalCaseData() = default;
  };

  struct DefaultCaseData {
    SILBasicBlock *block;
    NullablePtr<SILBasicBlock> contBlock;
    DefaultCaseHandler handler;
    DefaultDispatchTime dispatchTime;
    ProfileCounter count;

    DefaultCaseData(SILBasicBlock *block, NullablePtr<SILBasicBlock> contBlock,
                    DefaultCaseHandler handler,
                    DefaultDispatchTime dispatchTime, ProfileCounter count)
        : block(block), contBlock(contBlock), handler(handler),
          dispatchTime(dispatchTime), count(count) {}
    ~DefaultCaseData() = default;
  };

  SILGenBuilder &builder;
  SILLocation loc;
  ManagedValue optional;
  llvm::Optional<DefaultCaseData> defaultBlockData;
  llvm::SmallVector<NormalCaseData, 8> caseDataArray;

public:
  SwitchEnumBuilder(SILGenBuilder &builder, SILLocation loc,
                    ManagedValue optional)
      : builder(builder), loc(loc), optional(optional) {}

  void addDefaultCase(
      SILBasicBlock *defaultBlock, NullablePtr<SILBasicBlock> contBlock,
      DefaultCaseHandler handle,
      DefaultDispatchTime dispatchTime = DefaultDispatchTime::AfterNormalCases,
      ProfileCounter count = ProfileCounter()) {
    defaultBlockData.emplace(defaultBlock, contBlock, handle, dispatchTime,
                             count);
  }

  void addCase(EnumElementDecl *decl, SILBasicBlock *caseBlock,
               NullablePtr<SILBasicBlock> contBlock, NormalCaseHandler handle,
               ProfileCounter count = ProfileCounter()) {
    caseDataArray.emplace_back(decl, caseBlock, contBlock, handle, count);
  }

  void emit() &&;

private:
  SILGenFunction &getSGF() const { return builder.getSILGenFunction(); }
};

} // namespace Lowering
} // namespace swift

#endif

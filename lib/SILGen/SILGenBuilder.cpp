//===--- SILGenBuilder.cpp ------------------------------------------------===//
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

#include "SILGenBuilder.h"
#include "ArgumentSource.h"
#include "RValue.h"
#include "SILGenFunction.h"
#include "Scope.h"
#include "SwitchEnumBuilder.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/SubstitutionMap.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
//                              Utility Methods
//===----------------------------------------------------------------------===//

SILGenModule &SILGenBuilder::getSILGenModule() const { return SGF.SGM; }

//===----------------------------------------------------------------------===//
//                                Constructors
//===----------------------------------------------------------------------===//

SILGenBuilder::SILGenBuilder(SILGenFunction &SGF)
    : SILBuilder(SGF.F), SGF(SGF) {}

SILGenBuilder::SILGenBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB,
                             SmallVectorImpl<SILInstruction *> *insertedInsts)
    : SILBuilder(insertBB, insertedInsts), SGF(SGF) {}

SILGenBuilder::SILGenBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB,
                             SILBasicBlock::iterator insertInst)
    : SILBuilder(insertBB, insertInst), SGF(SGF) {}

//===----------------------------------------------------------------------===//
//                             Managed Value APIs
//===----------------------------------------------------------------------===//

ManagedValue SILGenBuilder::createPartialApply(SILLocation loc, SILValue fn,
                                               SubstitutionMap subs,
                                               ArrayRef<ManagedValue> args,
                                               ParameterConvention calleeConvention) {
  llvm::SmallVector<SILValue, 8> values;
  llvm::transform(args, std::back_inserter(values),
                  [&](ManagedValue mv) -> SILValue {
    return mv.forward(getSILGenFunction());
  });
  SILValue result =
      createPartialApply(loc, fn, subs, values, calleeConvention);
  // Partial apply instructions create a box, so we need to put on a cleanup.
  return getSILGenFunction().emitManagedRValueWithCleanup(result);
}

ManagedValue
SILGenBuilder::createConvertFunction(SILLocation loc, ManagedValue fn,
                                     SILType resultTy,
                                     bool withoutActuallyEscaping) {
  CleanupCloner cloner(*this, fn);
  SILValue result = createConvertFunction(loc, fn.forward(getSILGenFunction()),
                                          resultTy, withoutActuallyEscaping);
  return cloner.clone(result);
}

ManagedValue SILGenBuilder::createConvertEscapeToNoEscape(
    SILLocation loc, ManagedValue fn, SILType resultTy) {

  auto fnType = fn.getType().castTo<SILFunctionType>();
  auto resultFnType = resultTy.castTo<SILFunctionType>();

  // Escaping to noescape conversion.
  assert(resultFnType->getRepresentation() ==
             SILFunctionTypeRepresentation::Thick &&
         fnType->getRepresentation() ==
             SILFunctionTypeRepresentation::Thick &&
         !fnType->isNoEscape() && resultFnType->isNoEscape() &&
         "Expect a escaping to noescape conversion");
  (void)fnType;
  (void)resultFnType;
  SILValue fnValue = fn.getValue();
  SILValue result =
      createConvertEscapeToNoEscape(loc, fnValue, resultTy, false);
  return ManagedValue::forTrivialObjectRValue(result);
}

ManagedValue SILGenBuilder::createInitExistentialValue(
    SILLocation loc, SILType existentialType, CanType formalConcreteType,
    ManagedValue concrete, ArrayRef<ProtocolConformanceRef> conformances) {
  // *NOTE* we purposely do not use a cleanup cloner here. The reason why is no
  // matter whether we have a trivial or non-trivial input,
  // init_existential_value returns a +1 value (the COW box).
  SILValue v =
      createInitExistentialValue(loc, existentialType, formalConcreteType,
                                 concrete.forward(SGF), conformances);
  return SGF.emitManagedRValueWithCleanup(v);
}

ManagedValue SILGenBuilder::createInitExistentialRef(
    SILLocation Loc, SILType ExistentialType, CanType FormalConcreteType,
    ManagedValue Concrete, ArrayRef<ProtocolConformanceRef> Conformances) {
  CleanupCloner Cloner(*this, Concrete);
  InitExistentialRefInst *IERI =
      createInitExistentialRef(Loc, ExistentialType, FormalConcreteType,
                               Concrete.forward(SGF), Conformances);
  return Cloner.clone(IERI);
}

ManagedValue SILGenBuilder::createStructExtract(SILLocation loc,
                                                ManagedValue base,
                                                VarDecl *decl) {
  ManagedValue borrowedBase = base.formalAccessBorrow(SGF, loc);
  SILValue extract = createStructExtract(loc, borrowedBase.getValue(), decl);
  return ManagedValue::forUnmanaged(extract);
}

ManagedValue SILGenBuilder::createRefElementAddr(SILLocation loc,
                                                 ManagedValue operand,
                                                 VarDecl *field,
                                                 SILType resultTy) {
  operand = operand.formalAccessBorrow(SGF, loc);
  SILValue result = createRefElementAddr(loc, operand.getValue(), field);
  return ManagedValue::forUnmanaged(result);
}

ManagedValue SILGenBuilder::createCopyValue(SILLocation loc,
                                            ManagedValue originalValue) {
  auto &lowering = SGF.getTypeLowering(originalValue.getType());
  return createCopyValue(loc, originalValue, lowering);
}

ManagedValue SILGenBuilder::createCopyValue(SILLocation loc,
                                            ManagedValue originalValue,
                                            const TypeLowering &lowering) {
  if (lowering.isTrivial())
    return originalValue;

  SILType ty = originalValue.getType();
  assert(!ty.isAddress() && "Can not perform a copy value of an address typed "
         "value");

  if (ty.isObject() &&
      originalValue.getOwnershipKind() == ValueOwnershipKind::Any) {
    return originalValue;
  }

  SILValue result =
      lowering.emitCopyValue(*this, loc, originalValue.getValue());
  return SGF.emitManagedRValueWithCleanup(result, lowering);
}

#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  ManagedValue \
  SILGenBuilder::createCopy##Name##Value(SILLocation loc, \
                                         ManagedValue originalValue) { \
    auto ty = originalValue.getType().castTo<Name##StorageType>(); \
    assert(ty->isLoadable(ResilienceExpansion::Maximal)); \
    (void)ty; \
    SILValue result = createCopy##Name##Value(loc, originalValue.getValue()); \
    return SGF.emitManagedRValueWithCleanup(result); \
  }
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  ManagedValue \
  SILGenBuilder::createCopy##Name##Value(SILLocation loc, \
                                         ManagedValue originalValue) { \
    SILValue result = createCopy##Name##Value(loc, originalValue.getValue()); \
    return SGF.emitManagedRValueWithCleanup(result); \
  }
#define UNCHECKED_REF_STORAGE(Name, ...)                                       \
  ManagedValue SILGenBuilder::createCopy##Name##Value(                         \
      SILLocation loc, ManagedValue originalValue) {                           \
    /* *NOTE* The reason why this is unsafe is that we are converting and */   \
    /* unconditionally retaining, rather than before converting from */        \
    /* type->ref checking that our value is not yet uninitialized. */          \
    SILValue result = createCopy##Name##Value(loc, originalValue.getValue());  \
    return SGF.emitManagedRValueWithCleanup(result);                           \
  }
#include "swift/AST/ReferenceStorage.def"

ManagedValue SILGenBuilder::createOwnedPhiArgument(SILType type) {
  SILPhiArgument *arg =
      getInsertionBB()->createPhiArgument(type, ValueOwnershipKind::Owned);
  return SGF.emitManagedRValueWithCleanup(arg);
}

ManagedValue SILGenBuilder::createGuaranteedPhiArgument(SILType type) {
  SILPhiArgument *arg =
      getInsertionBB()->createPhiArgument(type, ValueOwnershipKind::Guaranteed);
  return SGF.emitManagedBorrowedArgumentWithCleanup(arg);
}

ManagedValue SILGenBuilder::createAllocRef(
    SILLocation loc, SILType refType, bool objc, bool canAllocOnStack,
    ArrayRef<SILType> inputElementTypes,
    ArrayRef<ManagedValue> inputElementCountOperands) {
  llvm::SmallVector<SILType, 8> elementTypes(inputElementTypes.begin(),
                                             inputElementTypes.end());
  llvm::SmallVector<SILValue, 8> elementCountOperands;
  llvm::transform(inputElementCountOperands,
                  std::back_inserter(elementCountOperands),
                  [](ManagedValue mv) -> SILValue { return mv.getValue(); });

  AllocRefInst *i = createAllocRef(loc, refType, objc, canAllocOnStack,
                                   elementTypes, elementCountOperands);
  return SGF.emitManagedRValueWithCleanup(i);
}

ManagedValue SILGenBuilder::createAllocRefDynamic(
    SILLocation loc, ManagedValue operand, SILType refType, bool objc,
    ArrayRef<SILType> inputElementTypes,
    ArrayRef<ManagedValue> inputElementCountOperands) {
  llvm::SmallVector<SILType, 8> elementTypes(inputElementTypes.begin(),
                                             inputElementTypes.end());
  llvm::SmallVector<SILValue, 8> elementCountOperands;
  llvm::transform(inputElementCountOperands,
                  std::back_inserter(elementCountOperands),
                  [](ManagedValue mv) -> SILValue { return mv.getValue(); });

  AllocRefDynamicInst *i =
      createAllocRefDynamic(loc, operand.getValue(), refType, objc,
                            elementTypes, elementCountOperands);
  return SGF.emitManagedRValueWithCleanup(i);
}

ManagedValue SILGenBuilder::createTupleExtract(SILLocation loc,
                                               ManagedValue base,
                                               unsigned index, SILType type) {
  ManagedValue borrowedBase = SGF.emitManagedBeginBorrow(loc, base.getValue());
  SILValue extract =
      createTupleExtract(loc, borrowedBase.getValue(), index, type);
  return ManagedValue::forUnmanaged(extract);
}

ManagedValue SILGenBuilder::createTupleExtract(SILLocation loc,
                                               ManagedValue value,
                                               unsigned index) {
  SILType type = value.getType().getTupleElementType(index);
  return createTupleExtract(loc, value, index, type);
}

ManagedValue SILGenBuilder::createLoadBorrow(SILLocation loc,
                                             ManagedValue base) {
  if (SGF.getTypeLowering(base.getType()).isTrivial()) {
    auto *i = createLoad(loc, base.getValue(), LoadOwnershipQualifier::Trivial);
    return ManagedValue::forUnmanaged(i);
  }

  auto *i = createLoadBorrow(loc, base.getValue());
  return SGF.emitManagedBorrowedRValueWithCleanup(base.getValue(), i);
}

ManagedValue SILGenBuilder::createFormalAccessLoadBorrow(SILLocation loc,
                                                         ManagedValue base) {
  if (SGF.getTypeLowering(base.getType()).isTrivial()) {
    auto *i = createLoad(loc, base.getValue(), LoadOwnershipQualifier::Trivial);
    return ManagedValue::forUnmanaged(i);
  }

  SILValue baseValue = base.getValue();
  auto *i = createLoadBorrow(loc, baseValue);
  return SGF.emitFormalEvaluationManagedBorrowedRValueWithCleanup(loc,
                                                                  baseValue, i);
}

ManagedValue
SILGenBuilder::createFormalAccessCopyValue(SILLocation loc,
                                           ManagedValue originalValue) {
  SILType ty = originalValue.getType();
  const auto &lowering = SGF.getTypeLowering(ty);
  if (lowering.isTrivial())
    return originalValue;

  assert(!lowering.isAddressOnly() && "cannot perform a copy value of an "
                                      "address only type");

  if (ty.isObject() &&
      originalValue.getOwnershipKind() == ValueOwnershipKind::Any) {
    return originalValue;
  }

  SILValue result =
      lowering.emitCopyValue(*this, loc, originalValue.getValue());
  return SGF.emitFormalAccessManagedRValueWithCleanup(loc, result);
}

ManagedValue SILGenBuilder::createFormalAccessCopyAddr(
    SILLocation loc, ManagedValue originalAddr, SILValue newAddr,
    IsTake_t isTake, IsInitialization_t isInit) {
  createCopyAddr(loc, originalAddr.getValue(), newAddr, isTake, isInit);
  return SGF.emitFormalAccessManagedBufferWithCleanup(loc, newAddr);
}

ManagedValue
SILGenBuilder::bufferForExpr(SILLocation loc, SILType ty,
                             const TypeLowering &lowering, SGFContext context,
                             llvm::function_ref<void(SILValue)> rvalueEmitter) {
  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  SILValue address = context.getAddressForInPlaceInitialization(SGF, loc);

  // If we couldn't emit into the Initialization, emit into a temporary
  // allocation.
  if (!address) {
    address = SGF.emitTemporaryAllocation(loc, ty.getObjectType());
  }

  rvalueEmitter(address);

  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  if (context.finishInPlaceInitialization(SGF)) {
    return ManagedValue::forInContext();
  }

  // Add a cleanup for the temporary we allocated.
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(address);

  return SGF.emitManagedBufferWithCleanup(address);
}

ManagedValue SILGenBuilder::formalAccessBufferForExpr(
    SILLocation loc, SILType ty, const TypeLowering &lowering,
    SGFContext context, llvm::function_ref<void(SILValue)> rvalueEmitter) {
  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  SILValue address = context.getAddressForInPlaceInitialization(SGF, loc);

  // If we couldn't emit into the Initialization, emit into a temporary
  // allocation.
  if (!address) {
    address = SGF.emitTemporaryAllocation(loc, ty.getObjectType());
  }

  rvalueEmitter(address);

  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  if (context.finishInPlaceInitialization(SGF)) {
    return ManagedValue::forInContext();
  }

  // Add a cleanup for the temporary we allocated.
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(address);

  return SGF.emitFormalAccessManagedBufferWithCleanup(loc, address);
}

ManagedValue SILGenBuilder::createUncheckedEnumData(SILLocation loc,
                                                    ManagedValue operand,
                                                    EnumElementDecl *element) {
  CleanupCloner cloner(*this, operand);
  SILValue result = createUncheckedEnumData(loc, operand.forward(SGF), element);
  return cloner.clone(result);
}

ManagedValue SILGenBuilder::createUncheckedTakeEnumDataAddr(
    SILLocation loc, ManagedValue operand, EnumElementDecl *element,
    SILType ty) {
  CleanupCloner cloner(*this, operand);
  SILValue result =
      createUncheckedTakeEnumDataAddr(loc, operand.forward(SGF), element);
  return cloner.clone(result);
}

ManagedValue SILGenBuilder::createLoadTake(SILLocation loc, ManagedValue v) {
  auto &lowering = SGF.getTypeLowering(v.getType());
  return createLoadTake(loc, v, lowering);
}

ManagedValue SILGenBuilder::createLoadTake(SILLocation loc, ManagedValue v,
                                           const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getAddressType() == v.getType());
  SILValue result =
      lowering.emitLoadOfCopy(*this, loc, v.forward(SGF), IsTake);
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(result);
  assert(!lowering.isAddressOnly() && "cannot retain an unloadable type");
  return SGF.emitManagedRValueWithCleanup(result, lowering);
}

ManagedValue SILGenBuilder::createLoadCopy(SILLocation loc, ManagedValue v) {
  auto &lowering = SGF.getTypeLowering(v.getType());
  return createLoadCopy(loc, v, lowering);
}

ManagedValue SILGenBuilder::createLoadCopy(SILLocation loc, ManagedValue v,
                                           const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getAddressType() == v.getType());
  SILValue result =
      lowering.emitLoadOfCopy(*this, loc, v.getValue(), IsNotTake);
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(result);
  assert((!lowering.isAddressOnly()
          || !SGF.silConv.useLoweredAddresses()) &&
         "cannot retain an unloadable type");
  return SGF.emitManagedRValueWithCleanup(result, lowering);
}

static ManagedValue createInputFunctionArgument(SILGenBuilder &B, SILType type,
                                                SILLocation loc,
                                                ValueDecl *decl = nullptr) {
  auto &SGF = B.getSILGenFunction();
  SILFunction &F = B.getFunction();
  assert((F.isBare() || decl) &&
         "Function arguments of non-bare functions must have a decl");
  auto *arg = F.begin()->createFunctionArgument(type, decl);
  switch (arg->getArgumentConvention()) {
  case SILArgumentConvention::Indirect_In_Guaranteed:
  case SILArgumentConvention::Direct_Guaranteed:
    // Guaranteed parameters are passed at +0.
    return ManagedValue::forUnmanaged(arg);
  case SILArgumentConvention::Direct_Unowned:
    // Unowned parameters are only guaranteed at the instant of the call, so we
    // must retain them even if we're in a context that can accept a +0 value.
    return ManagedValue::forUnmanaged(arg).copy(SGF, loc);

  case SILArgumentConvention::Direct_Owned:
    return SGF.emitManagedRValueWithCleanup(arg);

  case SILArgumentConvention::Indirect_In:
    if (SGF.silConv.useLoweredAddresses())
      return SGF.emitManagedBufferWithCleanup(arg);
    return SGF.emitManagedRValueWithCleanup(arg);

  case SILArgumentConvention::Indirect_Inout:
  case SILArgumentConvention::Indirect_InoutAliasable:
    // An inout parameter is +0 and guaranteed, but represents an lvalue.
    return ManagedValue::forLValue(arg);
  case SILArgumentConvention::Indirect_In_Constant:
    llvm_unreachable("Convention not produced by SILGen");
  case SILArgumentConvention::Direct_Deallocating:
  case SILArgumentConvention::Indirect_Out:
    llvm_unreachable("unsupported convention for API");
  }
  llvm_unreachable("bad parameter convention");
}

ManagedValue SILGenBuilder::createInputFunctionArgument(SILType type,
                                                        ValueDecl *decl) {
  return ::createInputFunctionArgument(*this, type, SILLocation(decl), decl);
}

ManagedValue
SILGenBuilder::createInputFunctionArgument(SILType type,
                                           Optional<SILLocation> inputLoc) {
  assert(inputLoc.hasValue() && "This optional is only for overload resolution "
                                "purposes! Do not pass in None here!");
  return ::createInputFunctionArgument(*this, type, *inputLoc);
}

ManagedValue
SILGenBuilder::createMarkUninitialized(ValueDecl *decl, ManagedValue operand,
                                       MarkUninitializedInst::Kind muKind) {
  // We either have an owned or trivial value.
  SILValue value = createMarkUninitialized(decl, operand.forward(SGF), muKind);
  assert(value->getType().isObject() && "Expected only objects here");

  // If we have a trivial value, just return without a cleanup.
  if (operand.getOwnershipKind() != ValueOwnershipKind::Owned) {
    return ManagedValue::forUnmanaged(value);
  }

  // Otherwise, recreate the cleanup.
  return SGF.emitManagedRValueWithCleanup(value);
}

ManagedValue SILGenBuilder::createEnum(SILLocation loc, ManagedValue payload,
                                       EnumElementDecl *decl, SILType type) {
  SILValue result = createEnum(loc, payload.forward(SGF), decl, type);
  if (result.getOwnershipKind() != ValueOwnershipKind::Owned)
    return ManagedValue::forUnmanaged(result);
  return SGF.emitManagedRValueWithCleanup(result);
}

ManagedValue SILGenBuilder::createUnconditionalCheckedCastValue(
    SILLocation loc, ManagedValue operand, SILType type) {
  SILValue result =
      createUnconditionalCheckedCastValue(loc, operand.forward(SGF), type);
  return SGF.emitManagedRValueWithCleanup(result);
}

ManagedValue SILGenBuilder::createUnconditionalCheckedCast(SILLocation loc,
                                                           ManagedValue operand,
                                                           SILType type) {
  SILValue result =
      createUnconditionalCheckedCast(loc, operand.forward(SGF), type);
  return SGF.emitManagedRValueWithCleanup(result);
}

void SILGenBuilder::createCheckedCastBranch(SILLocation loc, bool isExact,
                                            ManagedValue operand, SILType type,
                                            SILBasicBlock *trueBlock,
                                            SILBasicBlock *falseBlock,
                                            ProfileCounter Target1Count,
                                            ProfileCounter Target2Count) {
  createCheckedCastBranch(loc, isExact, operand.forward(SGF), type, trueBlock,
                          falseBlock, Target1Count, Target2Count);
}

void SILGenBuilder::createCheckedCastValueBranch(SILLocation loc,
                                                 ManagedValue operand,
                                                 SILType type,
                                                 SILBasicBlock *trueBlock,
                                                 SILBasicBlock *falseBlock) {
  createCheckedCastValueBranch(loc, operand.forward(SGF), type, trueBlock,
                               falseBlock);
}

ManagedValue SILGenBuilder::createUpcast(SILLocation loc, ManagedValue original,
                                         SILType type) {
  CleanupCloner cloner(*this, original);
  SILValue convertedValue = createUpcast(loc, original.forward(SGF), type);
  return cloner.clone(convertedValue);
}

ManagedValue SILGenBuilder::createOptionalSome(SILLocation loc,
                                               ManagedValue arg) {
  CleanupCloner cloner(*this, arg);
  auto &argTL = SGF.getTypeLowering(arg.getType());
  SILType optionalType = SILType::getOptionalType(arg.getType());
  if (argTL.isLoadable() || !SGF.silConv.useLoweredAddresses()) {
    SILValue someValue =
        createOptionalSome(loc, arg.forward(SGF), optionalType);
    return cloner.clone(someValue);
  }

  SILValue tempResult = SGF.emitTemporaryAllocation(loc, optionalType);
  RValue rvalue(SGF, loc, arg.getType().getASTType(), arg);
  ArgumentSource argValue(loc, std::move(rvalue));
  SGF.emitInjectOptionalValueInto(
      loc, std::move(argValue), tempResult,
      SGF.getTypeLowering(tempResult->getType()));
  return ManagedValue::forUnmanaged(tempResult);
}

ManagedValue SILGenBuilder::createManagedOptionalNone(SILLocation loc,
                                                      SILType type) {
  if (!type.isAddressOnly(getFunction()) ||
      !SGF.silConv.useLoweredAddresses()) {
    SILValue noneValue = createOptionalNone(loc, type);
    return ManagedValue::forUnmanaged(noneValue);
  }

  SILValue tempResult = SGF.emitTemporaryAllocation(loc, type);
  SGF.emitInjectOptionalNothingInto(loc, tempResult,
                                    SGF.getTypeLowering(type));
  return ManagedValue::forUnmanaged(tempResult);
}

ManagedValue SILGenBuilder::createManagedFunctionRef(SILLocation loc,
                                                     SILFunction *f) {
  return ManagedValue::forUnmanaged(createFunctionRefFor(loc, f));
}

ManagedValue SILGenBuilder::createTupleElementAddr(SILLocation Loc,
                                                   ManagedValue Base,
                                                   unsigned Index,
                                                   SILType Type) {
  SILValue TupleEltAddr =
      createTupleElementAddr(Loc, Base.getValue(), Index, Type);
  return ManagedValue::forUnmanaged(TupleEltAddr);
}

ManagedValue SILGenBuilder::createTupleElementAddr(SILLocation Loc,
                                                   ManagedValue Value,
                                                   unsigned Index) {
  SILType Type = Value.getType().getTupleElementType(Index);
  return createTupleElementAddr(Loc, Value, Index, Type);
}

ManagedValue SILGenBuilder::createUncheckedRefCast(SILLocation loc,
                                                   ManagedValue value,
                                                   SILType type) {
  CleanupCloner cloner(*this, value);
  SILValue cast = createUncheckedRefCast(loc, value.forward(SGF), type);
  return cloner.clone(cast);
}

ManagedValue SILGenBuilder::createUncheckedBitCast(SILLocation loc,
                                                   ManagedValue value,
                                                   SILType type) {
  CleanupCloner cloner(*this, value);
  SILValue cast = createUncheckedBitCast(loc, value.getValue(), type);

  // Currently createUncheckedBitCast only produces these
  // instructions. We assert here to make sure if this changes, this code is
  // updated.
  assert((isa<UncheckedTrivialBitCastInst>(cast) ||
          isa<UncheckedRefCastInst>(cast) ||
          isa<UncheckedBitwiseCastInst>(cast)) &&
         "SILGenBuilder is out of sync with SILBuilder.");

  // If we have a trivial inst, just return early.
  if (isa<UncheckedTrivialBitCastInst>(cast))
    return ManagedValue::forUnmanaged(cast);

  // If we perform an unchecked bitwise case, then we are producing a new RC
  // identity implying that we need a copy of the casted value to be returned so
  // that the inputs/outputs of the case have separate ownership.
  if (isa<UncheckedBitwiseCastInst>(cast)) {
    return ManagedValue::forUnmanaged(cast).copy(SGF, loc);
  }

  // Otherwise, we forward the cleanup of the input value and place the cleanup
  // on the cast value since unchecked_ref_cast is "forwarding".
  value.forward(SGF);
  return cloner.clone(cast);
}

ManagedValue SILGenBuilder::createOpenExistentialRef(SILLocation loc,
                                                     ManagedValue original,
                                                     SILType type) {
  CleanupCloner cloner(*this, original);
  SILValue openedExistential =
      createOpenExistentialRef(loc, original.forward(SGF), type);
  return cloner.clone(openedExistential);
}

ManagedValue SILGenBuilder::createOpenExistentialValue(SILLocation loc,
                                                       ManagedValue original,
                                                       SILType type) {
  ManagedValue borrowedExistential = original.formalAccessBorrow(SGF, loc);
  SILValue openedExistential =
      createOpenExistentialValue(loc, borrowedExistential.getValue(), type);
  return ManagedValue::forUnmanaged(openedExistential);
}

ManagedValue SILGenBuilder::createOpenExistentialBoxValue(SILLocation loc,
                                                          ManagedValue original,
                                                          SILType type) {
  ManagedValue borrowedExistential = original.formalAccessBorrow(SGF, loc);
  SILValue openedExistential =
      createOpenExistentialBoxValue(loc, borrowedExistential.getValue(), type);
  return ManagedValue::forUnmanaged(openedExistential);
}

ManagedValue SILGenBuilder::createOpenExistentialMetatype(SILLocation loc,
                                                          ManagedValue value,
                                                          SILType openedType) {
  SILValue result = SILGenBuilder::createOpenExistentialMetatype(
      loc, value.getValue(), openedType);
  return ManagedValue::forTrivialRValue(result);
}

ManagedValue SILGenBuilder::createStore(SILLocation loc, ManagedValue value,
                                        SILValue address,
                                        StoreOwnershipQualifier qualifier) {
  CleanupCloner cloner(*this, value);
  if (value.getType().isTrivial(SGF.F) ||
      value.getOwnershipKind() == ValueOwnershipKind::Any)
    qualifier = StoreOwnershipQualifier::Trivial;
  createStore(loc, value.forward(SGF), address, qualifier);
  return cloner.clone(address);
}

ManagedValue SILGenBuilder::createSuperMethod(SILLocation loc,
                                              ManagedValue operand,
                                              SILDeclRef member,
                                              SILType methodTy) {
  SILValue v = createSuperMethod(loc, operand.getValue(), member, methodTy);
  return ManagedValue::forUnmanaged(v);
}

ManagedValue SILGenBuilder::createObjCSuperMethod(SILLocation loc,
                                                  ManagedValue operand,
                                                  SILDeclRef member,
                                                  SILType methodTy) {
  SILValue v = createObjCSuperMethod(loc, operand.getValue(), member, methodTy);
  return ManagedValue::forUnmanaged(v);
}

ManagedValue SILGenBuilder::
createValueMetatype(SILLocation loc, SILType metatype,
                    ManagedValue base) {
  SILValue v = createValueMetatype(loc, metatype, base.getValue());
  return ManagedValue::forUnmanaged(v);
}

void SILGenBuilder::createStoreBorrow(SILLocation loc, ManagedValue value,
                                      SILValue address) {
  assert(value.getOwnershipKind() == ValueOwnershipKind::Guaranteed);
  createStoreBorrow(loc, value.getValue(), address);
}

void SILGenBuilder::createStoreBorrowOrTrivial(SILLocation loc,
                                               ManagedValue value,
                                               SILValue address) {
  if (value.getOwnershipKind() == ValueOwnershipKind::Any) {
    createStore(loc, value, address, StoreOwnershipQualifier::Trivial);
    return;
  }

  createStoreBorrow(loc, value, address);
}

ManagedValue SILGenBuilder::createBridgeObjectToRef(SILLocation loc,
                                                    ManagedValue mv,
                                                    SILType destType) {
  CleanupCloner cloner(*this, mv);
  SILValue result = createBridgeObjectToRef(loc, mv.forward(SGF), destType);
  return cloner.clone(result);
}

ManagedValue SILGenBuilder::createRefToBridgeObject(SILLocation loc,
                                                    ManagedValue mv,
                                                    SILValue bits) {
  CleanupCloner cloner(*this, mv);
  SILValue result = createRefToBridgeObject(loc, mv.forward(SGF), bits);
  return cloner.clone(result);
}

ManagedValue SILGenBuilder::createBlockToAnyObject(SILLocation loc,
                                                   ManagedValue v,
                                                   SILType destType) {
  assert(SGF.getASTContext().LangOpts.EnableObjCInterop);
  assert(destType.isAnyObject());
  assert(v.getType().is<SILFunctionType>());
  assert(v.getType().castTo<SILFunctionType>()->getRepresentation() ==
           SILFunctionTypeRepresentation::Block);

  // For now, we don't have a better instruction than this.
  return createUncheckedRefCast(loc, v, destType);
}

BranchInst *SILGenBuilder::createBranch(SILLocation loc,
                                        SILBasicBlock *targetBlock,
                                        ArrayRef<ManagedValue> args) {
  llvm::SmallVector<SILValue, 8> newArgs;
  llvm::transform(args, std::back_inserter(newArgs),
                  [&](ManagedValue mv) -> SILValue { return mv.forward(SGF); });
  return createBranch(loc, targetBlock, newArgs);
}

ReturnInst *SILGenBuilder::createReturn(SILLocation loc,
                                        ManagedValue returnValue) {
  return createReturn(loc, returnValue.forward(SGF));
}

ManagedValue SILGenBuilder::createTuple(SILLocation loc, SILType type,
                                        ArrayRef<ManagedValue> elements) {
  // Handle the empty tuple case.
  if (elements.empty()) {
    SILValue result = createTuple(loc, type, ArrayRef<SILValue>());
    return ManagedValue::forUnmanaged(result);
  }

  // We need to look for the first non-trivial value and use that as our cleanup
  // cloner value.
  auto iter = find_if(elements, [&](ManagedValue mv) -> bool {
    return !mv.getType().isTrivial(getFunction());
  });

  llvm::SmallVector<SILValue, 8> forwardedValues;
  // If we have all trivial values, then just create the tuple and return. No
  // cleanups need to be cloned.
  if (iter == elements.end()) {
    llvm::transform(elements, std::back_inserter(forwardedValues),
                    [&](ManagedValue mv) -> SILValue {
                      return mv.forward(getSILGenFunction());
                    });
    SILValue result = createTuple(loc, type, forwardedValues);
    return ManagedValue::forUnmanaged(result);
  }

  // Otherwise, we use that values cloner. This is taking advantage of
  // instructions that forward ownership requiring that all input values have
  // the same ownership if they are non-trivial.
  CleanupCloner cloner(*this, *iter);
  llvm::transform(elements, std::back_inserter(forwardedValues),
                  [&](ManagedValue mv) -> SILValue {
                    return mv.forward(getSILGenFunction());
                  });
  return cloner.clone(createTuple(loc, type, forwardedValues));
}

ManagedValue SILGenBuilder::createUncheckedAddrCast(SILLocation loc, ManagedValue op,
                                                    SILType resultTy) {
  CleanupCloner cloner(*this, op);
  SILValue cast = createUncheckedAddrCast(loc, op.forward(SGF), resultTy);
  return cloner.clone(cast);
}

ManagedValue SILGenBuilder::tryCreateUncheckedRefCast(SILLocation loc,
                                                      ManagedValue original,
                                                      SILType type) {
  CleanupCloner cloner(*this, original);
  SILValue result = tryCreateUncheckedRefCast(loc, original.getValue(), type);
  if (!result)
    return ManagedValue();
  original.forward(SGF);
  return cloner.clone(result);
}

ManagedValue SILGenBuilder::createUncheckedTrivialBitCast(SILLocation loc,
                                                          ManagedValue original,
                                                          SILType type) {
  SILValue result =
      SGF.B.createUncheckedTrivialBitCast(loc, original.getValue(), type);
  return ManagedValue::forUnmanaged(result);
}

void SILGenBuilder::emitDestructureValueOperation(
    SILLocation loc, ManagedValue value,
    llvm::function_ref<void(unsigned, ManagedValue)> func) {
  CleanupCloner cloner(*this, value);

  // NOTE: We can not directly use SILBuilder::emitDestructureValueOperation()
  // here since we need to create all of our cleanups before invoking \p
  // func. This is necessary since our func may want to emit conditional code
  // with an early exit, emitting unused cleanups from the current scope via the
  // function emitBranchAndCleanups(). If we have not yet created those
  // cleanups, we will introduce a leak along that path.
  SmallVector<ManagedValue, 8> destructuredValues;
  emitDestructureValueOperation(
      loc, value.forward(SGF), [&](unsigned index, SILValue subValue) {
        destructuredValues.push_back(cloner.clone(subValue));
      });
  for (auto p : llvm::enumerate(destructuredValues)) {
    func(p.index(), p.value());
  }
}

ManagedValue SILGenBuilder::createProjectBox(SILLocation loc, ManagedValue mv,
                                             unsigned index) {
  auto *pbi = createProjectBox(loc, mv.getValue(), index);
  return ManagedValue::forUnmanaged(pbi);
}

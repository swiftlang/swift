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
#include "SwitchCaseFullExpr.h"
#include "swift/AST/SubstitutionMap.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
//                             Cleanup Forwarder
//===----------------------------------------------------------------------===//

namespace {
class CleanupCloner {
  SILGenFunction &gen;
  bool hasCleanup;
  bool isLValue;
  ValueOwnershipKind ownershipKind;

public:
  CleanupCloner(SILGenBuilder &Builder, ManagedValue M)
      : gen(Builder.getSILGenFunction()), hasCleanup(M.hasCleanup()),
        isLValue(M.isLValue()), ownershipKind(M.getOwnershipKind()) {}

  ManagedValue clone(SILValue value) {
    if (isLValue) {
      return ManagedValue::forLValue(value);
    }

    if (!hasCleanup) {
      return ManagedValue::forUnmanaged(value);
    }

    if (value->getType().isAddress()) {
      return gen.emitManagedBufferWithCleanup(value);
    }

    return gen.emitManagedRValueWithCleanup(value);
  }
};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                              Utility Methods
//===----------------------------------------------------------------------===//

SILGenModule &SILGenBuilder::getSILGenModule() const { return gen.SGM; }

//===----------------------------------------------------------------------===//
//                                Constructors
//===----------------------------------------------------------------------===//

SILGenBuilder::SILGenBuilder(SILGenFunction &gen)
    : SILBuilder(gen.F), gen(gen) {}

SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB)
    : SILBuilder(insertBB), gen(gen) {}

SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB,
                             SmallVectorImpl<SILInstruction *> *insertedInsts)
    : SILBuilder(insertBB, insertedInsts), gen(gen) {}

SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB,
                             SILBasicBlock::iterator insertInst)
    : SILBuilder(insertBB, insertInst), gen(gen) {}

//===----------------------------------------------------------------------===//
//                            Instruction Emission
//===----------------------------------------------------------------------===//

MetatypeInst *SILGenBuilder::createMetatype(SILLocation loc, SILType metatype) {
  auto theMetatype = metatype.castTo<MetatypeType>();
  // Getting a nontrivial metatype requires forcing any conformances necessary
  // to instantiate the type.
  switch (theMetatype->getRepresentation()) {
  case MetatypeRepresentation::Thin:
    break;
  case MetatypeRepresentation::Thick:
  case MetatypeRepresentation::ObjC: {
    // Walk the type recursively to look for substitutions we may need.
    theMetatype.getInstanceType().findIf([&](Type t) -> bool {
      auto *decl = t->getAnyNominal();
      if (!decl)
        return false;

      auto *genericSig = decl->getGenericSignature();
      if (!genericSig)
        return false;

      auto subMap = t->getContextSubstitutionMap(getSILGenModule().SwiftModule,
                                                 decl);
      SmallVector<Substitution, 4> subs;
      genericSig->getSubstitutions(subMap, subs);
      getSILGenModule().useConformancesFromSubstitutions(subs);
      return false;
    });

    break;
  }
  }

  return SILBuilder::createMetatype(loc, metatype);
}

ApplyInst *SILGenBuilder::createApply(SILLocation loc, SILValue fn,
                                      SILType substFnTy, SILType result,
                                      SubstitutionList subs,
                                      ArrayRef<SILValue> args) {
  getSILGenModule().useConformancesFromSubstitutions(subs);
  return SILBuilder::createApply(loc, fn, substFnTy, result, subs, args, false);
}

TryApplyInst *
SILGenBuilder::createTryApply(SILLocation loc, SILValue fn, SILType substFnTy,
                              SubstitutionList subs, ArrayRef<SILValue> args,
                              SILBasicBlock *normalBB, SILBasicBlock *errorBB) {
  getSILGenModule().useConformancesFromSubstitutions(subs);
  return SILBuilder::createTryApply(loc, fn, substFnTy, subs, args, normalBB,
                                    errorBB);
}

PartialApplyInst *
SILGenBuilder::createPartialApply(SILLocation loc, SILValue fn,
                                  SILType substFnTy, SubstitutionList subs,
                                  ArrayRef<SILValue> args, SILType closureTy) {
  getSILGenModule().useConformancesFromSubstitutions(subs);
  return SILBuilder::createPartialApply(loc, fn, substFnTy, subs, args,
                                        closureTy);
}

BuiltinInst *SILGenBuilder::createBuiltin(SILLocation loc, Identifier name,
                                          SILType resultTy,
                                          SubstitutionList subs,
                                          ArrayRef<SILValue> args) {
  getSILGenModule().useConformancesFromSubstitutions(subs);
  return SILBuilder::createBuiltin(loc, name, resultTy, subs, args);
}

InitExistentialAddrInst *SILGenBuilder::createInitExistentialAddr(
    SILLocation loc, SILValue existential, CanType formalConcreteType,
    SILType loweredConcreteType,
    ArrayRef<ProtocolConformanceRef> conformances) {
  for (auto conformance : conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createInitExistentialAddr(
      loc, existential, formalConcreteType, loweredConcreteType, conformances);
}

InitExistentialOpaqueInst *SILGenBuilder::createInitExistentialOpaque(
    SILLocation Loc, SILType ExistentialType, CanType FormalConcreteType,
    SILValue Concrete, ArrayRef<ProtocolConformanceRef> Conformances) {
  for (auto conformance : Conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createInitExistentialOpaque(
      Loc, ExistentialType, FormalConcreteType, Concrete, Conformances);
}

InitExistentialMetatypeInst *SILGenBuilder::createInitExistentialMetatype(
    SILLocation loc, SILValue metatype, SILType existentialType,
    ArrayRef<ProtocolConformanceRef> conformances) {
  for (auto conformance : conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createInitExistentialMetatype(
      loc, metatype, existentialType, conformances);
}

InitExistentialRefInst *SILGenBuilder::createInitExistentialRef(
    SILLocation loc, SILType existentialType, CanType formalConcreteType,
    SILValue concreteValue, ArrayRef<ProtocolConformanceRef> conformances) {
  for (auto conformance : conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createInitExistentialRef(
      loc, existentialType, formalConcreteType, concreteValue, conformances);
}

AllocExistentialBoxInst *SILGenBuilder::createAllocExistentialBox(
    SILLocation loc, SILType existentialType, CanType concreteType,
    ArrayRef<ProtocolConformanceRef> conformances) {
  for (auto conformance : conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createAllocExistentialBox(loc, existentialType,
                                               concreteType, conformances);
}

ManagedValue SILGenBuilder::createStructExtract(SILLocation loc,
                                                ManagedValue base,
                                                VarDecl *decl) {
  ManagedValue borrowedBase = gen.emitManagedBeginBorrow(loc, base.getValue());
  SILValue extract =
      SILBuilder::createStructExtract(loc, borrowedBase.getValue(), decl);
  return ManagedValue::forUnmanaged(extract);
}

ManagedValue SILGenBuilder::createCopyValue(SILLocation loc,
                                            ManagedValue originalValue) {
  auto &lowering = getFunction().getTypeLowering(originalValue.getType());
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
      originalValue.getOwnershipKind() == ValueOwnershipKind::Trivial) {
    return originalValue;
  }

  SILValue result =
      lowering.emitCopyValue(*this, loc, originalValue.getValue());
  return gen.emitManagedRValueWithCleanup(result, lowering);
}

ManagedValue SILGenBuilder::createCopyUnownedValue(SILLocation loc,
                                                   ManagedValue originalValue) {
  auto unownedType = originalValue.getType().castTo<UnownedStorageType>();
  assert(unownedType->isLoadable(ResilienceExpansion::Maximal));
  (void)unownedType;

  SILValue result =
      SILBuilder::createCopyUnownedValue(loc, originalValue.getValue());
  return gen.emitManagedRValueWithCleanup(result);
}

ManagedValue
SILGenBuilder::createUnsafeCopyUnownedValue(SILLocation loc,
                                            ManagedValue originalValue) {
  auto unmanagedType = originalValue.getType().getAs<UnmanagedStorageType>();
  SILValue result = SILBuilder::createUnmanagedToRef(
      loc, originalValue.getValue(),
      SILType::getPrimitiveObjectType(unmanagedType.getReferentType()));
  SILBuilder::createUnmanagedRetainValue(loc, result, getDefaultAtomicity());
  return gen.emitManagedRValueWithCleanup(result);
}

ManagedValue SILGenBuilder::createOwnedPHIArgument(SILType type) {
  SILPHIArgument *arg =
      getInsertionBB()->createPHIArgument(type, ValueOwnershipKind::Owned);
  return gen.emitManagedRValueWithCleanup(arg);
}

ManagedValue SILGenBuilder::createAllocRef(
    SILLocation loc, SILType refType, bool objc, bool canAllocOnStack,
    ArrayRef<SILType> inputElementTypes,
    ArrayRef<ManagedValue> inputElementCountOperands) {
  llvm::SmallVector<SILType, 8> elementTypes(inputElementTypes.begin(),
                                             inputElementTypes.end());
  llvm::SmallVector<SILValue, 8> elementCountOperands;
  std::transform(std::begin(inputElementCountOperands),
                 std::end(inputElementCountOperands),
                 std::back_inserter(elementCountOperands),
                 [](ManagedValue mv) -> SILValue { return mv.getValue(); });

  AllocRefInst *i = SILBuilder::createAllocRef(
      loc, refType, objc, canAllocOnStack, elementTypes, elementCountOperands);
  return gen.emitManagedRValueWithCleanup(i);
}

ManagedValue SILGenBuilder::createAllocRefDynamic(
    SILLocation loc, ManagedValue operand, SILType refType, bool objc,
    ArrayRef<SILType> inputElementTypes,
    ArrayRef<ManagedValue> inputElementCountOperands) {
  llvm::SmallVector<SILType, 8> elementTypes(inputElementTypes.begin(),
                                             inputElementTypes.end());
  llvm::SmallVector<SILValue, 8> elementCountOperands;
  std::transform(std::begin(inputElementCountOperands),
                 std::end(inputElementCountOperands),
                 std::back_inserter(elementCountOperands),
                 [](ManagedValue mv) -> SILValue { return mv.getValue(); });

  AllocRefDynamicInst *i =
      SILBuilder::createAllocRefDynamic(loc, operand.getValue(), refType, objc,
                                        elementTypes, elementCountOperands);
  return gen.emitManagedRValueWithCleanup(i);
}

ManagedValue SILGenBuilder::createTupleExtract(SILLocation loc,
                                               ManagedValue base,
                                               unsigned index, SILType type) {
  ManagedValue borrowedBase = gen.emitManagedBeginBorrow(loc, base.getValue());
  SILValue extract =
      SILBuilder::createTupleExtract(loc, borrowedBase.getValue(), index, type);
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
  if (getFunction().getTypeLowering(base.getType()).isTrivial()) {
    auto *i = SILBuilder::createLoad(loc, base.getValue(),
                                     LoadOwnershipQualifier::Trivial);
    return ManagedValue::forUnmanaged(i);
  }

  auto *i = SILBuilder::createLoadBorrow(loc, base.getValue());
  return gen.emitManagedBorrowedRValueWithCleanup(base.getValue(), i);
}

ManagedValue SILGenBuilder::createFormalAccessLoadBorrow(SILLocation loc,
                                                         ManagedValue base) {
  if (getFunction().getTypeLowering(base.getType()).isTrivial()) {
    auto *i = SILBuilder::createLoad(loc, base.getValue(),
                                     LoadOwnershipQualifier::Trivial);
    return ManagedValue::forUnmanaged(i);
  }

  SILValue baseValue = base.getValue();
  auto *i = SILBuilder::createLoadBorrow(loc, baseValue);
  return gen.emitFormalEvaluationManagedBorrowedRValueWithCleanup(loc,
                                                                  baseValue, i);
}

ManagedValue
SILGenBuilder::createFormalAccessCopyValue(SILLocation loc,
                                           ManagedValue originalValue) {
  SILType ty = originalValue.getType();
  const auto &lowering = getFunction().getTypeLowering(ty);
  if (lowering.isTrivial())
    return originalValue;

  assert(!lowering.isAddressOnly() && "cannot perform a copy value of an "
                                      "address only type");

  if (ty.isObject() &&
      originalValue.getOwnershipKind() == ValueOwnershipKind::Trivial) {
    return originalValue;
  }

  SILValue result =
      lowering.emitCopyValue(*this, loc, originalValue.getValue());
  return gen.emitFormalAccessManagedRValueWithCleanup(loc, result);
}

ManagedValue SILGenBuilder::createFormalAccessCopyAddr(
    SILLocation loc, ManagedValue originalAddr, SILValue newAddr,
    IsTake_t isTake, IsInitialization_t isInit) {
  SILBuilder::createCopyAddr(loc, originalAddr.getValue(), newAddr, isTake,
                             isInit);
  return gen.emitFormalAccessManagedBufferWithCleanup(loc, newAddr);
}

ManagedValue SILGenBuilder::bufferForExpr(
    SILLocation loc, SILType ty, const TypeLowering &lowering,
    SGFContext context, std::function<void (SILValue)> rvalueEmitter) {
  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  SILValue address = context.getAddressForInPlaceInitialization();

  // If we couldn't emit into the Initialization, emit into a temporary
  // allocation.
  if (!address) {
    address = gen.emitTemporaryAllocation(loc, ty.getObjectType());
  }

  rvalueEmitter(address);

  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  if (context.getAddressForInPlaceInitialization()) {
    context.getEmitInto()->finishInitialization(gen);
    return ManagedValue::forInContext();
  }

  // Add a cleanup for the temporary we allocated.
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(address);

  return gen.emitManagedBufferWithCleanup(address);
}


ManagedValue SILGenBuilder::formalAccessBufferForExpr(
    SILLocation loc, SILType ty, const TypeLowering &lowering,
    SGFContext context, std::function<void(SILValue)> rvalueEmitter) {
  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  SILValue address = context.getAddressForInPlaceInitialization();

  // If we couldn't emit into the Initialization, emit into a temporary
  // allocation.
  if (!address) {
    address = gen.emitTemporaryAllocation(loc, ty.getObjectType());
  }

  rvalueEmitter(address);

  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  if (context.getAddressForInPlaceInitialization()) {
    context.getEmitInto()->finishInitialization(gen);
    return ManagedValue::forInContext();
  }

  // Add a cleanup for the temporary we allocated.
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(address);

  return gen.emitFormalAccessManagedBufferWithCleanup(loc, address);
}

ManagedValue SILGenBuilder::createUncheckedEnumData(SILLocation loc,
                                                    ManagedValue operand,
                                                    EnumElementDecl *element) {
  if (operand.hasCleanup()) {
    SILValue newValue =
        SILBuilder::createUncheckedEnumData(loc, operand.forward(gen), element);
    return gen.emitManagedRValueWithCleanup(newValue);
  }

  ManagedValue borrowedBase = operand.borrow(gen, loc);
  SILValue newValue = SILBuilder::createUncheckedEnumData(
      loc, borrowedBase.getValue(), element);
  return ManagedValue::forUnmanaged(newValue);
}

ManagedValue SILGenBuilder::createUncheckedTakeEnumDataAddr(
    SILLocation loc, ManagedValue operand, EnumElementDecl *element,
    SILType ty) {
  // First see if we have a cleanup. If we do, we are going to forward and emit
  // a managed buffer with cleanup.
  if (operand.hasCleanup()) {
    return gen.emitManagedBufferWithCleanup(
        SILBuilder::createUncheckedTakeEnumDataAddr(loc, operand.forward(gen),
                                                    element, ty));
  }

  SILValue result = SILBuilder::createUncheckedTakeEnumDataAddr(
      loc, operand.getUnmanagedValue(), element, ty);
  if (operand.isLValue())
    return ManagedValue::forLValue(result);
  return ManagedValue::forUnmanaged(result);
}

ManagedValue SILGenBuilder::createLoadTake(SILLocation loc, ManagedValue v) {
  auto &lowering = getFunction().getTypeLowering(v.getType());
  return createLoadTake(loc, v, lowering);
}

ManagedValue SILGenBuilder::createLoadTake(SILLocation loc, ManagedValue v,
                                           const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getAddressType() == v.getType());
  SILValue result =
      lowering.emitLoadOfCopy(*this, loc, v.forward(gen), IsTake);
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(result);
  assert(!lowering.isAddressOnly() && "cannot retain an unloadable type");
  return gen.emitManagedRValueWithCleanup(result, lowering);
}

ManagedValue SILGenBuilder::createLoadCopy(SILLocation loc, ManagedValue v) {
  auto &lowering = getFunction().getTypeLowering(v.getType());
  return createLoadCopy(loc, v, lowering);
}

ManagedValue SILGenBuilder::createLoadCopy(SILLocation loc, ManagedValue v,
                                           const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getAddressType() == v.getType());
  SILValue result =
      lowering.emitLoadOfCopy(*this, loc, v.forward(gen), IsNotTake);
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(result);
  assert(!lowering.isAddressOnly() && "cannot retain an unloadable type");
  return gen.emitManagedRValueWithCleanup(result, lowering);
}

ManagedValue SILGenBuilder::createFunctionArgument(SILType type,
                                                   ValueDecl *decl) {
  SILFunction &F = getFunction();

  SILFunctionArgument *arg = F.begin()->createFunctionArgument(type, decl);
  if (arg->getType().isObject()) {
    if (arg->getOwnershipKind().isTrivialOr(ValueOwnershipKind::Owned))
      return gen.emitManagedRValueWithCleanup(arg);
    return ManagedValue::forBorrowedRValue(arg);
  }

  return gen.emitManagedBufferWithCleanup(arg);
}

ManagedValue
SILGenBuilder::createMarkUninitialized(ValueDecl *decl, ManagedValue operand,
                                       MarkUninitializedInst::Kind muKind) {
  // We either have an owned or trivial value.
  SILValue value =
      SILBuilder::createMarkUninitialized(decl, operand.forward(gen), muKind);
  assert(value->getType().isObject() && "Expected only objects here");

  // If we have a trivial value, just return without a cleanup.
  if (operand.getOwnershipKind() != ValueOwnershipKind::Owned) {
    return ManagedValue::forUnmanaged(value);
  }

  // Otherwise, recreate the cleanup.
  return gen.emitManagedRValueWithCleanup(value);
}

ManagedValue SILGenBuilder::createEnum(SILLocation loc, ManagedValue payload,
                                       EnumElementDecl *decl, SILType type) {
  SILValue result =
      SILBuilder::createEnum(loc, payload.forward(gen), decl, type);
  if (result.getOwnershipKind() != ValueOwnershipKind::Owned)
    return ManagedValue::forUnmanaged(result);
  return gen.emitManagedRValueWithCleanup(result);
}

ManagedValue SILGenBuilder::createUnconditionalCheckedCastValue(
    SILLocation loc, ManagedValue operand, SILType type) {
  SILValue result = SILBuilder::createUnconditionalCheckedCastValue(
      loc, operand.forward(gen), type);
  return gen.emitManagedRValueWithCleanup(result);
}

ManagedValue SILGenBuilder::createUnconditionalCheckedCast(SILLocation loc,
                                                           ManagedValue operand,
                                                           SILType type) {
  SILValue result = SILBuilder::createUnconditionalCheckedCast(
      loc, operand.forward(gen), type);
  return gen.emitManagedRValueWithCleanup(result);
}

void SILGenBuilder::createCheckedCastBranch(SILLocation loc, bool isExact,
                                            ManagedValue operand, SILType type,
                                            SILBasicBlock *trueBlock,
                                            SILBasicBlock *falseBlock) {
  SILBuilder::createCheckedCastBranch(loc, isExact, operand.forward(gen), type,
                                      trueBlock, falseBlock);
}

void SILGenBuilder::createCheckedCastValueBranch(SILLocation loc,
                                                 ManagedValue operand,
                                                 SILType type,
                                                 SILBasicBlock *trueBlock,
                                                 SILBasicBlock *falseBlock) {
  SILBuilder::createCheckedCastValueBranch(loc, operand.forward(gen), type,
                                           trueBlock, falseBlock);
}

ManagedValue SILGenBuilder::createUpcast(SILLocation loc, ManagedValue original,
                                         SILType type) {
  CleanupCloner cloner(*this, original);
  SILValue convertedValue =
      SILBuilder::createUpcast(loc, original.forward(gen), type);
  return cloner.clone(convertedValue);
}

ManagedValue SILGenBuilder::createOptionalSome(SILLocation loc,
                                               ManagedValue arg) {
  CleanupCloner cloner(*this, arg);
  auto &argTL = getFunction().getTypeLowering(arg.getType());
  SILType optionalType = arg.getType().wrapAnyOptionalType(getFunction());
  if (argTL.isLoadable()) {
    SILValue someValue =
        SILBuilder::createOptionalSome(loc, arg.forward(gen), optionalType);
    return cloner.clone(someValue);
  }

  SILValue tempResult = gen.emitTemporaryAllocation(loc, optionalType);
  RValue rvalue(gen, loc, arg.getType().getSwiftRValueType(), arg);
  ArgumentSource argValue(loc, std::move(rvalue));
  gen.emitInjectOptionalValueInto(
      loc, std::move(argValue), tempResult,
      getFunction().getTypeLowering(tempResult->getType()));
  return ManagedValue::forUnmanaged(tempResult);
}

ManagedValue SILGenBuilder::createManagedOptionalNone(SILLocation loc,
                                                      SILType type) {
  if (!type.isAddressOnly(getModule())) {
    SILValue noneValue = SILBuilder::createOptionalNone(loc, type);
    return ManagedValue::forUnmanaged(noneValue);
  }

  SILValue tempResult = gen.emitTemporaryAllocation(loc, type);
  gen.emitInjectOptionalNothingInto(loc, tempResult,
                                    gen.F.getTypeLowering(type));
  return ManagedValue::forUnmanaged(tempResult);
}

ManagedValue SILGenBuilder::createTupleElementAddr(SILLocation Loc,
                                                   ManagedValue Base,
                                                   unsigned Index,
                                                   SILType Type) {
  SILValue TupleEltAddr =
      SILBuilder::createTupleElementAddr(Loc, Base.getValue(), Index, Type);
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
  SILValue cast =
      SILBuilder::createUncheckedRefCast(loc, value.forward(gen), type);
  return cloner.clone(cast);
}

ManagedValue SILGenBuilder::createOpenExistentialRef(SILLocation loc,
                                                     ManagedValue original,
                                                     SILType type) {
  CleanupCloner cloner(*this, original);
  SILValue openedExistential =
      SILBuilder::createOpenExistentialRef(loc, original.forward(gen), type);
  return cloner.clone(openedExistential);
}

//===----------------------------------------------------------------------===//
//                            Switch Enum Builder
//===----------------------------------------------------------------------===//

void SwitchEnumBuilder::emit() && {
  bool isAddressOnly = optional.getType().isAddressOnly(builder.getModule());
  using DeclBlockPair = std::pair<EnumElementDecl *, SILBasicBlock *>;
  {
    // TODO: We could store the data in CaseBB form and not have to do this.
    llvm::SmallVector<DeclBlockPair, 8> caseBlocks;
    std::transform(caseDataArray.begin(), caseDataArray.end(),
                   std::back_inserter(caseBlocks),
                   [](NormalCaseData &caseData) -> DeclBlockPair {
                     return {caseData.decl, caseData.block};
                   });
    SILBasicBlock *defaultBlock =
        defaultBlockData ? defaultBlockData->block : nullptr;
    if (isAddressOnly) {
      builder.createSwitchEnumAddr(loc, optional.getValue(), defaultBlock,
                                   caseBlocks);
    } else {
      if (optional.getType().isAddress()) {
        // TODO: Refactor this into a maybe load.
        if (optional.hasCleanup()) {
          optional = builder.createLoadTake(loc, optional);
        } else {
          optional = builder.createLoadCopy(loc, optional);
        }
      }
      builder.createSwitchEnum(loc, optional.forward(getSGF()), defaultBlock,
                               caseBlocks);
    }
  }

  for (NormalCaseData &caseData : caseDataArray) {
    EnumElementDecl *decl = caseData.decl;
    SILBasicBlock *caseBlock = caseData.block;
    NullablePtr<SILBasicBlock> contBlock = caseData.contBlock;
    NormalCaseHandler handler = caseData.handler;

    // Don't allow cleanups to escape the conditional block.
    SwitchCaseFullExpr presentScope(builder.getSILGenFunction(),
                                    CleanupLocation::get(loc),
                                    contBlock.getPtrOrNull());
    builder.emitBlock(caseBlock);

    ManagedValue input;
    if (decl->getArgumentInterfaceType()) {
      // Pull the payload out if we have one.
      SILType inputType =
          optional.getType().getEnumElementType(decl, builder.getModule());
      input = optional;
      if (!isAddressOnly) {
        input = builder.createOwnedPHIArgument(inputType);
      }
    }
    handler(input, presentScope);
    assert(!builder.hasValidInsertionPoint());
  }

  // If we have a default BB, create an argument for the original loaded value
  // and destroy it there.
  if (defaultBlockData) {
    SILBasicBlock *defaultBlock = defaultBlockData->block;
    NullablePtr<SILBasicBlock> contBB = defaultBlockData->contBlock;
    DefaultCaseHandler handler = defaultBlockData->handler;

    // Don't allow cleanups to escape the conditional block.
    SwitchCaseFullExpr presentScope(builder.getSILGenFunction(),
                                    CleanupLocation::get(loc),
                                    contBB.getPtrOrNull());
    builder.emitBlock(defaultBlock);
    ManagedValue input = optional;
    if (!isAddressOnly) {
      input = builder.createOwnedPHIArgument(optional.getType());
    }
    handler(input, presentScope);
    assert(!builder.hasValidInsertionPoint());
  }
}

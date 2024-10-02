//===--- SILBridgingImpl.h ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of bridging functions, which are either
// - depending on if PURE_BRIDGING_MODE is set - included in the cpp file or
// in the header file.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILBRIDGING_IMPL_H
#define SWIFT_SIL_SILBRIDGING_IMPL_H

#include "SILBridging.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Decl.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Types.h"
#include "swift/Basic/BasicBridging.h"
#include "swift/Basic/Nullability.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/InstWrappers.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDefaultWitnessTable.h"
#include "swift/SIL/SILFunctionConventions.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILWitnessTable.h"
#include <stdbool.h>
#include <stddef.h>
#include <string>

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

//===----------------------------------------------------------------------===//
//                             BridgedResultInfo
//===----------------------------------------------------------------------===//

BridgedResultConvention BridgedResultInfo::castToResultConvention(swift::ResultConvention convention) {
  return static_cast<BridgedResultConvention>(convention);
}

BridgedResultInfo::BridgedResultInfo(swift::SILResultInfo resultInfo):
  type(resultInfo.getInterfaceType().getPointer()),
  convention(castToResultConvention(resultInfo.getConvention()))
{}

SwiftInt BridgedResultInfoArray::count() const {
  return resultInfoArray.unbridged<swift::SILResultInfo>().size();
}

BridgedResultInfo BridgedResultInfoArray::at(SwiftInt resultIndex) const {
  return BridgedResultInfo(resultInfoArray.unbridged<swift::SILResultInfo>()[resultIndex]);
}

//===----------------------------------------------------------------------===//
//                             BridgedYieldInfo
//===----------------------------------------------------------------------===//

SwiftInt BridgedYieldInfoArray::count() const {
  return yieldInfoArray.unbridged<swift::SILYieldInfo>().size();
}

BridgedParameterInfo BridgedYieldInfoArray::at(SwiftInt resultIndex) const {
  return BridgedParameterInfo(yieldInfoArray.unbridged<swift::SILYieldInfo>()[resultIndex]);
}

//===----------------------------------------------------------------------===//
//                            BridgedParameterInfo
//===----------------------------------------------------------------------===//

inline swift::ParameterConvention getParameterConvention(BridgedArgumentConvention convention) {
  switch (convention) {
    case BridgedArgumentConvention::Indirect_In:             return swift::ParameterConvention::Indirect_In;
    case BridgedArgumentConvention::Indirect_In_Guaranteed:  return swift::ParameterConvention::Indirect_In_Guaranteed;
    case BridgedArgumentConvention::Indirect_Inout:          return swift::ParameterConvention::Indirect_Inout;
    case BridgedArgumentConvention::Indirect_InoutAliasable: return swift::ParameterConvention::Indirect_InoutAliasable;
    case BridgedArgumentConvention::Indirect_In_CXX:         return swift::ParameterConvention::Indirect_In_CXX;
    case BridgedArgumentConvention::Indirect_Out:            break;
    case BridgedArgumentConvention::Direct_Owned:            return swift::ParameterConvention::Direct_Owned;
    case BridgedArgumentConvention::Direct_Unowned:          return swift::ParameterConvention::Direct_Unowned;
    case BridgedArgumentConvention::Direct_Guaranteed:       return swift::ParameterConvention::Direct_Guaranteed;
    case BridgedArgumentConvention::Pack_Owned:              return swift::ParameterConvention::Pack_Owned;
    case BridgedArgumentConvention::Pack_Inout:              return swift::ParameterConvention::Pack_Inout;
    case BridgedArgumentConvention::Pack_Guaranteed:         return swift::ParameterConvention::Pack_Guaranteed;
    case BridgedArgumentConvention::Pack_Out:                break;
  }
  llvm_unreachable("invalid parameter convention");
}

inline BridgedArgumentConvention getArgumentConvention(swift::ParameterConvention convention) {
  switch (convention) {
    case swift::ParameterConvention::Indirect_In:              return BridgedArgumentConvention::Indirect_In;
    case swift::ParameterConvention::Indirect_In_Guaranteed:   return BridgedArgumentConvention::Indirect_In_Guaranteed;
    case swift::ParameterConvention::Indirect_Inout:           return BridgedArgumentConvention::Indirect_Inout;
    case swift::ParameterConvention::Indirect_InoutAliasable:  return BridgedArgumentConvention::Indirect_InoutAliasable;
    case swift::ParameterConvention::Indirect_In_CXX:          return BridgedArgumentConvention::Indirect_In_CXX;
    case swift::ParameterConvention::Direct_Owned:             return BridgedArgumentConvention::Direct_Owned;
    case swift::ParameterConvention::Direct_Unowned:           return BridgedArgumentConvention::Direct_Unowned;
    case swift::ParameterConvention::Direct_Guaranteed:        return BridgedArgumentConvention::Direct_Guaranteed;
    case swift::ParameterConvention::Pack_Owned:               return BridgedArgumentConvention::Pack_Owned;
    case swift::ParameterConvention::Pack_Inout:               return BridgedArgumentConvention::Pack_Inout;
    case swift::ParameterConvention::Pack_Guaranteed:          return BridgedArgumentConvention::Pack_Guaranteed;
  }
  llvm_unreachable("invalid parameter convention");
}

inline BridgedArgumentConvention castToArgumentConvention(swift::SILArgumentConvention convention) {
  return static_cast<BridgedArgumentConvention>(convention.Value);
}

BridgedParameterInfo::BridgedParameterInfo(swift::SILParameterInfo parameterInfo):
  type(parameterInfo.getInterfaceType()),
  convention(getArgumentConvention(parameterInfo.getConvention())),
  options(parameterInfo.getOptions().toRaw())
{}

swift::SILParameterInfo BridgedParameterInfo::unbridged() const {
  return swift::SILParameterInfo(type.unbridged(), getParameterConvention(convention),
                                 swift::SILParameterInfo::Options(options));
}

SwiftInt BridgedParameterInfoArray::count() const {
  return parameterInfoArray.unbridged<swift::SILParameterInfo>().size();
}

BridgedParameterInfo BridgedParameterInfoArray::at(SwiftInt parameterIndex) const {
  return BridgedParameterInfo(parameterInfoArray.unbridged<swift::SILParameterInfo>()[parameterIndex]);
}

//===----------------------------------------------------------------------===//
//                       BridgedLifetimeDependenceInfo
//===----------------------------------------------------------------------===//

BridgedLifetimeDependenceInfo::BridgedLifetimeDependenceInfo(swift::LifetimeDependenceInfo info)
    : inheritLifetimeParamIndices(info.getInheritIndices()),
      scopeLifetimeParamIndices(info.getScopeIndices()),
      targetIndex(info.getTargetIndex()), immortal(info.isImmortal()) {}

SwiftInt BridgedLifetimeDependenceInfoArray::count() const {
  return lifetimeDependenceInfoArray.unbridged<swift::LifetimeDependenceInfo>().size();
}

BridgedLifetimeDependenceInfo
BridgedLifetimeDependenceInfoArray::at(SwiftInt index) const {
  return BridgedLifetimeDependenceInfo(lifetimeDependenceInfoArray.unbridged<swift::LifetimeDependenceInfo>()[index]);
}

bool BridgedLifetimeDependenceInfo::empty() const {
  return !immortal && inheritLifetimeParamIndices == nullptr &&
         scopeLifetimeParamIndices == nullptr;
}

bool BridgedLifetimeDependenceInfo::checkInherit(SwiftInt index) const {
  return inheritLifetimeParamIndices &&
         inheritLifetimeParamIndices->contains(index);
}

bool BridgedLifetimeDependenceInfo::checkScope(SwiftInt index) const {
  return scopeLifetimeParamIndices &&
         scopeLifetimeParamIndices->contains(index);
}

SwiftInt BridgedLifetimeDependenceInfo::getTargetIndex() const {
  return targetIndex;
}

BridgedOwnedString BridgedLifetimeDependenceInfo::getDebugDescription() const {
  std::string lifetimeDependenceString;
  auto getOnIndices = [](swift::IndexSubset *bitvector) {
    std::string result;
    bool isFirstSetBit = true;
    for (unsigned i = 0; i < bitvector->getCapacity(); i++) {
      if (bitvector->contains(i)) {
        if (!isFirstSetBit) {
          result += ", ";
        }
        result += std::to_string(i);
        isFirstSetBit = false;
      }
    }
    return result;
  };
  if (inheritLifetimeParamIndices && !inheritLifetimeParamIndices->isEmpty()) {
    lifetimeDependenceString =
        "_inherit(" + getOnIndices(inheritLifetimeParamIndices) + ") ";
  }
  if (scopeLifetimeParamIndices && !scopeLifetimeParamIndices->isEmpty()) {
    lifetimeDependenceString +=
        "_scope(" + getOnIndices(scopeLifetimeParamIndices) + ") ";
  }
  return lifetimeDependenceString;
}

//===----------------------------------------------------------------------===//
//                               SILFunctionType
//===----------------------------------------------------------------------===//

BridgedResultInfoArray
SILFunctionType_getResultsWithError(BridgedCanType funcTy) {
  return {funcTy.unbridged()->castTo<swift::SILFunctionType>()->getResultsWithError()};
}

SwiftInt SILFunctionType_getNumIndirectFormalResultsWithError(BridgedCanType funcTy) {
  auto fnTy = funcTy.unbridged()->castTo<swift::SILFunctionType>();
  return fnTy->getNumIndirectFormalResults()
    + (fnTy->hasIndirectErrorResult() ? 1 : 0);
}

SwiftInt SILFunctionType_getNumPackResults(BridgedCanType funcTy) {
  return funcTy.unbridged()->castTo<swift::SILFunctionType>()
    ->getNumPackResults();
}

OptionalBridgedResultInfo SILFunctionType_getErrorResult(BridgedCanType funcTy) {
  auto fnTy = funcTy.unbridged()->castTo<swift::SILFunctionType>();
  auto resultInfo = fnTy->getOptionalErrorResult();
  if (resultInfo) {
    return {resultInfo->getInterfaceType().getPointer(),
            BridgedResultInfo::castToResultConvention(resultInfo->getConvention())};
  }
  return {nullptr, BridgedResultConvention::Indirect};

}

BridgedParameterInfoArray SILFunctionType_getParameters(BridgedCanType funcTy) {
  return {funcTy.unbridged()->castTo<swift::SILFunctionType>()->getParameters()};
}

bool SILFunctionType_hasSelfParam(BridgedCanType funcTy) {
  return funcTy.unbridged()->castTo<swift::SILFunctionType>()->hasSelfParam();
}

bool SILFunctionType_isTrivialNoescape(BridgedCanType funcTy) {
  return funcTy.unbridged()->castTo<swift::SILFunctionType>()->isTrivialNoEscape();
}

BridgedYieldInfoArray SILFunctionType_getYields(BridgedCanType funcTy) {
  return {funcTy.unbridged()->castTo<swift::SILFunctionType>()->getYields()};
}

BridgedLifetimeDependenceInfoArray
SILFunctionType_getLifetimeDependencies(BridgedCanType funcTy) {
  auto fnTy = funcTy.unbridged()->castTo<swift::SILFunctionType>();
  return {fnTy->getLifetimeDependencies()};
}

//===----------------------------------------------------------------------===//
//                                BridgedType
//===----------------------------------------------------------------------===//

BridgedType::BridgedType() : opaqueValue(nullptr) {}

BridgedType::BridgedType(swift::SILType t) : opaqueValue(t.getOpaqueValue()) {}

swift::SILType BridgedType::unbridged() const {
  return swift::SILType::getFromOpaqueValue(opaqueValue);
}

BridgedType::EnumElementIterator BridgedType::EnumElementIterator::getNext() const {
  return EnumElementIterator(std::next(unbridged()));
}

BridgedType BridgedType::createObjectType(BridgedCanType canTy) {
  return swift::SILType::getPrimitiveObjectType(canTy.unbridged());
}

BridgedType BridgedType::createAddressType(BridgedCanType canTy) {
  return swift::SILType::getPrimitiveAddressType(canTy.unbridged());
}

BridgedOwnedString BridgedType::getDebugDescription() const {
  return BridgedOwnedString(unbridged().getDebugDescription());
}

bool BridgedType::isNull() const {
  return unbridged().isNull();
}

bool BridgedType::isAddress() const {
  return unbridged().isAddress();
}

BridgedCanType BridgedType::getCanType() const {
  return unbridged().getASTType();
}

BridgedType BridgedType::getAddressType() const {
  return unbridged().getAddressType();
}

BridgedType BridgedType::getObjectType() const {
  return unbridged().getObjectType();
}

BridgedASTType BridgedType::getASTType() const {
  return {unbridged().getASTType().getPointer()};
}

BridgedDiagnosticArgument BridgedType::asDiagnosticArgument() const {
  return swift::DiagnosticArgument(unbridged().getASTType());
}

bool BridgedType::isTrivial(BridgedFunction f) const {
  return unbridged().isTrivial(f.getFunction());
}

bool BridgedType::isNonTrivialOrContainsRawPointer(BridgedFunction f) const {
  return unbridged().isNonTrivialOrContainsRawPointer(f.getFunction());
}

bool BridgedType::isValueTypeWithDeinit() const {
  return unbridged().isValueTypeWithDeinit();
}

bool BridgedType::isLoadable(BridgedFunction f) const {
  return unbridged().isLoadable(f.getFunction());
}

bool BridgedType::isReferenceCounted(BridgedFunction f) const {
  return unbridged().isReferenceCounted(f.getFunction());
}

bool BridgedType::isUnownedStorageType() const {
  return unbridged().isUnownedStorageType();
}

bool BridgedType::hasArchetype() const {
  return unbridged().hasArchetype();
}

bool BridgedType::isNominalOrBoundGenericNominal() const {
  return unbridged().getNominalOrBoundGenericNominal() != nullptr;
}

BridgedSubstitutionMap BridgedType::getContextSubstitutionMap() const {
  swift::CanType astType = unbridged().getASTType();
  return astType->getContextSubstitutionMap();
}

bool BridgedType::isGenericAtAnyLevel() const {
  swift::CanType astType = unbridged().getASTType();
  return astType->isSpecialized();
}

OptionalBridgedDeclObj BridgedType::getNominalOrBoundGenericNominal() const {
  return {unbridged().getNominalOrBoundGenericNominal()};
}

bool BridgedType::isClassOrBoundGenericClass() const {
  return unbridged().getClassOrBoundGenericClass() != 0;
}

bool BridgedType::isStructOrBoundGenericStruct() const {
  return unbridged().getStructOrBoundGenericStruct() != nullptr;
}

bool BridgedType::isTuple() const {
  return unbridged().isTuple();
}

bool BridgedType::isEnumOrBoundGenericEnum() const {
  return unbridged().getEnumOrBoundGenericEnum() != nullptr;
}

bool BridgedType::isFunction() const {
  return unbridged().isFunction();
}

bool BridgedType::isMetatype() const {
  return unbridged().isMetatype();
}

bool BridgedType::isClassExistential() const {
  return unbridged().isClassExistentialType();
}

bool BridgedType::isNoEscapeFunction() const {
  return unbridged().isNoEscapeFunction();
}

bool BridgedType::containsNoEscapeFunction() const {
  return unbridged().containsNoEscapeFunction();
}

bool BridgedType::isThickFunction() const {
  return unbridged().isThickFunction();
}

bool BridgedType::isAsyncFunction() const {
  return unbridged().isAsyncFunction();
}

bool BridgedType::isVoid() const {
  return unbridged().isVoid();
}

bool BridgedType::isEmpty(BridgedFunction f) const {
  return unbridged().isEmpty(*f.getFunction());
}

BridgedType::TraitResult BridgedType::canBeClass() const {
  return (TraitResult)unbridged().canBeClass();
}

bool BridgedType::isMoveOnly() const {
  return unbridged().isMoveOnly();
}

bool BridgedType::isEscapable(BridgedFunction f) const {
  return unbridged().isEscapable(*f.getFunction());
}

bool BridgedType::isOrContainsObjectiveCClass() const {
  return unbridged().isOrContainsObjectiveCClass();
}

bool BridgedType::isBuiltinInteger() const {
  return unbridged().isBuiltinInteger();
}

bool BridgedType::isBuiltinFloat() const {
  return unbridged().isBuiltinFloat();
}

bool BridgedType::isBuiltinVector() const {
  return unbridged().isBuiltinVector();
}

BridgedType BridgedType::getBuiltinVectorElementType() const {
  return unbridged().getBuiltinVectorElementType();
}

bool BridgedType::isBuiltinFixedWidthInteger(SwiftInt width) const {
  return unbridged().isBuiltinFixedWidthInteger((unsigned)width);
}

bool BridgedType::isExactSuperclassOf(BridgedType t) const {
  return unbridged().isExactSuperclassOf(t.unbridged());
}

BridgedType BridgedType::getLoweredInstanceTypeOfMetatype(BridgedFunction f) const {
  return unbridged().getLoweredInstanceTypeOfMetatype(f.getFunction());
}

bool BridgedType::isDynamicSelfMetatype() const {
  auto metaType = unbridged().castTo<swift::MetatypeType>();
  swift::Type instTy = metaType->getInstanceType();
  return instTy->is<swift::DynamicSelfType>();
}

BridgedType::MetatypeRepresentation BridgedType::getRepresentationOfMetatype(BridgedFunction f) const {
  return BridgedType::MetatypeRepresentation(
      unbridged().getRepresentationOfMetatype(f.getFunction()));
}

bool BridgedType::isCalleeConsumedFunction() const {
  return unbridged().isCalleeConsumedFunction();
}

bool BridgedType::isMarkedAsImmortal() const {
  return unbridged().isMarkedAsImmortal();
}

SwiftInt BridgedType::getCaseIdxOfEnumType(BridgedStringRef name) const {
  return unbridged().getCaseIdxOfEnumType(name.unbridged());
}

SwiftInt BridgedType::getNumNominalFields() const {
  return unbridged().getNumNominalFields();
}


BridgedType BridgedType::getFieldType(SwiftInt idx, BridgedFunction f) const {
  return unbridged().getFieldType(idx, f.getFunction());
}

SwiftInt BridgedType::getFieldIdxOfNominalType(BridgedStringRef name) const {
  return unbridged().getFieldIdxOfNominalType(name.unbridged());
}

BridgedStringRef BridgedType::getFieldName(SwiftInt idx) const {
  return unbridged().getFieldName(idx);
}

BridgedType::EnumElementIterator BridgedType::getFirstEnumCaseIterator() const {
  swift::EnumDecl *enumDecl = unbridged().getEnumOrBoundGenericEnum();
  return EnumElementIterator(enumDecl->getAllElements().begin());
}

bool BridgedType::isEndCaseIterator(EnumElementIterator i) const {
  swift::EnumDecl *enumDecl = unbridged().getEnumOrBoundGenericEnum();
  return i.unbridged() == enumDecl->getAllElements().end();
}

BridgedType BridgedType::getEnumCasePayload(EnumElementIterator i, BridgedFunction f) const {
  swift::EnumElementDecl *elt = *i.unbridged();
  if (elt->hasAssociatedValues())
    return unbridged().getEnumElementType(elt, f.getFunction());
  return swift::SILType();
}

SwiftInt BridgedType::getNumTupleElements() const {
  return unbridged().getNumTupleElements();
}

BridgedType BridgedType::getTupleElementType(SwiftInt idx) const {
  return unbridged().getTupleElementType(idx);
}

BridgedType BridgedType::getFunctionTypeWithNoEscape(bool withNoEscape) const {
  auto fnType = unbridged().getAs<swift::SILFunctionType>();
  auto newTy = fnType->getWithExtInfo(fnType->getExtInfo().withNoEscape(true));
  return swift::SILType::getPrimitiveObjectType(newTy);
}

BridgedArgumentConvention BridgedType::getCalleeConvention() const {
  auto fnType = unbridged().getAs<swift::SILFunctionType>();
  return getArgumentConvention(fnType->getCalleeConvention());
}

BridgedType BridgedType::getSuperClassType() const {
  return unbridged().getSuperclass();
}

//===----------------------------------------------------------------------===//
//                                BridgedValue
//===----------------------------------------------------------------------===//

inline BridgedValue::Ownership castOwnership(swift::OwnershipKind ownership) {
  switch (ownership) {
    case swift::OwnershipKind::Any:
      llvm_unreachable("Invalid ownership for value");
    case swift::OwnershipKind::Unowned:    return BridgedValue::Ownership::Unowned;
    case swift::OwnershipKind::Owned:      return BridgedValue::Ownership::Owned;
    case swift::OwnershipKind::Guaranteed: return BridgedValue::Ownership::Guaranteed;
    case swift::OwnershipKind::None:       return BridgedValue::Ownership::None;
  }
}

swift::ValueBase * _Nonnull BridgedValue::getSILValue() const {
  return static_cast<swift::ValueBase *>(obj);
}

swift::ValueBase * _Nullable OptionalBridgedValue::getSILValue() const {
  if (obj)
    return static_cast<swift::ValueBase *>(obj);
  return nullptr;
}

OptionalBridgedOperand BridgedValue::getFirstUse() const {
  return {*getSILValue()->use_begin()};
}

BridgedType BridgedValue::getType() const {
  return getSILValue()->getType();
}

BridgedValue::Ownership BridgedValue::getOwnership() const {
  return castOwnership(getSILValue()->getOwnershipKind());
}

BridgedFunction BridgedValue::SILUndef_getParentFunction() const {
  return {llvm::cast<swift::SILUndef>(getSILValue())->getParent()};
}

BridgedFunction BridgedValue::PlaceholderValue_getParentFunction() const {
  return {llvm::cast<swift::PlaceholderValue>(getSILValue())->getParent()};
}

//===----------------------------------------------------------------------===//
//                                BridgedOperand
//===----------------------------------------------------------------------===//

bool BridgedOperand::isTypeDependent() const { return op->isTypeDependent(); }

bool BridgedOperand::isLifetimeEnding() const { return op->isLifetimeEnding(); }

bool BridgedOperand::canAcceptOwnership(BridgedValue::Ownership ownership) const {
  return op->canAcceptKind(BridgedValue::castToOwnership(ownership));
}

OptionalBridgedOperand BridgedOperand::getNextUse() const {
  return {op->getNextUse()};
}

BridgedValue BridgedOperand::getValue() const { return {op->get()}; }

BridgedInstruction BridgedOperand::getUser() const {
  return {op->getUser()->asSILNode()};
}

BridgedOperand::OperandOwnership BridgedOperand::getOperandOwnership() const {
  switch (op->getOperandOwnership()) {
  case swift::OperandOwnership::NonUse:
    return OperandOwnership::NonUse;
  case swift::OperandOwnership::TrivialUse:
    return OperandOwnership::TrivialUse;
  case swift::OperandOwnership::InstantaneousUse:
    return OperandOwnership::InstantaneousUse;
  case swift::OperandOwnership::UnownedInstantaneousUse:
    return OperandOwnership::UnownedInstantaneousUse;
  case swift::OperandOwnership::ForwardingUnowned:
    return OperandOwnership::ForwardingUnowned;
  case swift::OperandOwnership::PointerEscape:
    return OperandOwnership::PointerEscape;
  case swift::OperandOwnership::BitwiseEscape:
    return OperandOwnership::BitwiseEscape;
  case swift::OperandOwnership::Borrow:
    return OperandOwnership::Borrow;
  case swift::OperandOwnership::DestroyingConsume:
    return OperandOwnership::DestroyingConsume;
  case swift::OperandOwnership::ForwardingConsume:
    return OperandOwnership::ForwardingConsume;
  case swift::OperandOwnership::InteriorPointer:
    return OperandOwnership::InteriorPointer;
  case swift::OperandOwnership::GuaranteedForwarding:
    return OperandOwnership::GuaranteedForwarding;
  case swift::OperandOwnership::EndBorrow:
    return OperandOwnership::EndBorrow;
  case swift::OperandOwnership::Reborrow:
    return OperandOwnership::Reborrow;
  }
}

BridgedOperand OptionalBridgedOperand::advancedBy(SwiftInt index) const { return {op + index}; }

// Assumes that `op` is not null.
SwiftInt OptionalBridgedOperand::distanceTo(BridgedOperand element) const { return element.op - op; }

//===----------------------------------------------------------------------===//
//                                BridgedArgument
//===----------------------------------------------------------------------===//

swift::SILArgument * _Nonnull BridgedArgument::getArgument() const {
  return static_cast<swift::SILArgument *>(obj);
}

BridgedBasicBlock BridgedArgument::getParent() const {
  return {getArgument()->getParent()};
}

bool BridgedArgument::isReborrow() const { return getArgument()->isReborrow(); }

OptionalBridgedDeclObj BridgedArgument::getVarDecl() const {
  return {llvm::dyn_cast_or_null<swift::VarDecl>(getArgument()->getDecl())};
}

void BridgedArgument::copyFlags(BridgedArgument fromArgument) const {
  auto *fArg = static_cast<swift::SILFunctionArgument *>(getArgument());
  fArg->copyFlags(static_cast<swift::SILFunctionArgument *>(fromArgument.getArgument()));
}

//===----------------------------------------------------------------------===//
//                                BridgedLocation
//===----------------------------------------------------------------------===//

BridgedLocation::BridgedLocation(const swift::SILDebugLocation &loc) {
  *reinterpret_cast<swift::SILDebugLocation *>(&storage) = loc;
}
const swift::SILDebugLocation &BridgedLocation::getLoc() const {
  return *reinterpret_cast<const swift::SILDebugLocation *>(&storage);
}
BridgedLocation BridgedLocation::getAutogeneratedLocation() const {
  return getLoc().getAutogeneratedLocation();
}
bool BridgedLocation::hasValidLineNumber() const {
  return getLoc().hasValidLineNumber();
}
bool BridgedLocation::isAutoGenerated() const {
  return getLoc().isAutoGenerated();
}
bool BridgedLocation::isInlined() const {
  return getLoc().getScope()->InlinedCallSite;
}
bool BridgedLocation::isEqualTo(BridgedLocation rhs) const {
  return getLoc().isEqualTo(rhs.getLoc());
}
BridgedSourceLoc BridgedLocation::getSourceLocation() const {
  swift::SILDebugLocation debugLoc = getLoc();
  swift::SILLocation silLoc = debugLoc.getLocation();
  swift::SourceLoc sourceLoc = silLoc.getSourceLoc();
  return BridgedSourceLoc(sourceLoc.getOpaquePointerValue());
}
bool BridgedLocation::hasSameSourceLocation(BridgedLocation rhs) const {
  return getLoc().hasSameSourceLocation(rhs.getLoc());
}
BridgedLocation BridgedLocation::fromNominalTypeDecl(BridgedDeclObj decl) {
  return swift::SILDebugLocation(decl.unbridged(), nullptr);
}
BridgedLocation BridgedLocation::getArtificialUnreachableLocation() {
  return swift::SILDebugLocation::getArtificialUnreachableLocation();
}

//===----------------------------------------------------------------------===//
//                                BridgedFunction
//===----------------------------------------------------------------------===//

swift::SILFunction * _Nonnull BridgedFunction::getFunction() const {
  return static_cast<swift::SILFunction *>(obj);
}

BridgedStringRef BridgedFunction::getName() const {
  return getFunction()->getName();
}

BridgedLocation BridgedFunction::getLocation() const {
  return {swift::SILDebugLocation(getFunction()->getLocation(), getFunction()->getDebugScope())}; 
}

bool BridgedFunction::hasOwnership() const { return getFunction()->hasOwnership(); }

bool BridgedFunction::hasLoweredAddresses() const { return getFunction()->getModule().useLoweredAddresses(); }

BridgedCanType BridgedFunction::getLoweredFunctionTypeInContext() const {
  auto expansion = getFunction()->getTypeExpansionContext();
  return getFunction()->getLoweredFunctionTypeInContext(expansion);
}

OptionalBridgedBasicBlock BridgedFunction::getFirstBlock() const {
  return {getFunction()->empty() ? nullptr : getFunction()->getEntryBlock()};
}

OptionalBridgedBasicBlock BridgedFunction::getLastBlock() const {
  return {getFunction()->empty() ? nullptr : &*getFunction()->rbegin()};
}

SwiftInt BridgedFunction::getNumIndirectFormalResults() const {
  return (SwiftInt)getFunction()->getLoweredFunctionType()->getNumIndirectFormalResults();
}

bool BridgedFunction::hasIndirectErrorResult() const {
  return (SwiftInt)getFunction()->getLoweredFunctionType()->hasIndirectErrorResult();
}

SwiftInt BridgedFunction::getNumSILArguments() const {
  return swift::SILFunctionConventions(getFunction()->getConventionsInContext()).getNumSILArguments();
}

BridgedType BridgedFunction::getSILArgumentType(SwiftInt idx) const {
  swift::SILFunctionConventions conv(getFunction()->getConventionsInContext());
  return conv.getSILArgumentType(idx, getFunction()->getTypeExpansionContext());
}

BridgedType BridgedFunction::getSILResultType() const {
  swift::SILFunctionConventions conv(getFunction()->getConventionsInContext());
  return conv.getSILResultType(getFunction()->getTypeExpansionContext());
}

bool BridgedFunction::isSwift51RuntimeAvailable() const {
  if (getFunction()->getResilienceExpansion() != swift::ResilienceExpansion::Maximal)
    return false;

  swift::ASTContext &ctxt = getFunction()->getModule().getASTContext();
  return swift::AvailabilityRange::forDeploymentTarget(ctxt).isContainedIn(
      ctxt.getSwift51Availability());
}

bool BridgedFunction::isPossiblyUsedExternally() const {
  return getFunction()->isPossiblyUsedExternally();
}

bool BridgedFunction::isTransparent() const {
  return getFunction()->isTransparent() == swift::IsTransparent;
}

bool BridgedFunction::isAsync() const {
  return getFunction()->isAsync();
}

bool BridgedFunction::isGlobalInitFunction() const {
  return getFunction()->isGlobalInit();
}

bool BridgedFunction::isGlobalInitOnceFunction() const {
  return getFunction()->isGlobalInitOnceFunction();
}

bool BridgedFunction::isDestructor() const {
  if (auto *declCtxt = getFunction()->getDeclContext()) {
    return llvm::isa<swift::DestructorDecl>(declCtxt);
  }
  return false;
}

bool BridgedFunction::isGeneric() const {
  return getFunction()->isGeneric();
}

bool BridgedFunction::hasSemanticsAttr(BridgedStringRef attrName) const {
  return getFunction()->hasSemanticsAttr(attrName.unbridged());
}

bool BridgedFunction::hasUnsafeNonEscapableResult() const {
  return getFunction()->hasUnsafeNonEscapableResult();
}

bool BridgedFunction::hasDynamicSelfMetadata() const {
  return getFunction()->hasDynamicSelfMetadata();
}

BridgedFunction::EffectsKind BridgedFunction::getEffectAttribute() const {
  return (EffectsKind)getFunction()->getEffectsKind();
}

BridgedFunction::PerformanceConstraints BridgedFunction::getPerformanceConstraints() const {
  return (PerformanceConstraints)getFunction()->getPerfConstraints();
}

BridgedFunction::InlineStrategy BridgedFunction::getInlineStrategy() const {
  return (InlineStrategy)getFunction()->getInlineStrategy();
}

BridgedFunction::ThunkKind BridgedFunction::isThunk() const {
  return (ThunkKind)getFunction()->isThunk();
}

void BridgedFunction::setThunk(ThunkKind kind) const {
  getFunction()->setThunk((swift::IsThunk_t)kind);
}

bool BridgedFunction::isSerialized() const {
  return getFunction()->isSerialized();
}

bool BridgedFunction::isAnySerialized() const {
  return getFunction()->isAnySerialized();
}

BridgedFunction::SerializedKind BridgedFunction::getSerializedKind() const {
  return (SerializedKind)getFunction()->getSerializedKind();
}

bool BridgedFunction::canBeInlinedIntoCaller(SerializedKind kind) const {
  return getFunction()->canBeInlinedIntoCaller(swift::SerializedKind_t(kind));
}

bool BridgedFunction::hasValidLinkageForFragileRef(SerializedKind kind) const {
  return getFunction()->hasValidLinkageForFragileRef(swift::SerializedKind_t(kind));
}

bool BridgedFunction::needsStackProtection() const {
  return getFunction()->needsStackProtection();
}

void BridgedFunction::setNeedStackProtection(bool needSP) const {
  getFunction()->setNeedStackProtection(needSP);
}

void BridgedFunction::setIsPerformanceConstraint(bool isPerfConstraint) const {
  getFunction()->setIsPerformanceConstraint(isPerfConstraint);
}

BridgedLinkage BridgedFunction::getLinkage() const {
  return (BridgedLinkage)getFunction()->getLinkage();
}

void BridgedFunction::setLinkage(BridgedLinkage linkage) const {
  getFunction()->setLinkage((swift::SILLinkage)linkage);
}

void BridgedFunction::setIsSerialized(bool isSerialized) const {
  getFunction()->setSerializedKind(isSerialized ? swift::IsSerialized : swift::IsNotSerialized);
}

bool BridgedFunction::isResilientNominalDecl(BridgedDeclObj decl) const {
  return decl.getAs<swift::NominalTypeDecl>()->isResilient(getFunction()->getModule().getSwiftModule(),
                                                           getFunction()->getResilienceExpansion());
}

BridgedType BridgedFunction::getLoweredType(BridgedASTType type) const {
  return BridgedType(getFunction()->getLoweredType(type.type));
}

BridgedType BridgedFunction::getLoweredType(BridgedType type) const {
  return BridgedType(getFunction()->getLoweredType(type.unbridged()));
}

swift::SILFunction * _Nullable OptionalBridgedFunction::getFunction() const {
  return static_cast<swift::SILFunction *>(obj);
}

//===----------------------------------------------------------------------===//
//                                BridgedGlobalVar
//===----------------------------------------------------------------------===//

swift::SILGlobalVariable * _Nonnull BridgedGlobalVar::getGlobal() const {
  return static_cast<swift::SILGlobalVariable *>(obj);
}

OptionalBridgedDeclObj BridgedGlobalVar::getDecl() const {
  return {getGlobal()->getDecl()};
}

BridgedStringRef BridgedGlobalVar::getName() const {
  return getGlobal()->getName();
}

bool BridgedGlobalVar::isLet() const { return getGlobal()->isLet(); }

void BridgedGlobalVar::setLet(bool value) const { getGlobal()->setLet(value); }

BridgedLinkage BridgedGlobalVar::getLinkage() const {
  return (BridgedLinkage)getGlobal()->getLinkage();
}

bool BridgedGlobalVar::isPossiblyUsedExternally() const {
  return getGlobal()->isPossiblyUsedExternally();
}

OptionalBridgedInstruction BridgedGlobalVar::getFirstStaticInitInst() const {
  if (getGlobal()->begin() == getGlobal()->end()) {
    return {nullptr};
  }
  swift::SILInstruction *firstInst = &*getGlobal()->begin();
  return {firstInst->asSILNode()};
}

//===----------------------------------------------------------------------===//
//                                BridgedMultiValueResult
//===----------------------------------------------------------------------===//

swift::MultipleValueInstructionResult * _Nonnull BridgedMultiValueResult::unbridged() const {
  return static_cast<swift::MultipleValueInstructionResult *>(obj);
}

BridgedInstruction BridgedMultiValueResult::getParent() const {
  return {unbridged()->getParent()};
}

SwiftInt BridgedMultiValueResult::getIndex() const {
  return (SwiftInt)unbridged()->getIndex();
}

//===----------------------------------------------------------------------===//
//                                BridgedTypeArray
//===----------------------------------------------------------------------===//

BridgedTypeArray 
BridgedTypeArray::fromReplacementTypes(BridgedSubstitutionMap substMap) {
  return substMap.unbridged().getReplacementTypes();
}

BridgedType BridgedTypeArray::getAt(SwiftInt index) const {
  swift::Type origTy = unbridged()[index];
  auto ty = origTy->getCanonicalType();
  if (ty->isLegalSILType())
    return swift::SILType::getPrimitiveObjectType(ty);
  return swift::SILType();
}

//===----------------------------------------------------------------------===//
//                                BridgedTypeArray
//===----------------------------------------------------------------------===//

BridgedType BridgedSILTypeArray::getAt(SwiftInt index) const {
  return unbridged()[index];
}

//===----------------------------------------------------------------------===//
//                                BridgedInstruction
//===----------------------------------------------------------------------===//

OptionalBridgedInstruction BridgedInstruction::getNext() const {
  auto iter = std::next(unbridged()->getIterator());
  if (iter == unbridged()->getParent()->end())
    return {nullptr};
  return {iter->asSILNode()};
}

OptionalBridgedInstruction BridgedInstruction::getPrevious() const {
  auto iter = std::next(unbridged()->getReverseIterator());
  if (iter == unbridged()->getParent()->rend())
    return {nullptr};
  return {iter->asSILNode()};
}

BridgedBasicBlock BridgedInstruction::getParent() const {
  assert(!unbridged()->isStaticInitializerInst() &&
         "cannot get the parent of a static initializer instruction");
  return {unbridged()->getParent()};
}

BridgedInstruction BridgedInstruction::getLastInstOfParent() const {
  return {unbridged()->getParent()->back().asSILNode()};
}

bool BridgedInstruction::isDeleted() const {
  return unbridged()->isDeleted();
}

bool BridgedInstruction::isInStaticInitializer() const {
  return unbridged()->isStaticInitializerInst();
}

BridgedOperandArray BridgedInstruction::getOperands() const {
  auto operands = unbridged()->getAllOperands();
  return {{operands.data()}, (SwiftInt)operands.size()};
}

BridgedOperandArray BridgedInstruction::getTypeDependentOperands() const {
  auto typeOperands = unbridged()->getTypeDependentOperands();
  return {{typeOperands.data()}, (SwiftInt)typeOperands.size()};
}

void BridgedInstruction::setOperand(SwiftInt index, BridgedValue value) const {
  unbridged()->setOperand((unsigned)index, value.getSILValue());
}

BridgedLocation BridgedInstruction::getLocation() const {
  return unbridged()->getDebugLocation();
}

BridgedMemoryBehavior BridgedInstruction::getMemBehavior() const {
  return (BridgedMemoryBehavior)unbridged()->getMemoryBehavior();
}

bool BridgedInstruction::mayRelease() const {
  return unbridged()->mayRelease();
}

bool BridgedInstruction::mayHaveSideEffects() const {
  return unbridged()->mayHaveSideEffects();
}

bool BridgedInstruction::maySuspend() const {
  return unbridged()->maySuspend();
}

bool BridgedInstruction::shouldBeForwarding() const {
  return llvm::isa<swift::OwnershipForwardingSingleValueInstruction>(unbridged()) ||
         llvm::isa<swift::OwnershipForwardingTermInst>(unbridged()) ||
         llvm::isa<swift::OwnershipForwardingMultipleValueInstruction>(unbridged());
}

SwiftInt BridgedInstruction::MultipleValueInstruction_getNumResults() const {
  return getAs<swift::MultipleValueInstruction>()->getNumResults();
}

BridgedMultiValueResult BridgedInstruction::MultipleValueInstruction_getResult(SwiftInt index) const {
  return {getAs<swift::MultipleValueInstruction>()->getResult(index)};
}

BridgedSuccessorArray BridgedInstruction::TermInst_getSuccessors() const {
  auto successors = getAs<swift::TermInst>()->getSuccessors();
  return {{successors.data()}, (SwiftInt)successors.size()};
}

swift::ForwardingInstruction * _Nonnull BridgedInstruction::getAsForwardingInstruction() const {
  auto *forwardingInst = swift::ForwardingInstruction::get(unbridged());
  assert(forwardingInst && "instruction is not defined as ForwardingInstruction");
  return forwardingInst;
}

OptionalBridgedOperand BridgedInstruction::ForwardingInst_singleForwardedOperand() const {
  return {swift::ForwardingOperation(unbridged()).getSingleForwardingOperand()};
}

BridgedOperandArray BridgedInstruction::ForwardingInst_forwardedOperands() const {
  auto operands =
      swift::ForwardingOperation(unbridged()).getForwardedOperands();
  return {{operands.data()}, (SwiftInt)operands.size()};
}

BridgedValue::Ownership BridgedInstruction::ForwardingInst_forwardingOwnership() const {
  return castOwnership(getAsForwardingInstruction()->getForwardingOwnershipKind());
}

void BridgedInstruction::ForwardingInst_setForwardingOwnership(BridgedValue::Ownership ownership) const {
  return getAsForwardingInstruction()->setForwardingOwnershipKind(BridgedValue::castToOwnership(ownership));
}

bool BridgedInstruction::ForwardingInst_preservesOwnership() const {
  return getAsForwardingInstruction()->preservesOwnership();
}

BridgedStringRef BridgedInstruction::CondFailInst_getMessage() const {
  return getAs<swift::CondFailInst>()->getMessage();
}

SwiftInt BridgedInstruction::LoadInst_getLoadOwnership() const {
  return (SwiftInt)getAs<swift::LoadInst>()->getOwnershipQualifier();
}

BridgedInstruction::BuiltinValueKind BridgedInstruction::BuiltinInst_getID() const {
  return (BuiltinValueKind)getAs<swift::BuiltinInst>()->getBuiltinInfo().ID;
}

BridgedInstruction::IntrinsicID BridgedInstruction::BuiltinInst_getIntrinsicID() const {
  switch (getAs<swift::BuiltinInst>()->getIntrinsicInfo().ID) {
    case llvm::Intrinsic::memcpy:  return IntrinsicID::memcpy;
    case llvm::Intrinsic::memmove: return IntrinsicID::memmove;
    default: return IntrinsicID::unknown;
  }
}

BridgedSubstitutionMap BridgedInstruction::BuiltinInst_getSubstitutionMap() const {
  return getAs<swift::BuiltinInst>()->getSubstitutions();
}

bool BridgedInstruction::PointerToAddressInst_isStrict() const {
  return getAs<swift::PointerToAddressInst>()->isStrict();
}

bool BridgedInstruction::AddressToPointerInst_needsStackProtection() const {
  return getAs<swift::AddressToPointerInst>()->needsStackProtection();
}

bool BridgedInstruction::IndexAddrInst_needsStackProtection() const {
  return getAs<swift::IndexAddrInst>()->needsStackProtection();
}

BridgedConformanceArray BridgedInstruction::InitExistentialRefInst_getConformances() const {
  return getAs<swift::InitExistentialRefInst>()->getConformances();
}

BridgedCanType BridgedInstruction::InitExistentialRefInst_getFormalConcreteType() const {
  return getAs<swift::InitExistentialRefInst>()->getFormalConcreteType();
}

BridgedGlobalVar BridgedInstruction::GlobalAccessInst_getGlobal() const {
  return {getAs<swift::GlobalAccessInst>()->getReferencedGlobal()};
}

BridgedGlobalVar BridgedInstruction::AllocGlobalInst_getGlobal() const {
  return {getAs<swift::AllocGlobalInst>()->getReferencedGlobal()};
}

BridgedFunction BridgedInstruction::FunctionRefBaseInst_getReferencedFunction() const {
  return {getAs<swift::FunctionRefBaseInst>()->getInitiallyReferencedFunction()};
}

BridgedInstruction::OptionalInt BridgedInstruction::IntegerLiteralInst_getValue() const {
  llvm::APInt result = getAs<swift::IntegerLiteralInst>()->getValue();
  if (result.getSignificantBits() <= std::min(std::numeric_limits<SwiftInt>::digits, 64)) {
    return {(SwiftInt)result.getSExtValue(), true};
  }
  return {0, false};
}

BridgedStringRef BridgedInstruction::StringLiteralInst_getValue() const {
  return getAs<swift::StringLiteralInst>()->getValue();
}

int BridgedInstruction::StringLiteralInst_getEncoding() const {
  return (int)getAs<swift::StringLiteralInst>()->getEncoding();
}

SwiftInt BridgedInstruction::TupleExtractInst_fieldIndex() const {
  return getAs<swift::TupleExtractInst>()->getFieldIndex();
}

SwiftInt BridgedInstruction::TupleElementAddrInst_fieldIndex() const {
  return getAs<swift::TupleElementAddrInst>()->getFieldIndex();
}

SwiftInt BridgedInstruction::StructExtractInst_fieldIndex() const {
  return getAs<swift::StructExtractInst>()->getFieldIndex();
}

OptionalBridgedValue BridgedInstruction::StructInst_getUniqueNonTrivialFieldValue() const {
  return {getAs<swift::StructInst>()->getUniqueNonTrivialFieldValue()};
}

SwiftInt BridgedInstruction::StructElementAddrInst_fieldIndex() const {
  return getAs<swift::StructElementAddrInst>()->getFieldIndex();
}

bool BridgedInstruction::BeginBorrow_isLexical() const {
  return getAs<swift::BeginBorrowInst>()->isLexical();
}

bool BridgedInstruction::BeginBorrow_hasPointerEscape() const {
  return getAs<swift::BeginBorrowInst>()->hasPointerEscape();
}

bool BridgedInstruction::BeginBorrow_isFromVarDecl() const {
  return getAs<swift::BeginBorrowInst>()->isFromVarDecl();
}

bool BridgedInstruction::MoveValue_isLexical() const {
  return getAs<swift::MoveValueInst>()->isLexical();
}

bool BridgedInstruction::MoveValue_hasPointerEscape() const {
  return getAs<swift::MoveValueInst>()->hasPointerEscape();
}

bool BridgedInstruction::MoveValue_isFromVarDecl() const {
  return getAs<swift::MoveValueInst>()->isFromVarDecl();
}

SwiftInt BridgedInstruction::ProjectBoxInst_fieldIndex() const {
  return getAs<swift::ProjectBoxInst>()->getFieldIndex();
}

bool BridgedInstruction::EndCOWMutationInst_doKeepUnique() const {
  return getAs<swift::EndCOWMutationInst>()->doKeepUnique();
}

bool BridgedInstruction::DestroyValueInst_isDeadEnd() const {
  return getAs<swift::DestroyValueInst>()->isDeadEnd();
}

SwiftInt BridgedInstruction::EnumInst_caseIndex() const {
  return getAs<swift::EnumInst>()->getCaseIndex();
}

SwiftInt BridgedInstruction::UncheckedEnumDataInst_caseIndex() const {
  return getAs<swift::UncheckedEnumDataInst>()->getCaseIndex();
}

SwiftInt BridgedInstruction::InitEnumDataAddrInst_caseIndex() const {
  return getAs<swift::InitEnumDataAddrInst>()->getCaseIndex();
}

SwiftInt BridgedInstruction::UncheckedTakeEnumDataAddrInst_caseIndex() const {
  return getAs<swift::UncheckedTakeEnumDataAddrInst>()->getCaseIndex();
}

SwiftInt BridgedInstruction::InjectEnumAddrInst_caseIndex() const {
  return getAs<swift::InjectEnumAddrInst>()->getCaseIndex();
}

SwiftInt BridgedInstruction::RefElementAddrInst_fieldIndex() const {
  return getAs<swift::RefElementAddrInst>()->getFieldIndex();
}

bool BridgedInstruction::RefElementAddrInst_fieldIsLet() const {
  return getAs<swift::RefElementAddrInst>()->getField()->isLet();
}

bool BridgedInstruction::RefElementAddrInst_isImmutable() const {
  return getAs<swift::RefElementAddrInst>()->isImmutable();
}

void BridgedInstruction::RefElementAddrInst_setImmutable(bool isImmutable) const {
  getAs<swift::RefElementAddrInst>()->setImmutable(isImmutable);
}

bool BridgedInstruction::RefTailAddrInst_isImmutable() const {
  return getAs<swift::RefTailAddrInst>()->isImmutable();
}

SwiftInt BridgedInstruction::PartialApplyInst_numArguments() const {
  return getAs<swift::PartialApplyInst>()->getNumArguments();
}

SwiftInt BridgedInstruction::ApplyInst_numArguments() const {
  return getAs<swift::ApplyInst>()->getNumArguments();
}

bool BridgedInstruction::ApplyInst_getNonThrowing() const {
  return getAs<swift::ApplyInst>()->isNonThrowing();
}

bool BridgedInstruction::ApplyInst_getNonAsync() const {
  return getAs<swift::ApplyInst>()->isNonAsync();
}

BridgedGenericSpecializationInformation BridgedInstruction::ApplyInst_getSpecializationInfo() const {
  return {getAs<swift::ApplyInst>()->getSpecializationInfo()};
}

BridgedGenericSpecializationInformation BridgedInstruction::TryApplyInst_getSpecializationInfo() const {
  return {getAs<swift::TryApplyInst>()->getSpecializationInfo()};
}

SwiftInt BridgedInstruction::ObjectInst_getNumBaseElements() const {
  return getAs<swift::ObjectInst>()->getNumBaseElements();
}

SwiftInt BridgedInstruction::PartialApply_getCalleeArgIndexOfFirstAppliedArg() const {
  return swift::ApplySite(unbridged()).getCalleeArgIndexOfFirstAppliedArg();
}

bool BridgedInstruction::PartialApplyInst_isOnStack() const {
  return getAs<swift::PartialApplyInst>()->isOnStack();
}

bool BridgedInstruction::PartialApplyInst_hasUnknownResultIsolation() const {
  return getAs<swift::PartialApplyInst>()->getResultIsolation() == swift::SILFunctionTypeIsolation::Unknown;
}

bool BridgedInstruction::AllocStackInst_hasDynamicLifetime() const {
  return getAs<swift::AllocStackInst>()->hasDynamicLifetime();
}

bool BridgedInstruction::AllocRefInstBase_isObjc() const {
  return getAs<swift::AllocRefInstBase>()->isObjC();
}

bool BridgedInstruction::AllocRefInstBase_canAllocOnStack() const {
  return getAs<swift::AllocRefInstBase>()->canAllocOnStack();
}

SwiftInt BridgedInstruction::AllocRefInstBase_getNumTailTypes() const {
  return getAs<swift::AllocRefInstBase>()->getNumTailTypes();
}

BridgedSILTypeArray BridgedInstruction::AllocRefInstBase_getTailAllocatedTypes() const {
  return getAs<const swift::AllocRefInstBase>()->getTailAllocatedTypes();
}

bool BridgedInstruction::AllocRefDynamicInst_isDynamicTypeDeinitAndSizeKnownEquivalentToBaseType() const {
  return getAs<swift::AllocRefDynamicInst>()->isDynamicTypeDeinitAndSizeKnownEquivalentToBaseType();
}

SwiftInt BridgedInstruction::BeginApplyInst_numArguments() const {
  return getAs<swift::BeginApplyInst>()->getNumArguments();
}

SwiftInt BridgedInstruction::TryApplyInst_numArguments() const {
  return getAs<swift::TryApplyInst>()->getNumArguments();
}

BridgedBasicBlock BridgedInstruction::BranchInst_getTargetBlock() const {
  return {getAs<swift::BranchInst>()->getDestBB()};
}

SwiftInt BridgedInstruction::SwitchEnumInst_getNumCases() const {
  return getAs<swift::SwitchEnumInst>()->getNumCases();
}

SwiftInt BridgedInstruction::SwitchEnumInst_getCaseIndex(SwiftInt idx) const {
  auto *seInst = getAs<swift::SwitchEnumInst>();
  return seInst->getModule().getCaseIndex(seInst->getCase(idx).first);
}

SwiftInt BridgedInstruction::StoreInst_getStoreOwnership() const {
  return (SwiftInt)getAs<swift::StoreInst>()->getOwnershipQualifier();
}

SwiftInt BridgedInstruction::AssignInst_getAssignOwnership() const {
  return (SwiftInt)getAs<swift::AssignInst>()->getOwnershipQualifier();
}

BridgedInstruction::MarkDependenceKind BridgedInstruction::MarkDependenceInst_dependenceKind() const {
  return (MarkDependenceKind)getAs<swift::MarkDependenceInst>()->dependenceKind();
}

void BridgedInstruction::MarkDependenceInst_resolveToNonEscaping() const {
  getAs<swift::MarkDependenceInst>()->resolveToNonEscaping();
}

SwiftInt BridgedInstruction::BeginAccessInst_getAccessKind() const {
  return (SwiftInt)getAs<swift::BeginAccessInst>()->getAccessKind();
}

bool BridgedInstruction::BeginAccessInst_isStatic() const {
  return getAs<swift::BeginAccessInst>()->getEnforcement() == swift::SILAccessEnforcement::Static;
}

bool BridgedInstruction::BeginAccessInst_isUnsafe() const {
  return getAs<swift::BeginAccessInst>()->getEnforcement() == swift::SILAccessEnforcement::Unsafe;
}

bool BridgedInstruction::CopyAddrInst_isTakeOfSrc() const {
  return getAs<swift::CopyAddrInst>()->isTakeOfSrc();
}

bool BridgedInstruction::CopyAddrInst_isInitializationOfDest() const {
  return getAs<swift::CopyAddrInst>()->isInitializationOfDest();
}

bool BridgedInstruction::ExplicitCopyAddrInst_isTakeOfSrc() const {
  return getAs<swift::ExplicitCopyAddrInst>()->isTakeOfSrc();
}

bool BridgedInstruction::ExplicitCopyAddrInst_isInitializationOfDest() const {
  return getAs<swift::ExplicitCopyAddrInst>()->isInitializationOfDest();
}

SwiftInt BridgedInstruction::MarkUninitializedInst_getKind() const {
  return (SwiftInt)getAs<swift::MarkUninitializedInst>()->getMarkUninitializedKind();
}

void BridgedInstruction::RefCountingInst_setIsAtomic(bool isAtomic) const {
  getAs<swift::RefCountingInst>()->setAtomicity(
      isAtomic ? swift::RefCountingInst::Atomicity::Atomic
               : swift::RefCountingInst::Atomicity::NonAtomic);
}

bool BridgedInstruction::RefCountingInst_getIsAtomic() const {
  return getAs<swift::RefCountingInst>()->getAtomicity() == swift::RefCountingInst::Atomicity::Atomic;
}

SwiftInt BridgedInstruction::CondBranchInst_getNumTrueArgs() const {
  return getAs<swift::CondBranchInst>()->getNumTrueArgs();
}

void BridgedInstruction::AllocRefInstBase_setIsStackAllocatable() const {
  getAs<swift::AllocRefInstBase>()->setStackAllocatable();
}

bool BridgedInstruction::AllocRefInst_isBare() const {
  return getAs<swift::AllocRefInst>()->isBare();
}

void BridgedInstruction::AllocRefInst_setIsBare() const {
  getAs<swift::AllocRefInst>()->setBare(true);
}

void BridgedInstruction::TermInst_replaceBranchTarget(BridgedBasicBlock from, BridgedBasicBlock to) const {
  getAs<swift::TermInst>()->replaceBranchTarget(from.unbridged(),
                                                to.unbridged());
}

SwiftInt BridgedInstruction::KeyPathInst_getNumComponents() const {
  if (swift::KeyPathPattern *pattern = getAs<swift::KeyPathInst>()->getPattern()) {
    return (SwiftInt)pattern->getComponents().size();
  }
  return 0;
}

void BridgedInstruction::KeyPathInst_getReferencedFunctions(SwiftInt componentIdx,
                                                            KeyPathFunctionResults * _Nonnull results) const {
  swift::KeyPathPattern *pattern = getAs<swift::KeyPathInst>()->getPattern();
  const swift::KeyPathPatternComponent &comp = pattern->getComponents()[componentIdx];
  results->numFunctions = 0;

  comp.visitReferencedFunctionsAndMethods([results](swift::SILFunction *func) {
      assert(results->numFunctions < KeyPathFunctionResults::maxFunctions);
      results->functions[results->numFunctions++] = {func};
    }, [](swift::SILDeclRef) {});
}

void BridgedInstruction::GlobalAddrInst_clearToken() const {
  getAs<swift::GlobalAddrInst>()->clearToken();
}

bool BridgedInstruction::GlobalValueInst_isBare() const {
  return getAs<swift::GlobalValueInst>()->isBare();
}

void BridgedInstruction::GlobalValueInst_setIsBare() const {
  getAs<swift::GlobalValueInst>()->setBare(true);
}

void BridgedInstruction::LoadInst_setOwnership(SwiftInt ownership) const {
  getAs<swift::LoadInst>()->setOwnershipQualifier((swift::LoadOwnershipQualifier)ownership);
}

BridgedBasicBlock BridgedInstruction::CheckedCastBranch_getSuccessBlock() const {
  return {getAs<swift::CheckedCastBranchInst>()->getSuccessBB()};
}

BridgedBasicBlock BridgedInstruction::CheckedCastBranch_getFailureBlock() const {
  return {getAs<swift::CheckedCastBranchInst>()->getFailureBB()};
}

BridgedBasicBlock BridgedInstruction::CheckedCastAddrBranch_getSuccessBlock() const {
  return {getAs<swift::CheckedCastAddrBranchInst>()->getSuccessBB()};
}

BridgedBasicBlock BridgedInstruction::CheckedCastAddrBranch_getFailureBlock() const {
  return {getAs<swift::CheckedCastAddrBranchInst>()->getFailureBB()};
}

BridgedInstruction::CastConsumptionKind BridgedInstruction::CheckedCastAddrBranch_getConsumptionKind() const {
  static_assert((int)BridgedInstruction::CastConsumptionKind::TakeAlways ==
                (int)swift::CastConsumptionKind::TakeAlways);
  static_assert((int)BridgedInstruction::CastConsumptionKind::TakeOnSuccess ==
                (int)swift::CastConsumptionKind::TakeOnSuccess);
  static_assert((int)BridgedInstruction::CastConsumptionKind::CopyOnSuccess ==
                (int)swift::CastConsumptionKind::CopyOnSuccess);

  return static_cast<BridgedInstruction::CastConsumptionKind>(
           getAs<swift::CheckedCastAddrBranchInst>()->getConsumptionKind());
}


BridgedSubstitutionMap BridgedInstruction::ApplySite_getSubstitutionMap() const {
  auto as = swift::ApplySite(unbridged());
  return as.getSubstitutionMap();
}

BridgedCanType BridgedInstruction::ApplySite_getSubstitutedCalleeType() const {
  auto as = swift::ApplySite(unbridged());
  return as.getSubstCalleeType();
}

SwiftInt BridgedInstruction::ApplySite_getNumArguments() const {
  return swift::ApplySite(unbridged()).getNumArguments();
}

bool BridgedInstruction::ApplySite_isCalleeNoReturn() const {
  return swift::ApplySite(unbridged()).isCalleeNoReturn();
}

SwiftInt BridgedInstruction::FullApplySite_numIndirectResultArguments() const {
  auto fas = swift::FullApplySite(unbridged());
  return fas.getNumIndirectSILResults();
}

bool BridgedInstruction::ConvertFunctionInst_withoutActuallyEscaping() const {
  return getAs<swift::ConvertFunctionInst>()->withoutActuallyEscaping();
}

BridgedCanType BridgedInstruction::TypeValueInst_getParamType() const {
  return getAs<swift::TypeValueInst>()->getParamType();
}

SwiftInt BridgedInstruction::TypeValueInst_getValue() const {
  auto tvi = getAs<swift::TypeValueInst>();

  // Assume we've already checked that the parameter type is an IntegerType.
  auto integer = tvi->getParamType()->castTo<swift::IntegerType>();

  if (integer->isNegative()) {
    return integer->getValue().getSExtValue();
  } else {
    return integer->getValue().getZExtValue();
  }
}

//===----------------------------------------------------------------------===//
//                     VarDeclInst and DebugVariableInst
//===----------------------------------------------------------------------===//

OptionalBridgedDeclObj BridgedInstruction::DebugValue_getDecl() const {
  return {getAs<swift::DebugValueInst>()->getDecl()};
}

OptionalBridgedDeclObj BridgedInstruction::AllocStack_getDecl() const {
  return {getAs<swift::AllocStackInst>()->getDecl()};
}

OptionalBridgedDeclObj BridgedInstruction::AllocBox_getDecl() const {
  return {getAs<swift::AllocBoxInst>()->getDecl()};
}

OptionalBridgedDeclObj BridgedInstruction::GlobalAddr_getDecl() const {
  return {getAs<swift::GlobalAddrInst>()->getReferencedGlobal()->getDecl()};
}

OptionalBridgedDeclObj BridgedInstruction::RefElementAddr_getDecl() const {
  return {getAs<swift::RefElementAddrInst>()->getField()};
}

OptionalBridgedSILDebugVariable
BridgedInstruction::DebugValue_getVarInfo() const {
  return getAs<swift::DebugValueInst>()->getVarInfo();
}

OptionalBridgedSILDebugVariable
BridgedInstruction::AllocStack_getVarInfo() const {
  return getAs<swift::AllocStackInst>()->getVarInfo();
}

OptionalBridgedSILDebugVariable
BridgedInstruction::AllocBox_getVarInfo() const {
  return getAs<swift::AllocBoxInst>()->getVarInfo();
}

//===----------------------------------------------------------------------===//
//                                BridgedBasicBlock
//===----------------------------------------------------------------------===//

OptionalBridgedBasicBlock BridgedBasicBlock::getNext() const {
  auto iter = std::next(unbridged()->getIterator());
  if (iter == unbridged()->getParent()->end())
    return {nullptr};
  return {&*iter};
}

OptionalBridgedBasicBlock BridgedBasicBlock::getPrevious() const {
  auto iter = std::next(unbridged()->getReverseIterator());
  if (iter == unbridged()->getParent()->rend())
    return {nullptr};
  return {&*iter};
}

BridgedFunction BridgedBasicBlock::getFunction() const {
  return {unbridged()->getParent()};
}

OptionalBridgedInstruction BridgedBasicBlock::getFirstInst() const {
  if (unbridged()->empty())
    return {nullptr};
  return {unbridged()->front().asSILNode()};
}

OptionalBridgedInstruction BridgedBasicBlock::getLastInst() const {
  if (unbridged()->empty())
    return {nullptr};
  return {unbridged()->back().asSILNode()};
}

SwiftInt BridgedBasicBlock::getNumArguments() const {
  return unbridged()->getNumArguments();
}

BridgedArgument BridgedBasicBlock::getArgument(SwiftInt index) const {
  return {unbridged()->getArgument(index)};
}

BridgedArgument BridgedBasicBlock::addBlockArgument(BridgedType type, BridgedValue::Ownership ownership) const {
  return {unbridged()->createPhiArgument(
      type.unbridged(), BridgedValue::castToOwnership(ownership))};
}

BridgedArgument BridgedBasicBlock::addFunctionArgument(BridgedType type) const {
  return {unbridged()->createFunctionArgument(type.unbridged())};
}

void BridgedBasicBlock::eraseArgument(SwiftInt index) const {
  unbridged()->eraseArgument(index);
}

void BridgedBasicBlock::moveAllInstructionsToBegin(BridgedBasicBlock dest) const {
  dest.unbridged()->spliceAtBegin(unbridged());
}

void BridgedBasicBlock::moveAllInstructionsToEnd(BridgedBasicBlock dest) const {
  dest.unbridged()->spliceAtEnd(unbridged());
}

void BridgedBasicBlock::moveArgumentsTo(BridgedBasicBlock dest) const {
  dest.unbridged()->moveArgumentList(unbridged());
}

OptionalBridgedSuccessor BridgedBasicBlock::getFirstPred() const {
  return {unbridged()->pred_begin().getSuccessorRef()};
}

//===----------------------------------------------------------------------===//
//                                BridgedSuccessor
//===----------------------------------------------------------------------===//

OptionalBridgedSuccessor BridgedSuccessor::getNext() const {
  return {succ->getNext()};
}

BridgedBasicBlock BridgedSuccessor::getTargetBlock() const {
  return succ->getBB();
}

BridgedInstruction BridgedSuccessor::getContainingInst() const {
  return {succ->getContainingInst()};
}

BridgedSuccessor OptionalBridgedSuccessor::advancedBy(SwiftInt index) const {
  return {succ + index};
}

//===----------------------------------------------------------------------===//
//                                BridgedDeclRef
//===----------------------------------------------------------------------===//

static_assert(sizeof(BridgedDeclRef) >= sizeof(swift::SILDeclRef),
              "BridgedDeclRef has wrong size");

BridgedLocation BridgedDeclRef::getLocation() const {
  return swift::SILDebugLocation(unbridged().getDecl(), nullptr);
}

BridgedDiagnosticArgument BridgedDeclRef::asDiagnosticArgument() const {
  return swift::DiagnosticArgument(unbridged().getDecl()->getName());
}

//===----------------------------------------------------------------------===//
//                                BridgedVTable
//===----------------------------------------------------------------------===//

BridgedVTableEntry::Kind BridgedVTableEntry::getKind() const {
  return (Kind)unbridged().getKind();
}

BRIDGED_INLINE bool BridgedVTableEntry::isNonOverridden() const {
  return unbridged().isNonOverridden();
}

BridgedDeclRef BridgedVTableEntry::getMethodDecl() const {
  return unbridged().getMethod();
}

BridgedFunction BridgedVTableEntry::getImplementation() const {
  return {unbridged().getImplementation()};
}

BridgedVTableEntry BridgedVTableEntry::create(Kind kind, bool nonOverridden,
                                              BridgedDeclRef methodDecl, BridgedFunction implementation) {
  return swift::SILVTableEntry(methodDecl.unbridged(), implementation.getFunction(),
                               (swift::SILVTableEntry::Kind)kind, nonOverridden);
}

SwiftInt BridgedVTable::getNumEntries() const {
  return SwiftInt(vTable->getEntries().size());
}

BridgedVTableEntry BridgedVTable::getEntry(SwiftInt index) const {
  return BridgedVTableEntry(vTable->getEntries()[index]);
}

BridgedDeclObj BridgedVTable::getClass() const {
  return vTable->getClass();
}

BridgedType BridgedVTable::getSpecializedClassType() const {
  return {vTable->getClassType()};
}

//===----------------------------------------------------------------------===//
//               BridgedWitnessTable, BridgedDefaultWitnessTable
//===----------------------------------------------------------------------===//

BridgedWitnessTableEntry::Kind BridgedWitnessTableEntry::getKind() const {
  return (Kind)unbridged().getKind();
}

BridgedDeclRef BridgedWitnessTableEntry::getMethodRequirement() const {
  return unbridged().getMethodWitness().Requirement;
}

OptionalBridgedFunction BridgedWitnessTableEntry::getMethodWitness() const {
  return {unbridged().getMethodWitness().Witness};
}

BridgedDeclObj BridgedWitnessTableEntry::getAssociatedTypeRequirement() const {
  return {unbridged().getAssociatedTypeWitness().Requirement};
}

BridgedCanType BridgedWitnessTableEntry::getAssociatedTypeWitness() const {
  return unbridged().getAssociatedTypeWitness().Witness;
}

BridgedCanType BridgedWitnessTableEntry::getAssociatedConformanceRequirement() const {
  return unbridged().getAssociatedConformanceWitness().Requirement;
}

BridgedDeclObj BridgedWitnessTableEntry::getAssociatedConformanceDecl() const {
  return {unbridged().getAssociatedConformanceWitness().Protocol};
}

BridgedConformance BridgedWitnessTableEntry::getAssociatedConformanceWitness() const {
  return {unbridged().getAssociatedConformanceWitness().Witness};
}

BridgedDeclObj BridgedWitnessTableEntry::getBaseProtocolRequirement() const {
  return {unbridged().getBaseProtocolWitness().Requirement};
}

BridgedConformance BridgedWitnessTableEntry::getBaseProtocolWitness() const {
  return swift::ProtocolConformanceRef(unbridged().getBaseProtocolWitness().Witness);
}


BridgedWitnessTableEntry BridgedWitnessTableEntry::createInvalid() {
  return swift::SILWitnessTable::Entry();
}

BridgedWitnessTableEntry BridgedWitnessTableEntry::createMethod(BridgedDeclRef requirement,
                                                                OptionalBridgedFunction witness) {
  return swift::SILWitnessTable::Entry(
    swift::SILWitnessTable::MethodWitness{requirement.unbridged(), witness.getFunction()});
}

BridgedWitnessTableEntry BridgedWitnessTableEntry::createAssociatedType(BridgedDeclObj requirement,
                                                                        BridgedCanType witness) {
  return swift::SILWitnessTable::Entry(
    swift::SILWitnessTable::AssociatedTypeWitness{requirement.getAs<swift::AssociatedTypeDecl>(),
                                                  witness.unbridged()});
}

BridgedWitnessTableEntry BridgedWitnessTableEntry::createAssociatedConformance(BridgedCanType requirement,
                                                                               BridgedDeclObj protocolDecl,
                                                                               BridgedConformance witness) {
  return swift::SILWitnessTable::Entry(
    swift::SILWitnessTable::AssociatedConformanceWitness{requirement.unbridged(),
                                                         protocolDecl.getAs<swift::ProtocolDecl>(),
                                                         witness.unbridged()});
}

BridgedWitnessTableEntry BridgedWitnessTableEntry::createBaseProtocol(BridgedDeclObj requirement,
                                                                      BridgedConformance witness) {
  return swift::SILWitnessTable::Entry(
    swift::SILWitnessTable::BaseProtocolWitness{requirement.getAs<swift::ProtocolDecl>(),
                                                witness.unbridged().getConcrete()});
}

SwiftInt BridgedWitnessTable::getNumEntries() const {
  return SwiftInt(table->getEntries().size());
}

BridgedWitnessTableEntry BridgedWitnessTable::getEntry(SwiftInt index) const {
  return BridgedWitnessTableEntry(table->getEntries()[index]);
}

bool BridgedWitnessTable::isDeclaration() const {
  return table->isDeclaration();
}

SwiftInt BridgedDefaultWitnessTable::getNumEntries() const {
  return SwiftInt(table->getEntries().size());
}

BridgedWitnessTableEntry BridgedDefaultWitnessTable::getEntry(SwiftInt index) const {
  return BridgedWitnessTableEntry(table->getEntries()[index]);
}

//===----------------------------------------------------------------------===//
//                                BridgedBuilder
//===----------------------------------------------------------------------===//

BridgedInstruction BridgedBuilder::createBuiltinBinaryFunction(BridgedStringRef name,
                                               BridgedType operandType, BridgedType resultType,
                                               BridgedValueArray arguments) const {
  llvm::SmallVector<swift::SILValue, 16> argValues;
  return {unbridged().createBuiltinBinaryFunction(
      regularLoc(), name.unbridged(), operandType.unbridged(),
      resultType.unbridged(), arguments.getValues(argValues))};
}

BridgedInstruction BridgedBuilder::createCondFail(BridgedValue condition, BridgedStringRef message) const {
  return {unbridged().createCondFail(regularLoc(), condition.getSILValue(),
                                     message.unbridged())};
}

BridgedInstruction BridgedBuilder::createIntegerLiteral(BridgedType type, SwiftInt value) const {
  return {
      unbridged().createIntegerLiteral(regularLoc(), type.unbridged(), value)};
}

BridgedInstruction BridgedBuilder::createAllocRef(BridgedType type,
    bool objc, bool canAllocOnStack, bool isBare,
    BridgedSILTypeArray elementTypes, BridgedValueArray elementCountOperands) const {
  llvm::SmallVector<swift::SILValue, 16> elementCountOperandsValues;
  return {unbridged().createAllocRef(
      regularLoc(), type.unbridged(), objc, canAllocOnStack, isBare,
      elementTypes.unbridged(),
      elementCountOperands.getValues(elementCountOperandsValues)
      )};
}

BridgedInstruction BridgedBuilder::createAllocStack(BridgedType type,
                                                    bool hasDynamicLifetime,
                                                    bool isLexical,
                                                    bool isFromVarDecl,
                                                    bool wasMoved) const {
  return {unbridged().createAllocStack(
      regularLoc(), type.unbridged(), std::nullopt,
      swift::HasDynamicLifetime_t(hasDynamicLifetime),
      swift::IsLexical_t(isLexical), swift::IsFromVarDecl_t(isFromVarDecl),
      swift::UsesMoveableValueDebugInfo_t(wasMoved), /*skipVarDeclAssert=*/ true)};
}

BridgedInstruction BridgedBuilder::createAllocVector(BridgedValue capacity, BridgedType type) const {
  return {unbridged().createAllocVector(regularLoc(), capacity.getSILValue(), type.unbridged())};
}

BridgedInstruction BridgedBuilder::createDeallocStack(BridgedValue operand) const {
  return {unbridged().createDeallocStack(regularLoc(), operand.getSILValue())};
}

BridgedInstruction BridgedBuilder::createDeallocStackRef(BridgedValue operand) const {
  return {
      unbridged().createDeallocStackRef(regularLoc(), operand.getSILValue())};
}

BridgedInstruction BridgedBuilder::createAddressToPointer(BridgedValue address, BridgedType pointerTy,
                                                          bool needsStackProtection) const {
  return {unbridged().createAddressToPointer(regularLoc(), address.getSILValue(), pointerTy.unbridged(),
                                             needsStackProtection)};
}

BridgedInstruction BridgedBuilder::createUncheckedRefCast(BridgedValue op, BridgedType type) const {
  return {unbridged().createUncheckedRefCast(regularLoc(), op.getSILValue(),
                                             type.unbridged())};
}

BridgedInstruction BridgedBuilder::createUpcast(BridgedValue op, BridgedType type) const {
  return {unbridged().createUpcast(regularLoc(), op.getSILValue(),
                                   type.unbridged())};
}

BridgedInstruction BridgedBuilder::createLoad(BridgedValue op, SwiftInt ownership) const {
  return {unbridged().createLoad(regularLoc(), op.getSILValue(),
                                 (swift::LoadOwnershipQualifier)ownership)};
}

BridgedInstruction BridgedBuilder::createLoadBorrow(BridgedValue op) const {
  return {unbridged().createLoadBorrow(regularLoc(), op.getSILValue())};
}

BridgedInstruction BridgedBuilder::createBeginDeallocRef(BridgedValue reference, BridgedValue allocation) const {
  return {unbridged().createBeginDeallocRef(
      regularLoc(), reference.getSILValue(), allocation.getSILValue())};
}

BridgedInstruction BridgedBuilder::createEndInitLetRef(BridgedValue op) const {
  return {unbridged().createEndInitLetRef(regularLoc(), op.getSILValue())};
}

BridgedInstruction BridgedBuilder::createRetainValue(BridgedValue op) const {
  auto b = unbridged();
  return {b.createRetainValue(regularLoc(), op.getSILValue(),
                              b.getDefaultAtomicity())};
}

BridgedInstruction BridgedBuilder::createReleaseValue(BridgedValue op) const {
  auto b = unbridged();
  return {b.createReleaseValue(regularLoc(), op.getSILValue(),
                               b.getDefaultAtomicity())};
}

BridgedInstruction BridgedBuilder::createStrongRetain(BridgedValue op) const {
  auto b = unbridged();
  return {b.createStrongRetain(regularLoc(), op.getSILValue(), b.getDefaultAtomicity())};
}

BridgedInstruction BridgedBuilder::createStrongRelease(BridgedValue op) const {
  auto b = unbridged();
  return {b.createStrongRelease(regularLoc(), op.getSILValue(), b.getDefaultAtomicity())};
}

BridgedInstruction BridgedBuilder::createUnownedRetain(BridgedValue op) const {
  auto b = unbridged();
  return {b.createUnownedRetain(regularLoc(), op.getSILValue(), b.getDefaultAtomicity())};
}

BridgedInstruction BridgedBuilder::createUnownedRelease(BridgedValue op) const {
  auto b = unbridged();
  return {b.createUnownedRelease(regularLoc(), op.getSILValue(), b.getDefaultAtomicity())};
}

BridgedInstruction BridgedBuilder::createFunctionRef(BridgedFunction function) const {
  return {unbridged().createFunctionRef(regularLoc(), function.getFunction())};
}

BridgedInstruction BridgedBuilder::createCopyValue(BridgedValue op) const {
  return {unbridged().createCopyValue(regularLoc(), op.getSILValue())};
}

BridgedInstruction BridgedBuilder::createBeginBorrow(BridgedValue op) const {
  return {unbridged().createBeginBorrow(regularLoc(), op.getSILValue())};
}

BridgedInstruction BridgedBuilder::createBorrowedFrom(BridgedValue borrowedValue,
                                                      BridgedValueArray enclosingValues) const {
  llvm::SmallVector<swift::SILValue, 16> evs;
  return {unbridged().createBorrowedFrom(regularLoc(), borrowedValue.getSILValue(),
                                         enclosingValues.getValues(evs))};
}

BridgedInstruction BridgedBuilder::createEndBorrow(BridgedValue op) const {
  return {unbridged().createEndBorrow(regularLoc(), op.getSILValue())};
}

BridgedInstruction BridgedBuilder::createCopyAddr(BridgedValue from, BridgedValue to,
                                  bool takeSource, bool initializeDest) const {
  return {unbridged().createCopyAddr(
      regularLoc(), from.getSILValue(), to.getSILValue(),
      swift::IsTake_t(takeSource), swift::IsInitialization_t(initializeDest))};
}

BridgedInstruction BridgedBuilder::createDestroyValue(BridgedValue op) const {
  return {unbridged().createDestroyValue(regularLoc(), op.getSILValue())};
}

BridgedInstruction BridgedBuilder::createDestroyAddr(BridgedValue op) const {
  return {unbridged().createDestroyAddr(regularLoc(), op.getSILValue())};
}

BridgedInstruction BridgedBuilder::createEndLifetime(BridgedValue op) const {
  return {unbridged().createEndLifetime(regularLoc(), op.getSILValue())};
}

BridgedInstruction BridgedBuilder::createDebugStep() const {
  return {unbridged().createDebugStep(regularLoc())};
}

BridgedInstruction BridgedBuilder::createApply(BridgedValue function, BridgedSubstitutionMap subMap,
                               BridgedValueArray arguments, bool isNonThrowing, bool isNonAsync,
                               BridgedGenericSpecializationInformation specInfo) const {
  llvm::SmallVector<swift::SILValue, 16> argValues;
  swift::ApplyOptions applyOpts;
  if (isNonThrowing) { applyOpts |= swift::ApplyFlags::DoesNotThrow; }
  if (isNonAsync) { applyOpts |= swift::ApplyFlags::DoesNotAwait; }

  return {unbridged().createApply(
      regularLoc(), function.getSILValue(), subMap.unbridged(),
      arguments.getValues(argValues), applyOpts, specInfo.data)};
}

BridgedInstruction BridgedBuilder::createTryApply(BridgedValue function, BridgedSubstitutionMap subMap,
                               BridgedValueArray arguments,
                               BridgedBasicBlock normalBB, BridgedBasicBlock errorBB,
                               bool isNonAsync,
                               BridgedGenericSpecializationInformation specInfo) const {
  llvm::SmallVector<swift::SILValue, 16> argValues;
  swift::ApplyOptions applyOpts;
  if (isNonAsync) { applyOpts |= swift::ApplyFlags::DoesNotAwait; }

  return {unbridged().createTryApply(
      regularLoc(), function.getSILValue(), subMap.unbridged(),
      arguments.getValues(argValues), normalBB.unbridged(), errorBB.unbridged(), applyOpts, specInfo.data)};
}

BridgedInstruction BridgedBuilder::createReturn(BridgedValue op) const {
  return {unbridged().createReturn(regularLoc(), op.getSILValue())};
}

BridgedInstruction BridgedBuilder::createThrow(BridgedValue op) const {
  return {unbridged().createThrow(regularLoc(), op.getSILValue())};
}

BridgedInstruction BridgedBuilder::createUncheckedEnumData(BridgedValue enumVal, SwiftInt caseIdx,
                                           BridgedType resultType) const {
  swift::SILValue en = enumVal.getSILValue();
  return {unbridged().createUncheckedEnumData(
      regularLoc(), enumVal.getSILValue(),
      en->getType().getEnumElement(caseIdx), resultType.unbridged())};
}

BridgedInstruction BridgedBuilder::createUncheckedTakeEnumDataAddr(BridgedValue enumAddr, SwiftInt caseIdx) const {
  swift::SILValue en = enumAddr.getSILValue();
  return {unbridged().createUncheckedTakeEnumDataAddr(regularLoc(), en, en->getType().getEnumElement(caseIdx))};
}

BridgedInstruction BridgedBuilder::createEnum(SwiftInt caseIdx, OptionalBridgedValue payload,
                              BridgedType resultType) const {
  swift::EnumElementDecl *caseDecl =
      resultType.unbridged().getEnumElement(caseIdx);
  swift::SILValue pl = payload.getSILValue();
  return {unbridged().createEnum(regularLoc(), pl, caseDecl,
                                 resultType.unbridged())};
}

BridgedInstruction BridgedBuilder::createThinToThickFunction(BridgedValue fn, BridgedType resultType) const {
  return {unbridged().createThinToThickFunction(regularLoc(), fn.getSILValue(),
                                                resultType.unbridged())};
}

BridgedInstruction BridgedBuilder::createPartialApply(BridgedValue funcRef, 
                                                      BridgedValueArray bridgedCapturedArgs,
                                                      BridgedArgumentConvention calleeConvention,
                                                      BridgedSubstitutionMap bridgedSubstitutionMap,
                                                      bool hasUnknownIsolation,
                                                      bool isOnStack) const {
  llvm::SmallVector<swift::SILValue, 8> capturedArgs;                                        
  return {unbridged().createPartialApply(
    regularLoc(), 
    funcRef.getSILValue(), 
    bridgedSubstitutionMap.unbridged(),
    bridgedCapturedArgs.getValues(capturedArgs), 
    getParameterConvention(calleeConvention),
    hasUnknownIsolation ? swift::SILFunctionTypeIsolation::Unknown : swift::SILFunctionTypeIsolation::Erased,
    isOnStack ? swift:: PartialApplyInst::OnStack : swift::PartialApplyInst::NotOnStack
  )};
}                                                                                  

BridgedInstruction BridgedBuilder::createBranch(BridgedBasicBlock destBlock, BridgedValueArray arguments) const {
  llvm::SmallVector<swift::SILValue, 16> argValues;
  return {unbridged().createBranch(regularLoc(), destBlock.unbridged(),
                                   arguments.getValues(argValues))};
}

BridgedInstruction BridgedBuilder::createUnreachable() const {
  return {unbridged().createUnreachable(regularLoc())};
}

BridgedInstruction BridgedBuilder::createObject(BridgedType type,
                                                BridgedValueArray arguments,
                                                SwiftInt numBaseElements) const {
  llvm::SmallVector<swift::SILValue, 16> argValues;
  return {unbridged().createObject(
      swift::ArtificialUnreachableLocation(), type.unbridged(),
      arguments.getValues(argValues), numBaseElements)};
}

BridgedInstruction BridgedBuilder::createVector(BridgedValueArray arguments) const {
  llvm::SmallVector<swift::SILValue, 16> argValues;
  return {unbridged().createVector(swift::ArtificialUnreachableLocation(), arguments.getValues(argValues))};
}

BridgedInstruction BridgedBuilder::createGlobalAddr(BridgedGlobalVar global,
                                                    OptionalBridgedValue dependencyToken) const {
  return {unbridged().createGlobalAddr(regularLoc(), global.getGlobal(), dependencyToken.getSILValue())};
}

BridgedInstruction BridgedBuilder::createGlobalValue(BridgedGlobalVar global, bool isBare) const {
  return {
      unbridged().createGlobalValue(regularLoc(), global.getGlobal(), isBare)};
}

BridgedInstruction BridgedBuilder::createStruct(BridgedType type, BridgedValueArray elements) const {
  llvm::SmallVector<swift::SILValue, 16> elementValues;
  return {unbridged().createStruct(regularLoc(), type.unbridged(),
                                   elements.getValues(elementValues))};
}

BridgedInstruction BridgedBuilder::createStructExtract(BridgedValue str, SwiftInt fieldIndex) const {
  swift::SILValue v = str.getSILValue();
  return {unbridged().createStructExtract(
      regularLoc(), v, v->getType().getFieldDecl(fieldIndex))};
}

BridgedInstruction BridgedBuilder::createStructElementAddr(BridgedValue addr, SwiftInt fieldIndex) const {
  swift::SILValue v = addr.getSILValue();
  return {unbridged().createStructElementAddr(
      regularLoc(), v, v->getType().getFieldDecl(fieldIndex))};
}

BridgedInstruction BridgedBuilder::createDestructureStruct(BridgedValue str) const {
  return {unbridged().createDestructureStruct(regularLoc(), str.getSILValue())};
}

BridgedInstruction BridgedBuilder::createTuple(BridgedType type, BridgedValueArray elements) const {
  llvm::SmallVector<swift::SILValue, 16> elementValues;
  return {unbridged().createTuple(regularLoc(), type.unbridged(),
                                  elements.getValues(elementValues))};
}

BridgedInstruction BridgedBuilder::createTupleExtract(BridgedValue str, SwiftInt elementIndex) const {
  swift::SILValue v = str.getSILValue();
  return {unbridged().createTupleExtract(regularLoc(), v, elementIndex)};
}

BridgedInstruction BridgedBuilder::createTupleElementAddr(BridgedValue addr, SwiftInt elementIndex) const {
  swift::SILValue v = addr.getSILValue();
  return {unbridged().createTupleElementAddr(regularLoc(), v, elementIndex)};
}

BridgedInstruction BridgedBuilder::createDestructureTuple(BridgedValue str) const {
  return {unbridged().createDestructureTuple(regularLoc(), str.getSILValue())};
}

BridgedInstruction BridgedBuilder::createStore(BridgedValue src, BridgedValue dst,
                               SwiftInt ownership) const {
  return {unbridged().createStore(regularLoc(), src.getSILValue(),
                                  dst.getSILValue(),
                                  (swift::StoreOwnershipQualifier)ownership)};
}

BridgedInstruction BridgedBuilder::createInitExistentialRef(BridgedValue instance,
                                            BridgedType type,
                                            BridgedCanType formalConcreteType,
                                            BridgedConformanceArray conformances) const {
  return {unbridged().createInitExistentialRef(
      regularLoc(), type.unbridged(), formalConcreteType.unbridged(),
      instance.getSILValue(), conformances.unbridged())};
}

BridgedInstruction BridgedBuilder::createInitExistentialMetatype(BridgedValue metatype,
                                            BridgedType existentialType,
                                            BridgedConformanceArray conformances) const {
  return {unbridged().createInitExistentialMetatype(
      regularLoc(), metatype.getSILValue(), existentialType.unbridged(),
      conformances.unbridged())};
}

BridgedInstruction BridgedBuilder::createMetatype(BridgedType type,
                                                  BridgedType::MetatypeRepresentation representation) const {
  auto *mt =
      swift::MetatypeType::get(type.unbridged().getASTType(),
                               (swift::MetatypeRepresentation)representation);
  auto t = swift::SILType::getPrimitiveObjectType(swift::CanType(mt));
  return {unbridged().createMetatype(regularLoc(), t)};
}

BridgedInstruction BridgedBuilder::createEndCOWMutation(BridgedValue instance, bool keepUnique) const {
  return {unbridged().createEndCOWMutation(regularLoc(), instance.getSILValue(),
                                           keepUnique)};
}

BridgedInstruction BridgedBuilder::createMarkDependence(BridgedValue value, BridgedValue base, BridgedInstruction::MarkDependenceKind kind) const {
  return {unbridged().createMarkDependence(regularLoc(), value.getSILValue(), base.getSILValue(), swift::MarkDependenceKind(kind))};
}

BridgedInstruction BridgedBuilder::createEndAccess(BridgedValue value) const {
  return {unbridged().createEndAccess(regularLoc(), value.getSILValue(), false)};
}

BridgedInstruction BridgedBuilder::createConvertFunction(BridgedValue originalFunction, BridgedType resultType, bool withoutActuallyEscaping) const {
return {unbridged().createConvertFunction(regularLoc(), originalFunction.getSILValue(), resultType.unbridged(), withoutActuallyEscaping)};
}

BridgedInstruction BridgedBuilder::createConvertEscapeToNoEscape(BridgedValue originalFunction, BridgedType resultType, bool isLifetimeGuaranteed) const {
  return {unbridged().createConvertEscapeToNoEscape(regularLoc(), originalFunction.getSILValue(), resultType.unbridged(), isLifetimeGuaranteed)};
}

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif

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
#include "swift/AST/SourceFile.h"
#include "swift/AST/StorageImpl.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Types.h"
#include "swift/Basic/BasicBridging.h"
#include "swift/Basic/Nullability.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/InstWrappers.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDefaultWitnessTable.h"
#include "swift/SIL/SILFunctionConventions.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILWitnessTable.h"
#include "swift/SILOptimizer/Utils/ConstExpr.h"
#include "swift/SIL/SILConstants.h"
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
      addressableParamIndices(info.getAddressableIndices()),
      conditionallyAddressableParamIndices(
        info.getConditionallyAddressableIndices()),
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

bool BridgedLifetimeDependenceInfo::checkAddressable(SwiftInt index) const {
  return addressableParamIndices && addressableParamIndices->contains(index);
}

bool BridgedLifetimeDependenceInfo::
checkConditionallyAddressable(SwiftInt index) const {
  return conditionallyAddressableParamIndices
    && conditionallyAddressableParamIndices->contains(index);
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
  return BridgedOwnedString(lifetimeDependenceString);
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

static_assert(sizeof(BridgedType::EnumElementIterator) >= sizeof(swift::EnumDecl::ElementRange::iterator));

static inline BridgedType::EnumElementIterator bridge(swift::EnumDecl::ElementRange::iterator i) {
  BridgedType::EnumElementIterator bridgedIter;
  *reinterpret_cast<swift::EnumDecl::ElementRange::iterator *>(&bridgedIter.storage) = i;
  return bridgedIter;
}

static inline swift::EnumDecl::ElementRange::iterator unbridge(BridgedType::EnumElementIterator i) {
  return *reinterpret_cast<const swift::EnumDecl::ElementRange::iterator *>(&i.storage);
}

BridgedType::EnumElementIterator BridgedType::EnumElementIterator::getNext() const {
  return bridge(std::next(unbridge(*this)));
}

BridgedType BridgedType::createSILType(BridgedCanType canTy) {
  auto ty = canTy.unbridged();
  if (ty->isLegalSILType())
    return swift::SILType::getPrimitiveObjectType(ty);
  return swift::SILType();
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

bool BridgedType::isTrivial(BridgedFunction f) const {
  return unbridged().isTrivial(f.getFunction());
}

bool BridgedType::isNonTrivialOrContainsRawPointer(BridgedFunction f) const {
  return unbridged().isNonTrivialOrContainsRawPointer(f.getFunction());
}

bool BridgedType::isLoadable(BridgedFunction f) const {
  return unbridged().isLoadable(f.getFunction());
}

bool BridgedType::isReferenceCounted(BridgedFunction f) const {
  return unbridged().isReferenceCounted(f.getFunction());
}

bool BridgedType::containsNoEscapeFunction() const {
  return unbridged().containsNoEscapeFunction();
}

bool BridgedType::isEmpty(BridgedFunction f) const {
  return unbridged().isEmpty(*f.getFunction());
}

bool BridgedType::isMoveOnly() const {
  return unbridged().isMoveOnly();
}

bool BridgedType::isEscapable(BridgedFunction f) const {
  return unbridged().isEscapable(*f.getFunction());
}

bool BridgedType::isExactSuperclassOf(BridgedType t) const {
  return unbridged().isExactSuperclassOf(t.unbridged());
}

bool BridgedType::isMarkedAsImmortal() const {
  return unbridged().isMarkedAsImmortal();
}

bool BridgedType::isAddressableForDeps(BridgedFunction f) const {
  return unbridged().isAddressableForDeps(*f.getFunction());
}

SwiftInt BridgedType::getCaseIdxOfEnumType(BridgedStringRef name) const {
  return unbridged().getCaseIdxOfEnumType(name.unbridged());
}

SwiftInt BridgedType::getNumBoxFields(BridgedCanType boxTy) {
  return boxTy.unbridged()->castTo<swift::SILBoxType>()->getLayout()->getFields().size();
}

BridgedType BridgedType::getBoxFieldType(BridgedCanType boxTy, SwiftInt idx, BridgedFunction f) {
  auto *fn = f.getFunction();
  return swift::getSILBoxFieldType(fn->getTypeExpansionContext(), boxTy.unbridged()->castTo<swift::SILBoxType>(),
                                   fn->getModule().Types, idx);
}

bool BridgedType::getBoxFieldIsMutable(BridgedCanType boxTy, SwiftInt idx) {
  return boxTy.unbridged()->castTo<swift::SILBoxType>()->getLayout()->getFields()[idx].isMutable();
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
  return bridge(enumDecl->getAllElements().begin());
}

bool BridgedType::isEndCaseIterator(EnumElementIterator i) const {
  swift::EnumDecl *enumDecl = unbridged().getEnumOrBoundGenericEnum();
  return unbridge(i) == enumDecl->getAllElements().end();
}

BridgedType BridgedType::getEnumCasePayload(EnumElementIterator i, BridgedFunction f) const {
  swift::EnumElementDecl *elt = *unbridge(i);
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

//===----------------------------------------------------------------------===//
//                                BridgedValue
//===----------------------------------------------------------------------===//

static inline BridgedValue::Ownership bridge(swift::OwnershipKind ownership) {
  switch (ownership) {
    case swift::OwnershipKind::Any:
      llvm_unreachable("Invalid ownership for value");
    case swift::OwnershipKind::Unowned:    return BridgedValue::Ownership::Unowned;
    case swift::OwnershipKind::Owned:      return BridgedValue::Ownership::Owned;
    case swift::OwnershipKind::Guaranteed: return BridgedValue::Ownership::Guaranteed;
    case swift::OwnershipKind::None:       return BridgedValue::Ownership::None;
  }
}

swift::ValueOwnershipKind BridgedValue::unbridge(Ownership ownership) {
  switch (ownership) {
    case BridgedValue::Ownership::Unowned:    return swift::OwnershipKind::Unowned;
    case BridgedValue::Ownership::Owned:      return swift::OwnershipKind::Owned;
    case BridgedValue::Ownership::Guaranteed: return swift::OwnershipKind::Guaranteed;
    case BridgedValue::Ownership::None:       return swift::OwnershipKind::None;
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
  return bridge(getSILValue()->getOwnershipKind());
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
  return op->canAcceptKind(BridgedValue::unbridge(ownership));
}

bool BridgedOperand::isDeleted() const {
  return op->getUser() == nullptr;
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
  case swift::OperandOwnership::AnyInteriorPointer:
    return OperandOwnership::AnyInteriorPointer;
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
void BridgedArgument::setReborrow(bool reborrow) const {
  getArgument()->setReborrow(reborrow);
}

bool BridgedArgument::FunctionArgument_isLexical() const {
  return llvm::cast<swift::SILFunctionArgument>(getArgument())->getLifetime().isLexical();
}

bool BridgedArgument::FunctionArgument_isClosureCapture() const {
  return llvm::cast<swift::SILFunctionArgument>(
    getArgument())->isClosureCapture();
}

OptionalBridgedDeclObj BridgedArgument::getDecl() const {
  return {getArgument()->getDecl()};
}

void BridgedArgument::copyFlags(BridgedArgument fromArgument) const {
  auto *fArg = static_cast<swift::SILFunctionArgument *>(getArgument());
  fArg->copyFlags(static_cast<swift::SILFunctionArgument *>(fromArgument.getArgument()));
}

swift::SILArgument * _Nullable OptionalBridgedArgument::unbridged() const {
  if (!obj)
    return nullptr;
  return static_cast<swift::SILArgument *>(obj);
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
BridgedLocation BridgedLocation::getCleanupLocation() const {
  return getLoc().getCleanupLocation();
}
BridgedLocation BridgedLocation::withScopeOf(BridgedLocation other) const {
  return swift::SILDebugLocation(getLoc().getLocation(), other.getLoc().getScope());
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
bool BridgedLocation::isFilenameAndLocation() const {
  return getLoc().getLocation().isFilenameAndLocation();
}
BridgedLocation::FilenameAndLocation BridgedLocation::getFilenameAndLocation() const {
  auto fnal = getLoc().getLocation().getFilenameAndLocation();
  return {BridgedStringRef(fnal->filename), (SwiftInt)fnal->line, (SwiftInt)fnal->column};
}
bool BridgedLocation::hasSameSourceLocation(BridgedLocation rhs) const {
  return getLoc().hasSameSourceLocation(rhs.getLoc());
}
OptionalBridgedDeclObj BridgedLocation::getDecl() const {
  return {getLoc().getLocation().getAsASTNode<swift::Decl>()};
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

bool BridgedFunction::isAccessor() const {
  if (auto *valDecl = getFunction()->getDeclRef().getDecl()) {
    return llvm::isa<swift::AccessorDecl>(valDecl);
  }
  return false;
}

BridgedStringRef BridgedFunction::getAccessorName() const {
  auto *accessorDecl  = llvm::cast<swift::AccessorDecl>(getFunction()->getDeclRef().getDecl());
  return accessorKindName(accessorDecl->getAccessorKind());
}

bool BridgedFunction::hasOwnership() const { return getFunction()->hasOwnership(); }

bool BridgedFunction::hasLoweredAddresses() const { return getFunction()->getModule().useLoweredAddresses(); }

BridgedCanType BridgedFunction::getLoweredFunctionTypeInContext() const {
  auto expansion = getFunction()->getTypeExpansionContext();
  return getFunction()->getLoweredFunctionTypeInContext(expansion);
}

BridgedGenericSignature BridgedFunction::getGenericSignature() const {
  return {getFunction()->getGenericSignature().getPointer()};
}

BridgedSubstitutionMap BridgedFunction::getForwardingSubstitutionMap() const {
  return {getFunction()->getForwardingSubstitutionMap()};
}

BridgedASTType BridgedFunction::mapTypeIntoContext(BridgedASTType ty) const {
  return {getFunction()->mapTypeIntoContext(ty.unbridged()).getPointer()};
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

BridgedDeclRef BridgedFunction::getDeclRef() const {
  return getFunction()->getDeclRef();
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

bool BridgedFunction::shouldOptimize() const {
  return getFunction()->shouldOptimize();
}

bool BridgedFunction::isReferencedInModule() const {
  return getFunction()->getRefCount() != 0;
}

bool BridgedFunction::wasDeserializedCanonical() const {
  return getFunction()->wasDeserializedCanonical();
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

bool BridgedFunction::conformanceMatchesActorIsolation(BridgedConformance conformance) const {
  return swift::matchesActorIsolation(conformance.unbridged(), getFunction());
}

bool BridgedFunction::isSpecialization() const {
  return getFunction()->isSpecialization();
}

bool BridgedFunction::isResilientNominalDecl(BridgedDeclObj decl) const {
  return decl.getAs<swift::NominalTypeDecl>()->isResilient(getFunction()->getModule().getSwiftModule(),
                                                           getFunction()->getResilienceExpansion());
}

BridgedType BridgedFunction::getLoweredType(BridgedASTType type, bool maximallyAbstracted) const {
  if (maximallyAbstracted) {
    return BridgedType(getFunction()->getLoweredType(swift::Lowering::AbstractionPattern::getOpaque(), type.type));
  }
  return BridgedType(getFunction()->getLoweredType(type.type));
}

BridgedType BridgedFunction::getLoweredType(BridgedType type) const {
  return BridgedType(getFunction()->getLoweredType(type.unbridged()));
}

swift::SILFunction * _Nullable OptionalBridgedFunction::getFunction() const {
  return static_cast<swift::SILFunction *>(obj);
}

BridgedFunction::OptionalSourceFileKind BridgedFunction::getSourceFileKind() const {
  if (auto *dc = getFunction()->getDeclContext()) {
    if (auto *sourceFile = dc->getParentSourceFile())
      return (OptionalSourceFileKind)sourceFile->Kind;
  }
  return OptionalSourceFileKind::None;
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

BridgedType BridgedGlobalVar::getType() const {
  return getGlobal()->getLoweredType();
}

BridgedLinkage BridgedGlobalVar::getLinkage() const {
  return (BridgedLinkage)getGlobal()->getLinkage();
}

BridgedSourceLoc BridgedGlobalVar::getSourceLocation() const {
  if (getGlobal()->hasLocation())
    return getGlobal()->getLocation().getSourceLoc();
  else
    return BridgedSourceLoc();
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
//                              BridgedSILTypeArray
//===----------------------------------------------------------------------===//

BridgedType BridgedSILTypeArray::getAt(SwiftInt index) const {
  return typeArray.unbridged<swift::SILType>()[index];
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
  return bridge(getAsForwardingInstruction()->getForwardingOwnershipKind());
}

void BridgedInstruction::ForwardingInst_setForwardingOwnership(BridgedValue::Ownership ownership) const {
  return getAsForwardingInstruction()->setForwardingOwnershipKind(BridgedValue::unbridge(ownership));
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

bool BridgedInstruction::LoadBorrowInst_isUnchecked() const {
  return (SwiftInt)getAs<swift::LoadBorrowInst>()->isUnchecked();
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

BridgedStringRef BridgedInstruction::BuiltinInst_getName() const {
  return getAs<swift::BuiltinInst>()->getName().str();
}

BridgedSubstitutionMap BridgedInstruction::BuiltinInst_getSubstitutionMap() const {
  return getAs<swift::BuiltinInst>()->getSubstitutions();
}

bool BridgedInstruction::PointerToAddressInst_isStrict() const {
  return getAs<swift::PointerToAddressInst>()->isStrict();
}

bool BridgedInstruction::PointerToAddressInst_isInvariant() const {
  return getAs<swift::PointerToAddressInst>()->isInvariant();
}

uint64_t BridgedInstruction::PointerToAddressInst_getAlignment() const {
  auto maybeAlign = getAs<swift::PointerToAddressInst>()->alignment();
  if (maybeAlign.has_value()) {
    assert(maybeAlign->value() != 0);
    return maybeAlign->value();
  }
  return 0;
}

void BridgedInstruction::PointerToAddressInst_setAlignment(uint64_t alignment) const {
  getAs<swift::PointerToAddressInst>()->setAlignment(llvm::MaybeAlign(alignment));
}

bool BridgedInstruction::AddressToPointerInst_needsStackProtection() const {
  return getAs<swift::AddressToPointerInst>()->needsStackProtection();
}

bool BridgedInstruction::IndexAddrInst_needsStackProtection() const {
  return getAs<swift::IndexAddrInst>()->needsStackProtection();
}

BridgedConformanceArray BridgedInstruction::InitExistentialRefInst_getConformances() const {
  return {getAs<swift::InitExistentialRefInst>()->getConformances()};
}

BridgedCanType BridgedInstruction::InitExistentialRefInst_getFormalConcreteType() const {
  return getAs<swift::InitExistentialRefInst>()->getFormalConcreteType();
}

bool BridgedInstruction::OpenExistentialAddr_isImmutable() const {
  switch (getAs<swift::OpenExistentialAddrInst>()->getAccessKind()) {
    case swift::OpenedExistentialAccess::Immutable: return true;
    case swift::OpenedExistentialAccess::Mutable: return false;
  }
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

BridgedOptionalInt BridgedInstruction::IntegerLiteralInst_getValue() const {
  llvm::APInt result = getAs<swift::IntegerLiteralInst>()->getValue();
  return BridgedOptionalInt::getFromAPInt(result);
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

bool BridgedInstruction::TryApplyInst_getNonAsync() const {
  return getAs<swift::TryApplyInst>()->isNonAsync();  
}

BridgedGenericSpecializationInformation BridgedInstruction::TryApplyInst_getSpecializationInfo() const {
  return {getAs<swift::TryApplyInst>()->getSpecializationInfo()};
}

BridgedDeclRef BridgedInstruction::ClassMethodInst_getMember() const {
  return getAs<swift::ClassMethodInst>()->getMember();
}

BridgedDeclRef BridgedInstruction::WitnessMethodInst_getMember() const {
  return getAs<swift::WitnessMethodInst>()->getMember();
}

BridgedCanType BridgedInstruction::WitnessMethodInst_getLookupType() const {
  return getAs<swift::WitnessMethodInst>()->getLookupType();  
}

BridgedDeclObj BridgedInstruction::WitnessMethodInst_getLookupProtocol() const {
  return getAs<swift::WitnessMethodInst>()->getLookupProtocol();
}

BridgedConformance BridgedInstruction::WitnessMethodInst_getConformance() const {
  return getAs<swift::WitnessMethodInst>()->getConformance();
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
  return getAs<swift::PartialApplyInst>()->getResultIsolation() ==
         swift::SILFunctionTypeIsolation::forUnknown();
}

bool BridgedInstruction::AllocStackInst_hasDynamicLifetime() const {
  return getAs<swift::AllocStackInst>()->hasDynamicLifetime();
}

bool BridgedInstruction::AllocStackInst_isFromVarDecl() const {
  return getAs<swift::AllocStackInst>()->isFromVarDecl();
}

bool BridgedInstruction::AllocStackInst_usesMoveableValueDebugInfo() const {
  return getAs<swift::AllocStackInst>()->usesMoveableValueDebugInfo();
}

bool BridgedInstruction::AllocStackInst_isLexical() const {
  return getAs<swift::AllocStackInst>()->isLexical();
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
  return {getAs<const swift::AllocRefInstBase>()->getTailAllocatedTypes()};
}

bool BridgedInstruction::AllocRefDynamicInst_isDynamicTypeDeinitAndSizeKnownEquivalentToBaseType() const {
  return getAs<swift::AllocRefDynamicInst>()->isDynamicTypeDeinitAndSizeKnownEquivalentToBaseType();
}

SwiftInt BridgedInstruction::BeginApplyInst_numArguments() const {
  return getAs<swift::BeginApplyInst>()->getNumArguments();
}

bool BridgedInstruction::BeginApplyInst_isCalleeAllocated() const {
  return getAs<swift::BeginApplyInst>()->isCalleeAllocated();
}

SwiftInt BridgedInstruction::TryApplyInst_numArguments() const {
  return getAs<swift::TryApplyInst>()->getNumArguments();
}

BridgedArgumentConvention BridgedInstruction::YieldInst_getConvention(BridgedOperand forOperand) const {
  return castToArgumentConvention(getAs<swift::YieldInst>()->getArgumentConventionForOperand(*forOperand.op));
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

void BridgedInstruction::MarkDependenceInstruction_resolveToNonEscaping() const {
  if (auto *mdi = llvm::dyn_cast<swift::MarkDependenceInst>(unbridged())) {
    mdi->resolveToNonEscaping();
  } else {
    getAs<swift::MarkDependenceAddrInst>()->resolveToNonEscaping();
  }
}

void BridgedInstruction::MarkDependenceInstruction_settleToEscaping() const {
  if (auto *mdi = llvm::dyn_cast<swift::MarkDependenceInst>(unbridged())) {
    mdi->settleToEscaping();
  } else {
    getAs<swift::MarkDependenceAddrInst>()->settleToEscaping();
  }
}

BridgedInstruction::MarkDependenceKind BridgedInstruction::MarkDependenceAddrInst_dependenceKind() const {
  return (MarkDependenceKind)getAs<swift::MarkDependenceAddrInst>()->dependenceKind();
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

void BridgedInstruction::BeginAccess_setAccessKind(SwiftInt accessKind) const {
  getAs<swift::BeginAccessInst>()->setAccessKind((swift::SILAccessKind)accessKind);
}

bool BridgedInstruction::CopyAddrInst_isTakeOfSrc() const {
  return getAs<swift::CopyAddrInst>()->isTakeOfSrc();
}

bool BridgedInstruction::CopyAddrInst_isInitializationOfDest() const {
  return getAs<swift::CopyAddrInst>()->isInitializationOfDest();
}

void BridgedInstruction::CopyAddrInst_setIsTakeOfSrc(bool isTakeOfSrc) const {
  return getAs<swift::CopyAddrInst>()->setIsTakeOfSrc(isTakeOfSrc ? swift::IsTake : swift::IsNotTake);
}

void BridgedInstruction::CopyAddrInst_setIsInitializationOfDest(bool isInitializationOfDest) const {
  return getAs<swift::CopyAddrInst>()->setIsInitializationOfDest(
      isInitializationOfDest ? swift::IsInitialization : swift::IsNotInitialization);
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

void BridgedInstruction::CheckedCastBranch_updateSourceFormalTypeFromOperandLoweredType() const {
  getAs<swift::CheckedCastBranchInst>()->updateSourceFormalTypeFromOperandLoweredType();
}

BridgedCanType BridgedInstruction::UnconditionalCheckedCast_getSourceFormalType() const {
  return {getAs<swift::UnconditionalCheckedCastInst>()->getSourceFormalType()};  
}

BridgedCanType BridgedInstruction::UnconditionalCheckedCast_getTargetFormalType() const {
  return {getAs<swift::UnconditionalCheckedCastInst>()->getTargetFormalType()};    
}

BridgedInstruction::CheckedCastInstOptions
BridgedInstruction::UnconditionalCheckedCast_getCheckedCastOptions() const {
  return BridgedInstruction::CheckedCastInstOptions{
      getAs<swift::UnconditionalCheckedCastInst>()->getCheckedCastOptions()
        .getStorage()};
}

BridgedCanType BridgedInstruction::UnconditionalCheckedCastAddr_getSourceFormalType() const {
  return {getAs<swift::UnconditionalCheckedCastAddrInst>()->getSourceFormalType()};  
}

BridgedCanType BridgedInstruction::UnconditionalCheckedCastAddr_getTargetFormalType() const {
  return {getAs<swift::UnconditionalCheckedCastAddrInst>()->getTargetFormalType()};    
}

BridgedInstruction::CheckedCastInstOptions
BridgedInstruction::UnconditionalCheckedCastAddr_getCheckedCastOptions() const {
  return BridgedInstruction::CheckedCastInstOptions{
      getAs<swift::UnconditionalCheckedCastAddrInst>()->getCheckedCastOptions()
        .getStorage()};
}

BridgedBasicBlock BridgedInstruction::CheckedCastBranch_getSuccessBlock() const {
  return {getAs<swift::CheckedCastBranchInst>()->getSuccessBB()};
}

BridgedBasicBlock BridgedInstruction::CheckedCastBranch_getFailureBlock() const {
  return {getAs<swift::CheckedCastBranchInst>()->getFailureBB()};
}

BridgedInstruction::CheckedCastInstOptions
BridgedInstruction::CheckedCastBranch_getCheckedCastOptions() const {
  return BridgedInstruction::CheckedCastInstOptions{
      getAs<swift::CheckedCastBranchInst>()->getCheckedCastOptions()
        .getStorage()};
}

BridgedCanType BridgedInstruction::CheckedCastAddrBranch_getSourceFormalType() const {
  return {getAs<swift::CheckedCastAddrBranchInst>()->getSourceFormalType()};
}

BridgedCanType BridgedInstruction::CheckedCastAddrBranch_getTargetFormalType() const {
  return {getAs<swift::CheckedCastAddrBranchInst>()->getTargetFormalType()};  
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

BridgedInstruction::CheckedCastInstOptions
BridgedInstruction::CheckedCastAddrBranch_getCheckedCastOptions() const {
  return BridgedInstruction::CheckedCastInstOptions{
      getAs<swift::CheckedCastAddrBranchInst>()->getCheckedCastOptions()
        .getStorage()};
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

//===----------------------------------------------------------------------===//
//                     VarDeclInst and DebugVariableInst
//===----------------------------------------------------------------------===//

static_assert(sizeof(swift::SILDebugVariable) <= sizeof(BridgedSILDebugVariable));

BridgedSILDebugVariable::BridgedSILDebugVariable(const swift::SILDebugVariable &var) {
  new (&storage) swift::SILDebugVariable(var);
}

BridgedSILDebugVariable::BridgedSILDebugVariable(const BridgedSILDebugVariable &rhs) {
  new (&storage) swift::SILDebugVariable(rhs.unbridge());
}

BridgedSILDebugVariable::~BridgedSILDebugVariable() {
  reinterpret_cast<swift::SILDebugVariable *>(&storage)->~SILDebugVariable();
}

BridgedSILDebugVariable &BridgedSILDebugVariable::operator=(const BridgedSILDebugVariable &rhs) {
  *reinterpret_cast<swift::SILDebugVariable *>(&storage) = rhs.unbridge();
  return *this;
}

swift::SILDebugVariable BridgedSILDebugVariable::unbridge() const {
  return *reinterpret_cast<const swift::SILDebugVariable *>(&storage);
}

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

bool BridgedInstruction::DebugValue_hasVarInfo() const {
  return getAs<swift::DebugValueInst>()->getVarInfo().has_value();
}
BridgedSILDebugVariable BridgedInstruction::DebugValue_getVarInfo() const {
  return BridgedSILDebugVariable(getAs<swift::DebugValueInst>()->getVarInfo().value());
}

bool BridgedInstruction::AllocStack_hasVarInfo() const {
  return getAs<swift::AllocStackInst>()->getVarInfo().has_value();
}
BridgedSILDebugVariable BridgedInstruction::AllocStack_getVarInfo() const {
  return BridgedSILDebugVariable(getAs<swift::AllocStackInst>()->getVarInfo().value());
}

bool BridgedInstruction::AllocBox_hasVarInfo() const {
  return getAs<swift::AllocBoxInst>()->getVarInfo().has_value();
}
BridgedSILDebugVariable BridgedInstruction::AllocBox_getVarInfo() const {
  return BridgedSILDebugVariable(getAs<swift::AllocBoxInst>()->getVarInfo().value());
}

//===----------------------------------------------------------------------===//
//                       OptionalBridgedInstruction
//===----------------------------------------------------------------------===//

swift::SILInstruction * _Nullable OptionalBridgedInstruction::unbridged() const {
  if (!obj)
    return nullptr;
  return llvm::cast<swift::SILInstruction>(static_cast<swift::SILNode *>(obj)->castToInstruction());
}

//===----------------------------------------------------------------------===//
//                                BridgedBasicBlock
//===----------------------------------------------------------------------===//

BridgedBasicBlock::BridgedBasicBlock(swift::SILBasicBlock * _Nonnull block)
  : obj(block) {}

swift::SILBasicBlock * _Nonnull BridgedBasicBlock::unbridged() const {
  return static_cast<swift::SILBasicBlock *>(obj);
}

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
      type.unbridged(), BridgedValue::unbridge(ownership))};
}

BridgedArgument BridgedBasicBlock::addFunctionArgument(BridgedType type) const {
  return {unbridged()->createFunctionArgument(type.unbridged())};
}

BridgedArgument BridgedBasicBlock::insertFunctionArgument(SwiftInt atPosition, BridgedType type,
                                                          BridgedValue::Ownership ownership,
                                                          OptionalBridgedDeclObj decl) const {
  return {unbridged()->insertFunctionArgument((unsigned)atPosition, type.unbridged(),
                                              BridgedValue::unbridge(ownership),
                                              decl.getAs<swift::ValueDecl>())};
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

swift::SILBasicBlock * _Nullable OptionalBridgedBasicBlock::unbridged() const {
  return obj ? static_cast<swift::SILBasicBlock *>(obj) : nullptr;
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

BridgedDeclRef::BridgedDeclRef(swift::SILDeclRef declRef) {
  *reinterpret_cast<swift::SILDeclRef *>(&storage) = declRef;
}

swift::SILDeclRef BridgedDeclRef::unbridged() const {
  return *reinterpret_cast<const swift::SILDeclRef *>(&storage);
}

bool BridgedDeclRef::isEqualTo(BridgedDeclRef rhs) const {
  return unbridged() == rhs.unbridged();
}

BridgedLocation BridgedDeclRef::getLocation() const {
  return swift::SILDebugLocation(unbridged().getDecl(), nullptr);
}

BridgedDeclObj BridgedDeclRef::getDecl() const {
  return {unbridged().getDecl()};
}

BridgedDiagnosticArgument BridgedDeclRef::asDiagnosticArgument() const {
  return swift::DiagnosticArgument(unbridged().getDecl()->getName());
}

//===----------------------------------------------------------------------===//
//                                BridgedVTable
//===----------------------------------------------------------------------===//

BridgedVTableEntry::BridgedVTableEntry(const swift::SILVTableEntry &entry) {
  *reinterpret_cast<swift::SILVTableEntry *>(&storage) = entry;
}

const swift::SILVTableEntry &BridgedVTableEntry::unbridged() const {
  return *reinterpret_cast<const swift::SILVTableEntry *>(&storage);
}

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

OptionalBridgedVTableEntry BridgedVTable::lookupMethod(BridgedDeclRef member) const {
  if (vTable->getEntries().empty()) {
    return OptionalBridgedVTableEntry();
  }
  swift::SILModule &mod = vTable->getEntries()[0].getImplementation()->getModule();
  if (auto entry = vTable->getEntry(mod, member.unbridged()))
    return BridgedVTableEntry(entry.value());

  return OptionalBridgedVTableEntry();
}


BridgedType BridgedVTable::getSpecializedClassType() const {
  return {vTable->getClassType()};
}

void BridgedVTable::replaceEntries(BridgedArrayRef bridgedEntries) const {
  llvm::SmallVector<swift::SILVTableEntry, 8> entries;
  for (const BridgedVTableEntry &e : bridgedEntries.unbridged<BridgedVTableEntry>()) {
    entries.push_back(e.unbridged());
  }
  vTable->replaceEntries(entries);
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
  return bridge(swift::SILWitnessTable::Entry());
}

BridgedWitnessTableEntry BridgedWitnessTableEntry::createMethod(BridgedDeclRef requirement,
                                                                OptionalBridgedFunction witness) {
  return bridge(swift::SILWitnessTable::Entry(
    swift::SILWitnessTable::MethodWitness{requirement.unbridged(), witness.getFunction()}));
}

BridgedWitnessTableEntry BridgedWitnessTableEntry::createAssociatedType(BridgedDeclObj requirement,
                                                                        BridgedCanType witness) {
  return bridge(swift::SILWitnessTable::Entry(
    swift::SILWitnessTable::AssociatedTypeWitness{requirement.getAs<swift::AssociatedTypeDecl>(),
                                                  witness.unbridged()}));
}

BridgedWitnessTableEntry BridgedWitnessTableEntry::createAssociatedConformance(BridgedCanType requirement,
                                                                               BridgedConformance witness) {
  return bridge(swift::SILWitnessTable::Entry(
    swift::SILWitnessTable::AssociatedConformanceWitness{requirement.unbridged(),
                                                         witness.unbridged()}));
}

BridgedWitnessTableEntry BridgedWitnessTableEntry::createBaseProtocol(BridgedDeclObj requirement,
                                                                      BridgedConformance witness) {
  return bridge(swift::SILWitnessTable::Entry(
    swift::SILWitnessTable::BaseProtocolWitness{requirement.getAs<swift::ProtocolDecl>(),
                                                witness.unbridged().getConcrete()}));
}

SwiftInt BridgedWitnessTable::getNumEntries() const {
  return SwiftInt(table->getEntries().size());
}

BridgedWitnessTableEntry BridgedWitnessTable::getEntry(SwiftInt index) const {
  return BridgedWitnessTableEntry::bridge(table->getEntries()[index]);
}

bool BridgedWitnessTable::isDeclaration() const {
  return table->isDeclaration();
}

bool BridgedWitnessTable::isSpecialized() const {
  return table->isSpecialized();
}

SwiftInt BridgedDefaultWitnessTable::getNumEntries() const {
  return SwiftInt(table->getEntries().size());
}

BridgedWitnessTableEntry BridgedDefaultWitnessTable::getEntry(SwiftInt index) const {
  return BridgedWitnessTableEntry::bridge(table->getEntries()[index]);
}

//===----------------------------------------------------------------------===//
//                         ConstExprFunctionState
//===----------------------------------------------------------------------===//
BridgedConstExprFunctionState BridgedConstExprFunctionState::create() {
  auto allocator = new swift::SymbolicValueBumpAllocator();
  auto evaluator = new swift::ConstExprEvaluator(*allocator, 0);
  auto numEvaluatedSILInstructions = new unsigned int(0);
  auto state = new swift::ConstExprFunctionState(*evaluator, nullptr, {},
                                                 *numEvaluatedSILInstructions, true);
  return {state, allocator, evaluator, numEvaluatedSILInstructions};
}

bool BridgedConstExprFunctionState::isConstantValue(BridgedValue bridgedValue) {
  auto value = bridgedValue.getSILValue();
  auto symbolicValue = state->getConstantValue(value);
  return symbolicValue.isConstant();
}

void BridgedConstExprFunctionState::deinitialize() {
  delete state;
  delete numEvaluatedSILInstructions;
  delete constantEvaluator;
  delete allocator;
}


//===----------------------------------------------------------------------===//
//                                BridgedBuilder
//===----------------------------------------------------------------------===//

swift::SILBuilder BridgedBuilder::unbridged() const {
  switch (insertAt) {
  case BridgedBuilder::InsertAt::beforeInst:
    return swift::SILBuilder(BridgedInstruction(insertionObj).unbridged(),
                             loc.getLoc().getScope());
  case BridgedBuilder::InsertAt::endOfBlock:
    return swift::SILBuilder(BridgedBasicBlock(insertionObj).unbridged(),
                             loc.getLoc().getScope());
  case BridgedBuilder::InsertAt::startOfFunction:
    return swift::SILBuilder(BridgedFunction(insertionObj).getFunction()->getEntryBlock(),
                             loc.getLoc().getScope());
  case BridgedBuilder::InsertAt::intoGlobal:
    return swift::SILBuilder(BridgedGlobalVar(insertionObj).getGlobal());
  }
}

swift::SILLocation BridgedBuilder::regularLoc() const {
  auto l = loc.getLoc().getLocation();
  switch (l.getKind()) {
    case swift::SILLocation::ReturnKind:
    case swift::SILLocation::ImplicitReturnKind:
    case swift::SILLocation::ArtificialUnreachableKind:
      return swift::RegularLocation(l);
    default:
      return l;
  }
}

swift::SILLocation BridgedBuilder::returnLoc() const {
  auto l = loc.getLoc().getLocation();
  switch (l.getKind()) {
    case swift::SILLocation::ArtificialUnreachableKind:
      return swift::RegularLocation(l);
    default:
      return l;
  }
}

BridgedInstruction BridgedBuilder::createBuiltin(BridgedStringRef name, BridgedType type,
                                                 BridgedSubstitutionMap subs,
                                                 BridgedValueArray arguments) const {
  llvm::SmallVector<swift::SILValue, 16> argValues;
  auto builder = unbridged();
  auto ident = builder.getASTContext().getIdentifier(name.unbridged());
  return {builder.createBuiltin(
      regularLoc(), ident, type.unbridged(),
      subs.unbridged(), arguments.getValues(argValues))};
}

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
      elementTypes.typeArray.unbridged<swift::SILType>(),
      elementCountOperands.getValues(elementCountOperandsValues)
      )};
}

BridgedInstruction BridgedBuilder::createAllocStack(BridgedType type,
                                                    OptionalBridgedSILDebugVariable debugVar,
                                                    bool hasDynamicLifetime,
                                                    bool isLexical,
                                                    bool isFromVarDecl,
                                                    bool wasMoved) const {
  std::optional<swift::SILDebugVariable> var = std::nullopt;
  if (debugVar.hasDebugVar)
    var = debugVar.debugVar.unbridge();
  return {unbridged().createAllocStack(
      regularLoc(), type.unbridged(), var,
      swift::HasDynamicLifetime_t(hasDynamicLifetime),
      swift::IsLexical_t(isLexical), swift::IsFromVarDecl_t(isFromVarDecl),
      swift::UsesMoveableValueDebugInfo_t(wasMoved), /*skipVarDeclAssert=*/ true)};
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

BridgedInstruction BridgedBuilder::createPointerToAddress(BridgedValue pointer, BridgedType addressTy,
                                                          bool isStrict, bool isInvariant,
                                                          uint64_t alignment) const {
  return {unbridged().createPointerToAddress(regularLoc(), pointer.getSILValue(), addressTy.unbridged(),
                                             isStrict, isInvariant,
                                             alignment == 0 ? llvm::MaybeAlign() : llvm::Align(alignment))};
}

BridgedInstruction BridgedBuilder::createIndexAddr(BridgedValue base, BridgedValue index,
                                                   bool needsStackProtection) const {
  return {unbridged().createIndexAddr(regularLoc(), base.getSILValue(), index.getSILValue(),
                                      needsStackProtection)};
}

BridgedInstruction BridgedBuilder::createUncheckedRefCast(BridgedValue op, BridgedType type) const {
  return {unbridged().createUncheckedRefCast(regularLoc(), op.getSILValue(),
                                             type.unbridged())};
}

BridgedInstruction BridgedBuilder::createUncheckedAddrCast(BridgedValue op, BridgedType type) const {
  return {unbridged().createUncheckedAddrCast(regularLoc(), op.getSILValue(),
                                              type.unbridged())};
}

BridgedInstruction BridgedBuilder::createUpcast(BridgedValue op, BridgedType type) const {
  return {unbridged().createUpcast(regularLoc(), op.getSILValue(),
                                   type.unbridged())};
}

BridgedInstruction BridgedBuilder::createCheckedCastAddrBranch(
    BridgedValue source, BridgedCanType sourceFormalType,
    BridgedValue destination, BridgedCanType targetFormalType,
    BridgedInstruction::CheckedCastInstOptions options,
    BridgedInstruction::CastConsumptionKind consumptionKind,
    BridgedBasicBlock successBlock, BridgedBasicBlock failureBlock) const
{
  return {unbridged().createCheckedCastAddrBranch(
            regularLoc(),
            swift::CheckedCastInstOptions(options.storage),
            (swift::CastConsumptionKind)consumptionKind,
            source.getSILValue(), sourceFormalType.unbridged(),
            destination.getSILValue(), targetFormalType.unbridged(),
            successBlock.unbridged(), failureBlock.unbridged())};
}

BridgedInstruction BridgedBuilder::createUnconditionalCheckedCastAddr(
    BridgedInstruction::CheckedCastInstOptions options,
    BridgedValue source, BridgedCanType sourceFormalType,
    BridgedValue destination, BridgedCanType targetFormalType) const
{
  return {unbridged().createUnconditionalCheckedCastAddr(
            regularLoc(),
            swift::CheckedCastInstOptions(options.storage),
            source.getSILValue(), sourceFormalType.unbridged(),
            destination.getSILValue(), targetFormalType.unbridged())};
}

BridgedInstruction BridgedBuilder::createLoad(BridgedValue op, SwiftInt ownership) const {
  return {unbridged().createLoad(regularLoc(), op.getSILValue(),
                                 (swift::LoadOwnershipQualifier)ownership)};
}


BridgedInstruction BridgedBuilder::createUncheckedOwnershipConversion(BridgedValue op,
                                                                      BridgedValue::Ownership ownership) const {
  return {unbridged().createUncheckedOwnershipConversion(regularLoc(), op.getSILValue(),
                                                         BridgedValue::unbridge(ownership))};
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

BridgedInstruction BridgedBuilder::createBeginBorrow(BridgedValue op,
                                                     bool isLexical,
                                                     bool hasPointerEscape,
                                                     bool isFromVarDecl) const {
  return {unbridged().createBeginBorrow(regularLoc(), op.getSILValue(),
                                        swift::IsLexical_t(isLexical),
                                        swift::HasPointerEscape_t(hasPointerEscape),
                                        swift::IsFromVarDecl_t(isFromVarDecl))};
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

BridgedInstruction BridgedBuilder::createDebugValue(BridgedValue op,
                                                    BridgedSILDebugVariable var) const {
  return {unbridged().createDebugValue(regularLoc(), op.getSILValue(), var.unbridge())};
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

BridgedInstruction BridgedBuilder::createWitnessMethod(BridgedCanType lookupType,
                                        BridgedConformance conformance,
                                        BridgedDeclRef member, BridgedType methodType) const {
  return {unbridged().createWitnessMethod(regularLoc(), lookupType.unbridged(), conformance.unbridged(),
                                          member.unbridged(), methodType.unbridged())};
}


BridgedInstruction BridgedBuilder::createReturn(BridgedValue op) const {
  return {unbridged().createReturn(returnLoc(), op.getSILValue())};
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

BridgedInstruction BridgedBuilder::createInitEnumDataAddr(BridgedValue enumAddr,
                                                          SwiftInt caseIdx,
                                                          BridgedType type) const {
  swift::SILValue en = enumAddr.getSILValue();
  return {unbridged().createInitEnumDataAddr(regularLoc(), en, en->getType().getEnumElement(caseIdx),
                                             type.unbridged())};
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
      regularLoc(), funcRef.getSILValue(), bridgedSubstitutionMap.unbridged(),
      bridgedCapturedArgs.getValues(capturedArgs),
      getParameterConvention(calleeConvention),
      hasUnknownIsolation ? swift::SILFunctionTypeIsolation::forUnknown()
                          : swift::SILFunctionTypeIsolation::forErased(),
      isOnStack ? swift::PartialApplyInst::OnStack
                : swift::PartialApplyInst::NotOnStack)};
}                                                                                  

BridgedInstruction BridgedBuilder::createBranch(BridgedBasicBlock destBlock, BridgedValueArray arguments) const {
  llvm::SmallVector<swift::SILValue, 16> argValues;
  return {unbridged().createBranch(regularLoc(), destBlock.unbridged(),
                                   arguments.getValues(argValues))};
}

BridgedInstruction BridgedBuilder::createUnreachable() const {
  return {unbridged().createUnreachable(loc.getLoc().getLocation())};
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

BridgedInstruction BridgedBuilder::createVectorBaseAddr(BridgedValue vector) const {
  return {unbridged().createVectorBaseAddr(regularLoc(), vector.getSILValue())};
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
      instance.getSILValue(), conformances.pcArray.unbridged<swift::ProtocolConformanceRef>())};
}

BridgedInstruction BridgedBuilder::createInitExistentialMetatype(BridgedValue metatype,
                                            BridgedType existentialType,
                                            BridgedConformanceArray conformances) const {
  return {unbridged().createInitExistentialMetatype(
      regularLoc(), metatype.getSILValue(), existentialType.unbridged(),
      conformances.pcArray.unbridged<swift::ProtocolConformanceRef>())};
}

BridgedInstruction BridgedBuilder::createMetatype(BridgedCanType instanceType,
                                                  BridgedASTType::MetatypeRepresentation representation) const {
  auto *mt =
      swift::MetatypeType::get(instanceType.unbridged(),
                               (swift::MetatypeRepresentation)representation);
  auto t = swift::SILType::getPrimitiveObjectType(swift::CanType(mt));
  return {unbridged().createMetatype(regularLoc(), t)};
}

BridgedInstruction BridgedBuilder::createEndCOWMutation(BridgedValue instance, bool keepUnique) const {
  return {unbridged().createEndCOWMutation(regularLoc(), instance.getSILValue(),
                                           keepUnique)};
}

BridgedInstruction
BridgedBuilder::createEndCOWMutationAddr(BridgedValue instance) const {
  return {unbridged().createEndCOWMutationAddr(regularLoc(),
                                               instance.getSILValue())};
}

BridgedInstruction BridgedBuilder::createMarkDependence(BridgedValue value, BridgedValue base, BridgedInstruction::MarkDependenceKind kind) const {
  return {unbridged().createMarkDependence(regularLoc(), value.getSILValue(), base.getSILValue(), swift::MarkDependenceKind(kind))};
}

BridgedInstruction BridgedBuilder::createMarkDependenceAddr(BridgedValue value, BridgedValue base, BridgedInstruction::MarkDependenceKind kind) const {
  return {unbridged().createMarkDependenceAddr(
      regularLoc(), value.getSILValue(), base.getSILValue(),
      swift::MarkDependenceKind(kind))};
}

BridgedInstruction BridgedBuilder::createEndAccess(BridgedValue value) const {
  return {unbridged().createEndAccess(regularLoc(), value.getSILValue(), false)};
}

BridgedInstruction BridgedBuilder::createEndApply(BridgedValue value) const {
  swift::ASTContext &ctxt = unbridged().getASTContext();
  return {unbridged().createEndApply(regularLoc(), value.getSILValue(),
                                     swift::SILType::getEmptyTupleType(ctxt))};
}

BridgedInstruction BridgedBuilder::createAbortApply(BridgedValue value) const {
  return {unbridged().createAbortApply(regularLoc(), value.getSILValue())};
}

BridgedInstruction BridgedBuilder::createConvertFunction(BridgedValue originalFunction, BridgedType resultType, bool withoutActuallyEscaping) const {
return {unbridged().createConvertFunction(regularLoc(), originalFunction.getSILValue(), resultType.unbridged(), withoutActuallyEscaping)};
}

BridgedInstruction BridgedBuilder::createConvertEscapeToNoEscape(BridgedValue originalFunction, BridgedType resultType, bool isLifetimeGuaranteed) const {
  return {unbridged().createConvertEscapeToNoEscape(regularLoc(), originalFunction.getSILValue(), resultType.unbridged(), isLifetimeGuaranteed)};
}

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif

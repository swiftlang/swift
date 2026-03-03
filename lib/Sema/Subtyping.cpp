//===--- Subtyping.cpp - Swift subtyping and conversion rules 00-----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements various utilities for reasoning about the Swift
// subtyping relation.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/Sema/Subtyping.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/TypeVariableType.h"

#define DEBUG_TYPE "Subtyping"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace constraints;

void swift::constraints::getTypeVariablesWithVariance(
    Type type, TypePosition pos,
    SmallPtrSetImpl<TypeVariableType *> &covariant,
    SmallPtrSetImpl<TypeVariableType *> &contravariant,
    SmallPtrSetImpl<TypeVariableType *> &invariant,
    bool funcResultIsInvariant,
    bool skipDependentMemberTypes) {
  auto rec = [&](Type type, TypePosition pos) {
    getTypeVariablesWithVariance(type, pos,
                                 covariant, contravariant, invariant,
                                 /*funcResultIsInvariant=*/false,
                                 skipDependentMemberTypes);
  };

  if (pos != TypePosition::Invariant &&
      pos != TypePosition::Shape) {
    if (auto *typeVar = type->getAs<TypeVariableType>()) {
      switch (pos) {
      case TypePosition::Covariant:
        covariant.insert(typeVar);
        return;
      case TypePosition::Contravariant:
        contravariant.insert(typeVar);
        return;
      case TypePosition::Shape:
      case TypePosition::Invariant:
        ASSERT(false && "Handled above");
        return;
      }
    } else if (auto *funcTy = type->getAs<FunctionType>()) {
      for (auto param : funcTy->getParams()) {
        auto paramTy = param.getOldType();
        if (param.isInOut())
          rec(paramTy, TypePosition::Invariant);
        else
          rec(paramTy, pos.flipped());
      }

      auto resultTy = funcTy->getResult();
      if (funcResultIsInvariant)
        rec(resultTy, TypePosition::Invariant);
      else
        rec(resultTy, pos);

      // FIXME: Error type variance?
      if (auto thrownError = funcTy->getThrownError())
        rec(thrownError, TypePosition::Invariant);

      return;
    } else if (auto *tupleTy = type->getAs<TupleType>()) {
      for (auto eltTy : tupleTy->getElementTypes())
        rec(eltTy, pos);
      return;
    } else if (auto *metatypeTy = type->getAs<MetatypeType>()) {
      auto instanceTy = metatypeTy->getInstanceType();
      rec(instanceTy, pos);
      return;
    } else if (auto objectTy = type->getOptionalObjectType()) {
      rec(objectTy, pos);
      return;
    } else if (auto elementTy = type->getArrayElementType()) {
      rec(elementTy, pos);
      return;
    } else if (auto elementTy = ConstraintSystem::isSetType(type)) {
      // FIXME: This differs from TypeTransform.h because we say that
      // the Set element type is covariant, which is correct.
      rec(*elementTy, pos);
      return;
    } else if (auto pair = ConstraintSystem::isDictionaryType(type)) {
      // FIXME: This differs from TypeTransform.h because we say that
      // the Dictionary key type is covariant, which is correct.
      rec(pair->first, pos);
      rec(pair->second, pos);
      return;
    }
  }

  type->getTypeVariables(invariant, skipDependentMemberTypes);
}

/// Determine whether the candidate type is a subclass of the superclass type.
bool swift::constraints::isSubclassOf(Type candidateType, Type superclassType) {
  if (!superclassType->getClassOrBoundGenericClass())
    return false;

  if (!candidateType->getClassOrBoundGenericClass()) {
    candidateType = candidateType->getSuperclass();
    if (!candidateType)
      return false;
  }

  do {
    auto result = isLikelyExactMatch(candidateType, superclassType);
    ASSERT(result);
    if (*result)
      return true;

    candidateType = candidateType->getSuperclass();
  } while (candidateType);

  return false;
}

/// Determine whether the candidate type can be erased to the given
/// existential type. This check is approximate, because it disregards
/// conditional conformance and parameterized protocol types.
bool swift::constraints::isSubtypeOfExistentialType(Type candidateType,
                                                    Type existentialType) {
  auto layout = existentialType->getExistentialLayout();

  if (auto layoutConstraint = layout.getLayoutConstraint()) {
    if (layoutConstraint->isClass() &&
        !(candidateType->isClassExistentialType() ||
          candidateType->mayHaveSuperclass()))
      return false;
  }

  if (layout.explicitSuperclass &&
      !isSubclassOf(candidateType, layout.explicitSuperclass))
    return false;

  return llvm::all_of(layout.getProtocols(), [&](ProtocolDecl *P) {
    auto result = TypeChecker::containsProtocol(candidateType, P,
                                                /*allowMissing=*/false);
    return result.first || result.second;
  });
}

/// T conv $T0
/// $T0 conforms P
bool ConstraintSystem::isConformanceTransitiveForSupertype(
    ConversionBehavior behavior, ProtocolDecl *proto) {
  // Sendable conformance is too loose to conclude anything.
  if (proto->isSpecificProtocol(KnownProtocolKind::Sendable))
    return false;

  auto key = std::make_pair(behavior, proto);
  auto found = ConformanceTransitiveForSupertypeCache.find(key);
  if (found != ConformanceTransitiveForSupertypeCache.end())
    return found->second;

  auto &ctx = getASTContext();

  // Enumerate possible nominal supertypes of a type having the
  // given conversion behavior.
  SmallVector<NominalTypeDecl *, 4> declsToCheck;

  if (behavior != ConversionBehavior::Optional) {
    // Every T converts to Optional<T>.
    if (auto *optionalDecl = ctx.getOptionalDecl())
      declsToCheck.push_back(optionalDecl);
  }

  // Every hashable T converts to AnyHashable.
  // FIXME: Actually check if the type is hashable.
  if (auto *anyHashableDecl = ctx.getAnyHashableDecl())
    declsToCheck.push_back(anyHashableDecl);

  auto addPointers = [&]() {
    declsToCheck.push_back(ctx.getUnsafePointerDecl());
    declsToCheck.push_back(ctx.getUnsafeRawPointerDecl());
  };

  auto addMutablePointers = [&]() {
    addPointers();
    declsToCheck.push_back(ctx.getUnsafeMutablePointerDecl());
    declsToCheck.push_back(ctx.getUnsafeMutableRawPointerDecl());
  };

  bool result = true;

  switch (behavior) {
  case ConversionBehavior::None:
  case ConversionBehavior::Class:
  case ConversionBehavior::Dictionary:
  case ConversionBehavior::Set:
  case ConversionBehavior::Optional:
  case ConversionBehavior::AnyHashable:
  case ConversionBehavior::Tuple:
    break;

  case ConversionBehavior::String:
    // Strings convert to UnsafePointer.
    addPointers();
    break;

  case ConversionBehavior::Array:
    addPointers();
    break;

  case ConversionBehavior::Pointer:
    addMutablePointers();
    break;

  case ConversionBehavior::Double:
    // Note this is funny, but valid. We return false if
    // either Double or CGFloat conform to the protocol,
    // so the only "transitive" protocols in this case
    // are those that neither CGFloat nor Double conform
    // to.
    if (auto *doubleDecl = ctx.getDoubleDecl())
      declsToCheck.push_back(doubleDecl);
    if (auto *cgFloatDecl = ctx.getCGFloatDecl())
      declsToCheck.push_back(cgFloatDecl);
    break;

  case ConversionBehavior::Function:
  case ConversionBehavior::Metatype:
  case ConversionBehavior::ExistentialMetatype:
    // FIXME: Metatypes and functions.
    result = false;
    break;

  case ConversionBehavior::LValue:
    ASSERT(false && "Must unwrap lvalue type first!");
    break;

  case ConversionBehavior::InOut:
    // InOut types convert to mutable pointers.
    addMutablePointers();
    break;

  case ConversionBehavior::Existential:
    // FIXME: Implement this.
    result = false;
    break;

  case ConversionBehavior::Unknown:
    // Can't say anything in this case.
    result = false;
    break;
  }

  if (result) {
    // Check if any of our nominal types conform.
    // If they do, then conformance is not transitive.
    for (auto *decl : declsToCheck) {
      SmallVector<ProtocolConformance *, 1> results;
      decl->lookupConformance(proto, results);
      if (!results.empty()) {
        result = false;
        break;
      }
    }
  }

  // Cache the result.
  bool inserted =
    ConformanceTransitiveForSupertypeCache.insert(
      std::make_pair(key, result)).second;
  ASSERT(inserted);

  return result;
}

bool swift::constraints::checkTransitiveSupertypeConformance(
    ConstraintSystem &cs, Type type, ProtocolDecl *proto) {
  // Every lvalue type can be converted to its object type, so
  // we must consider conversions of the object type in this case.
  if (auto *lvalueType = type->getAs<LValueType>())
    type = lvalueType->getObjectType();
  auto behavior = getConversionBehavior(type);
  if (cs.isConformanceTransitiveForSupertype(behavior, proto)) {
    // Unwrap InOut and LValue type.
    return !cs.lookupConformance(type->getWithoutSpecifierType(), proto)
        .isInvalid();
  }
  return true;
}

/// $T0 conv T
/// $T0 conforms P
bool ConstraintSystem::isConformanceTransitiveForSubtype(
    ConversionBehavior behavior, ProtocolDecl *proto) {
  // Sendable conformance is too loose to conclude anything.
  if (proto->isSpecificProtocol(KnownProtocolKind::Sendable))
    return false;

  switch (behavior) {
  case ConversionBehavior::None:
  case ConversionBehavior::String:
  case ConversionBehavior::Array:
  case ConversionBehavior::Dictionary:
  case ConversionBehavior::Set:
    // All subtypes of these have the same nominal type,
    // and conform to the same protocols.
    return true;

  case ConversionBehavior::Tuple:
    // All subtypes of a tuple remain a tuple, and conform
    // to the same protocols.
    return true;

  case ConversionBehavior::Class:
    // A subclass might conform to more protocols than a
    // superclass.
    return false;

  case ConversionBehavior::Double: {
    auto key = std::make_pair(behavior, proto);
    auto found = ConformanceTransitiveForSubtypeCache.find(key);
    if (found != ConformanceTransitiveForSubtypeCache.end())
      return found->second;

    SmallVector<NominalTypeDecl *, 4> declsToCheck;

    auto &ctx = getASTContext();
    if (auto *cgFloatDecl = ctx.getCGFloatDecl())
      declsToCheck.push_back(cgFloatDecl);
    if (auto *doubleDecl = ctx.getDoubleDecl())
      declsToCheck.push_back(doubleDecl);

    bool result = false;
    for (auto *decl : declsToCheck) {
      SmallVector<ProtocolConformance *, 1> results;
      decl->lookupConformance(proto, results);
      if (!results.empty()) {
        result = true;
        break;
      }
    }

    // Cache the result.
    bool inserted =
      ConformanceTransitiveForSubtypeCache.insert(
        std::make_pair(key, result)).second;
    ASSERT(inserted);

    return result;
  }

  case ConversionBehavior::InOut:
  case ConversionBehavior::LValue:
    // InOutType and LValueType have no proper subtypes.
    return true;

  case ConversionBehavior::Optional:
    // FIXME: Check payload type.
    return false;

  case ConversionBehavior::AnyHashable:
    // All Hashable types are subtypes of AnyHashable, so
    // we cannot conclude anything about protocol conformance
    // in this case.
    return false;

  case ConversionBehavior::Pointer:
    // FIXME: Check pointer types.
    return false;

  case ConversionBehavior::Function:
  case ConversionBehavior::Metatype:
  case ConversionBehavior::ExistentialMetatype:
    // FIXME: Metatypes and functions.
    return false;

  case ConversionBehavior::Existential:
  case ConversionBehavior::Unknown:
    // Can't say anything in this case.
    return false;
  }
}

bool swift::constraints::checkTransitiveSubtypeConformance(
    ConstraintSystem &cs, Type type, ProtocolDecl *proto) {
  auto behavior = getConversionBehavior(type);
  if (cs.isConformanceTransitiveForSubtype(behavior, proto)) {
    // Unwrap InOut and LValue type.
    return !cs.lookupConformance(type->getWithoutSpecifierType(), proto)
        .isInvalid();
  }
  return true;
}

std::optional<bool>
swift::constraints::isLikelyExactMatch(Type lhs, Type rhs) {
  if (!lhs->hasTypeVariable() && !lhs->hasTypeParameter() &&
      !rhs->hasTypeVariable() && !rhs->hasTypeParameter()) {
    return lhs->isEqual(rhs);
  }

  // FIXME: Make this more precise.
  if (auto *lhsDecl = lhs->getAnyNominal()) {
    if (!rhs->is<TypeVariableType>() && !rhs->isTypeParameter()) {
      auto *rhsDecl = rhs->getAnyNominal();
      return lhsDecl == rhsDecl;
    }
  }

  // FIXME: Handle other type kinds.
  return std::nullopt;
}

ConversionBehavior
swift::constraints::getConversionBehavior(Type type) {
  if (type->is<BuiltinType>())
    return ConversionBehavior::None;

  if (type->is<StructType>()) {
    if (type->isAnyHashable())
      return ConversionBehavior::AnyHashable;
    else if (type->isString())
      return ConversionBehavior::String;
    else if (type->isDouble() || type->isCGFloat())
      return ConversionBehavior::Double;
    else if (type->getAnyPointerElementType())
      return ConversionBehavior::Pointer;

    return ConversionBehavior::None;
  }

  if (auto *structTy = type->getAs<BoundGenericStructType>()) {
    if (auto eltTy = structTy->getArrayElementType()) {
      return ConversionBehavior::Array;
    } else if (ConstraintSystem::isDictionaryType(structTy)) {
      return ConversionBehavior::Dictionary;
    } else if (ConstraintSystem::isSetType(structTy)) {
      return ConversionBehavior::Set;
    } else if (type->getAnyPointerElementType()) {
      return ConversionBehavior::Pointer;
    }

    return ConversionBehavior::None;
  }

  if (auto *enumTy = type->getAs<BoundGenericEnumType>()) {
    if (enumTy->getOptionalObjectType())
      return ConversionBehavior::Optional;

    return ConversionBehavior::None;
  }

  if (type->is<ClassType>() ||
      type->is<BoundGenericClassType>() ||
      type->is<DynamicSelfType>())
    return ConversionBehavior::Class;

  if (type->is<EnumType>() ||
      type->is<BuiltinType>() || type->is<ArchetypeType>())
    return ConversionBehavior::None;

  if (type->is<MetatypeType>())
    return ConversionBehavior::Metatype;

  if (type->is<InOutType>())
    return ConversionBehavior::InOut;

  if (type->is<LValueType>())
    return ConversionBehavior::LValue;

  if (type->isVoid())
    return ConversionBehavior::None;

  // We only support tuples and function parameter lists with a known length
  // for now.
  if (type->is<TupleType>() || type->is<FunctionType>()) {
    if (type->hasParameterPack())
      return ConversionBehavior::Unknown;

    SmallPtrSet<TypeVariableType *, 4> referencedTypeVars;
    type->getTypeVariables(referencedTypeVars);
    for (auto *typeVar : referencedTypeVars) {
      if (typeVar->getImpl().isPackExpansion())
        return ConversionBehavior::Unknown;
    }

    if (type->is<TupleType>())
      return ConversionBehavior::Tuple;
    return ConversionBehavior::Function;
  }

  if (type->is<ExistentialType>())
    return ConversionBehavior::Existential;

  if (type->is<ExistentialMetatypeType>())
    return ConversionBehavior::ExistentialMetatype;

  return ConversionBehavior::Unknown;
}

bool swift::constraints::hasProperSubtypes(Type type) {
  switch (getConversionBehavior(type)) {
  case ConversionBehavior::None:
  case ConversionBehavior::String:
    return false;
  case ConversionBehavior::Array:
    return hasProperSubtypes(type->getArrayElementType());
  case ConversionBehavior::Dictionary: {
    auto pair = ConstraintSystem::isDictionaryType(type);
    return hasProperSubtypes(pair->first) || hasProperSubtypes(pair->second);
  }
  case ConversionBehavior::Set:
    return hasProperSubtypes(*ConstraintSystem::isSetType(type));
  case ConversionBehavior::LValue:
  case ConversionBehavior::InOut:
    // LValueType and InOutType are invariant.
    return false;
  case ConversionBehavior::Class:
  case ConversionBehavior::AnyHashable:
  case ConversionBehavior::Double:
  case ConversionBehavior::Pointer:
  case ConversionBehavior::Optional:
  case ConversionBehavior::Function:
  case ConversionBehavior::Metatype:
  case ConversionBehavior::Tuple:
  case ConversionBehavior::Existential:
  case ConversionBehavior::ExistentialMetatype:
  case ConversionBehavior::Unknown:
    return true;
  }
}

bool swift::constraints::hasProperSupertypes(Type type) {
  switch (getConversionBehavior(type)) {
  case ConversionBehavior::None:
    if (auto *archetypeType = type->getAs<ArchetypeType>()) {
      // An archetype is a subtype of its superclass.
      if (archetypeType->getSuperclass())
        return true;
    }

    return false;
  case ConversionBehavior::String:
    // Strings convert to pointers.
    return true;
  case ConversionBehavior::Class: {
    if (type->is<DynamicSelfType>())
      return true;

    auto *classDecl = type->getClassOrBoundGenericClass();
    return classDecl->getSuperclassDecl();
  }
  case ConversionBehavior::AnyHashable:
    return false;
  case ConversionBehavior::LValue:
    // Every lvalue type is a subtype of its object type.
    return true;
  case ConversionBehavior::InOut:
    // InOutType is a subtype of various pointer types.
    return true;
  case ConversionBehavior::Array:
  case ConversionBehavior::Dictionary:
  case ConversionBehavior::Set:
  case ConversionBehavior::Double:
  case ConversionBehavior::Pointer:
  case ConversionBehavior::Optional:
  case ConversionBehavior::Function:
  case ConversionBehavior::Metatype:
  case ConversionBehavior::Tuple:
  case ConversionBehavior::Existential:
  case ConversionBehavior::ExistentialMetatype:
  case ConversionBehavior::Unknown:
    return true;
  }
}

static ClassDecl *getBridgedObjCClass(ClassDecl *classDecl) {
  if (auto *attr = classDecl->getAttrs().getAttribute<ObjCBridgedAttr>())
    return attr->getObjCClass();
  return nullptr;
}

ConflictReason swift::constraints::canPossiblyConvertTo(
    ConstraintSystem &cs,
    Type lhs, Type rhs,
    GenericSignature sig) {
  if (lhs->isEqual(rhs))
    return std::nullopt;

  auto lhsKind = getConversionBehavior(lhs);
  auto rhsKind = getConversionBehavior(rhs);

  // Conversion between two types with the same conversion behavior.
  if (lhsKind == rhsKind) {
    switch (lhsKind) {
    case ConversionBehavior::None: {
      auto result = isLikelyExactMatch(lhs, rhs);
      if (result.has_value() && !*result)
        return ConflictFlag::Exact;

      break;
    }

    case ConversionBehavior::Class: {
      // Unwrap DynamicSelfType.
      bool lhsWasSelf = false;
      if (auto *lhsSelf = lhs->getAs<DynamicSelfType>()) {
        lhsWasSelf = true;
        lhs = lhsSelf->getSelfType();
      }

      bool rhsWasSelf = false;
      if (auto *rhsSelf = rhs->getAs<DynamicSelfType>()) {
        rhsWasSelf = true;
        rhs = rhsSelf->getSelfType();
      }

      // DynamicSelfType-to-DynamicSelfType conversions are exact.
      if (lhsWasSelf && rhsWasSelf) {
        auto result = isLikelyExactMatch(lhs, rhs);
        if (result.has_value() && !*result)
          return ConflictFlag::Class;
      }

      // No conversions to DynamicSelfType.
      if (rhsWasSelf)
        return ConflictFlag::Class;

      auto *lhsDecl = lhs->getClassOrBoundGenericClass();
      auto *rhsDecl = rhs->getClassOrBoundGenericClass();

      // Toll-free bridging CF -> ObjC.
      if (lhsDecl->getForeignClassKind() == ClassDecl::ForeignKind::CFType &&
          rhsDecl->getForeignClassKind() != ClassDecl::ForeignKind::CFType) {
        if (auto *lhsBridged = getBridgedObjCClass(lhsDecl)) {
          lhs = lhsBridged->getDeclaredInterfaceType();
          ASSERT(!lhs->hasTypeParameter());
        }

      // Toll-free bridging ObjC -> CF.
      } else if (lhsDecl->getForeignClassKind() != ClassDecl::ForeignKind::CFType &&
                 rhsDecl->getForeignClassKind() == ClassDecl::ForeignKind::CFType) {
        if (auto *rhsBridged = getBridgedObjCClass(rhsDecl)) {
          rhs = rhsBridged->getDeclaredInterfaceType();
          ASSERT(!rhs->hasTypeParameter());
        }
      }

      // Check for a subclassing relationship.
      if (!isSubclassOf(lhs, rhs))
        return ConflictFlag::Class;

      break;
    }

    case ConversionBehavior::Double:
      // There are only two types with this behavior, and they convert
      // to each other.
      break;

    case ConversionBehavior::String:
      // String converts to String.
      break;

    case ConversionBehavior::AnyHashable:
      // AnyHashable converts to AnyHashable.
      break;

    case ConversionBehavior::Pointer:
      // Too many Pointer-to-Pointer conversions to care about.
      //
      // FIXME: Eventually, the encoding here will be complete though, and this
      // will be used by matchTypes().
      break;

    case ConversionBehavior::Array: {
      // Array-to-Array conversions.
      auto subResult = canPossiblyConvertTo(
          cs,
          lhs->getArrayElementType(),
          rhs->getArrayElementType(), sig);
      if (subResult)
        return subResult | ConflictFlag::Array;

      break;
    }
    case ConversionBehavior::Dictionary: {
      // Dictionary-to-Dictionary conversions.
      auto lhsPair = *ConstraintSystem::isDictionaryType(lhs);
      auto rhsPair = *ConstraintSystem::isDictionaryType(rhs);
      auto keyResult = canPossiblyConvertTo(
          cs,
          lhsPair.first,
          rhsPair.first, sig);
      if (keyResult)
        return keyResult | ConflictFlag::DictionaryKey;
      auto valueResult = canPossiblyConvertTo(
          cs,
          lhsPair.second,
          rhsPair.second, sig);
      if (valueResult)
        return valueResult | ConflictFlag::DictionaryKey;
      break;
    }
    case ConversionBehavior::Set: {
      // Set-to-Set conversions.
      auto lhsElt = *ConstraintSystem::isSetType(lhs);
      auto rhsElt = *ConstraintSystem::isSetType(rhs);

      auto subResult = canPossiblyConvertTo(
          cs,
          lhsElt,
          rhsElt, sig);
      if (subResult)
        return subResult | ConflictFlag::Set;
      break;
    }
    case ConversionBehavior::Optional: {
      // Optional-to-optional conversion.
      auto argObjectType = lhs->getOptionalObjectType();
      auto objectType = rhs->getOptionalObjectType();
      auto optionalToOptional = canPossiblyConvertTo(
          cs, argObjectType, objectType, sig);

      if (optionalToOptional)
        return optionalToOptional | ConflictFlag::Optional;
      break;
    }
    case ConversionBehavior::Metatype: {
      // FIXME: Inaccurate, because not all conversions are allowed in
      // instance type position.
      auto argInstanceType = lhs->getMetatypeInstanceType();
      auto instanceType = rhs->getMetatypeInstanceType();
      auto instanceConversion = canPossiblyConvertTo(
          cs, argInstanceType, instanceType, sig);

      if (instanceConversion)
        return instanceConversion | ConflictFlag::Metatype;
      break;
    }
    case ConversionBehavior::Function:
      // FIXME: Implement.
      break;
    case ConversionBehavior::InOut:
    case ConversionBehavior::LValue: {
      // InOut-to-InOut and LValue-to-LValue conversions are invariant.
      auto result = isLikelyExactMatch(
          lhs->getWithoutSpecifierType(),
          rhs->getWithoutSpecifierType());
      if (result.has_value() && !*result)
        return ConflictFlag::Exact;

      break;
    }
    case ConversionBehavior::Tuple: {
      auto *lhsTuple = lhs->castTo<TupleType>();
      auto *rhsTuple = rhs->castTo<TupleType>();

      if (lhsTuple->getNumElements() != rhsTuple->getNumElements())
        return ConflictFlag::TupleArity;

      for (unsigned i : indices(lhsTuple->getElements())) {
        auto lhsElt = lhsTuple->getElementType(i);
        auto rhsElt = rhsTuple->getElementType(i);
        auto result = canPossiblyConvertTo(cs, lhsElt, rhsElt, sig);
        if (result)
          return result | ConflictFlag::TupleElement;
      }

      break;
    }
    case ConversionBehavior::Existential:
      // Existential-to-existential conversions.
      if (!isSubtypeOfExistentialType(lhs, rhs))
        return ConflictFlag::Existential;

      break;
    case ConversionBehavior::ExistentialMetatype:
      // Existential metatype-to-existential metatype conversions.
      if (!isSubtypeOfExistentialType(lhs->getMetatypeInstanceType(),
                                      rhs->getMetatypeInstanceType())) {
        return ConflictFlag::Existential;
      }

      break;
    case ConversionBehavior::Unknown:
      break;
    }

  // Every lvalue type is a subtype of its object type.
  } else if (lhsKind == ConversionBehavior::LValue) {
    if (rhsKind == ConversionBehavior::InOut) {
      // LValue-to-InOut conversions are invariant.
      auto result = isLikelyExactMatch(
          lhs->getWithoutSpecifierType(),
          rhs->getWithoutSpecifierType());
      if (result.has_value() && !*result)
        return ConflictFlag::Exact;

    } else {
      // Attempt LValue-to-RValue conversion.
      return canPossiblyConvertTo(cs, lhs->getWithoutSpecifierType(),
                                  rhs, sig);
    }

  // Handle case where the kinds don't match, and we're not converting
  // from an unknown type.
  } else if (lhsKind != ConversionBehavior::Unknown &&
             lhsKind != ConversionBehavior::InOut) {
    switch (rhsKind) {
    case ConversionBehavior::Class: {
      // Protocol metatypes can convert to instances of the Protocol class
      // on Objective-C interop platforms.
      //
      // FIXME: Make this less conservative.
      if (lhsKind == ConversionBehavior::Metatype)
        break;

      // No conversions to DynamicSelfType.
      if (rhs->is<DynamicSelfType>())
        return ConflictFlag::Category;

      // Archetypes, existentials, and dynamic Self can convert to classes.
      if (isSubclassOf(lhs, rhs))
        break;

      // Nothing else converts to a class.
      return ConflictFlag::Category;
    }

    case ConversionBehavior::AnyHashable:
      // FIXME: Check if lhs definitely not Hashable.
      break;

    case ConversionBehavior::Pointer:
      // Array, String, and InOutType convert to pointers.
      if (lhsKind != ConversionBehavior::Array &&
          lhsKind != ConversionBehavior::String &&
          lhsKind != ConversionBehavior::InOut) {
        return ConflictFlag::Category;
      }

      break;

    case ConversionBehavior::Optional: {
      // We have a non-optional on the left. Try value-to-optional.
      auto objectType = rhs->getOptionalObjectType();
      auto valueToOptional = canPossiblyConvertTo(
          cs, lhs, objectType, sig);
      if (valueToOptional)
        return valueToOptional | ConflictFlag::Optional;

      break;
    }

    case ConversionBehavior::InOut:
    case ConversionBehavior::LValue:
    case ConversionBehavior::None:
    case ConversionBehavior::String:
    case ConversionBehavior::Double:
    case ConversionBehavior::Array:
    case ConversionBehavior::Dictionary:
    case ConversionBehavior::Set:
    case ConversionBehavior::Function:
    case ConversionBehavior::Metatype:
    case ConversionBehavior::Tuple:
      return ConflictFlag::Category;

    case ConversionBehavior::Existential:
      // Concrete-to-existential conversions.
      if (!isSubtypeOfExistentialType(lhs, rhs))
        return ConflictFlag::Existential;

      break;

    case ConversionBehavior::ExistentialMetatype:
      if (lhsKind != ConversionBehavior::Metatype) {
        return ConflictFlag::Category;
      }

      // Concrete metatype-to-existential metatype conversions.
      if (!isSubtypeOfExistentialType(lhs->getMetatypeInstanceType(),
                                      rhs->getMetatypeInstanceType())) {
        return ConflictFlag::Existential;
      }

      break;

    case ConversionBehavior::Unknown:
      break;
    }
  }

  // FIXME: Move this into isLikelyExactMatch()
  if (sig) {
    // Skip this if lhs is a type variable, because then lookupConformance()
    // always returns an abstract conformance for that type.
    if (rhs->isTypeParameter() && !lhs->isTypeVariableOrMember()) {
      bool failed = llvm::any_of(
          sig->getRequiredProtocols(rhs),
          [&](ProtocolDecl *proto) {
            return !checkTransitiveSupertypeConformance(cs, lhs, proto);
          });
      if (failed)
        return ConflictFlag::Conformance;
    }

    if (lhs->isTypeParameter() && !rhs->isTypeVariableOrMember()) {
      bool failed = llvm::any_of(
          sig->getRequiredProtocols(lhs),
          [&](ProtocolDecl *proto) {
            return !checkTransitiveSubtypeConformance(cs, rhs, proto);
          });
      if (failed)
        return ConflictFlag::Conformance;
    }
  }

  LLVM_DEBUG(llvm::dbgs() << "unknown conversion:\n";
             lhs->dump(llvm::dbgs());
             rhs->dump(llvm::dbgs()));
  return std::nullopt;
}

namespace {
enum class Operation { Join, Meet };
}

static void decomposeConstraintType(Type t,
                                    llvm::SmallSetVector<ProtocolDecl *, 4> &protos,
                                    Type &superclass, bool &anyObject,
                                    InvertibleProtocolSet &invertible) {
  if (auto *protoTy = t->getAs<ProtocolType>()) {
    protos.insert(protoTy->getDecl());
  } else if (auto *paramTy = t->getAs<ParameterizedProtocolType>()) {
    // FIXME: Do something smart with the generic arguments here.
    protos.insert(paramTy->getBaseType()->getDecl());
  } else if (auto *compositionTy = t->getAs<ProtocolCompositionType>()) {
    for (auto memberTy : compositionTy->getMembers()) {
      decomposeConstraintType(memberTy, protos, superclass,
                              anyObject, invertible);
    }

    anyObject |= compositionTy->hasExplicitAnyObject();
    invertible |= compositionTy->getInverses();
  } else if (t->getClassOrBoundGenericClass()) {
    superclass = t;
  } else {
    ABORT([&](auto &out) {
      out << "Unknown constraint type:\n";
      t->dump(out);
    });
  }
}

static Type existentialConstraintJoinMeetImpl(
    Operation op, Type lhs, Type rhs) {
  auto &ctx = lhs->getASTContext();

  llvm::SmallSetVector<ProtocolDecl *, 4> lhsProtos;
  Type lhsSuperclass;
  bool lhsAnyObject = false;
  InvertibleProtocolSet lhsInverses;
  decomposeConstraintType(lhs, lhsProtos, lhsSuperclass, lhsAnyObject, lhsInverses);

  llvm::SmallSetVector<ProtocolDecl *, 4> rhsProtos;
  Type rhsSuperclass;
  bool rhsAnyObject = false;
  InvertibleProtocolSet rhsInverses;
  decomposeConstraintType(rhs, rhsProtos, rhsSuperclass, rhsAnyObject, rhsInverses);

  SmallVector<Type, 4> members;
  Type superclass;
  bool anyObject = false;
  InvertibleProtocolSet inverses = lhsInverses;
  if (op == Operation::Join) {
    // Intersect all inherited protocols.
    for (unsigned i = 0, e = lhsProtos.size(); i < e; ++i) {
      for (auto *inherited : lhsProtos[i]->getAllInheritedProtocols()) {
        lhsProtos.insert(inherited);
      }
    }

    for (unsigned i = 0, e = rhsProtos.size(); i < e; ++i) {
      for (auto *inherited : rhsProtos[i]->getAllInheritedProtocols()) {
        rhsProtos.insert(inherited);
      }
    }

    for (auto *proto : lhsProtos) {
      if (proto->getInvertibleProtocolKind())
        continue;
      if (rhsProtos.count(proto))
        members.push_back(proto->getDeclaredInterfaceType());
    }

    // Join the superclass bound.
    if (lhsSuperclass && rhsSuperclass) {
      bool existentialUpperBound = false;
      superclass = subtypeJoin(lhsSuperclass, rhsSuperclass,
                               &existentialUpperBound);

      if (existentialUpperBound)
        superclass = Type();
    } else {
      // Drop the superclass bound.
    }

    anyObject = lhsAnyObject && rhsAnyObject;
    inverses.insertAll(rhsInverses);
  } else {
    // Take the union of all protocols.
    for (auto *proto : lhsProtos) {
      if (proto->getInvertibleProtocolKind())
        continue;
      members.push_back(proto->getDeclaredInterfaceType());
    }
    for (auto *proto : rhsProtos) {
      if (proto->getInvertibleProtocolKind())
        continue;
      if (lhsProtos.count(proto) == 0)
        members.push_back(proto->getDeclaredInterfaceType());
    }

    // Compute the meet of the superclass bound.
    if (lhsSuperclass && rhsSuperclass) {
      bool uninhabited = false;
      superclass = subtypeMeet(lhsSuperclass, rhsSuperclass,
                               &uninhabited);
      if (uninhabited)
        return Type();
    } else if (lhsSuperclass) {
      superclass = lhsSuperclass;
    } else {
      superclass = rhsSuperclass;
    }

    anyObject = lhsAnyObject || rhsAnyObject;
    inverses.intersect(rhsInverses);

    // FIXME: Check for conflicts
  }

  if (superclass)
    members.push_back(superclass);

  if (members.empty() && inverses.empty() && !anyObject)
    return Type();

  return ProtocolCompositionType::get(ctx, members, inverses, anyObject);
}

static Type subtypeJoinMeetImpl(Operation op, Type lhs, Type rhs,
                                bool *failed) {
  if (lhs->isEqual(rhs))
    return lhs;

  auto fail = [&]() -> Type {
    *failed = true;
    auto &ctx = lhs->getASTContext();
    if (op == Operation::Join)
      return ctx.getAnyExistentialType();
    return ctx.getNeverType();
  };

  auto rec = [&](Type lhs, Type rhs) -> Type {
    return subtypeJoinMeetImpl(op, lhs, rhs, failed);
  };

  auto lhsKind = getConversionBehavior(lhs);
  auto rhsKind = getConversionBehavior(rhs);

  if (lhsKind == ConversionBehavior::Unknown ||
      rhsKind == ConversionBehavior::Unknown)
    return fail();

  if (lhsKind == rhsKind) {
    switch (lhsKind) {
    case ConversionBehavior::String:
    case ConversionBehavior::AnyHashable:
      ASSERT(false && "Already handled above");
      break;

    case ConversionBehavior::None:
    case ConversionBehavior::LValue:
    case ConversionBehavior::InOut:
      // These are either singleton types, or they're invariant.
      //
      // FIXME: Handle type variables here.
      return fail();

    case ConversionBehavior::Class: {
      // FIXME: CF toll-free bridging

      if (op == Operation::Join) {
        // Try to find a common superclass.
        SmallVector<Type, 2> lhsSuper;

        auto lhsClass = lhs;
        while (lhsClass) {
          lhsSuper.push_back(lhsClass);
          lhsClass = lhsClass->getSuperclass();
        }

        SmallVector<Type, 2> rhsSuper;
        auto rhsClass = rhs;
        while (rhsClass) {
          rhsSuper.push_back(rhsClass);
          rhsClass = rhsClass->getSuperclass();
        }

        std::reverse(lhsSuper.begin(), lhsSuper.end());
        std::reverse(rhsSuper.begin(), rhsSuper.end());

        unsigned i = std::min(lhsSuper.size(), rhsSuper.size());
        while (i > 0) {
          --i;
          if (lhsSuper[i]->isEqual(rhsSuper[i]))
            return lhsSuper[i];
        }
      } else {
        // Check if one is a subclass of the other.
        if (isSubclassOf(lhs, rhs))
          return lhs;
        else if (isSubclassOf(rhs, lhs))
          return rhs;
      }

      return fail();
    }

    case ConversionBehavior::Array: {
      auto result = rec(lhs->getArrayElementType(),
                        rhs->getArrayElementType());
      return ArraySliceType::get(result);
    }

    case ConversionBehavior::Dictionary: {
      auto lhsPair = ConstraintSystem::isDictionaryType(lhs);
      auto rhsPair = ConstraintSystem::isDictionaryType(rhs);

      auto keyResult = rec(lhsPair->first, rhsPair->first);
      auto valueResult = rec(lhsPair->second, rhsPair->second);

      return DictionaryType::get(keyResult, valueResult);
    }

    case ConversionBehavior::Set: {
      auto lhsElt = *ConstraintSystem::isSetType(lhs);
      auto rhsElt = *ConstraintSystem::isSetType(rhs);

      auto result = rec(lhsElt, rhsElt);

      auto &ctx = lhs->getASTContext();
      return BoundGenericType::get(ctx.getSetDecl(), Type(), result);
    }

    case ConversionBehavior::Double:
      // Double join CGFloat = Double meet CGFloat = Double.
      if (lhs->isDouble())
        return lhs;
      ASSERT(lhs->isCGFloat() && rhs->isDouble());
      return rhs;

    case ConversionBehavior::Pointer:
      // FIXME
      return fail();

    case ConversionBehavior::Optional: {
      auto result = rec(lhs->getOptionalObjectType(),
                        rhs->getOptionalObjectType());
      return OptionalType::get(result);
    }

    case ConversionBehavior::Function: {
      auto *lhsFunc = lhs->castTo<FunctionType>();
      auto *rhsFunc = rhs->castTo<FunctionType>();

      auto result = rec(lhsFunc->getResult(), rhsFunc->getResult());

      SmallVector<AnyFunctionType::Param, 4> params;

      // Note: getConversionBehavior() guarantees the function types don't
      // contain any parameter packs, so we may assume their lengths are
      // known.
      if (lhsFunc->getNumParams() != rhsFunc->getNumParams())
        return fail();

      for (unsigned i : indices(lhsFunc->getParams())) {
        auto lhsParam = lhsFunc->getParams()[i];
        auto rhsParam = rhsFunc->getParams()[i];

        if (lhsParam.getParameterFlags() != rhsParam.getParameterFlags())
          return fail();

        Type paramType;
        if (lhsParam.isInOut() || lhsParam.isVariadic()) {
          ASSERT(rhsParam.isInOut());
          auto result = isLikelyExactMatch(lhsParam.getPlainType(),
                                           rhsParam.getPlainType());
          if (!result)
            return fail();
          if (!*result)
            return fail();

          paramType = lhsParam.getPlainType();
        } else if (op == Operation::Join) {
          bool uninhabited = false;
          paramType = subtypeMeet(lhsParam.getPlainType(),
                                  rhsParam.getPlainType(),
                                  &uninhabited);
          if (uninhabited)
            return fail();
        } else {
          bool existentialUpperBound = false;
          paramType = subtypeJoin(lhsParam.getPlainType(),
                                  rhsParam.getPlainType(),
                                  &existentialUpperBound);
          if (existentialUpperBound)
            return fail();
        }

        params.push_back(lhsParam.withType(paramType));
      }

      // FIXME: What about the rest of ExtInfo?
      auto extInfo = lhsFunc->getExtInfo();

      // An escaping function type can be converted into a noescape
      // function type but not vice versa, so the join is noescape
      // in this case.
      if (lhsFunc->isNoEscape() || rhsFunc->isNoEscape())
        extInfo = extInfo.intoBuilder().withNoEscape().build();

      return FunctionType::get(params, result, extInfo);
    }

    case ConversionBehavior::Metatype: {
      // FIXME: Inaccurate, because not all conversions are allowed in
      // instance type position.
      auto result = rec(lhs->getMetatypeInstanceType(),
                        rhs->getMetatypeInstanceType());
      if (auto *existentialTy = result->getAs<ExistentialType>())
        return ExistentialMetatypeType::get(existentialTy->getConstraintType());
      return MetatypeType::get(result);
    }

    case ConversionBehavior::Tuple: {
      auto *lhsTuple = lhs->castTo<TupleType>();
      auto *rhsTuple = rhs->castTo<TupleType>();

      // Note: getConversionBehavior() guarantees the tuples don't contain
      // any parameter packs, so we may assume their lengths are known.
      if (lhsTuple->getNumElements() != rhsTuple->getNumElements())
        return fail();

      bool lhsLabels = llvm::any_of(lhsTuple->getElements(),
                                    [&](TupleTypeElt elt) -> bool {
                                      return elt.hasName();
                                    });

      SmallVector<TupleTypeElt, 2> elts;
      for (unsigned i : indices(lhsTuple->getElements())) {
        auto &lhsElt = lhsTuple->getElement(i);
        auto &rhsElt = rhsTuple->getElement(i);
        auto result = rec(lhsElt.getType(), rhsElt.getType());
        if (lhsLabels)
          elts.emplace_back(result, lhsElt.getName());
        else
          elts.emplace_back(result, rhsElt.getName());
      }

      return TupleType::get(elts, lhs->getASTContext());
    }

    case ConversionBehavior::Existential: {
      Type lhsConstraint = lhs->castTo<ExistentialType>()->getConstraintType();
      Type rhsConstraint = rhs->castTo<ExistentialType>()->getConstraintType();
      auto result = existentialConstraintJoinMeetImpl(
          op, lhsConstraint, rhsConstraint);
      if (!result)
        return fail();
      if (result->getClassOrBoundGenericClass())
        return result;
      ASSERT(!result->is<ExistentialType>());
      return ExistentialType::get(result);
    }

    case ConversionBehavior::ExistentialMetatype: {
      Type lhsConstraint = lhs->castTo<ExistentialMetatypeType>()->getInstanceType();
      Type rhsConstraint = rhs->castTo<ExistentialMetatypeType>()->getInstanceType();
      auto result = existentialConstraintJoinMeetImpl(
          op, lhsConstraint, rhsConstraint);
      if (!result)
        return fail();
      if (result->getClassOrBoundGenericClass())
        return MetatypeType::get(result);
      ASSERT(!result->is<ExistentialType>());
      return ExistentialMetatypeType::get(result);
    }

    case ConversionBehavior::Unknown:
      ASSERT(false && "Handled above");
    }
  }

  // The join and meet operations are symmetric.
  auto either = [&](ConversionBehavior kind) {
    if (rhsKind == kind) {
      std::swap(lhs, rhs);
      std::swap(lhsKind, rhsKind);
      return true;
    } else if (lhsKind == kind) {
      return true;
    } else {
      return false;
    }
  };

  if (either(ConversionBehavior::Optional)) {
    // Optional<T> join U = Optional<T join U>
    // Optional<T> meet U = T meet U
    auto joined = rec(lhs->getOptionalObjectType(), rhs);
    if (op == Operation::Join)
      return OptionalType::get(joined);
    return joined;
  }

  if (either(ConversionBehavior::AnyHashable)) {
    // If T conforms to Hashable:
    //
    // AnyHashable join T = AnyHashable
    // AnyHashable meet T = T
    auto &ctx = lhs->getASTContext();
    auto *hashableProto = ctx.getProtocol(KnownProtocolKind::Hashable);
    if (!hashableProto)
      return fail();
    if (!lookupConformance(rhs, hashableProto))
      return fail();
    if (op == Operation::Join)
      return lhs;
    return rhs;
  }

  if (either(ConversionBehavior::Existential)) {
    if (op == Operation::Join) {
      // Incomplete implementation.
      //
      // FIXME: Delete requirements concrete type doesn't satisfy, and form new
      // existential.
      if (isSubtypeOfExistentialType(rhs, lhs))
        return lhs;

      if (auto superclassTy = lhs->getSuperclass())
        return rec(superclassTy, rhs);
    } else {
      if (isSubtypeOfExistentialType(rhs, lhs))
        return rhs;
    }
  }

  if (either(ConversionBehavior::ExistentialMetatype)) {
    auto lhsInstance = lhs->getMetatypeInstanceType();
    auto rhsInstance = rhs->getMetatypeInstanceType();

    if (op == Operation::Join) {
      // Incomplete implementation.
      //
      // FIXME: Delete requirements concrete type doesn't satisfy, and form new
      // existential.
      if (isSubtypeOfExistentialType(rhsInstance, lhsInstance))
        return lhs;

      if (auto superclassTy = lhsInstance->getSuperclass()) {
        return rec(MetatypeType::get(superclassTy), rhs);
      }
    } else {
      if (isSubtypeOfExistentialType(rhsInstance, lhsInstance))
        return rhs;
    }
  }

  return fail();
}

Type swift::constraints::subtypeJoin(Type lhs, Type rhs,
                                     bool *existentialUpperBound) {
  return subtypeJoinMeetImpl(Operation::Join, lhs, rhs,
                             existentialUpperBound);
}

Type swift::constraints::subtypeMeet(Type lhs, Type rhs,
                                     bool *uninhabited) {
  return subtypeJoinMeetImpl(Operation::Meet, lhs, rhs,
                             uninhabited);
}

void swift::constraints::simple_display(llvm::raw_ostream &out,
                                        ConflictReason reason) {
  if (!reason)
    return;

  out << "conflict:";

  if (reason.contains(ConflictFlag::Category))
    out << " category";
  if (reason.contains(ConflictFlag::Exact))
    out << " exact";
  if (reason.contains(ConflictFlag::Class))
    out << " class";
  if (reason.contains(ConflictFlag::Metatype))
    out << " metatype";
  if (reason.contains(ConflictFlag::Array))
    out << " array";
  if (reason.contains(ConflictFlag::DictionaryKey))
    out << " dictionary_key";
  if (reason.contains(ConflictFlag::DictionaryValue))
    out << " dictionary_value";
  if (reason.contains(ConflictFlag::Set))
    out << " set";
  if (reason.contains(ConflictFlag::Optional))
    out << " optional";
  if (reason.contains(ConflictFlag::Conformance))
    out << " conformance";
  if (reason.contains(ConflictFlag::TupleArity))
    out << " tuple_arity";
  if (reason.contains(ConflictFlag::TupleElement))
    out << " tuple_element";
  if (reason.contains(ConflictFlag::Existential))
    out << " existential";
}
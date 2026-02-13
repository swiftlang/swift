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

#include "swift/Sema/Subtyping.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Sema/ConstraintSystem.h"

#define DEBUG_TYPE "Subtyping"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace constraints;
using namespace inference;

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

  case ConversionBehavior::Structural:
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
    // All subtypes of these have the same nominal type.
    return true;

  case ConversionBehavior::Class:
    // If a subclass conforms, so does the superclass.
    return true;

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
        result = false;
        break;
      }
    }

    result = true;

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
  case ConversionBehavior::Pointer:
    // FIXME: Check pointer types.
    return false;

  case ConversionBehavior::Structural:
  case ConversionBehavior::Unknown:
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

  if (type->is<ClassType>() || type->is<BoundGenericClassType>())
    return ConversionBehavior::Class;

  if (type->is<EnumType>() ||
      type->is<BuiltinType>() || type->is<ArchetypeType>())
    return ConversionBehavior::None;

  if (type->is<FunctionType>() || type->is<MetatypeType>())
    return ConversionBehavior::Structural;

  if (type->is<InOutType>())
    return ConversionBehavior::InOut;

  if (type->is<LValueType>())
    return ConversionBehavior::LValue;

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
  case ConversionBehavior::Structural:
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
  case ConversionBehavior::Structural:
  case ConversionBehavior::Unknown:
    return true;
  }
}

static ClassDecl *getBridgedObjCClass(ClassDecl *classDecl) {
  return classDecl->getAttrs().getAttribute<ObjCBridgedAttr>()->getObjCClass();
}

ConflictReason swift::constraints::canPossiblyConvertTo(
    ConstraintSystem &cs,
    Type lhs, Type rhs,
    GenericSignature sig) {
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
      auto *lhsDecl = lhs->getClassOrBoundGenericClass();
      auto *rhsDecl = rhs->getClassOrBoundGenericClass();

      // Toll-free bridging CF -> ObjC.
      if (lhsDecl->getForeignClassKind() == ClassDecl::ForeignKind::CFType &&
          rhsDecl->getForeignClassKind() != ClassDecl::ForeignKind::CFType) {
        lhsDecl = getBridgedObjCClass(lhsDecl);

      // Toll-free bridging ObjC -> CF.
      } else if (lhsDecl->getForeignClassKind() != ClassDecl::ForeignKind::CFType &&
                 rhsDecl->getForeignClassKind() == ClassDecl::ForeignKind::CFType) {
        rhsDecl = getBridgedObjCClass(rhsDecl);
      }

      // Check for a subclassing relationship.
      if (!rhsDecl->isSuperclassOf(lhsDecl))
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
      // FIXME: Implement
      break;

    case ConversionBehavior::Array: {
      auto subResult = canPossiblyConvertTo(
          cs,
          lhs->getArrayElementType(),
          rhs->getArrayElementType(), sig);
      if (subResult)
        return subResult | ConflictFlag::Array;

      break;
    }
    case ConversionBehavior::Dictionary: {
      // FIXME: Implement
      break;
    }
    case ConversionBehavior::Set: {
      // FIXME: Implement
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
    case ConversionBehavior::Structural:
      if (lhs->getCanonicalType()->getKind()
          != rhs->getCanonicalType()->getKind())
        return ConflictFlag::Structural;
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
      // Archetypes can convert to classes.
      if (lhs->is<ArchetypeType>()) {
        auto superclassType = lhs->getSuperclass();
        if (!superclassType)
          return ConflictFlag::Class;

        return canPossiblyConvertTo(cs, superclassType, rhs, sig);
      }

      // Protocol metatypes can convert to instances of the Protocol class
      // on Objective-C interop platforms.
      if (lhs->is<MetatypeType>())
        break;

      // Nothing else converts to a class except for existentials
      // (which are 'ConversionBehavior::Unknown').
      return ConflictFlag::Category;
    }

    case ConversionBehavior::AnyHashable:
      // FIXME: Check if lhs definitely not Hashable
      break;

    case ConversionBehavior::Pointer:
      // FIXME: Array, String, InOutType convert to pointers
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
    case ConversionBehavior::Structural:
      return ConflictFlag::Category;

    case ConversionBehavior::Unknown:
      break;
    }
  }

  // FIXME: Move this into isLikelyExactMatch()
  if (sig) {
    if (rhs->isTypeParameter()) {
      bool failed = llvm::any_of(
          sig->getRequiredProtocols(rhs),
          [&](ProtocolDecl *proto) {
            return !checkTransitiveSupertypeConformance(cs, lhs, proto);
          });
      if (failed)
        return ConflictFlag::Conformance;
    }

    if (lhs->isTypeParameter()) {
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

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

std::optional<bool>
swift::constraints::isLikelyExactMatch(Type lhs, Type rhs) {
  if (!lhs->hasTypeVariable() && !lhs->hasTypeParameter() &&
      !rhs->hasTypeVariable() && !rhs->hasTypeParameter()) {
    return lhs->isEqual(rhs);
  }

  if (auto *lhsDecl = lhs->getAnyNominal()) {
    // FIXME: Make this more precise.
    auto *rhsDecl = rhs->getAnyNominal();
    return lhsDecl == rhsDecl;
  }

  // FIXME: Handle other type kinds.
  return std::nullopt;
}

ConversionBehavior
swift::constraints::getConversionBehavior(Type type) {
  if (type->is<StructType>()) {
    if (type->isAnyHashable())
      return ConversionBehavior::AnyHashable;
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

  if (type->isVoid())
    return ConversionBehavior::None;

  return ConversionBehavior::Unknown;
}

bool swift::constraints::hasProperSubtypes(Type type) {
  switch (getConversionBehavior(type)) {
  case ConversionBehavior::None:
    return false;
  case ConversionBehavior::Array:
    return hasProperSubtypes(type->getArrayElementType());
  case ConversionBehavior::Dictionary: {
    auto pair = ConstraintSystem::isDictionaryType(type);
    return hasProperSubtypes(pair->first) || hasProperSubtypes(pair->second);
  }
  case ConversionBehavior::Set:
    return hasProperSubtypes(*ConstraintSystem::isSetType(type));
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
  case ConversionBehavior::Class: {
    auto *classDecl = type->getClassOrBoundGenericClass();
    return classDecl->getSuperclassDecl();
  }
  case ConversionBehavior::AnyHashable:
    return false;
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

static bool shouldBeConservativeWithProto(ProtocolDecl *proto) {
  if (proto->isMarkerProtocol())
    return true;

  if (proto->isObjC())
    return true;

  return false;
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
    case ConversionBehavior::Unknown:
      break;
    }

  // Handle case where the kinds don't match, and we're not converting
  // from an unknown type.
  } else if (lhsKind != ConversionBehavior::Unknown) {
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

    case ConversionBehavior::None:
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

  if (sig) {
    // If '$LHS conv $RHS' and '$LHS conforms P', does it follow
    // that '$RHS conforms P'?
    auto isConformanceTransitiveOnLHS = [lhsKind, lhs]() -> bool {
      // FIXME: String converts to UnsafePointer<UInt8> which
      // can satisfy a conformance to P that String does not
      // satisfy. Encode this more thoroughly.
      if (lhsKind == ConversionBehavior::None)
        return !lhs->isString();

      return false;
    };

    if (rhs->isTypeParameter() &&
        isConformanceTransitiveOnLHS()) {
      bool failed = llvm::any_of(
          sig->getRequiredProtocols(rhs),
          [&](ProtocolDecl *proto) {
            if (shouldBeConservativeWithProto(proto))
              return false;
            return !lookupConformance(lhs, proto);
          });
      if (failed)
        return ConflictFlag::Conformance;
    }

    // If '$LHS conv $RHS' and '$RHS conforms P', does it follow
    // that '$LHS conforms P'?
    auto isConformanceTransitiveOnRHS = [rhsKind]() -> bool {
      if (rhsKind == ConversionBehavior::None ||
          rhsKind == ConversionBehavior::Array ||
          rhsKind == ConversionBehavior::Dictionary ||
          rhsKind == ConversionBehavior::Set)
        return true;

      return false;
    };

    if (lhs->isTypeParameter() &&
        isConformanceTransitiveOnRHS()) {
      bool failed = llvm::any_of(
          sig->getRequiredProtocols(lhs),
          [&](ProtocolDecl *proto) {
            if (shouldBeConservativeWithProto(proto))
              return false;
            return !lookupConformance(rhs, proto);
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

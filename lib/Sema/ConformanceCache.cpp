//===--- ConformanceCache.cpp - Caching conformance lookups ---------------===//
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
// This file implements various utilities for caching conformance lookups, and
// performing transitive conformance lookups, where we reason about whether
// the subtypes or supertypes of a known type can possibly conform to some
// protocol.
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/ConformanceCache.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/Subtyping.h"

#define DEBUG_TYPE "Subtyping"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace constraints;

ProtocolConformanceRef
ConformanceCache::lookupConformance(Type type, ProtocolDecl *protocol) {
  auto cacheKey = std::make_pair(type.getPointer(), protocol);

  auto cachedConformance = Conformances.find(cacheKey);
  if (cachedConformance != Conformances.end())
    return cachedConformance->second;

  auto conformance =
      swift::lookupConformance(type, protocol, /*allowMissing=*/true);
  Conformances[cacheKey] = conformance;
  return conformance;
}

/// T conv $T0
/// $T0 conforms P
bool ConformanceCache::isConformanceTransitiveForSupertype(
    ConversionBehavior behavior, ProtocolDecl *proto) {
  // Sendable conformance is too loose to conclude anything.
  if (proto->isSpecificProtocol(KnownProtocolKind::Sendable))
    return false;

  auto key = std::make_pair(behavior, proto);
  auto found = ConformanceTransitiveForSupertypeCache.find(key);
  if (found != ConformanceTransitiveForSupertypeCache.end())
    return found->second;

  auto &ctx = proto->getASTContext();

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

bool ConformanceCache::checkTransitiveSupertypeConformance(
    Type type, ProtocolDecl *proto) {
  // Every lvalue type can be converted to its object type, so
  // we must consider conversions of the object type in this case.
  if (auto *lvalueType = type->getAs<LValueType>())
    type = lvalueType->getObjectType();
  auto behavior = getConversionBehavior(type);
  if (isConformanceTransitiveForSupertype(behavior, proto)) {
    // Unwrap InOut and LValue type.
    return !lookupConformance(type->getWithoutSpecifierType(), proto)
        .isInvalid();
  }
  return true;
}

/// $T0 conv T
/// $T0 conforms P
bool ConformanceCache::isConformanceTransitiveForSubtype(
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

    auto &ctx = proto->getASTContext();
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

bool ConformanceCache::checkTransitiveSubtypeConformance(
    Type type, ProtocolDecl *proto) {
  auto behavior = getConversionBehavior(type);
  if (isConformanceTransitiveForSubtype(behavior, proto)) {
    // Unwrap InOut and LValue type.
    return !lookupConformance(type->getWithoutSpecifierType(), proto)
        .isInvalid();
  }
  return true;
}
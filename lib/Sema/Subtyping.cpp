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
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Sema/ConstraintSystem.h"

using namespace swift;
using namespace constraints;

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

  return ConversionBehavior::Unknown;
}

/// Check whether there exists a type that could be implicitly converted
/// to a given type i.e. is the given type is Double or Optional<..> this
/// function is going to return true because CGFloat could be converted
/// to a Double and non-optional value could be injected into an optional.
bool swift::constraints::hasConversions(Type type) {
  switch (getConversionBehavior(type)) {
  case ConversionBehavior::None:
    return false;
  case ConversionBehavior::Array:
    return hasConversions(type->getArrayElementType());
  case ConversionBehavior::Dictionary: {
    auto pair = ConstraintSystem::isDictionaryType(type);
    return hasConversions(pair->first) || hasConversions(pair->second);
  }
  case ConversionBehavior::Set:
    return hasConversions(*ConstraintSystem::isSetType(type));
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

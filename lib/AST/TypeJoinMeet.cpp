//===--- TypeJoinMeet.cpp - Swift Type "join" and "meet"  -----------------===//
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
//
//  This file implements the "join" operation for types (and, eventually,
//  "meet").
//
//===----------------------------------------------------------------------===//
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/SmallPtrSet.h"
using namespace swift;

Type Type::join(Type type1, Type type2) {
  assert(!type1->hasTypeVariable() && !type2->hasTypeVariable() &&
         "Cannot compute join of types involving type variables");

  // FIXME: This algorithm is woefully incomplete, and is only currently used
  // for optimizing away extra exploratory work in the constraint solver. It
  // should eventually encompass all of the subtyping rules of the language.

  // If the types are equivalent, the join is obvious.
  if (type1->isEqual(type2))
    return type1;

  // If both are class metatypes, compute the join of the instance type and
  // wrap the result in a metatype.
  if (auto *metatype1 = type1->getAs<MetatypeType>()) {
    if (auto *metatype2 = type2->getAs<MetatypeType>()) {
      auto instance1 = metatype1->getInstanceType();
      auto instance2 = metatype2->getInstanceType();
      if (instance1->mayHaveSuperclass() &&
          instance2->mayHaveSuperclass()) {
        auto result = Type::join(instance1, instance2);
        if (!result)
          return result;
        return MetatypeType::get(result);
      }
    }
  }

  // If both are existential metatypes, compute the join of the instance type
  // and wrap the result in an existential metatype.
  if (auto *metatype1 = type1->getAs<ExistentialMetatypeType>()) {
    if (auto *metatype2 = type2->getAs<ExistentialMetatypeType>()) {
      auto instance1 = metatype1->getInstanceType();
      auto instance2 = metatype2->getInstanceType();
      auto result = Type::join(instance1, instance2);
      if (!result)
        return result;
      return ExistentialMetatypeType::get(result);
    }
  }

  // If both are class types or opaque types that potentially have superclasses,
  // find the common superclass.
  if (type1->mayHaveSuperclass() && type2->mayHaveSuperclass()) {
    ASTContext &ctx = type1->getASTContext();
    LazyResolver *resolver = ctx.getLazyResolver();

    /// Walk the superclasses of type1 looking for type2. Record them for our
    /// second step.
    llvm::SmallPtrSet<CanType, 8> superclassesOfType1;
    CanType canType2 = type2->getCanonicalType();
    for (Type super1 = type1; super1; super1 = super1->getSuperclass(resolver)){
      CanType canSuper1 = super1->getCanonicalType();

      // If we have found the second type, we're done.
      if (canSuper1 == canType2) return super1;

      superclassesOfType1.insert(canSuper1);
    }

    // Look through the superclasses of type2 to determine if any were also
    // superclasses of type1.
    for (Type super2 = type2; super2; super2 = super2->getSuperclass(resolver)){
      CanType canSuper2 = super2->getCanonicalType();

      // If we found the first type, we're done.
      if (superclassesOfType1.count(canSuper2)) return super2;
    }

    // There is no common superclass; we're done.
    return nullptr;
  }

  // If one or both of the types are optional types, look at the underlying
  // object type.
  OptionalTypeKind otk1, otk2;
  Type objectType1 = type1->getAnyOptionalObjectType(otk1);
  Type objectType2 = type2->getAnyOptionalObjectType(otk2);
  if (otk1 == OTK_Optional || otk2 == OTK_Optional) {
    // Compute the join of the unwrapped type. If there is none, we're done.
    Type unwrappedJoin = join(objectType1 ? objectType1 : type1,
                              objectType2 ? objectType2 : type2);
    if (!unwrappedJoin) return nullptr;

    return OptionalType::get(unwrappedJoin);
  }

  // The join can only be an existential.
  return nullptr;
}


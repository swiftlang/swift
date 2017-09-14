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
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/SmallPtrSet.h"
using namespace swift;

struct TypeJoin : TypeVisitor<TypeJoin, Type> {
  Type First;

  static Type getSuperclassJoin(Type first, Type second);

  Type visitClassType(Type second);
  Type visitBoundGenericClassType(Type second);
  Type visitArchetypeType(Type second);
  Type visitDynamicSelfType(Type second);
  Type visitMetatypeType(Type second);
  Type visitExistentialMetatypeType(Type second);
  Type visitBoundGenericEnumType(Type second);

  Type visitOptionalType(Type second);

  Type visitType(Type second) {
    // FIXME: Make this unreachable by implementing all possible
    //        combinations.
    return nullptr;
  }
public:
  TypeJoin(Type First) : First(First) {}
};

Type TypeJoin::getSuperclassJoin(Type first, Type second) {
  // FIXME: Return Any
  if (!first->mayHaveSuperclass() || !second->mayHaveSuperclass())
    return nullptr;

  /// Walk the superclasses of type1 looking for second. Record them for our
  /// second step.
  llvm::SmallPtrSet<CanType, 8> superclassesOfFirst;
  CanType canSecond = second->getCanonicalType();
  for (Type super = first; super; super = super->getSuperclass()) {
    CanType canSuper = super->getCanonicalType();

    // If we have found the second type, we're done.
    if (canSuper == canSecond) return super;

    superclassesOfFirst.insert(canSuper);
  }

  // Look through the superclasses of second to determine if any were also
  // superclasses of first.
  for (Type super = second; super; super = super->getSuperclass()) {
    CanType canSuper = super->getCanonicalType();

    // If we found the first type, we're done.
    if (superclassesOfFirst.count(canSuper)) return super;
  }

  // FIXME: Return Any
  // There is no common superclass; we're done.
  return nullptr;
}

Type TypeJoin::visitClassType(Type second) {
  return getSuperclassJoin(First, second);
}

Type TypeJoin::visitBoundGenericClassType(Type second) {
  return getSuperclassJoin(First, second);
}

Type TypeJoin::visitArchetypeType(Type second) {
  return getSuperclassJoin(First, second);
}

Type TypeJoin::visitDynamicSelfType(Type second) {
  return getSuperclassJoin(First, second);
}

Type TypeJoin::visitMetatypeType(Type second) {
  assert(!First->mayHaveSuperclass() && !second->mayHaveSuperclass());

  // FIXME: Return Any
  if (First->getKind() != second->getKind())
    return nullptr;

  auto firstInstance = First->castTo<AnyMetatypeType>()->getInstanceType();
  auto secondInstance = second->castTo<AnyMetatypeType>()->getInstanceType();

  auto joinInstance = TypeJoin(firstInstance).visit(secondInstance);

  // FIXME: Return Any
  if (!joinInstance)
    return nullptr;

  return MetatypeType::get(joinInstance);
}

Type TypeJoin::visitExistentialMetatypeType(Type second) {
  assert(!First->mayHaveSuperclass() && !second->mayHaveSuperclass());

  // FIXME: Return Any
  if (First->getKind() != second->getKind())
    return nullptr;

  auto firstInstance = First->castTo<AnyMetatypeType>()->getInstanceType();
  auto secondInstance = second->castTo<AnyMetatypeType>()->getInstanceType();

  auto joinInstance = TypeJoin(firstInstance).visit(secondInstance);

  // FIXME: Return Any
  if (!joinInstance)
    return nullptr;

  return ExistentialMetatypeType::get(joinInstance);
}

Type TypeJoin::visitBoundGenericEnumType(Type second) {
  // FIXME: Return Any
  if (First->getKind() != second->getKind())
    return nullptr;

  OptionalTypeKind otk1, otk2;
  Type objectType1 = First->getAnyOptionalObjectType(otk1);
  Type objectType2 = second->getAnyOptionalObjectType(otk2);
  if (otk1 == OTK_Optional || otk2 == OTK_Optional) {
    // Compute the join of the unwrapped type. If there is none, we're done.
    Type unwrappedJoin = Type::join(objectType1 ? objectType1 : First,
                                    objectType2 ? objectType2 : second);
    // FIXME: More general joins of enums need to be handled.
    if (!unwrappedJoin) return nullptr;

    return OptionalType::get(unwrappedJoin);
  }

  // FIXME: More general joins of enums need to be handled, and
  //        then Any should be returned when there is no better
  //        choice.
  return nullptr;
}

Type TypeJoin::visitOptionalType(Type second) {
  auto canFirst = First->getCanonicalType();
  auto canSecond = second->getCanonicalType();

  return TypeJoin(canFirst).visit(canSecond);
}

Type Type::join(Type type1, Type type2) {
  assert(!type1->hasTypeVariable() && !type2->hasTypeVariable() &&
         "Cannot compute join of types involving type variables");

  assert(type1->getWithoutSpecifierType()->isEqual(type1) &&
         "Expected simple type!");
  assert(type2->getWithoutSpecifierType()->isEqual(type2) &&
         "Expected simple type!");

  // FIXME: This algorithm is woefully incomplete, and is only currently used
  // for optimizing away extra exploratory work in the constraint solver. It
  // should eventually encompass all of the subtyping rules of the language.

  // If the types are equivalent, the join is obvious.
  if (type1->isEqual(type2))
    return type1;

  // Until we handle all the combinations of joins, we need to make
  // sure we visit the optional side.
  OptionalTypeKind otk;
  if (type2->getAnyOptionalObjectType(otk))
    return TypeJoin(type1).visit(type2);

  return TypeJoin(type2).visit(type1);
}

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

// FIXME: This is currently woefully incomplete, and is only currently
// used for optimizing away extra exploratory work in the constraint
// solver. It should eventually encompass all of the subtyping rules
// of the language.
struct TypeJoin : TypeVisitor<TypeJoin, Type> {
  Type First;

  TypeJoin(Type First) : First(First) {
    assert(First && "Unexpected null type!");
  }

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
    // FIXME: Implement all the visitors.
    //    llvm_unreachable("Unimplemented type visitor!");
    return First->getASTContext().TheAnyType;
  }

public:
  static Type join(Type first, Type second) {
    if (!first || !second) {
      if (first)
        return ErrorType::get(first->getASTContext());

      if (second)
        return ErrorType::get(second->getASTContext());

      return Type();
    }

    assert(!first->hasTypeVariable() && !second->hasTypeVariable() &&
           "Cannot compute join of types involving type variables");

    assert(first->getWithoutSpecifierType()->isEqual(first) &&
           "Expected simple type!");
    assert(second->getWithoutSpecifierType()->isEqual(second) &&
           "Expected simple type!");

    // If the types are equivalent, the join is obvious.
    if (first->isEqual(second))
      return first;

    // Until we handle all the combinations of joins, we need to make
    // sure we visit the optional side.
    OptionalTypeKind otk;
    if (second->getAnyOptionalObjectType(otk))
      return TypeJoin(first).visit(second);

    return TypeJoin(second).visit(first);
  }
};

Type TypeJoin::getSuperclassJoin(Type first, Type second) {
  if (!first || !second)
    return TypeJoin::join(first, second);

  if (!first->mayHaveSuperclass() || !second->mayHaveSuperclass())
    return first->getASTContext().TheAnyType;

  /// Walk the superclasses of `first` looking for `second`. Record them
  /// for our second step.
  llvm::SmallPtrSet<CanType, 8> superclassesOfFirst;
  CanType canSecond = second->getCanonicalType();
  for (Type super = first; super; super = super->getSuperclass()) {
    CanType canSuper = super->getCanonicalType();

    // If we have found the second type, we're done.
    if (canSuper == canSecond)
      return super;

    superclassesOfFirst.insert(canSuper);
  }

  // Look through the superclasses of second to determine if any were also
  // superclasses of first.
  for (Type super = second; super; super = super->getSuperclass()) {
    CanType canSuper = super->getCanonicalType();

    // If we found the first type, we're done.
    if (superclassesOfFirst.count(canSuper))
      return super;
  }

  // There is no common superclass; we're done.
  return first->getASTContext().TheAnyType;
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
  if (First->getKind() != second->getKind())
    return First->getASTContext().TheAnyType;

  auto firstInstance = First->castTo<AnyMetatypeType>()->getInstanceType();
  auto secondInstance = second->castTo<AnyMetatypeType>()->getInstanceType();

  auto joinInstance = join(firstInstance, secondInstance);

  if (!joinInstance)
    return First->getASTContext().TheAnyType;

  return MetatypeType::get(joinInstance);
}

Type TypeJoin::visitExistentialMetatypeType(Type second) {
  if (First->getKind() != second->getKind())
    return First->getASTContext().TheAnyType;

  auto firstInstance = First->castTo<AnyMetatypeType>()->getInstanceType();
  auto secondInstance = second->castTo<AnyMetatypeType>()->getInstanceType();

  auto joinInstance = join(firstInstance, secondInstance);

  if (!joinInstance)
    return First->getASTContext().TheAnyType;

  return ExistentialMetatypeType::get(joinInstance);
}

Type TypeJoin::visitBoundGenericEnumType(Type second) {
  if (First->getKind() != second->getKind())
    return First->getASTContext().TheAnyType;

  OptionalTypeKind otk1, otk2;
  Type objectType1 = First->getAnyOptionalObjectType(otk1);
  Type objectType2 = second->getAnyOptionalObjectType(otk2);
  if (otk1 == OTK_Optional || otk2 == OTK_Optional) {
    // Compute the join of the unwrapped type. If there is none, we're done.
    Type unwrappedJoin = join(objectType1 ? objectType1 : First,
                              objectType2 ? objectType2 : second);
    // FIXME: More general joins of enums need to be handled.
    if (!unwrappedJoin)
      return First->getASTContext().TheAnyType;

    return OptionalType::get(unwrappedJoin);
  }

  // FIXME: More general joins of enums need to be handled.
  return First->getASTContext().TheAnyType;
}

Type TypeJoin::visitOptionalType(Type second) {
  auto canFirst = First->getCanonicalType();
  auto canSecond = second->getCanonicalType();

  return TypeJoin::join(canFirst, canSecond);
}

Type Type::join(Type type1, Type type2) {
  assert(type1 && type2 && "Unexpected null type!");

  return TypeJoin::join(type1, type2);
}

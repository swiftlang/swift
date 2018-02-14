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
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/SmallPtrSet.h"
using namespace swift;

// FIXME: This is currently woefully incomplete, and is only currently
// used for optimizing away extra exploratory work in the constraint
// solver. It should eventually encompass all of the subtyping rules
// of the language.
struct TypeJoin : CanTypeVisitor<TypeJoin, CanType> {
  CanType First;

  TypeJoin(CanType First) : First(First) {
    assert(First && "Unexpected null type!");
  }

  static CanType getSuperclassJoin(CanType first, CanType second);

  CanType visitClassType(CanType second);
  CanType visitBoundGenericClassType(CanType second);
  CanType visitArchetypeType(CanType second);
  CanType visitDynamicSelfType(CanType second);
  CanType visitMetatypeType(CanType second);
  CanType visitExistentialMetatypeType(CanType second);
  CanType visitBoundGenericEnumType(CanType second);

  CanType visitOptionalType(CanType second);

  CanType visitType(CanType second) {
    // FIXME: Implement all the visitors.
    //    llvm_unreachable("Unimplemented type visitor!");
    return First->getASTContext().TheAnyType;
  }

public:
  static CanType join(CanType first, CanType second) {
    assert(!first->hasTypeVariable() && !second->hasTypeVariable() &&
           "Cannot compute join of types involving type variables");

    assert(first->getWithoutSpecifierType()->isEqual(first) &&
           "Expected simple type!");
    assert(second->getWithoutSpecifierType()->isEqual(second) &&
           "Expected simple type!");

    // If the types are equivalent, the join is obvious.
    if (first == second)
      return first;

    // Until we handle all the combinations of joins, we need to make
    // sure we visit the optional side.
    OptionalTypeKind otk;
    if (second->getAnyOptionalObjectType(otk))
      return TypeJoin(first).visit(second);

    return TypeJoin(second).visit(first);
  }
};

CanType TypeJoin::getSuperclassJoin(CanType first, CanType second) {
  if (!first->mayHaveSuperclass() || !second->mayHaveSuperclass())
    return first->getASTContext().TheAnyType;

  /// Walk the superclasses of `first` looking for `second`. Record them
  /// for our second step.
  llvm::SmallPtrSet<CanType, 8> superclassesOfFirst;
  for (Type super = first; super; super = super->getSuperclass()) {
    auto canSuper = super->getCanonicalType();

    // If we have found the second type, we're done.
    if (canSuper == second)
      return canSuper;

    superclassesOfFirst.insert(canSuper);
  }

  // Look through the superclasses of second to determine if any were also
  // superclasses of first.
  for (Type super = second; super; super = super->getSuperclass()) {
    auto canSuper = super->getCanonicalType();

    // If we found the first type, we're done.
    if (superclassesOfFirst.count(canSuper))
      return canSuper;
  }

  // There is no common superclass; we're done.
  return first->getASTContext().TheAnyType;
}

CanType TypeJoin::visitClassType(CanType second) {
  return getSuperclassJoin(First, second);
}

CanType TypeJoin::visitBoundGenericClassType(CanType second) {
  return getSuperclassJoin(First, second);
}

CanType TypeJoin::visitArchetypeType(CanType second) {
  return getSuperclassJoin(First, second);
}

CanType TypeJoin::visitDynamicSelfType(CanType second) {
  return getSuperclassJoin(First, second);
}

CanType TypeJoin::visitMetatypeType(CanType second) {
  if (First->getKind() != second->getKind())
    return First->getASTContext().TheAnyType;

  auto firstInstance =
      First->castTo<AnyMetatypeType>()->getInstanceType()->getCanonicalType();
  auto secondInstance =
      second->castTo<AnyMetatypeType>()->getInstanceType()->getCanonicalType();

  auto joinInstance = join(firstInstance, secondInstance);

  if (!joinInstance)
    return First->getASTContext().TheAnyType;

  return MetatypeType::get(joinInstance)->getCanonicalType();
}

CanType TypeJoin::visitExistentialMetatypeType(CanType second) {
  if (First->getKind() != second->getKind())
    return First->getASTContext().TheAnyType;

  auto firstInstance =
      First->castTo<AnyMetatypeType>()->getInstanceType()->getCanonicalType();
  auto secondInstance =
      second->castTo<AnyMetatypeType>()->getInstanceType()->getCanonicalType();

  auto joinInstance = join(firstInstance, secondInstance);

  if (!joinInstance)
    return First->getASTContext().TheAnyType;

  return ExistentialMetatypeType::get(joinInstance)->getCanonicalType();
}

CanType TypeJoin::visitBoundGenericEnumType(CanType second) {
  if (First->getKind() != second->getKind())
    return First->getASTContext().TheAnyType;

  OptionalTypeKind otk1, otk2;
  auto firstObject = First->getAnyOptionalObjectType(otk1);
  auto secondObject = second->getAnyOptionalObjectType(otk2);
  if (otk1 == OTK_Optional || otk2 == OTK_Optional) {
    auto canFirst = firstObject->getCanonicalType();
    auto canSecond = secondObject->getCanonicalType();

    // Compute the join of the unwrapped type. If there is none, we're done.
    auto unwrappedJoin =
        join(canFirst ? canFirst : First, canSecond ? canSecond : second);
    // FIXME: More general joins of enums need to be handled.
    if (!unwrappedJoin)
      return First->getASTContext().TheAnyType;

    return OptionalType::get(unwrappedJoin)->getCanonicalType();
  }

  // FIXME: More general joins of enums need to be handled.
  return First->getASTContext().TheAnyType;
}

Type Type::join(Type first, Type second) {
  assert(first && second && "Unexpected null type!");

  if (!first || !second) {
    if (first)
      return Type(ErrorType::get(first->getASTContext()));

    if (second)
      return Type(ErrorType::get(second->getASTContext()));

    return Type();
  }

  // FIXME: Remove this once all of the cases are implemented.
  // If one or both of the types are optional types,
  // look at the underlying object type.
  OptionalTypeKind firstKind, secondKind;
  Type objectType1 = first->getAnyOptionalObjectType(firstKind);
  Type objectType2 = second->getAnyOptionalObjectType(secondKind);

  if (firstKind != OptionalTypeKind::OTK_None ||
      secondKind != OptionalTypeKind::OTK_None) {
    auto &ctx = first->getASTContext();
    // Compute the join of the unwrapped type. If there is none, we're done.
    Type unwrappedJoin = join(objectType1 ? objectType1 : first,
                              objectType2 ? objectType2 : second);

    auto isAnyType = [&](Type candidate) -> bool {
      return candidate && candidate->isEqual(ctx.TheAnyType);
    };

    // If join produced 'Any' but neither type was itself 'Any',
    // let's return empty type to indicate that there is no join.
    if (!unwrappedJoin || (isAnyType(unwrappedJoin) &&
                          !isAnyType(objectType1) && !isAnyType(objectType2)))
        return nullptr;

    return OptionalType::get(unwrappedJoin);
  }

  return TypeJoin::join(first->getCanonicalType(), second->getCanonicalType());
}

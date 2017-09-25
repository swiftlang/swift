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
#include "llvm/ADT/Optional.h"
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

    // Optionals broadly interact with all the other types since
    //   T <: T? for any T.
    // So we'll always attempt to dispatch Optional here rather than
    // make every visitor check for it explicitly.

    // If the second type is an Optional, dispatch to that visitor.
    OptionalTypeKind otk;
    if (second->getAnyOptionalObjectType(otk))
      return TypeJoin(first).visit(second);

    // Otherwise the first type might be an optional (or not), so
    // dispatch there.
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
  return ExistentialMetatypeType::get(joinInstance)->getCanonicalType();
}

/// We'll define the subtype relationships of Optionals as follows:
///   S  <: S?
///   S? <: T? if S <: T (covariant)
///   S! <: S
///   S! <: T! if S <: T (covariant)
static Optional<CanType> joinOptional(CanType first, CanType second) {
  OptionalTypeKind firstKind, secondKind;
  auto firstObject = first->getAnyOptionalObjectType(firstKind);
  auto secondObject = second->getAnyOptionalObjectType(secondKind);

  // If neither is any kind of Optional, we're done.
  if (firstKind == OTK_None && secondKind == OTK_None)
    return None;

  CanType canFirst = (firstObject ? firstObject : first)->getCanonicalType();
  CanType canSecond =
      (secondObject ? secondObject : second)->getCanonicalType();

  auto joined = TypeJoin::join(canFirst, canSecond);

  // If either is a plain Optional, the result is a plain Optional.
  if (firstKind == OTK_Optional || secondKind == OTK_Optional)
    return OptionalType::get(joined)->getCanonicalType();

  assert((firstKind == OTK_ImplicitlyUnwrappedOptional ||
          secondKind == OTK_ImplicitlyUnwrappedOptional) &&
         "Expected an implicitly unwrapped optional type!");

  if (firstKind == OTK_ImplicitlyUnwrappedOptional &&
      secondKind == OTK_ImplicitlyUnwrappedOptional)
    return ImplicitlyUnwrappedOptionalType::get(joined)->getCanonicalType();

  return joined;
}

CanType TypeJoin::visitBoundGenericEnumType(CanType second) {
  // Deal with either First or second (or both) being optionals.
  if (auto joined = joinOptional(First, second))
    return joined.getValue();

  if (First->getKind() != second->getKind())
    return First->getASTContext().TheAnyType;

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

  return TypeJoin::join(first->getCanonicalType(), second->getCanonicalType());
}

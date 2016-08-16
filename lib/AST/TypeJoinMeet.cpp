//===--- TypeJoinMeet.cpp - Swift Type "Join" and "Meet"  -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the "meet" operation for types (and, eventually,
//  "join").
//
//===----------------------------------------------------------------------===//
#include "swift/AST/ASTContext.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/SmallPtrSet.h"
using namespace swift;

Type Type::meet(Type type1, Type type2) {
  assert(!type1->hasTypeVariable() && !type2->hasTypeVariable() &&
         "Cannot compute meet of types involving type variables");

  // FIXME: This algorithm is woefully incomplete, and is only currently used
  // for optimizing away extra exploratory work in the constraint solver. It
  // should eventually encompass all of the subtyping rules of the language.

  // If the types are equivalent, the meet is obvious.
  if (type1->isEqual(type2))
    return type1;

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

  // The meet can only be an existential.
  return nullptr;
}


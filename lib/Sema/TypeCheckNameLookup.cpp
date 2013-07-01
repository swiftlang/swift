//===--- TypeCheckNameLookup.cpp - Type Checker Name Lookup ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements name lookup within the type checker, which can
// involve additional type-checking operations and the implicit
// declaration of members (such as constructors).
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "swift/AST/NameLookup.h"
#include <algorithm>

using namespace swift;

LookupResult TypeChecker::lookupMember(Type type, Identifier name) {
  LookupResult result;
  unsigned options = NL_Default;
  
  // We can't have tuple types here; they need to be handled elsewhere.
  assert(!type->is<TupleType>());

  // Constructor lookup is special.
  // FIXME: string comparison here is lame.
  if (name.str().equals("constructor")) {
    // We only have constructors for nominal declarations.
    auto nominalDecl = type->getAnyNominal();
    if (!nominalDecl)
      return result;

    // Define implicit default constructor for a struct.
    if (auto structDecl = dyn_cast<StructDecl>(nominalDecl)) {
      if (structsNeedingImplicitDefaultConstructor.count(structDecl)) {
        defineDefaultConstructor(structDecl);
      }
    }

    // If we're looking for constructors in a oneof, return the oneof
    // elements.
    // FIXME: This feels like a hack.
    if (auto oneofDecl = dyn_cast<OneOfDecl>(nominalDecl)) {
      for (auto member : oneofDecl->getMembers()) {
        if (auto element = dyn_cast<OneOfElementDecl>(member))
          result.addResult(element);
      }
    }

    // Fall through to look for constructors via the normal means.
    options = NL_Constructor;
  }

  // Look for the member.
  if (!TU.lookupQualified(type, name, options, result.Results))
    return result;

  return result;
}

LookupTypeResult TypeChecker::lookupMemberType(Type type, Identifier name) {
  LookupTypeResult result;

  // Look for members with the given name.
  SmallVector<ValueDecl *, 4> decls;
  if (!TU.lookupQualified(type, name, NL_Default, decls))
    return result;

  // Look through the declarations, keeping only the unique type declarations.
  llvm::SmallPtrSet<CanType, 4> types;
  for (auto decl : decls){
    // Ignore non-types found by name lookup.
    auto typeDecl = dyn_cast<TypeDecl>(decl);
    if (!typeDecl)
      continue;

    // If there are any type variables in the base type, don't substitute.
    // FIXME: This feels like a hack.
    if (type->hasTypeVariable()) {
      result.Results.push_back({typeDecl, typeDecl->getDeclaredType()});
      continue;
    }

    // Substitute the the base into the member's type.
    auto memberType = substMemberTypeWithBase(typeDecl->getDeclaredType(),
                                              typeDecl, type);
    if (!memberType)
      continue;

    // If we haven't seen this type result yet, add it to the result set.
    if (types.insert(memberType->getCanonicalType()))
      result.Results.push_back({typeDecl, memberType});
  }

  return result;
}

LookupResult TypeChecker::lookupConstructors(Type type) {
  // FIXME: Use of string literal here is lame.
  return lookupMember(type, Context.getIdentifier("constructor"));
}

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

using namespace swift;

LookupResult TypeChecker::lookupMember(Type type, Identifier name,
                                       bool isTypeLookup) {
  LookupResult result;
  bool VisitSuperclasses = true;
  
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
    VisitSuperclasses = false;
  }

  // Look for the member.
  MemberLookup lookup(type, name, TU, isTypeLookup, VisitSuperclasses);
  for (const auto &res : lookup.Results) {
    if (res.D)
      result.addResult(res.D);
  }
  return result;
}

LookupResult TypeChecker::lookupConstructors(Type type) {
  // FIXME: Use of string literal here is lame.
  return lookupMember(type, Context.getIdentifier("constructor"), false);
}

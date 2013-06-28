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
  
  // We can't have tuple types here; they need to be handled elsewhere.
  assert(!type->is<TupleType>());

  // Constructor lookup is special (and simpler than normal member lookup).
  // FIXME: string comparison here is lame.
  if (name.str().equals("constructor")) {
    // If we're looking for a constructor in a struct that needs an
    // implicit default constructor to be defined, define it now.
    if (auto nominalDecl = type->getNominalOrBoundGenericNominal()) {
      if (auto structDecl = dyn_cast<StructDecl>(nominalDecl)) {
        if (structsNeedingImplicitDefaultConstructor.count(structDecl)) {
          defineDefaultConstructor(structDecl);
        }
      }
    }

    ConstructorLookup lookup(type, TU);
    for (auto res : lookup.Results) {
      result.addResult(res);
    }
    return result;
  }

  // Look for the member.
  MemberLookup lookup(type, name, TU, isTypeLookup);
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

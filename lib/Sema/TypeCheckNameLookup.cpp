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

void LookupResult::filter(const std::function<bool(ValueDecl *)> &pred) {
  Results.erase(std::remove_if(Results.begin(), Results.end(),
                               [&](ValueDecl *decl) -> bool {
                                 return !pred(decl);
                               }),
                Results.end());
}

LookupResult TypeChecker::lookupMember(Type type, Identifier name,
                                       bool allowDynamicLookup) {
  LookupResult result;
  unsigned options = NL_QualifiedDefault;
  if (allowDynamicLookup)
    options = options | NL_DynamicLookup;
  
  // We can't have tuple types here; they need to be handled elsewhere.
  assert(!type->is<TupleType>());

  // Constructor lookup is special.
  // FIXME: string comparison here is lame.
  if (name.str().equals("init")) {
    // We only have constructors for nominal declarations.
    auto nominalDecl = type->getAnyNominal();
    if (!nominalDecl)
      return result;

    // Define implicit default constructor for a struct/class.
    if (typesNeedingImplicitDefaultConstructor.count(nominalDecl)) {
      defineDefaultConstructor(nominalDecl);
    }

    // If we're looking for constructors in an enum, return the enum
    // elements.
    // FIXME: This feels like a hack.
    if (auto enumDecl = dyn_cast<EnumDecl>(nominalDecl)) {
      for (auto member : enumDecl->getMembers()) {
        if (auto element = dyn_cast<EnumElementDecl>(member))
          result.addResult(element);
      }
    }

    // Fall through to look for constructors via the normal means.
    options = NL_Constructor;
  } else if (name.str().equals("__conversion")) {
    // Conversion lookups never permit dynamic lookup.
    options = options & ~NL_DynamicLookup;
  }

  // Look for the member.
  if (!TU.lookupQualified(type, name, options, this, result.Results))
    return result;

  return result;
}

LookupTypeResult TypeChecker::lookupMemberType(Type type, Identifier name) {
  LookupTypeResult result;

  // Look through the metatype.
  if (auto metaT = type->getAs<MetaTypeType>())
    type = metaT->getInstanceType();

  // Callers must cope with dependent types directly.
  assert(!type->isDependentType());

  // Look for members with the given name.
  SmallVector<ValueDecl *, 4> decls;
  unsigned options = NL_QualifiedDefault | NL_ProtocolMembers;
  if (!TU.lookupQualified(type, name, options, this, decls))
    return result;

  // Look through the declarations, keeping only the unique type declarations.
  llvm::SmallPtrSet<CanType, 4> types;
  for (auto decl : decls) {
    // Ignore non-types found by name lookup.
    auto typeDecl = dyn_cast<TypeDecl>(decl);
    if (!typeDecl)
      continue;

    // FIXME: This should happen before we attempt shadowing checks.
    validateTypeDecl(typeDecl);

    // If there are any type variables in the base type, don't substitute.
    // FIXME: This is a total hack that won't actually work.
    if (type->hasTypeVariable()) {
      result.Results.push_back({typeDecl, typeDecl->getDeclaredType()});
      continue;
    }

    Type memberType;

    // If we found a member of a protocol type when looking into a non-protocol,
    // non-archetype type, only include this member in the result set if
    // this member was used as the default definition or otherwise inferred.
    if (auto protocol = dyn_cast<ProtocolDecl>(typeDecl->getDeclContext())) {
      if (!type->is<ArchetypeType>() && !type->isExistentialType()) {
        // If the type does not actually conform to the protocol, skip this
        // member entirely.
        // FIXME: This is an error path. Should we try to recover?
        ProtocolConformance *conformance = nullptr;
        if (!conformsToProtocol(type, protocol, &conformance) || !conformance)
          continue;

        // Use the type witness.
        auto assocType = cast<AssociatedTypeDecl>(typeDecl);
        memberType = conformance->getTypeWitness(assocType).Replacement;
        assert(memberType && "Missing type witness?");
      }
    }

    // If we didn't assign a member type above, derive it based on the declared
    // type of the declaration.
    if (!memberType) {
      // Substitute the the base into the member's type.
      memberType = substMemberTypeWithBase(typeDecl->getDeclaredType(),
                                           typeDecl, type);
    }
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
  return lookupMember(type, Context.getIdentifier("init"),
                      /*allowDynamicLookup=*/false);
}

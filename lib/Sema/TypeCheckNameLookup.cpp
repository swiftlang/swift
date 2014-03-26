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

LookupResult TypeChecker::lookupMember(Type type, DeclName name,
                                       DeclContext *dc,
                                       bool allowDynamicLookup) {
  LookupResult result;
  unsigned options = NL_QualifiedDefault;
  if (allowDynamicLookup)
    options = options | NL_DynamicLookup;
  
  // We can't have tuple types here; they need to be handled elsewhere.
  assert(!type->is<TupleType>());

  // Conversion lookups never permit dynamic lookup.
  if (name.isSimpleName("__conversion")) {
    options = options & ~NL_DynamicLookup;
  }

  // Look for the member.
  if (!dc->lookupQualified(type, name, options, this, result.Results)) {
    // If we didn't find anything, /and/ this is a nominal type, check to see
    // if any of the nominal's protocols are derivable and contain the
    // name we're looking for. (Note that we are not including extensions
    // here -- default derivation doesn't apply in extensions.)
    bool isMetatype = false;
    NominalTypeDecl *nominalDecl;
    if (auto metatypeType = type->getAs<MetatypeType>()) {
      isMetatype = true;
      nominalDecl = metatypeType->getInstanceType()->getAnyNominal();
    } else {
      nominalDecl = type->getAnyNominal();
    }
    if (!nominalDecl)
      return result;
    
    // Force the creation of any delayed members, to ensure proper member
    // lookup.
    nominalDecl->forceDelayed();

    bool anyChange = false;
    for (auto proto : nominalDecl->getProtocols()) {
      if (!nominalDecl->derivesProtocolConformance(proto))
        continue;

      Type protoType = proto->getDeclaredType();
      if (isMetatype)
        protoType = MetatypeType::get(protoType, Context);

      SmallVector<ValueDecl *, 4> requirements;
      if (dc->lookupQualified(protoType, name, options, this, requirements)) {
        // If the protocol contains the member we're looking for, force the
        // conformance to be derived.
        // FIXME: We don't actually need to emit the definitions here.
        if (conformsToProtocol(nominalDecl->getDeclaredType(), proto, dc))
          anyChange = true;

        // Don't just break out of the loop, though...it's possible (though
        // unlikely) that two protocols with derivable conformances both have
        // functions with the same name but different types.
      }
    }

    if (anyChange)
      dc->lookupQualified(type, name, options, this, result.Results);
  }

  return result;
}

LookupTypeResult TypeChecker::lookupMemberType(Type type, Identifier name,
                                               DeclContext *dc) {
  LookupTypeResult result;

  // Look through the metatype.
  if (auto metaT = type->getAs<MetatypeType>())
    type = metaT->getInstanceType();
  
  // Callers must cope with dependent types directly.  
  assert(!type->is<DependentMemberType>() && !type->is<GenericTypeParamType>());
         
  // Look for members with the given name.
  SmallVector<ValueDecl *, 4> decls;
  unsigned options = NL_QualifiedDefault | NL_ProtocolMembers;
  if (!dc->lookupQualified(type, name, options, this, decls))
    return result;

  // Look through the declarations, keeping only the unique type declarations.
  llvm::SmallPtrSet<CanType, 4> types;
  for (auto decl : decls) {
    // Ignore non-types found by name lookup.
    auto typeDecl = dyn_cast<TypeDecl>(decl);
    if (!typeDecl)
      continue;

    // FIXME: This should happen before we attempt shadowing checks.
    validateDecl(typeDecl);

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
        if (!conformsToProtocol(type, protocol, dc, &conformance) ||
            !conformance || !conformance->isComplete())
          continue;

        // Use the type witness.
        auto assocType = cast<AssociatedTypeDecl>(typeDecl);
        memberType = conformance->getTypeWitness(assocType, this).Replacement;
        assert(memberType && "Missing type witness?");
      }
    }

    // If we didn't assign a member type above, derive it based on the declared
    // type of the declaration.
    if (!memberType) {
      // Substitute the the base into the member's type.
      memberType = substMemberTypeWithBase(dc->getParentModule(),
                                           typeDecl->getDeclaredType(),
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

LookupResult TypeChecker::lookupConstructors(Type type, DeclContext *dc) {
  return lookupMember(type, Context.Id_init,
                      dc, /*allowDynamicLookup=*/false);
}

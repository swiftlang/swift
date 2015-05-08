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

void LookupResult::filter(const std::function<bool(Result)> &pred) {
  Results.erase(std::remove_if(Results.begin(), Results.end(),
                               [&](Result result) -> bool {
                                 return !pred(result);
                               }),
                Results.end());
}

LookupResult TypeChecker::lookupUnqualified(DeclContext *dc, DeclName name,
                                            SourceLoc loc,
                                            NameLookupOptions options) {
  // Determine whether we're searching from a protocol extension.
  bool searchingFromProtoExt = false;
  for (auto outerDC = dc; outerDC; outerDC = outerDC->getParent()) {
    if (auto ext = dyn_cast<ExtensionDecl>(outerDC)) {
      if (ext->getExtendedType()->is<ProtocolType>()) {
        searchingFromProtoExt = true;
        break;
      }
    }
  }

  UnqualifiedLookup lookup(name, dc, this,
                           options.contains(NameLookupFlags::KnownPrivate),
                           loc,
                           options.contains(NameLookupFlags::OnlyTypes),
                           options.contains(NameLookupFlags::ProtocolMembers));

  LookupResult result;
  llvm::SmallDenseMap<std::pair<ValueDecl *, ValueDecl *>, bool, 8> known;
  for (const auto &found : lookup.Results) {
    // Members of protocols or protocol extensions need special
    // treatment.
    ValueDecl *foundDecl = found.getValueDecl();
    DeclContext *foundDC = foundDecl->getDeclContext();
    auto foundProto = foundDC->isProtocolOrProtocolExtensionContext();
    if (foundProto &&
        !(isa<GenericTypeParamDecl>(foundDecl)) &&
        !(isa<FuncDecl>(foundDecl) && cast<FuncDecl>(foundDecl)->isOperator())){
      // Was the declaration we found declared within the protocol itself?
      // (Otherwise, it was is an extension of the protocol).
      bool foundInProto = isa<ProtocolDecl>(foundDC);
      bool foundInProtoExt = !foundInProto;

      // Determine the nominal type through which we found the
      // declaration.
      NominalTypeDecl *baseNominal;
      if (auto baseParam = dyn_cast<ParamDecl>(found.getBaseDecl())) {
        auto baseDC = baseParam->getDeclContext();
        if (isa<AbstractFunctionDecl>(baseDC))
          baseDC = baseDC->getParent();

        baseNominal = baseDC->isNominalTypeOrNominalTypeExtensionContext();
        assert(baseNominal && "Did not find nominal type");
      } else {
        baseNominal = cast<NominalTypeDecl>(found.getBaseDecl());
      }

      // If we found something within the protocol itself, and our
      // search began somewhere that is not in a protocol or extension
      // thereof, remap this declaration to the witness.
      if (foundInProto && !isa<ProtocolDecl>(baseNominal)) {
        ConformanceCheckOptions conformanceOptions;
        if (options.contains(NameLookupFlags::KnownPrivate))
          conformanceOptions |= ConformanceCheckFlags::InExpression;
        ProtocolConformance *conformance = nullptr;
        ValueDecl *witness = nullptr;
        if (conformsToProtocol(baseNominal->getDeclaredType(),
                               foundProto, dc, conformanceOptions,
                               &conformance)) {
          if (auto assocType = dyn_cast<AssociatedTypeDecl>(foundDecl)) {
            witness = conformance->getTypeWitnessSubstAndDecl(assocType, this)
                        .second;
          } else {
            witness = conformance->getWitness(foundDecl, this).getDecl();
          }

          if (witness &&
              known.insert({{witness, found.getBaseDecl()}, false}).second)
            result.add({witness, found.getBaseDecl()});
        }

        continue;
      }

      // If we found something in a protocol extension, and our
      // search began in the same protocol, ignore it.
      if (foundInProtoExt && baseNominal == foundProto &&
          !searchingFromProtoExt)
        continue;

      // Otherwise, we either found something in a protocol
      // extension that can be used directly, or we found a protocol
      // requirement where we can use it directly.
    }

    if (known.insert({{found.getValueDecl(), found.getBaseDecl()}, false})
          .second)
      result.add({found.getValueDecl(), found.getBaseDecl()});
  }
  return result;
}

LookupResult TypeChecker::lookupMember(DeclContext *dc,
                                       Type type, DeclName name,
                                       NameLookupOptions options) {
  LookupResult result;
  unsigned subOptions = NL_QualifiedDefault;
  if (options.contains(NameLookupFlags::KnownPrivate))
    subOptions |= NL_KnownNonCascadingDependency;
  if (options.contains(NameLookupFlags::DynamicLookup))
    subOptions |= NL_DynamicLookup;

  // Dig out the type that we'll actually be looking into, and determine
  // whether it is a nominal type.
  Type lookupType = type;
  bool isMetatype = false;
  if (auto lvalueType = lookupType->getAs<LValueType>()) {
    lookupType = lvalueType->getObjectType();
  }
  if (auto metaType = lookupType->getAs<MetatypeType>()) {
    isMetatype = true;
    lookupType = metaType->getInstanceType();
  }
  NominalTypeDecl *nominalLookupType = lookupType->getAnyNominal();

  /// Whether to consider protocol members or not.
  bool considerProtocolMembers
    = nominalLookupType && !isa<ProtocolDecl>(nominalLookupType);
  if (considerProtocolMembers)
    subOptions |= NL_ProtocolMembers;

  // We can't have tuple types here; they need to be handled elsewhere.
  assert(!type->is<TupleType>());

  // Local function that performs lookup.
  SmallVector<ValueDecl *, 4> protocolMembers;
  auto doLookup = [&]() {
    result.clear();

    SmallVector<ValueDecl *, 4> lookupResults;
    dc->lookupQualified(type, name, subOptions, this, lookupResults);
    for (auto value : lookupResults) {
      // If requested, separate out the protocol members.
      if (considerProtocolMembers &&
          isa<ProtocolDecl>(value->getDeclContext())) {
        protocolMembers.push_back(value);
        continue;
      }

      NominalTypeDecl *owner
        = value->getDeclContext()->isNominalTypeOrNominalTypeExtensionContext();
      result.add({value, owner});
    }
  };

  doLookup();

  if (result.empty()) {
    // If we didn't find anything, /and/ this is a nominal type, check to see
    // if any of the nominal's protocols are derivable and contain the
    // name we're looking for. (Note that we are not including extensions
    // here -- default derivation doesn't apply in extensions.)
    if (!nominalLookupType)
      return result;
    
    // Force the creation of any delayed members, to ensure proper member
    // lookup.
    this->forceExternalDeclMembers(nominalLookupType);

    ConformanceCheckOptions conformanceOptions;
    if (options.contains(NameLookupFlags::KnownPrivate))
      conformanceOptions |= ConformanceCheckFlags::InExpression;

    for (auto member : protocolMembers) {
      auto proto = cast<ProtocolDecl>(member->getDeclContext());
      ProtocolConformance *conformance = nullptr;
      if (conformsToProtocol(type, proto, dc, conformanceOptions,
                             &conformance) &&
          conformance) {
        // FIXME: Just swap in this result, once
        // forceExternalDeclMembers() is dead.
        (void)conformance->getWitness(member, this);
      }
    }

    // Perform the lookup again.
    // FIXME: This is only because forceExternalDeclMembers() might do something
    // interesting.
    doLookup();
  }

  return result;
}

LookupTypeResult TypeChecker::lookupMemberType(DeclContext *dc,
                                               Type type, Identifier name,
                                               NameLookupOptions options) {
  LookupTypeResult result;

  // Look through an inout type.
  if (auto inout = type->getAs<InOutType>())
    type = inout->getObjectType();

  // Look through the metatype.
  if (auto metaT = type->getAs<AnyMetatypeType>())
    type = metaT->getInstanceType();
  
  // Callers must cope with dependent types directly.  
  assert(!type->is<DependentMemberType>() && !type->is<GenericTypeParamType>());
         
  // Look for members with the given name.
  SmallVector<ValueDecl *, 4> decls;
  unsigned subOptions = NL_QualifiedDefault;
  if (options.contains(NameLookupFlags::KnownPrivate))
    subOptions |= NL_KnownNonCascadingDependency;
  if (options.contains(NameLookupFlags::ProtocolMembers))
    subOptions |= NL_ProtocolMembers;    

  if (!dc->lookupQualified(type, name, subOptions, this, decls))
    return result;

  // Look through the declarations, keeping only the unique type declarations.
  llvm::SmallPtrSet<CanType, 4> types;
  SmallVector<AssociatedTypeDecl *, 4> inferredAssociatedTypes;
  for (auto decl : decls) {
    // Ignore non-types found by name lookup.
    auto typeDecl = dyn_cast<TypeDecl>(decl);
    if (!typeDecl)
      continue;

    // FIXME: This should happen before we attempt shadowing checks.
    validateDecl(typeDecl);

    // If we found a member of a protocol type when looking into a non-protocol,
    // non-archetype type, only include this member in the result set if
    // this member was used as the default definition or otherwise inferred.
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(typeDecl)) {
      if (!type->is<ArchetypeType>() && !type->isExistentialType()) {
        inferredAssociatedTypes.push_back(assocType);
        continue;
      }
    }

    // Substitute the the base into the member's type.
    if (Type memberType = substMemberTypeWithBase(dc->getParentModule(),
                                                  typeDecl, type,
                                                  /*isTypeReference=*/true)) {
      // If we haven't seen this type result yet, add it to the result set.
      if (types.insert(memberType->getCanonicalType()).second)
        result.Results.push_back({typeDecl, memberType});
    }
  }

  if (result.Results.empty()) {
    // We couldn't find any normal declarations. Let's try inferring
    // associated types.
    ConformanceCheckOptions conformanceOptions;
    if (options.contains(NameLookupFlags::KnownPrivate))
      conformanceOptions |= ConformanceCheckFlags::InExpression;

    for (AssociatedTypeDecl *assocType : inferredAssociatedTypes) {
      // If the type does not actually conform to the protocol, skip this
      // member entirely.
      // FIXME: The "isComplete()" check here is bogus. It's entirely possible
      // that we're in the middle of checking this protocol and just need a
      // particular witness.
      auto *protocol = cast<ProtocolDecl>(assocType->getDeclContext());
      ProtocolConformance *conformance = nullptr;
      if (!conformsToProtocol(type, protocol, dc, conformanceOptions,
                              &conformance) ||
          !conformance) {
        // FIXME: This is an error path. Should we try to recover?
        continue;
      }

      // Use the type witness.
      Type memberType =
        conformance->getTypeWitness(assocType, this).getReplacement();
      assert(memberType && "Missing type witness?");

      // If we haven't seen this type result yet, add it to the result set.
      if (types.insert(memberType->getCanonicalType()).second)
        result.Results.push_back({assocType, memberType});
    }
  }

  return result;
}

LookupResult TypeChecker::lookupConstructors(DeclContext *dc, Type type,
                                             NameLookupOptions options) {
  return lookupMember(dc, type, Context.Id_init, options);
}

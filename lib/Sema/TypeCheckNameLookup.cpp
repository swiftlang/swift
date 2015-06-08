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

namespace {
  /// Builder that helps construct a lookup result from the raw lookup
  /// data.
  class LookupResultBuilder {
    TypeChecker &TC;
    LookupResult &Result;
    DeclContext *DC;
    NameLookupOptions Options;
    bool ConsiderProtocolMembers;
    bool SearchingFromProtoExt = false;

    /// The vector of found declarations.
    SmallVector<ValueDecl *, 4> FoundDecls;

    /// The set of known declarations.
    llvm::SmallDenseMap<std::pair<ValueDecl *, ValueDecl *>, bool, 4> Known;

  public:
    LookupResultBuilder(TypeChecker &tc, LookupResult &result, DeclContext *dc,
                        NameLookupOptions options, bool considerProtocolMembers,
                        bool searchingFromProtoExt)
      : TC(tc), Result(result), DC(dc), Options(options),
        ConsiderProtocolMembers(considerProtocolMembers),
        SearchingFromProtoExt(searchingFromProtoExt) { }

    ~LookupResultBuilder() {
      // If any of the results have a base, we need to remove
      // overridden and shadowed declarations.
      // FIXME: We should *always* remove overridden and shadowed declarations,
      // but there are weird assumptions about the results of unqualified
      // name lookup, e.g., that a local variable not having a type indicates
      // that it hasn't been seen yet.
      if (std::find_if(Result.begin(), Result.end(),
                       [](const LookupResult::Result &found) {
                         return found.Base != nullptr;
                       }) == Result.end())
        return;

      bool anyRemoved = false;

      // Remove any overridden declarations from the found-declarations set.
      if (removeOverriddenDecls(FoundDecls))
        anyRemoved = true;

      // Remove any shadowed declarations from the found-declarations set.
      if (removeShadowedDecls(FoundDecls, DC->getParentModule(), &TC))
        anyRemoved = true;

      // Filter out those results that have been removed from the
      // found-declarations set.
      unsigned foundIdx = 0, foundSize = FoundDecls.size();
      Result.filter([&](LookupResult::Result result) -> bool {
          // If the current result matches the remaining found declaration,
          // keep it and move to the next found declaration.
          if (foundIdx < foundSize && result.Decl == FoundDecls[foundIdx]) {
            ++foundIdx;
            return true;
          }

          // Otherwise, this result should be filtered out.
          return false;
        });
    }

    /// Add a new result.
    ///
    /// \param found The declaration we found.
    ///
    /// \param base The base declaration through which we found the
    /// declaration.
    ///
    /// \param foundInType The type through which we found the
    /// declaration.
    void add(ValueDecl *found, ValueDecl *base, Type foundInType) {
      // If we only want types and we didn't get one, bail out.
      if (Options.contains(NameLookupFlags::OnlyTypes) && !isa<TypeDecl>(found))
        return;

      ConformanceCheckOptions conformanceOptions;
      if (Options.contains(NameLookupFlags::KnownPrivate))
        conformanceOptions |= ConformanceCheckFlags::InExpression;

      DeclContext *foundDC = found->getDeclContext();
      auto foundProto = foundDC->isProtocolOrProtocolExtensionContext();

      // Determine the nominal type through which we found the
      // declaration.
      NominalTypeDecl *baseNominal = nullptr;
      if (!base) {
        // Nothing to do.
      } else if (auto baseParam = dyn_cast<ParamDecl>(base)) {
        auto baseDC = baseParam->getDeclContext();
        if (isa<AbstractFunctionDecl>(baseDC))
          baseDC = baseDC->getParent();

        baseNominal = baseDC->isNominalTypeOrNominalTypeExtensionContext();
        assert(baseNominal && "Did not find nominal type");
      } else {
        baseNominal = cast<NominalTypeDecl>(base);
      }

      // If this isn't a protocol member to be given special
      // treatment, just add the result.
      if (!ConsiderProtocolMembers ||
          !isa<ProtocolDecl>(found->getDeclContext()) ||
          SearchingFromProtoExt ||
          isa<GenericTypeParamDecl>(found) ||
          (isa<FuncDecl>(found) && cast<FuncDecl>(found)->isOperator())) {
        if (Known.insert({{found, base}, false}).second) {
          Result.add({found, base});
          FoundDecls.push_back(found);
        }
        return;
      }

      // If we found something within the protocol itself, and our
      // search began somewhere that is not in a protocol or extension
      // thereof, remap this declaration to the witness.
      if (isa<ProtocolDecl>(foundDC) && !isa<ProtocolDecl>(baseNominal)) {
        // Dig out the protocol conformance.
        ProtocolConformance *conformance = nullptr;
        if (!TC.conformsToProtocol(foundInType, foundProto, DC,
                                   conformanceOptions, &conformance) ||
            !conformance)
          return;

        // Dig out the witness.
        ValueDecl *witness;
        if (auto assocType = dyn_cast<AssociatedTypeDecl>(found)) {
          witness = conformance->getTypeWitnessSubstAndDecl(assocType, &TC)
            .second;
        } else {
          witness = conformance->getWitness(found, &TC).getDecl();
        }

        // FIXME: the "isa<ProtocolDecl>()" check will be wrong for
        // default implementations in protocols.
        if (witness && !isa<ProtocolDecl>(witness->getDeclContext())) {
          if (Known.insert({{witness, base}, false}).second) {
            Result.add({witness, base});
            FoundDecls.push_back(witness);
          }
        }
        return;
      }
    }
  };
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
  bool considerProtocolMembers
    = options.contains(NameLookupFlags::ProtocolMembers);
  LookupResultBuilder builder(*this, result, dc, options,
                              considerProtocolMembers,
                              searchingFromProtoExt);
  for (const auto &found : lookup.Results) {
    // Determine which type we looked through to find this result.
    Type foundInType;
    if (!found.getBaseDecl()) {
      // Not found within a type.
    } else if (auto baseParam = dyn_cast<ParamDecl>(found.getBaseDecl())) {
      auto baseDC = baseParam->getDeclContext();
      if (isa<AbstractFunctionDecl>(baseDC))
        baseDC = baseDC->getParent();
      foundInType = baseDC->getDeclaredTypeInContext();
    } else {
      auto baseNominal = cast<NominalTypeDecl>(found.getBaseDecl());
      for (auto currentDC = dc; currentDC; currentDC = currentDC->getParent()) {
        if (currentDC->isNominalTypeOrNominalTypeExtensionContext()
              == baseNominal) {
          foundInType = currentDC->getDeclaredTypeInContext();
        }
      }
      assert(foundInType && "bogus base declaration?");
    }

    builder.add(found.getValueDecl(), found.getBaseDecl(), foundInType);
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
  if (auto lvalueType = lookupType->getAs<LValueType>()) {
    lookupType = lvalueType->getObjectType();
  }
  if (auto metaType = lookupType->getAs<MetatypeType>()) {
    lookupType = metaType->getInstanceType();
  }
  NominalTypeDecl *nominalLookupType = lookupType->getAnyNominal();

  /// Whether to consider protocol members or not.
  bool considerProtocolMembers
    = nominalLookupType && !isa<ProtocolDecl>(nominalLookupType) &&
      options.contains(NameLookupFlags::ProtocolMembers);
  if (considerProtocolMembers)
    subOptions |= NL_ProtocolMembers;

  // We handle our own overriding/shadowing filtering.
  subOptions &= ~NL_RemoveOverridden;
  subOptions &= ~NL_RemoveNonVisible;

  // We can't have tuple types here; they need to be handled elsewhere.
  assert(!type->is<TupleType>());

  // Local function that performs lookup.
  auto doLookup = [&]() {
    result.clear();

    LookupResultBuilder builder(*this, result, dc, options,
                                considerProtocolMembers,
                                false);
    SmallVector<ValueDecl *, 4> lookupResults;
    dc->lookupQualified(type, name, subOptions, this, lookupResults);

    for (auto found : lookupResults) {
      builder.add(found, nominalLookupType, type);
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

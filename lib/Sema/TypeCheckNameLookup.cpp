//===--- TypeCheckNameLookup.cpp - Type Checker Name Lookup ---------------===//
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
// This file implements name lookup within the type checker, which can
// involve additional type-checking operations and the implicit
// declaration of members (such as constructors).
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/TopCollection.h"
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
    bool IsMemberLookup;

    /// The vector of found declarations.
    SmallVector<ValueDecl *, 4> FoundDecls;

    /// The set of known declarations.
    llvm::SmallDenseMap<std::pair<ValueDecl *, ValueDecl *>, bool, 4> Known;

  public:
    LookupResultBuilder(TypeChecker &tc, LookupResult &result, DeclContext *dc,
                        NameLookupOptions options,
                        bool isMemberLookup)
      : TC(tc), Result(result), DC(dc), Options(options),
        IsMemberLookup(isMemberLookup) { }

    ~LookupResultBuilder() {
      // If any of the results have a base, we need to remove
      // overridden and shadowed declarations.
      // FIXME: We should *always* remove overridden and shadowed declarations,
      // but there are weird assumptions about the results of unqualified
      // name lookup, e.g., that a local variable not having a type indicates
      // that it hasn't been seen yet.
      if (!IsMemberLookup &&
          std::find_if(Result.begin(), Result.end(),
                       [](const LookupResult::Result &found) {
                         return found.Base != nullptr;
                       }) == Result.end())
        return;

      // Remove any overridden declarations from the found-declarations set.
      removeOverriddenDecls(FoundDecls);

      // Remove any shadowed declarations from the found-declarations set.
      removeShadowedDecls(FoundDecls, DC->getParentModule(), &TC);

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
      ConformanceCheckOptions conformanceOptions;
      if (Options.contains(NameLookupFlags::KnownPrivate))
        conformanceOptions |= ConformanceCheckFlags::InExpression;

      DeclContext *foundDC = found->getDeclContext();
      auto foundProto = foundDC->getAsProtocolOrProtocolExtensionContext();

      auto addResult = [&](ValueDecl *result) {
        if (Known.insert({{result, base}, false}).second) {
          Result.add({result, base});
          FoundDecls.push_back(result);
        }
      };

      // If this isn't a protocol member to be given special
      // treatment, just add the result.
      if (!Options.contains(NameLookupFlags::ProtocolMembers) ||
          !isa<ProtocolDecl>(foundDC) ||
          isa<GenericTypeParamDecl>(found) ||
          (isa<FuncDecl>(found) && cast<FuncDecl>(found)->isOperator()) ||
          foundInType->isAnyExistentialType()) {
        addResult(found);
        return;
      }

      assert(isa<ProtocolDecl>(foundDC));

      // If we found something within the protocol itself, and our
      // search began somewhere that is not in a protocol or extension
      // thereof, remap this declaration to the witness.
      if (foundInType->is<ArchetypeType>() ||
          Options.contains(NameLookupFlags::PerformConformanceCheck)) {
        // Dig out the protocol conformance.
        auto conformance = TC.conformsToProtocol(foundInType, foundProto, DC,
                                                 conformanceOptions);
        if (!conformance)
          return;

        if (conformance->isAbstract()) {
          assert(foundInType->is<ArchetypeType>());
          addResult(found);
          return;
        }

        // If we're validating the protocol recursively, bail out.
        if (!foundProto->hasValidSignature())
          return;

        // Dig out the witness.
        ValueDecl *witness = nullptr;
        auto concrete = conformance->getConcrete();
        if (auto assocType = dyn_cast<AssociatedTypeDecl>(found)) {
          witness = concrete->getTypeWitnessAndDecl(assocType, &TC)
            .second;
        } else if (found->isProtocolRequirement()) {
          witness = concrete->getWitnessDecl(found, &TC);
        }

        // FIXME: the "isa<ProtocolDecl>()" check will be wrong for
        // default implementations in protocols.
        if (witness && !isa<ProtocolDecl>(witness->getDeclContext()))
          addResult(witness);

        return;
      }
    }
  };
} // end anonymous namespace

LookupResult TypeChecker::lookupUnqualified(DeclContext *dc, DeclName name,
                                            SourceLoc loc,
                                            NameLookupOptions options) {
  UnqualifiedLookup lookup(
      name, dc, this,
      options.contains(NameLookupFlags::KnownPrivate),
      loc,
      /*IsTypeLookup=*/false,
      options.contains(NameLookupFlags::ProtocolMembers),
      options.contains(NameLookupFlags::IgnoreAccessibility));

  LookupResult result;
  LookupResultBuilder builder(*this, result, dc, options,
                              /*memberLookup*/false);
  for (const auto &found : lookup.Results) {
    // Determine which type we looked through to find this result.
    Type foundInType;

    if (!found.getBaseDecl()) {
      // Not found within a type.
    } else {
      DeclContext *baseDC = nullptr;
      if (auto baseParam = dyn_cast<ParamDecl>(found.getBaseDecl())) {
        baseDC = baseParam->getDeclContext()->getParent();
      } else {
        baseDC = cast<NominalTypeDecl>(found.getBaseDecl());
      }

      foundInType = dc->mapTypeIntoContext(
        baseDC->getDeclaredInterfaceType());
      assert(foundInType && "bogus base declaration?");
    }

    builder.add(found.getValueDecl(), found.getBaseDecl(), foundInType);
  }
  return result;
}

SmallVector<TypeDecl *, 1>
TypeChecker::lookupUnqualifiedType(DeclContext *dc, DeclName name,
                                   SourceLoc loc,
                                   NameLookupOptions options) {
  SmallVector<TypeDecl *, 1> decls;

  // Try lookup without ProtocolMembers first.
  UnqualifiedLookup lookup(
      name, dc, this,
      options.contains(NameLookupFlags::KnownPrivate),
      loc,
      /*IsTypeLookup=*/true,
      /*AllowProtocolMembers=*/false,
      options.contains(NameLookupFlags::IgnoreAccessibility));
  for (auto found : lookup.Results)
    decls.push_back(cast<TypeDecl>(found.getValueDecl()));

  if (decls.empty() &&
      options.contains(NameLookupFlags::ProtocolMembers)) {
    // Try again, this time with protocol members.
    //
    // FIXME: Fix the problem where if NominalTypeDecl::getAllProtocols()
    // is called too early, we start resolving extensions -- even those
    // which do provide conformances.
    UnqualifiedLookup lookup(
        name, dc, this,
        options.contains(NameLookupFlags::KnownPrivate),
        loc,
        /*IsTypeLookup=*/true,
        /*AllowProtocolMembers=*/true,
        options.contains(NameLookupFlags::IgnoreAccessibility));

    for (auto found : lookup.Results)
      decls.push_back(cast<TypeDecl>(found.getValueDecl()));
  }

  // If we're looking for a type named `CodingKeys`, then we've got some
  // additional work to do here. CodingKeys is a special type: although not a
  // protocol requirement of the Encodable/Decodable protocols, it is
  // synthesized as part of Encodable/Decodable conformance. Unqualified lookup
  // may occur before the type is synthesized, though, in which case, we need to
  // make sure it gets synthesized.
  //
  // The unqualified lookups above, however, cannot force Encodable/Decodable
  // synthesis to occur (since they know nothing about typechecking). Thus, we
  // also need to find the closest qualified type which might contain a
  // CodingKeys type and synthesize it.
  //
  // If this type is "closer" than the closest unqualified type, then we can
  // use it.
  if (name.isSimpleName(Context.Id_CodingKeys)) {
    SmallVector<TypeDecl *, 1> qualifiedDecls;

    auto *currentDC = dc;
    while (currentDC) {
      auto *nominal =
        currentDC->getAsNominalTypeOrNominalTypeExtensionContext();
      if (nominal) {
        // We've found a nominal context to look in. Let's perform a qualified
        // lookup here (which will cause CodingKeys synthesis if possible).
        auto lookupOptions = options | NameLookupFlags::ProtocolMembers;
        auto results = lookupMemberType(nominal,
                                        nominal->getDeclaredInterfaceType(),
                                        Context.Id_CodingKeys, lookupOptions);
        if (results.size() > 0) {
          for (auto &result : results) {
            qualifiedDecls.push_back(result.first);
          }

          break;
        }
      }

      currentDC = currentDC->getParent();
    }

    // The results from the unqualified lookup may take precedence if they are
    // more local than the ones produced from the qualified lookup.
    //
    // If either set of lookup results is empty, then there's no comparison to
    // be done.
    if (decls.empty()) {
      return qualifiedDecls;
    } else if (qualifiedDecls.empty()) {
      return decls;
    }

    // All of the types found in the unqualified lookup share the same locality
    // from here, as do all of the types from the qualified lookup.
    // It's safe to compare the first element of each.
    //
    // To do locality comparison, we look at DeclContexts; if one contains the
    // other, then the contained one is more local.
    auto firstDeclContext = [](decltype(decls.begin()) begin,
                               decltype(decls.end()) end) {
      auto it = std::find_if(begin, end, [](const TypeDecl *decl) {
        return dyn_cast<DeclContext>(decl) != nullptr;
      });

      return it == end ? nullptr : cast<DeclContext>(*it);
    };

    auto unqualifiedDC = firstDeclContext(decls.begin(), decls.end());
    if (!unqualifiedDC) {
      // There wasn't a type in unqualified decls that ended up being a
      // comparable DeclContext. In this case, we fall back to the the
      // unqualified decls as the default.
      return decls;
    }

    auto qualifiedDC = firstDeclContext(qualifiedDecls.begin(), decls.end());
    if (!qualifiedDC) {
      // There wasn't a type in qualified decls that ended up being a comparable
      // DeclContext. In this case, we fall back to the the unqualified decls as
      // the default.
      return decls;
    }

    // If unqualifiedDC and qualifiedDC share a parent DeclContext, then they
    // are equally local. Otherwise, if unqualifiedDC is a child of
    // qualifiedDC's parent context, then it must be more local than
    // qualifiedDC.
    const auto *unqualifiedParentDC = unqualifiedDC->getParent();
    const auto *qualifiedParentDC = qualifiedDC->getParent();
    if (unqualifiedParentDC == qualifiedParentDC) {
      // Both equally local. Fall back to the default of unqualified.
      return decls;
    }

    if (unqualifiedDC->isChildContextOf(qualifiedParentDC)) {
      return decls;
    } else {
      return qualifiedDecls;
    }
  }

  return decls;
}

LookupResult TypeChecker::lookupMember(DeclContext *dc,
                                       Type type, DeclName name,
                                       NameLookupOptions options) {
  assert(type->mayHaveMembers());

  LookupResult result;
  NLOptions subOptions = NL_QualifiedDefault;
  if (options.contains(NameLookupFlags::KnownPrivate))
    subOptions |= NL_KnownNonCascadingDependency;
  if (options.contains(NameLookupFlags::DynamicLookup))
    subOptions |= NL_DynamicLookup;
  if (options.contains(NameLookupFlags::IgnoreAccessibility))
    subOptions |= NL_IgnoreAccessibility;

  NominalTypeDecl *nominalLookupType = type->getAnyNominal();

  if (options.contains(NameLookupFlags::ProtocolMembers))
    subOptions |= NL_ProtocolMembers;

  // We handle our own overriding/shadowing filtering.
  subOptions &= ~NL_RemoveOverridden;
  subOptions &= ~NL_RemoveNonVisible;

  // Local function that performs lookup.
  auto doLookup = [&]() {
    result.clear();

    LookupResultBuilder builder(*this, result, dc, options,
                                /*memberLookup*/true);
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

bool TypeChecker::isUnsupportedMemberTypeAccess(Type type, TypeDecl *typeDecl) {
  auto memberType = typeDecl->getDeclaredInterfaceType();

  // We don't allow lookups of a non-generic typealias of an unbound
  // generic type, because we have no way to model such a type in the
  // AST.
  //
  // For generic typealiases, the typealias itself has an unbound
  // generic form whose parent type can be another unbound generic
  // type.
  //
  // FIXME: Could lift this restriction once we have sugared
  // "member types".
  if (type->is<UnboundGenericType>() &&
      isa<TypeAliasDecl>(typeDecl) &&
      cast<TypeAliasDecl>(typeDecl)->getGenericParams() == nullptr &&
      memberType->hasTypeParameter()) {
    return true;
  }

  if (type->is<UnboundGenericType>() &&
      isa<AssociatedTypeDecl>(typeDecl)) {
    return true;
  }

  // We don't allow lookups of an associated type or typealias of an
  // existential type, because we have no way to represent such types.
  if (typeDecl->getDeclContext()->getAsProtocolOrProtocolExtensionContext()) {
    if (type->isExistentialType() &&
        (isa<TypeAliasDecl>(typeDecl) ||
         isa<AssociatedTypeDecl>(typeDecl))) {
      if (memberType->hasTypeParameter()) {
        return true;
      }
    }
  }

  return false;
}

LookupTypeResult TypeChecker::lookupMemberType(DeclContext *dc,
                                               Type type, Identifier name,
                                               NameLookupOptions options) {
  LookupTypeResult result;

  // Look for members with the given name.
  SmallVector<ValueDecl *, 4> decls;
  NLOptions subOptions = NL_QualifiedDefault | NL_OnlyTypes;

  if (options.contains(NameLookupFlags::KnownPrivate))
    subOptions |= NL_KnownNonCascadingDependency;
  if (options.contains(NameLookupFlags::ProtocolMembers))
    subOptions |= NL_ProtocolMembers;
  if (options.contains(NameLookupFlags::IgnoreAccessibility))
    subOptions |= NL_IgnoreAccessibility;

  auto lookupSuccess = dc->lookupQualified(type, name, subOptions, this, decls);

  // If the requested type is a CodingKeys type on something which may require
  // Encodable/Decodable synthesis, it may not have been synthesized yet. If
  // this is the case, force synthesis and attempt lookup again.
  if (!lookupSuccess && name == Context.Id_CodingKeys) {
    if (auto *targetDecl = type->getAnyNominal()) {
      auto *encodableProto = Context.getProtocol(KnownProtocolKind::Encodable);
      auto *decodableProto = Context.getProtocol(KnownProtocolKind::Decodable);

      ConformanceCheckOptions conformanceOptions;
      if (options.contains(NameLookupFlags::KnownPrivate))
        conformanceOptions |= ConformanceCheckFlags::InExpression;

      // If the type either conforms to Encodable/Decodable directly, or
      // inherits from something which does, the conformance check will force
      // synthesis, which will allow us to try again.
      NormalProtocolConformance *conformance = nullptr;
      Optional<ProtocolConformanceRef> conformanceRef;
      if ((conformanceRef = conformsToProtocol(type, decodableProto, targetDecl,
                                               conformanceOptions,
                                               SourceLoc()))) {
        if (auto *concrete = conformanceRef->getConcrete()) {
          conformance = dyn_cast<NormalProtocolConformance>(concrete);
        }
      }

      if (!conformance &&
          (conformanceRef = conformsToProtocol(type, encodableProto, targetDecl,
                                               conformanceOptions,
                                               SourceLoc()))) {
        if (auto *concrete = conformanceRef->getConcrete()) {
          conformance = dyn_cast<NormalProtocolConformance>(concrete);
        }
      }

      if (conformance) {
        // Check conformance, forcing synthesis.
        //
        // If synthesizing conformance fails, this will produce diagnostics.
        // However, these diagnostics are going to be produced again later when
        // protocol conformance is being checked in a separate pass. To prevent
        // this duplication, we'll throw the diagnostics away here.
        auto diagnosticTransaction = DiagnosticTransaction(Context.Diags);
        checkConformance(conformance);
        diagnosticTransaction.abort();

        lookupSuccess = dc->lookupQualified(type, name, subOptions, this, decls);
      }
    }
  }

  if (!lookupSuccess)
    return result;

  // Look through the declarations, keeping only the unique type declarations.
  llvm::SmallPtrSet<CanType, 4> types;
  SmallVector<AssociatedTypeDecl *, 4> inferredAssociatedTypes;
  for (auto decl : decls) {
    auto *typeDecl = cast<TypeDecl>(decl);

    // FIXME: This should happen before we attempt shadowing checks.
    validateDecl(typeDecl);
    if (!typeDecl->hasInterfaceType()) // FIXME: recursion-breaking hack
      continue;

    auto memberType = typeDecl->getDeclaredInterfaceType();

    if (isUnsupportedMemberTypeAccess(type, typeDecl)) {
      // Add the type to the result set, so that we can diagnose the
      // reference instead of just saying the member does not exist.
      if (types.insert(memberType->getCanonicalType()).second)
        result.Results.push_back({typeDecl, memberType});

      continue;
    }

    // If we're looking up an associated type of a concrete type,
    // record it later for conformance checking; we might find a more
    // direct typealias with the same name later.
    if (typeDecl->getDeclContext()->getAsProtocolOrProtocolExtensionContext()) {
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(typeDecl)) {
        if (!type->is<ArchetypeType>() &&
            !type->isTypeParameter()) {
          if (options.contains(NameLookupFlags::PerformConformanceCheck))
            inferredAssociatedTypes.push_back(assocType);
          continue;
        }
      }

      if (isa<TypeAliasDecl>(typeDecl)) {
        if (!type->is<ArchetypeType>() &&
            !type->isTypeParameter() &&
            memberType->hasTypeParameter() &&
            !options.contains(NameLookupFlags::PerformConformanceCheck)) {
          continue;
        }
      }

      // Nominal type members of protocols cannot be accessed with an
      // archetype base, because we have no way to recover the correct
      // substitutions.
      if (type->is<ArchetypeType>() &&
          isa<NominalTypeDecl>(typeDecl)) {
        continue;
      }
    }

    // Substitute the base into the member's type.
    memberType = substMemberTypeWithBase(dc->getParentModule(),
                                         typeDecl, type);

    // If we haven't seen this type result yet, add it to the result set.
    if (types.insert(memberType->getCanonicalType()).second)
      result.Results.push_back({typeDecl, memberType});
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
      auto *protocol = cast<ProtocolDecl>(assocType->getDeclContext());

      // If we're validating the protocol recursively, bail out.
      if (!protocol->hasValidSignature())
        continue;

      auto conformance = conformsToProtocol(type, protocol, dc,
                                            conformanceOptions);
      if (!conformance) {
        // FIXME: This is an error path. Should we try to recover?
        continue;
      }

      // Use the type witness.
      auto concrete = conformance->getConcrete();
      Type memberType = concrete->getTypeWitness(assocType, this);

      // This is the only case where NormalProtocolConformance::
      // getTypeWitnessAndDecl() returns a null type.
      if (concrete->getState() ==
          ProtocolConformanceState::CheckingTypeWitnesses)
        continue;

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

enum : unsigned {
  /// Never consider a candidate that's this distance away or worse.
  UnreasonableCallEditDistance = 8,

  /// Don't consider candidates that score worse than the given distance
  /// from the best candidate.
  MaxCallEditDistanceFromBestCandidate = 1
};

static unsigned getCallEditDistance(DeclName argName, DeclName paramName,
                                    unsigned maxEditDistance) {
  // TODO: consider arguments.
  // TODO: maybe ignore certain kinds of missing / present labels for the
  //   first argument label?
  // TODO: word-based rather than character-based?
  // TODO: Handle special names
  StringRef argBase = argName.getBaseIdentifier().str();
  StringRef paramBase = paramName.getBaseIdentifier().str();

  unsigned distance = argBase.edit_distance(paramBase, maxEditDistance);

  // Bound the distance to UnreasonableCallEditDistance.
  if (distance >= maxEditDistance ||
      distance > (paramBase.size() + 2) / 3) {
    return UnreasonableCallEditDistance;
  }

  return distance;
}

static bool isPlausibleTypo(DeclRefKind refKind, DeclName typedName,
                            ValueDecl *candidate) {
  // Ignore anonymous declarations.
  if (!candidate->hasName())
    return false;

  // An operator / identifier mismatch is never a plausible typo.
  auto fn = dyn_cast<FuncDecl>(candidate);
  if (typedName.isOperator() != (fn && fn->isOperator()))
    return false;
  if (!typedName.isOperator())
    return true;

  // TODO: honor ref kind?  This is trickier than it sounds because we
  // may not have processed attributes and types on the candidate yet.
  return true;
}

static bool isLocInVarInit(TypeChecker &TC, VarDecl *var, SourceLoc loc) {
  auto binding = var->getParentPatternBinding();
  if (!binding || binding->isImplicit())
    return false;

  auto initRange = binding->getSourceRange();
  return TC.Context.SourceMgr.rangeContainsTokenLoc(initRange, loc);
}

namespace {
  class TypoCorrectionResolver : public DelegatingLazyResolver {
    TypeChecker &TC() { return static_cast<TypeChecker&>(Principal); }
    SourceLoc NameLoc;
  public:
    TypoCorrectionResolver(TypeChecker &TC, SourceLoc nameLoc)
      : DelegatingLazyResolver(TC), NameLoc(nameLoc) {}

    void resolveDeclSignature(ValueDecl *VD) override {
      if (VD->isInvalid() || VD->hasInterfaceType()) return;

      // Don't process a variable if we're within its initializer.
      if (auto var = dyn_cast<VarDecl>(VD)) {
        if (isLocInVarInit(TC(), var, NameLoc))
          return;
      }

      DelegatingLazyResolver::resolveDeclSignature(VD);
    }
  };
} // end anonymous namespace

void TypeChecker::performTypoCorrection(DeclContext *DC, DeclRefKind refKind,
                                        Type baseTypeOrNull,
                                        DeclName targetDeclName,
                                        SourceLoc nameLoc,
                                        NameLookupOptions lookupOptions,
                                        LookupResult &result,
                                        GenericSignatureBuilder *gsb,
                                        unsigned maxResults) {
  // Disable typo-correction if we won't show the diagnostic anyway.
  if (getLangOpts().DisableTypoCorrection ||
      (Diags.hasFatalErrorOccurred() &&
       !Diags.getShowDiagnosticsAfterFatalError()))
    return;

  // Fill in a collection of the most reasonable entries.
  TopCollection<unsigned, ValueDecl*> entries(maxResults);
  auto consumer = makeDeclConsumer([&](ValueDecl *decl,
                                       DeclVisibilityKind reason) {
    // Never match an operator with an identifier or vice-versa; this is
    // not a plausible typo.
    if (!isPlausibleTypo(refKind, targetDeclName, decl))
      return;

    // Don't suggest a variable within its own initializer.
    if (auto var = dyn_cast<VarDecl>(decl)) {
      if (isLocInVarInit(*this, var, nameLoc))
        return;
    }

    // Don't waste time computing edit distances that are more than
    // the worst in our collection.
    unsigned maxDistance =
      entries.getMinUninterestingScore(UnreasonableCallEditDistance);

    unsigned distance =
      getCallEditDistance(targetDeclName, decl->getFullName(), maxDistance);

    // Ignore values that are further than a reasonable distance.
    if (distance >= UnreasonableCallEditDistance)
      return;

    entries.insert(distance, std::move(decl));
  });

  TypoCorrectionResolver resolver(*this, nameLoc);
  if (baseTypeOrNull) {
    lookupVisibleMemberDecls(consumer, baseTypeOrNull, DC, &resolver,
                             /*include instance members*/ true, gsb);
  } else {
    lookupVisibleDecls(consumer, DC, &resolver, /*top level*/ true, nameLoc);
  }

  // Impose a maximum distance from the best score.
  entries.filterMaxScoreRange(MaxCallEditDistanceFromBestCandidate);

  for (auto &entry : entries)
    result.add({ entry.Value, nullptr });
}

static InFlightDiagnostic
diagnoseTypoCorrection(TypeChecker &tc, DeclNameLoc loc, ValueDecl *decl) {
  if (auto var = dyn_cast<VarDecl>(decl)) {
    // Suggest 'self' at the use point instead of pointing at the start
    // of the function.
    if (var->isSelfParameter())
      return tc.diagnose(loc.getBaseNameLoc(), diag::note_typo_candidate,
                         var->getName().str());
  }

  if (!decl->getLoc().isValid() && decl->getDeclContext()->isTypeContext()) {
    Decl *parentDecl = dyn_cast<ExtensionDecl>(decl->getDeclContext());
    if (!parentDecl) parentDecl = cast<NominalTypeDecl>(decl->getDeclContext());

    if (parentDecl->getLoc().isValid()) {
      StringRef kind = (isa<VarDecl>(decl) ? "property" :
                        isa<ConstructorDecl>(decl) ? "initializer" :
                        isa<FuncDecl>(decl) ? "method" :
                        "member");

      // TODO: Handle special names
      return tc.diagnose(parentDecl, diag::note_typo_candidate_implicit_member,
                         decl->getBaseName().getIdentifier().str(), kind);
    }
  }

  // TODO: Handle special names
  return tc.diagnose(decl, diag::note_typo_candidate,
                     decl->getBaseName().getIdentifier().str());
}

void TypeChecker::noteTypoCorrection(DeclName writtenName, DeclNameLoc loc,
                                     const LookupResult::Result &suggestion) {
  auto decl = suggestion.Decl;
  auto &&diagnostic = diagnoseTypoCorrection(*this, loc, decl);

  DeclName declName = decl->getFullName();

  if (writtenName.getBaseName() != declName.getBaseName())
    diagnostic.fixItReplace(loc.getBaseNameLoc(),
                            declName.getBaseIdentifier().str());

  // TODO: add fix-its for typo'ed argument labels.  This is trickier
  // because of the reordering rules.
}

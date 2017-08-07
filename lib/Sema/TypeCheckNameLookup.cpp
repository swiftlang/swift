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
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/TopCollection.h"
#include <algorithm>

using namespace swift;

void LookupResult::filter(const std::function<bool(LookupResultEntry)> &pred) {
  Results.erase(std::remove_if(Results.begin(), Results.end(),
                               [&](LookupResultEntry result) -> bool {
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
    llvm::SmallDenseMap<std::pair<ValueDecl *, DeclContext *>, bool, 4> Known;

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
                       [](const LookupResultEntry &found) {
                         return found.getBaseDecl() != nullptr;
                       }) == Result.end())
        return;

      // Remove any overridden declarations from the found-declarations set.
      removeOverriddenDecls(FoundDecls);

      // Remove any shadowed declarations from the found-declarations set.
      removeShadowedDecls(FoundDecls, DC->getParentModule(), &TC);

      // Filter out those results that have been removed from the
      // found-declarations set.
      unsigned foundIdx = 0, foundSize = FoundDecls.size();
      Result.filter([&](LookupResultEntry result) -> bool {
          // If the current result matches the remaining found declaration,
          // keep it and move to the next found declaration.
          if (foundIdx < foundSize &&
              result.getValueDecl() == FoundDecls[foundIdx]) {
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
    /// \param baseDC The declaration context through which we found the
    /// declaration.
    ///
    /// \param foundInType The type through which we found the
    /// declaration.
    void add(ValueDecl *found, DeclContext *baseDC, Type foundInType) {
      ConformanceCheckOptions conformanceOptions;
      if (Options.contains(NameLookupFlags::KnownPrivate))
        conformanceOptions |= ConformanceCheckFlags::InExpression;

      DeclContext *foundDC = found->getDeclContext();
      auto foundProto = foundDC->getAsProtocolOrProtocolExtensionContext();

      auto addResult = [&](ValueDecl *result) {
        if (Known.insert({{result, baseDC}, false}).second) {
          Result.add(LookupResultEntry(baseDC, result));
          FoundDecls.push_back(result);
        }
      };

      // If this isn't a protocol member to be given special
      // treatment, just add the result.
      if (!Options.contains(NameLookupFlags::ProtocolMembers) ||
          !isa<ProtocolDecl>(foundDC) ||
          isa<GenericTypeParamDecl>(found) ||
          (isa<FuncDecl>(found) && cast<FuncDecl>(found)->isOperator())) {
        addResult(found);
        return;
      }

      assert(isa<ProtocolDecl>(foundDC));

      if (!Options.contains(NameLookupFlags::PerformConformanceCheck))
        return;

      // If we found something within the protocol itself, and our
      // search began somewhere that is not in a protocol or extension
      // thereof, remap this declaration to the witness.
      auto conformingType = foundInType;

      // When performing a lookup on a subclass existential, we might
      // find a member of the class that witnesses a requirement on a
      // protocol that the class conforms to.
      //
      // Since subclass existentials don't normally conform to protocols,
      // pull out the superclass instead, and use that below.
      if (foundInType->isExistentialType()) {
        auto layout = foundInType->getExistentialLayout();
        if (layout.superclass)
          conformingType = layout.superclass;
      }

      // Dig out the protocol conformance.
      auto conformance = TC.conformsToProtocol(conformingType, foundProto, DC,
                                                 conformanceOptions);
      if (!conformance) {
        // If there's no conformance, we have an existential
        // and we found a member from one of the protocols, and
        // not a class constraint if any.
        assert(foundInType->isExistentialType());
        addResult(found);
        return;
      }

      if (conformance->isAbstract()) {
        assert(foundInType->is<ArchetypeType>() ||
               foundInType->isExistentialType());
        addResult(found);
        return;
      }

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
      //
      // If we have an imported conformance or the witness could
      // not be deserialized, getWitnessDecl() will just return
      // the requirement, so just drop the lookup result here.
      if (witness && !isa<ProtocolDecl>(witness->getDeclContext()))
        addResult(witness);
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

    if (auto *baseDC = found.getDeclContext()) {
      if (!baseDC->isTypeContext()) {
        baseDC = baseDC->getParent();
        assert(baseDC->isTypeContext());
      }
      foundInType = dc->mapTypeIntoContext(
        baseDC->getDeclaredInterfaceType());
      assert(foundInType && "bogus base declaration?");
    }

    builder.add(found.getValueDecl(), found.getDeclContext(), foundInType);
  }
  return result;
}

LookupResult
TypeChecker::lookupUnqualifiedType(DeclContext *dc, DeclName name,
                                   SourceLoc loc,
                                   NameLookupOptions options) {
  {
    // Try lookup without ProtocolMembers first.
    UnqualifiedLookup lookup(
      name, dc, this,
        options.contains(NameLookupFlags::KnownPrivate),
        loc,
        /*IsTypeLookup=*/true,
        /*AllowProtocolMembers=*/false,
        options.contains(NameLookupFlags::IgnoreAccessibility));

    if (!lookup.Results.empty() ||
        !options.contains(NameLookupFlags::ProtocolMembers)) {
      return LookupResult(lookup.Results);
    }
  }

  {
    // Try again, this time with protocol members.
    //
    // FIXME: Fix the problem where if NominalTypeDecl::getAllProtocols()
    // is called too early, we start resolving extensions -- even those
    // which do provide not conformances.
    UnqualifiedLookup lookup(
      name, dc, this,
        options.contains(NameLookupFlags::KnownPrivate),
        loc,
        /*IsTypeLookup=*/true,
        /*AllowProtocolMembers=*/true,
        options.contains(NameLookupFlags::IgnoreAccessibility));

    return LookupResult(lookup.Results);
  }
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

  if (options.contains(NameLookupFlags::ProtocolMembers))
    subOptions |= NL_ProtocolMembers;

  // We handle our own overriding/shadowing filtering.
  subOptions &= ~NL_RemoveOverridden;
  subOptions &= ~NL_RemoveNonVisible;

  LookupResultBuilder builder(*this, result, dc, options,
                              /*memberLookup*/true);
  SmallVector<ValueDecl *, 4> lookupResults;
  dc->lookupQualified(type, name, subOptions, this, lookupResults);

  for (auto found : lookupResults)
    builder.add(found, nullptr, type);

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

  if (!dc->lookupQualified(type, name, subOptions, this, decls))
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
  if (argName.getBaseName().getKind() != paramName.getBaseName().getKind()) {
    return UnreasonableCallEditDistance;
  }

  if (argName.getBaseName().getKind() != DeclBaseName::Kind::Normal) {
    return 0;
  }
  assert(argName.getBaseName().getKind() == DeclBaseName::Kind::Normal);

  StringRef argBase = argName.getBaseIdentifier().str();
  StringRef paramBase = paramName.getBaseIdentifier().str();

  unsigned distance = argBase.edit_distance(paramBase, maxEditDistance);

  // Bound the distance to UnreasonableCallEditDistance.
  if (distance >= maxEditDistance || distance > (paramBase.size() + 2) / 3) {
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
  // Disable typo-correction if we won't show the diagnostic anyway or if
  // we've hit our typo correction limit.
  if (NumTypoCorrections >= getLangOpts().TypoCorrectionLimit ||
      (Diags.hasFatalErrorOccurred() &&
       !Diags.getShowDiagnosticsAfterFatalError()))
    return;

  ++NumTypoCorrections;

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
    result.add(LookupResultEntry(entry.Value));
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

      return tc.diagnose(parentDecl, diag::note_typo_candidate_implicit_member,
                         decl->getBaseName().getIdentifier().str(), kind);
    }
  }

  return tc.diagnose(decl, diag::note_typo_candidate,
                     decl->getBaseName().getIdentifier().str());
}

void TypeChecker::noteTypoCorrection(DeclName writtenName, DeclNameLoc loc,
                                     ValueDecl *decl) {
  auto &&diagnostic = diagnoseTypoCorrection(*this, loc, decl);

  DeclName declName = decl->getFullName();

  if (writtenName.getBaseName() != declName.getBaseName())
    diagnostic.fixItReplace(loc.getBaseNameLoc(),
                            declName.getBaseIdentifier().str());

  // TODO: add fix-its for typo'ed argument labels.  This is trickier
  // because of the reordering rules.
}

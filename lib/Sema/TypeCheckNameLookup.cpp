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
#include "TypoCorrection.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/TopCollection.h"
#include <algorithm>

using namespace swift;

void LookupResult::filter(
    llvm::function_ref<bool(LookupResultEntry, bool)> pred) {
  size_t index = 0;
  size_t originalFirstOuter = IndexOfFirstOuterResult;
  Results.erase(std::remove_if(Results.begin(), Results.end(),
                               [&](LookupResultEntry result) -> bool {
                                 auto isInner = index < originalFirstOuter;
                                 index++;
                                 if (pred(result, !isInner))
                                   return false;

                                 // Need to remove this, which means, if it is
                                 // an inner result, the outer results need to
                                 // shift down.
                                 if (isInner)
                                   IndexOfFirstOuterResult--;
                                 return true;
                               }),
                Results.end());
}

void LookupResult::shiftDownResults() {
  // Remove inner results.
  Results.erase(Results.begin(), Results.begin() + IndexOfFirstOuterResult);
  IndexOfFirstOuterResult = 0;

  if (Results.empty())
    return;

  // Compute IndexOfFirstOuterResult.
  const DeclContext *dcInner = Results.front().getValueDecl()->getDeclContext();
  for (auto &&result : Results) {
    const DeclContext *dc = result.getValueDecl()->getDeclContext();
    if (dc == dcInner ||
        (dc->isModuleScopeContext() && dcInner->isModuleScopeContext()))
      ++IndexOfFirstOuterResult;
    else
      break;
  }
}

namespace {
  /// Builder that helps construct a lookup result from the raw lookup
  /// data.
  class LookupResultBuilder {
    LookupResult &Result;
    DeclContext *DC;
    NameLookupOptions Options;
    bool IsMemberLookup;

    /// The vector of found declarations.
    SmallVector<ValueDecl *, 4> FoundDecls;
    /// The vector of found declarations.
    SmallVector<ValueDecl *, 4> FoundOuterDecls;

    /// The set of known declarations.
    llvm::SmallDenseMap<std::pair<ValueDecl *, DeclContext *>, bool, 4> Known;

  public:
    LookupResultBuilder(LookupResult &result, DeclContext *dc,
                        NameLookupOptions options,
                        bool isMemberLookup)
      : Result(result), DC(dc), Options(options),
        IsMemberLookup(isMemberLookup) {
      if (!dc->getASTContext().LangOpts.EnableAccessControl)
        Options |= NameLookupFlags::IgnoreAccessControl;
    }

    /// Determine whether we should filter out the results by removing
    /// overridden and shadowed declarations.
    /// FIXME: We should *always* do this, but there are weird assumptions
    /// about the results of unqualified name lookup, e.g., that a local
    /// variable not having a type indicates that it hasn't been seen yet.
    bool shouldFilterResults() const {
      // Member lookups always filter results.
      if (IsMemberLookup) return true;

      bool allAreInOtherModules = true;
      auto currentModule = DC->getParentModule();
      for (const auto &found : Result) {
        // We found a member, so we need to filter.
        if (found.getBaseDecl() != nullptr)
          return true;

        // We found something in our own module.
        if (found.getValueDecl()->getDeclContext()->getParentModule() ==
              currentModule)
          allAreInOtherModules = false;
      }

      // FIXME: Only perform shadowing if we found things from other modules.
      // This prevents us from introducing additional type-checking work
      // during name lookup.
      return allAreInOtherModules;
    }

    ~LookupResultBuilder() {
      // Check whether we should do this filtering aat all.
      if (!shouldFilterResults()) return;

      // Remove any overridden declarations from the found-declarations set.
      removeOverriddenDecls(FoundDecls);
      removeOverriddenDecls(FoundOuterDecls);

      // Remove any shadowed declarations from the found-declarations set.
      removeShadowedDecls(FoundDecls, DC->getParentModule());
      removeShadowedDecls(FoundOuterDecls, DC->getParentModule());

      // Filter out those results that have been removed from the
      // found-declarations set.
      unsigned foundIdx = 0, foundSize = FoundDecls.size(),
               foundOuterSize = FoundOuterDecls.size();
      Result.filter([&](LookupResultEntry result, bool isOuter) -> bool {
        unsigned idx = foundIdx;
        unsigned limit = foundSize;
        ArrayRef<ValueDecl *> decls = FoundDecls;
        if (isOuter) {
          idx = foundIdx - foundSize;
          limit = foundOuterSize;
          decls = FoundOuterDecls;
        }
        // If the current result matches the remaining found declaration,
        // keep it and move to the next found declaration.
        if (idx < limit && result.getValueDecl() == decls[idx]) {
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
    ///
    /// \param isOuter Whether this is an outer result (i.e. a result that isn't
    /// from the innermost scope with results)
    void add(ValueDecl *found, DeclContext *baseDC, Type foundInType,
             bool isOuter) {
      ConformanceCheckOptions conformanceOptions;
      if (Options.contains(NameLookupFlags::KnownPrivate))
        conformanceOptions |= ConformanceCheckFlags::InExpression;
      conformanceOptions |= ConformanceCheckFlags::SkipConditionalRequirements;

      DeclContext *foundDC = found->getDeclContext();

      auto addResult = [&](ValueDecl *result) {
        if (Known.insert({{result, baseDC}, false}).second) {
          Result.add(LookupResultEntry(baseDC, result), isOuter);
          if (isOuter)
            FoundOuterDecls.push_back(result);
          else
            FoundDecls.push_back(result);
        }
      };

      // If this isn't a protocol member to be given special
      // treatment, just add the result.
      if (!Options.contains(NameLookupFlags::ProtocolMembers) ||
          !isa<ProtocolDecl>(foundDC) ||
          isa<GenericTypeParamDecl>(found) ||
          isa<TypeAliasDecl>(found) ||
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
        if (auto superclass = layout.getSuperclass()) {
          conformingType = superclass;
        } else {
          // Non-subclass existential: don't need to look for further
          // conformance or witness.
          addResult(found);
          return;
        }
      }

      // Dig out the protocol conformance.
      auto *foundProto = cast<ProtocolDecl>(foundDC);
      auto resolver = DC->getASTContext().getLazyResolver();
      assert(resolver && "Need an active resolver");
      auto &tc = *static_cast<TypeChecker *>(resolver);
      auto conformance = tc.conformsToProtocol(conformingType, foundProto, DC,
                                               conformanceOptions);
      if (!conformance) {
        // If there's no conformance, we have an existential
        // and we found a member from one of the protocols, and
        // not a class constraint if any.
        assert(foundInType->isExistentialType() || foundInType->hasError());
        if (foundInType->isExistentialType())
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
        witness = concrete->getTypeWitnessAndDecl(assocType, nullptr)
          .second;
      } else if (found->isProtocolRequirement()) {
        witness = concrete->getWitnessDecl(found, nullptr);

        // It is possible that a requirement is visible to us, but
        // not the witness. In this case, just return the requirement;
        // we will perform virtual dispatch on the concrete type.
        if (witness &&
            !Options.contains(NameLookupFlags::IgnoreAccessControl) &&
            !witness->isAccessibleFrom(DC)) {
          addResult(found);
          return;
        }
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

static UnqualifiedLookup::Options
convertToUnqualifiedLookupOptions(NameLookupOptions options) {
  UnqualifiedLookup::Options newOptions;
  if (options.contains(NameLookupFlags::KnownPrivate))
    newOptions |= UnqualifiedLookup::Flags::KnownPrivate;
  if (options.contains(NameLookupFlags::ProtocolMembers))
    newOptions |= UnqualifiedLookup::Flags::AllowProtocolMembers;
  if (options.contains(NameLookupFlags::IgnoreAccessControl))
    newOptions |= UnqualifiedLookup::Flags::IgnoreAccessControl;
  if (options.contains(NameLookupFlags::IncludeOuterResults))
    newOptions |= UnqualifiedLookup::Flags::IncludeOuterResults;

  return newOptions;
}

LookupResult TypeChecker::lookupUnqualified(DeclContext *dc, DeclName name,
                                            SourceLoc loc,
                                            NameLookupOptions options) {
  UnqualifiedLookup lookup(name, dc, nullptr, loc,
                           convertToUnqualifiedLookupOptions(options));

  LookupResult result;
  LookupResultBuilder builder(result, dc, options, /*memberLookup*/false);
  for (auto idx : indices(lookup.Results)) {
    const auto &found = lookup.Results[idx];
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

    builder.add(found.getValueDecl(), found.getDeclContext(), foundInType,
                /*isOuter=*/idx >= lookup.IndexOfFirstOuterResult);
  }
  return result;
}

LookupResult
TypeChecker::lookupUnqualifiedType(DeclContext *dc, DeclName name,
                                   SourceLoc loc,
                                   NameLookupOptions options) {
  auto ulOptions = convertToUnqualifiedLookupOptions(options) |
                   UnqualifiedLookup::Flags::TypeLookup;
  {
    // Try lookup without ProtocolMembers first.
    UnqualifiedLookup lookup(
        name, dc, nullptr, loc,
        ulOptions - UnqualifiedLookup::Flags::AllowProtocolMembers);

    if (!lookup.Results.empty() ||
        !options.contains(NameLookupFlags::ProtocolMembers)) {
      return LookupResult(lookup.Results, lookup.IndexOfFirstOuterResult);
    }
  }

  {
    // Try again, this time with protocol members.
    //
    // FIXME: Fix the problem where if NominalTypeDecl::getAllProtocols()
    // is called too early, we start resolving extensions -- even those
    // which do provide not conformances.
    UnqualifiedLookup lookup(
        name, dc, nullptr, loc,
        ulOptions | UnqualifiedLookup::Flags::AllowProtocolMembers);

    return LookupResult(lookup.Results, lookup.IndexOfFirstOuterResult);
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
  if (options.contains(NameLookupFlags::IgnoreAccessControl))
    subOptions |= NL_IgnoreAccessControl;

  if (options.contains(NameLookupFlags::ProtocolMembers))
    subOptions |= NL_ProtocolMembers;

  if (options.contains(NameLookupFlags::IncludeAttributeImplements))
    subOptions |= NL_IncludeAttributeImplements;

  // We handle our own overriding/shadowing filtering.
  subOptions &= ~NL_RemoveOverridden;
  subOptions &= ~NL_RemoveNonVisible;

  LookupResultBuilder builder(result, dc, options,
                              /*memberLookup*/true);
  SmallVector<ValueDecl *, 4> lookupResults;
  dc->lookupQualified(type, name, subOptions, nullptr, lookupResults);

  for (auto found : lookupResults)
    builder.add(found, nullptr, type, /*isOuter=*/false);

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

  if (type->isExistentialType() &&
      typeDecl->getDeclContext()->getSelfProtocolDecl()) {
    // TODO: Temporarily allow typealias and associated type lookup on
    //       existential type iff it doesn't have any type parameters.
    if (isa<TypeAliasDecl>(typeDecl) || isa<AssociatedTypeDecl>(typeDecl))
      return memberType->hasTypeParameter();

    // Don't allow lookups of nested types of an existential type,
    // because there is no way to represent such types.
    return true;
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
  if (options.contains(NameLookupFlags::IgnoreAccessControl))
    subOptions |= NL_IgnoreAccessControl;

  if (!dc->lookupQualified(type, name, subOptions, nullptr, decls))
    return result;

  // Look through the declarations, keeping only the unique type declarations.
  llvm::SmallPtrSet<CanType, 4> types;
  SmallVector<AssociatedTypeDecl *, 4> inferredAssociatedTypes;
  for (auto decl : decls) {
    auto *typeDecl = cast<TypeDecl>(decl);

    // FIXME: This should happen before we attempt shadowing checks.
    if (!typeDecl->hasInterfaceType()) {
      dc->getASTContext().getLazyResolver()->resolveDeclSignature(typeDecl);
      if (!typeDecl->hasInterfaceType()) // FIXME: recursion-breaking hack
        continue;
    }

    auto memberType = typeDecl->getDeclaredInterfaceType();

    if (isUnsupportedMemberTypeAccess(type, typeDecl)) {
      // Add the type to the result set, so that we can diagnose the
      // reference instead of just saying the member does not exist.
      if (types.insert(memberType->getCanonicalType()).second)
        result.Results.push_back({typeDecl, memberType, nullptr});

      continue;
    }

    // If we're looking up an associated type of a concrete type,
    // record it later for conformance checking; we might find a more
    // direct typealias with the same name later.
    if (typeDecl->getDeclContext()->getSelfProtocolDecl()) {
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(typeDecl)) {
        if (!type->is<ArchetypeType>() &&
            !type->isTypeParameter()) {
          if (options.contains(NameLookupFlags::PerformConformanceCheck))
            inferredAssociatedTypes.push_back(assocType);
          continue;
        }
      }

      // FIXME: This is a hack, we should be able to remove this entire 'if'
      // statement once we learn how to deal with the circularity here.
      if (isa<TypeAliasDecl>(typeDecl) &&
          isa<ProtocolDecl>(typeDecl->getDeclContext())) {
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
      result.Results.push_back({typeDecl, memberType, nullptr});
  }

  if (result.Results.empty()) {
    // We couldn't find any normal declarations. Let's try inferring
    // associated types.
    ConformanceCheckOptions conformanceOptions;
    if (options.contains(NameLookupFlags::KnownPrivate))
      conformanceOptions |= ConformanceCheckFlags::InExpression;
    conformanceOptions |= ConformanceCheckFlags::SkipConditionalRequirements;

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

      // This is the only case where NormalProtocolConformance::
      // getTypeWitnessAndDecl() returns a null type.
      if (concrete->getState() ==
          ProtocolConformanceState::CheckingTypeWitnesses)
        continue;

      auto lazyResolver = dc->getASTContext().getLazyResolver();
      auto typeDecl =
        concrete->getTypeWitnessAndDecl(assocType, lazyResolver).second;

      assert(typeDecl && "Missing type witness?");

      auto memberType =
          substMemberTypeWithBase(dc->getParentModule(), typeDecl, type);
      if (types.insert(memberType->getCanonicalType()).second)
        result.Results.push_back({typeDecl, memberType, assocType});
    }
  }

  return result;
}

LookupResult TypeChecker::lookupConstructors(DeclContext *dc, Type type,
                                             NameLookupOptions options) {
  return lookupMember(dc, type, DeclBaseName::createConstructor(), options);
}

enum : unsigned {
  /// Never consider a candidate that's this distance away or worse.
  UnreasonableCallEditDistance = 8,

  /// Don't consider candidates that score worse than the given distance
  /// from the best candidate.
  MaxCallEditDistanceFromBestCandidate = 1
};

static unsigned getCallEditDistance(DeclName writtenName,
                                    DeclName correctedName,
                                    unsigned maxEditDistance) {
  // TODO: consider arguments.
  // TODO: maybe ignore certain kinds of missing / present labels for the
  //   first argument label?
  // TODO: word-based rather than character-based?
  if (writtenName.getBaseName().getKind() !=
        correctedName.getBaseName().getKind()) {
    return UnreasonableCallEditDistance;
  }

  if (writtenName.getBaseName().getKind() != DeclBaseName::Kind::Normal) {
    return 0;
  }

  StringRef writtenBase = writtenName.getBaseName().userFacingName();
  StringRef correctedBase = correctedName.getBaseName().userFacingName();

  unsigned distance = writtenBase.edit_distance(correctedBase, maxEditDistance);

  // Bound the distance to UnreasonableCallEditDistance.
  if (distance >= maxEditDistance ||
      distance > (correctedBase.size() + 2) / 3) {
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

void TypeChecker::performTypoCorrection(DeclContext *DC, DeclRefKind refKind,
                                        Type baseTypeOrNull,
                                        NameLookupOptions lookupOptions,
                                        TypoCorrectionResults &corrections,
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
  TopCollection<unsigned, ValueDecl *> entries(maxResults);
  auto consumer = makeDeclConsumer([&](ValueDecl *decl,
                                       DeclVisibilityKind reason) {
    // Never match an operator with an identifier or vice-versa; this is
    // not a plausible typo.
    if (!isPlausibleTypo(refKind, corrections.WrittenName, decl))
      return;

    // Don't suggest a variable within its own initializer.
    if (auto var = dyn_cast<VarDecl>(decl)) {
      if (isLocInVarInit(*this, var, corrections.Loc.getBaseNameLoc()))
        return;
    }

    auto candidateName = decl->getFullName();

    // Don't waste time computing edit distances that are more than
    // the worst in our collection.
    unsigned maxDistance =
      entries.getMinUninterestingScore(UnreasonableCallEditDistance);

    unsigned distance =
      getCallEditDistance(corrections.WrittenName, candidateName,
                          maxDistance);

    // Ignore values that are further than a reasonable distance.
    if (distance >= UnreasonableCallEditDistance)
      return;

    entries.insert(distance, std::move(decl));
  });

  if (baseTypeOrNull) {
    lookupVisibleMemberDecls(consumer, baseTypeOrNull, DC, this,
                             /*include instance members*/ true, gsb);
  } else {
    lookupVisibleDecls(consumer, DC, this, /*top level*/ true,
                       corrections.Loc.getBaseNameLoc());
  }

  // Impose a maximum distance from the best score.
  entries.filterMaxScoreRange(MaxCallEditDistanceFromBestCandidate);

  for (auto &entry : entries)
    corrections.Candidates.push_back(entry.Value);
}

void
TypoCorrectionResults::addAllCandidatesToLookup(LookupResult &lookup) const {
  for (auto candidate : Candidates)
    lookup.add(LookupResultEntry(candidate), /*isOuter=*/false);
}

static Decl *findExplicitParentForImplicitDecl(ValueDecl *decl) {
  if (!decl->getLoc().isValid() && decl->getDeclContext()->isTypeContext()) {
    Decl *parentDecl = dyn_cast<ExtensionDecl>(decl->getDeclContext());
    if (!parentDecl) parentDecl = cast<NominalTypeDecl>(decl->getDeclContext());
    if (parentDecl->getLoc().isValid())
      return parentDecl;
  }

  return nullptr;
}

static InFlightDiagnostic
noteTypoCorrection(TypeChecker &tc, DeclNameLoc loc, ValueDecl *decl,
                   bool wasClaimed) {
  if (auto var = dyn_cast<VarDecl>(decl)) {
    // Suggest 'self' at the use point instead of pointing at the start
    // of the function.
    if (var->isSelfParameter()) {
      if (wasClaimed) {
        // We don't need an extra note for this case because the programmer
        // knows what 'self' refers to.
        return InFlightDiagnostic();
      }

      return tc.diagnose(loc.getBaseNameLoc(), diag::note_typo_candidate,
                         var->getName().str());
    }
  }

  if (Decl *parentDecl = findExplicitParentForImplicitDecl(decl)) {
    StringRef kind = (isa<VarDecl>(decl) ? "property" :
                      isa<ConstructorDecl>(decl) ? "initializer" :
                      isa<FuncDecl>(decl) ? "method" :
                      "member");

    return tc.diagnose(parentDecl,
                       wasClaimed ? diag::implicit_member_declared_here
                                  : diag::note_typo_candidate_implicit_member,
                       decl->getBaseName().userFacingName(), kind);
  }

  if (wasClaimed) {
    return tc.diagnose(decl, diag::decl_declared_here, decl->getBaseName());
  } else {
    return tc.diagnose(decl, diag::note_typo_candidate,
                       decl->getBaseName().userFacingName());
  }
}

void TypoCorrectionResults::noteAllCandidates() const {
  for (auto candidate : Candidates) {
    auto &&diagnostic =
      noteTypoCorrection(TC, Loc, candidate, ClaimedCorrection);

    // Don't add fix-its if we claimed the correction for the primary
    // diagnostic.
    if (!ClaimedCorrection) {
      SyntacticTypoCorrection correction(WrittenName, Loc,
                                         candidate->getFullName());
      correction.addFixits(diagnostic);
    }
  }
}

void SyntacticTypoCorrection::addFixits(InFlightDiagnostic &diagnostic) const {
  if (WrittenName.getBaseName() != CorrectedName.getBaseName())
    diagnostic.fixItReplace(Loc.getBaseNameLoc(),
                            CorrectedName.getBaseName().userFacingName());

  // TODO: add fix-its for typo'ed argument labels.  This is trickier
  // because of the reordering rules.
}

Optional<SyntacticTypoCorrection>
TypoCorrectionResults::claimUniqueCorrection() {
  // Look for a unique base name.  We ignore the rest of the name for now
  // because we don't actually typo-correct any of that.
  DeclBaseName uniqueCorrectedName;
  for (auto candidate : Candidates) {
    auto candidateName = candidate->getBaseName();

    // If this is the first name, record it.
    if (uniqueCorrectedName.empty())
      uniqueCorrectedName = candidateName;

    // If this is a different name from the last candidate, we don't have
    // a unique correction.
    else if (uniqueCorrectedName != candidateName)
      return None;
  }

  // If we didn't find any candidates, we're done.
  if (uniqueCorrectedName.empty())
    return None;

  // If the corrected name doesn't differ from the written name in its base
  // name, it's not simple enough for this (for now).
  if (WrittenName.getBaseName() == uniqueCorrectedName)
    return None;

  // Flag that we've claimed the correction.
  ClaimedCorrection = true;

  return SyntacticTypoCorrection(WrittenName, Loc, uniqueCorrectedName);
}

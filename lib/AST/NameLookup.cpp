//===--- NameLookup.cpp - Swift Name Lookup Routines ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements interfaces for performing name lookup.
//
//===----------------------------------------------------------------------===//

#include "NameLookupImpl.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTScope.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/DebuggerClient.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "namelookup"

using namespace swift;

ValueDecl *LookupResultEntry::getBaseDecl() const {
  if (BaseDC == nullptr)
    return nullptr;

  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(BaseDC))
    return AFD->getImplicitSelfDecl();

  if (auto *PBI = dyn_cast<PatternBindingInitializer>(BaseDC)) {
    auto *selfDecl = PBI->getImplicitSelfDecl();
    assert(selfDecl);
    return selfDecl;
  }

  auto *nominalDecl = BaseDC->getAsNominalTypeOrNominalTypeExtensionContext();
  assert(nominalDecl);
  return nominalDecl;
}

void DebuggerClient::anchor() {}

void AccessFilteringDeclConsumer::foundDecl(ValueDecl *D,
                                            DeclVisibilityKind reason) {
  if (D->getASTContext().LangOpts.EnableAccessControl) {
    if (D->isInvalid())
      return;
    if (!D->isAccessibleFrom(DC))
      return;
  }
  ChainedConsumer.foundDecl(D, reason);
}


template <typename Fn>
static void forAllVisibleModules(const DeclContext *DC, const Fn &fn) {
  DeclContext *moduleScope = DC->getModuleScopeContext();
  if (auto file = dyn_cast<FileUnit>(moduleScope))
    file->forAllVisibleModules(fn);
  else
    cast<ModuleDecl>(moduleScope)->forAllVisibleModules(ModuleDecl::AccessPathTy(), fn);
}

bool swift::removeOverriddenDecls(SmallVectorImpl<ValueDecl*> &decls) {
  if (decls.size() < 2)
    return false;

  llvm::SmallPtrSet<ValueDecl*, 8> overridden;
  for (auto decl : decls) {
    while (auto overrides = decl->getOverriddenDecl()) {
      overridden.insert(overrides);

      // Because initializers from Objective-C base classes have greater
      // visibility than initializers written in Swift classes, we can
      // have a "break" in the set of declarations we found, where
      // C.init overrides B.init overrides A.init, but only C.init and
      // A.init are in the chain. Make sure we still remove A.init from the
      // set in this case.
      if (decl->getFullName().getBaseName() == DeclBaseName::createConstructor()) {
        /// FIXME: Avoid the possibility of an infinite loop by fixing the root
        ///        cause instead (incomplete circularity detection).
        assert(decl != overrides && "Circular class inheritance?");
        decl = overrides;
        continue;
      }

      break;
    }
  }

  // If no methods were overridden, we're done.
  if (overridden.empty()) return false;

  // Erase any overridden declarations
  bool anyOverridden = false;
  decls.erase(std::remove_if(decls.begin(), decls.end(),
                             [&](ValueDecl *decl) -> bool {
                               if (overridden.count(decl) > 0) {
                                 anyOverridden = true;
                                 return true;
                               }

                               return false;
                             }),
              decls.end());

  return anyOverridden;
}

enum class ConstructorComparison {
  Worse,
  Same,
  Better,
};

/// Determines whether \p ctor1 is a "better" initializer than \p ctor2.
static ConstructorComparison compareConstructors(ConstructorDecl *ctor1,
                                                 ConstructorDecl *ctor2,
                                                 const swift::ASTContext &ctx) {
  bool available1 = !ctor1->getAttrs().isUnavailable(ctx);
  bool available2 = !ctor2->getAttrs().isUnavailable(ctx);

  // An unavailable initializer is always worse than an available initializer.
  if (available1 < available2)
    return ConstructorComparison::Worse;

  if (available1 > available2)
    return ConstructorComparison::Better;

  CtorInitializerKind kind1 = ctor1->getInitKind();
  CtorInitializerKind kind2 = ctor2->getInitKind();

  if (kind1 > kind2)
    return ConstructorComparison::Worse;

  if (kind1 < kind2)
    return ConstructorComparison::Better;

  return ConstructorComparison::Same;
}

bool swift::removeShadowedDecls(SmallVectorImpl<ValueDecl*> &decls,
                                const ModuleDecl *curModule) {
  auto typeResolver = curModule->getASTContext().getLazyResolver();

  // Category declarations by their signatures.
  llvm::SmallDenseMap<std::pair<CanType, DeclBaseName>,
                      llvm::TinyPtrVector<ValueDecl *>>
    CollidingDeclGroups;

  /// Objective-C initializers are tracked by their context type and
  /// full name.
  llvm::SmallDenseMap<std::pair<CanType, DeclName>, 
                      llvm::TinyPtrVector<ConstructorDecl *>>
    ObjCCollidingConstructors;
  bool anyCollisions = false;
  for (auto decl : decls) {
    // FIXME: Egregious hack to avoid failing when there are no declared types.
    // FIXME: Pass this down instead of getting it from the ASTContext.
    if (typeResolver)
      typeResolver->resolveDeclSignature(decl);
    
    // If the decl is currently being validated, this is likely a recursive
    // reference and we'll want to skip ahead so as to avoid having its type
    // attempt to desugar itself.
    if (!decl->hasValidSignature())
      continue;

    // FIXME: the canonical type makes a poor signature, because we don't
    // canonicalize away default arguments.
    auto signature = decl->getInterfaceType()->getCanonicalType();

    // FIXME: The type of a variable or subscript doesn't include
    // enough context to distinguish entities from different
    // constrained extensions, so use the overload signature's
    // type. This is layering a partial fix upon a total hack.
    if (auto asd = dyn_cast<AbstractStorageDecl>(decl))
      signature = asd->getOverloadSignatureType();

    // If we've seen a declaration with this signature before, note it.
    auto &knownDecls =
        CollidingDeclGroups[std::make_pair(signature, decl->getBaseName())];
    if (!knownDecls.empty())
      anyCollisions = true;

    knownDecls.push_back(decl);

    // Specifically keep track of Objective-C initializers, which can come from
    // either init methods or factory methods.
    if (decl->hasClangNode()) {
      if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
        auto ctorSignature
          = std::make_pair(ctor->getDeclContext()->getDeclaredInterfaceType()
                               ->getCanonicalType(),
                           decl->getFullName());
        auto &knownCtors = ObjCCollidingConstructors[ctorSignature];
        if (!knownCtors.empty())
          anyCollisions = true;
        knownCtors.push_back(ctor);
      }
    }
  }

  // If there were no signature collisions, there is nothing to do.
  if (!anyCollisions)
    return false;

  // Determine the set of declarations that are shadowed by other declarations.
  llvm::SmallPtrSet<ValueDecl *, 4> shadowed;
  ASTContext &ctx = decls[0]->getASTContext();
  for (auto &collidingDecls : CollidingDeclGroups) {
    // If only one declaration has this signature, it isn't shadowed by
    // anything.
    if (collidingDecls.second.size() == 1)
      continue;

    // Compare each declaration to every other declaration. This is
    // unavoidably O(n^2) in the number of declarations, but because they
    // all have the same signature, we expect n to remain small.
    for (unsigned firstIdx = 0, n = collidingDecls.second.size();
         firstIdx != n; ++firstIdx) {
      auto firstDecl = collidingDecls.second[firstIdx];
      auto firstModule = firstDecl->getModuleContext();
      for (unsigned secondIdx = firstIdx + 1; secondIdx != n; ++secondIdx) {
        // Determine whether one module takes precedence over another.
        auto secondDecl = collidingDecls.second[secondIdx];
        auto secondModule = secondDecl->getModuleContext();

        // If one declaration is in a protocol or extension thereof and the
        // other is not, prefer the one that is not.
        if ((bool)firstDecl->getDeclContext()
              ->getAsProtocolOrProtocolExtensionContext()
              != (bool)secondDecl->getDeclContext()
                   ->getAsProtocolOrProtocolExtensionContext()) {
          if (firstDecl->getDeclContext()
                ->getAsProtocolOrProtocolExtensionContext()) {
            shadowed.insert(firstDecl);
            break;
          } else {
            shadowed.insert(secondDecl);
            continue;
          }
        }

        // If one declaration is available and the other is not, prefer the
        // available one.
        if (firstDecl->getAttrs().isUnavailable(ctx) !=
              secondDecl->getAttrs().isUnavailable(ctx)) {
         if (firstDecl->getAttrs().isUnavailable(ctx)) {
           shadowed.insert(firstDecl);
           break;
         } else {
           shadowed.insert(secondDecl);
           continue;
         }
        }

        // Don't apply module-shadowing rules to members of protocol types.
        if (isa<ProtocolDecl>(firstDecl->getDeclContext()) ||
            isa<ProtocolDecl>(secondDecl->getDeclContext()))
          continue;

        // Prefer declarations in the current module over those in another
        // module.
        // FIXME: This is a hack. We should query a (lazily-built, cached)
        // module graph to determine shadowing.
        if ((firstModule == curModule) != (secondModule == curModule)) {
          // If the first module is the current module, the second declaration
          // is shadowed by the first.
          if (firstModule == curModule) {
            shadowed.insert(secondDecl);
            continue;
          }

          // Otherwise, the first declaration is shadowed by the second. There is
          // no point in continuing to compare the first declaration to others.
          shadowed.insert(firstDecl);
          break;
        }

        // Prefer declarations in an overlay to similar declarations in
        // the Clang module it customizes.
        if (firstDecl->hasClangNode() != secondDecl->hasClangNode()) {
          auto clangLoader = ctx.getClangModuleLoader();
          if (!clangLoader) continue;

          if (clangLoader->isInOverlayModuleForImportedModule(
                                                firstDecl->getDeclContext(),
                                                secondDecl->getDeclContext())) {
            shadowed.insert(secondDecl);
            continue;
          }

          if (clangLoader->isInOverlayModuleForImportedModule(
                                                 secondDecl->getDeclContext(),
                                                 firstDecl->getDeclContext())) {
            shadowed.insert(firstDecl);
            break;
          }
        }
      }
    }
  }
  
  // Check for collisions among Objective-C initializers. When such collisions
  // exist, we pick the
  for (const auto &colliding : ObjCCollidingConstructors) {
    if (colliding.second.size() == 1)
      continue;

    // Find the "best" constructor with this signature.
    ConstructorDecl *bestCtor = colliding.second[0];
    for (auto ctor : colliding.second) {
      auto comparison = compareConstructors(ctor, bestCtor, ctx);
      if (comparison == ConstructorComparison::Better)
        bestCtor = ctor;
    }

    // Shadow any initializers that are worse.
    for (auto ctor : colliding.second) {
      auto comparison = compareConstructors(ctor, bestCtor, ctx);
      if (comparison == ConstructorComparison::Worse)
        shadowed.insert(ctor);
    }
  }

  // If none of the declarations were shadowed, we're done.
  if (shadowed.empty())
    return false;

  // Remove shadowed declarations from the list of declarations.
  bool anyRemoved = false;
  decls.erase(std::remove_if(decls.begin(), decls.end(),
                             [&](ValueDecl *vd) {
                               if (shadowed.count(vd) > 0) {
                                 anyRemoved = true;
                                 return true;
                               }

                               return false;
                             }),
              decls.end());

  return anyRemoved;
}

namespace {
enum class DiscriminatorMatch {
  NoDiscriminator,
  Matches,
  Different
};
} // end anonymous namespace

static DiscriminatorMatch matchDiscriminator(Identifier discriminator,
                                             const ValueDecl *value) {
  if (value->getFormalAccess() > AccessLevel::FilePrivate)
    return DiscriminatorMatch::NoDiscriminator;

  auto containingFile =
    dyn_cast<FileUnit>(value->getDeclContext()->getModuleScopeContext());
  if (!containingFile)
    return DiscriminatorMatch::Different;

  if (discriminator == containingFile->getDiscriminatorForPrivateValue(value))
    return DiscriminatorMatch::Matches;

  return DiscriminatorMatch::Different;
}

static DiscriminatorMatch
matchDiscriminator(Identifier discriminator,
                   LookupResultEntry lookupResult) {
  return matchDiscriminator(discriminator, lookupResult.getValueDecl());
}

template <typename Result>
static void filterForDiscriminator(SmallVectorImpl<Result> &results,
                                   DebuggerClient *debugClient) {
  Identifier discriminator = debugClient->getPreferredPrivateDiscriminator();
  if (discriminator.empty())
    return;

  auto lastMatchIter = std::find_if(results.rbegin(), results.rend(),
                                    [discriminator](Result next) -> bool {
    return
      matchDiscriminator(discriminator, next) == DiscriminatorMatch::Matches;
  });
  if (lastMatchIter == results.rend())
    return;

  Result lastMatch = *lastMatchIter;

  auto newEnd = std::remove_if(results.begin(), lastMatchIter.base()-1,
                               [discriminator](Result next) -> bool {
    return
      matchDiscriminator(discriminator, next) == DiscriminatorMatch::Different;
  });
  results.erase(newEnd, results.end());
  results.push_back(lastMatch);
}

static void recordLookupOfTopLevelName(DeclContext *topLevelContext,
                                       DeclName name,
                                       bool isCascading) {
  auto SF = dyn_cast<SourceFile>(topLevelContext);
  if (!SF)
    return;
  auto *nameTracker = SF->getReferencedNameTracker();
  if (!nameTracker)
    return;
  nameTracker->addTopLevelName(name.getBaseName(), isCascading);
}

/// Determine the local declaration visibility key for an \c ASTScope in which
/// name lookup successfully resolved.
static DeclVisibilityKind getLocalDeclVisibilityKind(const ASTScope *scope) {
  switch (scope->getKind()) {
  case ASTScopeKind::Preexpanded:
  case ASTScopeKind::SourceFile:
  case ASTScopeKind::TypeDecl:
  case ASTScopeKind::AbstractFunctionDecl:
  case ASTScopeKind::TypeOrExtensionBody:
  case ASTScopeKind::AbstractFunctionBody:
  case ASTScopeKind::DefaultArgument:
  case ASTScopeKind::PatternBinding:
  case ASTScopeKind::IfStmt:
  case ASTScopeKind::GuardStmt:
  case ASTScopeKind::RepeatWhileStmt:
  case ASTScopeKind::ForEachStmt:
  case ASTScopeKind::DoCatchStmt:
  case ASTScopeKind::SwitchStmt:
  case ASTScopeKind::Accessors:
  case ASTScopeKind::TopLevelCode:
    llvm_unreachable("no local declarations?");

  case ASTScopeKind::ExtensionGenericParams:
  case ASTScopeKind::GenericParams:
    return DeclVisibilityKind::GenericParameter;

  case ASTScopeKind::AbstractFunctionParams:
  case ASTScopeKind::Closure:
  case ASTScopeKind::PatternInitializer:  // lazy var 'self'
    return DeclVisibilityKind::FunctionParameter;

  case ASTScopeKind::AfterPatternBinding:
  case ASTScopeKind::ConditionalClause:
  case ASTScopeKind::ForEachPattern:
  case ASTScopeKind::BraceStmt:
  case ASTScopeKind::CatchStmt:
  case ASTScopeKind::CaseStmt:
    return DeclVisibilityKind::LocalVariable;
  }

  llvm_unreachable("Unhandled ASTScopeKind in switch.");
}

UnqualifiedLookup::UnqualifiedLookup(DeclName Name, DeclContext *DC,
                                     LazyResolver *TypeResolver, SourceLoc Loc,
                                     Options options)
  : IndexOfFirstOuterResult(0)
{
  ModuleDecl &M = *DC->getParentModule();
  ASTContext &Ctx = M.getASTContext();
  if (!TypeResolver) TypeResolver = Ctx.getLazyResolver();
  const SourceManager &SM = Ctx.SourceMgr;
  DebuggerClient *DebugClient = M.getDebugClient();

  auto isOriginallyTypeLookup = options.contains(Flags::TypeLookup);
  NamedDeclConsumer Consumer(Name, Results, isOriginallyTypeLookup);

  NLOptions baseNLOptions = NL_UnqualifiedDefault;
  if (options.contains(Flags::AllowProtocolMembers))
    baseNLOptions |= NL_ProtocolMembers;
  if (isOriginallyTypeLookup)
    baseNLOptions |= NL_OnlyTypes;
  if (options.contains(Flags::IgnoreAccessControl))
    baseNLOptions |= NL_IgnoreAccessControl;

  Optional<bool> isCascadingUse;
  if (options.contains(Flags::KnownPrivate))
    isCascadingUse = false;

  SmallVector<LookupResultEntry, 4> UnavailableInnerResults;

  auto shouldReturnBasedOnResults = [&](bool noMoreOuterResults = false) {
    if (Results.empty())
      return false;

    if (IndexOfFirstOuterResult == 0)
      IndexOfFirstOuterResult = Results.size();

    return !options.contains(Flags::IncludeOuterResults) || noMoreOuterResults;
  };

  if (Loc.isValid() &&
      DC->getParentSourceFile()->Kind != SourceFileKind::REPL &&
      Ctx.LangOpts.EnableASTScopeLookup) {
    // Find the source file in which we are performing the lookup.
    SourceFile &sourceFile = *DC->getParentSourceFile();

    // Find the scope from which we will initiate unqualified name lookup.
    const ASTScope *lookupScope
      = sourceFile.getScope().findInnermostEnclosingScope(Loc);

    // Operator lookup is always at module scope.
    if (Name.isOperator()) {
      if (!isCascadingUse.hasValue()) {
        DeclContext *innermostDC =
          lookupScope->getInnermostEnclosingDeclContext();
        isCascadingUse =
          innermostDC->isCascadingContextForLookup(
            /*functionsAreNonCascading=*/true);
      }

      lookupScope = &sourceFile.getScope();
    }
  
    // Walk scopes outward from the innermost scope until we find something.
    DeclContext *selfDC = nullptr;
    for (auto currentScope = lookupScope; currentScope;
         currentScope = currentScope->getParent()) {
      // Perform local lookup within this scope.
      auto localBindings = currentScope->getLocalBindings();
      for (auto local : localBindings) {
        Consumer.foundDecl(local,
                           getLocalDeclVisibilityKind(currentScope));
      }

      // If we found anything, we're done.
      if (shouldReturnBasedOnResults())
        return;

      // When we are in the body of a method, get the 'self' declaration.
      if (currentScope->getKind() == ASTScopeKind::AbstractFunctionBody &&
          currentScope->getAbstractFunctionDecl()->getDeclContext()
            ->isTypeContext()) {
        selfDC = currentScope->getAbstractFunctionDecl();
        continue;
      }

      // If there is a declaration context associated with this scope, we might
      // want to look in it.
      if (auto dc = currentScope->getDeclContext()) {
        // If we haven't determined whether we have a cascading use, do so now.
        if (!isCascadingUse.hasValue()) {
          isCascadingUse =
            dc->isCascadingContextForLookup(/*functionsAreNonCascading=*/false);
        }

        // Pattern binding initializers are only interesting insofar as they
        // affect lookup in an enclosing nominal type or extension thereof.
        if (auto *bindingInit = dyn_cast<PatternBindingInitializer>(dc)) {
          // Lazy variable initializer contexts have a 'self' parameter for
          // instance member lookup.
          if (bindingInit->getImplicitSelfDecl())
            selfDC = bindingInit;

          continue;
        }

        // Default arguments only have 'static' access to the members of the
        // enclosing type, if there is one.
        if (isa<DefaultArgumentInitializer>(dc)) continue;

        // Functions/initializers/deinitializers are only interesting insofar as
        // they affect lookup in an enclosing nominal type or extension thereof.
        if (isa<AbstractFunctionDecl>(dc)) continue;

        // Subscripts have no lookup of their own.
        if (isa<SubscriptDecl>(dc)) continue;

        // Closures have no lookup of their own.
        if (isa<AbstractClosureExpr>(dc)) continue;

        // Top-level declarations have no lookup of their own.
        if (isa<TopLevelCodeDecl>(dc)) continue;

        // Typealiases have no lookup of their own.
        if (isa<TypeAliasDecl>(dc)) continue;

        // Lookup in the source file's scope marks the end.
        if (isa<SourceFile>(dc)) {
          // FIXME: A bit of a hack.
          DC = dc;
          break;
        }

        // We have a nominal type or an extension thereof. Perform lookup into
        // the nominal type.
        auto nominal = dc->getAsNominalTypeOrNominalTypeExtensionContext();
        if (!nominal) continue;

        // Dig out the type we're looking into.
        // FIXME: We shouldn't need to compute a type to perform this lookup.
        Type lookupType = dc->getSelfTypeInContext();

        if (lookupType->hasError()) continue;

        NLOptions options = baseNLOptions;
        // Perform lookup into the type.
        if (isCascadingUse.getValue())
          options |= NL_KnownCascadingDependency;
        else
          options |= NL_KnownNonCascadingDependency;

        SmallVector<ValueDecl *, 4> lookup;
        dc->lookupQualified(lookupType, Name, options, TypeResolver, lookup);

        auto startIndex = Results.size();
        for (auto result : lookup) {
          auto *baseDC = dc;
          if (!isa<TypeDecl>(result) && selfDC) baseDC = selfDC;
          Results.push_back(LookupResultEntry(baseDC, result));
        }

        if (!Results.empty()) {
          // Predicate that determines whether a lookup result should
          // be unavailable except as a last-ditch effort.
          auto unavailableLookupResult =
              [&](const LookupResultEntry &result) {
            auto &effectiveVersion = Ctx.LangOpts.EffectiveLanguageVersion;
            return result.getValueDecl()->getAttrs()
                .isUnavailableInSwiftVersion(effectiveVersion);
          };

          // If all of the results we just found are unavailable, keep looking.
          auto begin = Results.begin() + startIndex;
          if (std::all_of(begin, Results.end(), unavailableLookupResult)) {
            UnavailableInnerResults.append(begin, Results.end());
            Results.erase(begin, Results.end());
          } else {
            if (DebugClient)
              filterForDiscriminator(Results, DebugClient);

            if (shouldReturnBasedOnResults())
              return;
          }
        }

        // Forget the 'self' declaration.
        selfDC = nullptr;
      }
    }
  } else {
    // Never perform local lookup for operators.
    if (Name.isOperator()) {
      if (!isCascadingUse.hasValue()) {
        isCascadingUse =
          DC->isCascadingContextForLookup(/*functionsAreNonCascading=*/true);
      }
      DC = DC->getModuleScopeContext();

    } else {
      // If we are inside of a method, check to see if there are any ivars in
      // scope, and if so, whether this is a reference to one of them.
      // FIXME: We should persist this information between lookups.
      while (!DC->isModuleScopeContext()) {
        DeclContext *BaseDC = nullptr;
        DeclContext *MetaBaseDC = nullptr;
        GenericParamList *GenericParams = nullptr;
        Type ExtendedType;

        if (auto *PBI = dyn_cast<PatternBindingInitializer>(DC)) {
          auto *PBD = PBI->getBinding();
          assert(PBD);

          // Lazy variable initializer contexts have a 'self' parameter for
          // instance member lookup.
          if (auto *selfParam = PBI->getImplicitSelfDecl()) {
            Consumer.foundDecl(selfParam,
                               DeclVisibilityKind::FunctionParameter);
            if (shouldReturnBasedOnResults())
              return;

            DC = DC->getParent();

            ExtendedType = DC->getSelfTypeInContext();
            MetaBaseDC = DC;
            BaseDC = PBI;
          }
          // Initializers for stored properties of types perform static
          // lookup into the surrounding context.
          else if (PBD->getDeclContext()->isTypeContext()) {
            DC = DC->getParent();

            ExtendedType = DC->getSelfTypeInContext();
            MetaBaseDC = DC;
            BaseDC = MetaBaseDC;

            isCascadingUse = DC->isCascadingContextForLookup(false);
          }
          // Otherwise, we have an initializer for a global or local property.
          // There's not much to find here, we'll keep going up to a parent
          // context.

          if (!isCascadingUse.hasValue())
            isCascadingUse = DC->isCascadingContextForLookup(false);
        } else if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
          // Look for local variables; normally, the parser resolves these
          // for us, but it can't do the right thing inside local types.
          // FIXME: when we can parse and typecheck the function body partially
          // for code completion, AFD->getBody() check can be removed.
          if (Loc.isValid() && AFD->getBody()) {
            if (!isCascadingUse.hasValue()) {
              isCascadingUse =
                  !SM.rangeContainsTokenLoc(AFD->getBodySourceRange(), Loc);
            }

            namelookup::FindLocalVal localVal(SM, Loc, Consumer);
            localVal.visit(AFD->getBody());
            if (shouldReturnBasedOnResults())
              return;

            if (auto *P = AFD->getImplicitSelfDecl())
              localVal.checkValueDecl(P, DeclVisibilityKind::FunctionParameter);
            localVal.checkParameterList(AFD->getParameters());
            if (shouldReturnBasedOnResults())
              return;
          }
          if (!isCascadingUse.hasValue() || isCascadingUse.getValue())
            isCascadingUse = AFD->isCascadingContextForLookup(false);

          if (AFD->getDeclContext()->isTypeContext()) {
            ExtendedType = AFD->getDeclContext()->getSelfTypeInContext();
            BaseDC = AFD;
            MetaBaseDC = AFD->getDeclContext();
            DC = DC->getParent();

            // If we're not in the body of the function (for example, we
            // might be type checking a default argument expression and
            // performing name lookup from there), the base declaration
            // is the nominal type, not 'self'.
            if (!AFD->isImplicit() &&
                Loc.isValid() &&
                AFD->getBodySourceRange().isValid() &&
                !SM.rangeContainsTokenLoc(AFD->getBodySourceRange(), Loc)) {
              BaseDC = MetaBaseDC;
            }
          }

          // Look in the generic parameters after checking our local declaration.
          GenericParams = AFD->getGenericParams();
        } else if (auto *SD = dyn_cast<SubscriptDecl>(DC)) {
          GenericParams = SD->getGenericParams();
        } else if (auto *ACE = dyn_cast<AbstractClosureExpr>(DC)) {
          // Look for local variables; normally, the parser resolves these
          // for us, but it can't do the right thing inside local types.
          if (Loc.isValid()) {
            if (auto *CE = dyn_cast<ClosureExpr>(ACE)) {
              namelookup::FindLocalVal localVal(SM, Loc, Consumer);
              localVal.visit(CE->getBody());
              if (shouldReturnBasedOnResults())
                return;
              localVal.checkParameterList(CE->getParameters());
              if (shouldReturnBasedOnResults())
                return;
            }
          }
          if (!isCascadingUse.hasValue())
            isCascadingUse = ACE->isCascadingContextForLookup(false);
        } else if (auto *ED = dyn_cast<ExtensionDecl>(DC)) {
          ExtendedType = ED->getSelfTypeInContext();

          BaseDC = ED;
          MetaBaseDC = ED;
          if (!isCascadingUse.hasValue())
            isCascadingUse = ED->isCascadingContextForLookup(false);
        } else if (auto *ND = dyn_cast<NominalTypeDecl>(DC)) {
          ExtendedType = ND->getDeclaredType();
          BaseDC = DC;
          MetaBaseDC = DC;
          if (!isCascadingUse.hasValue())
            isCascadingUse = ND->isCascadingContextForLookup(false);
        } else if (auto I = dyn_cast<DefaultArgumentInitializer>(DC)) {
          // In a default argument, skip immediately out of both the
          // initializer and the function.
          isCascadingUse = false;
          DC = I->getParent()->getParent();
          continue;
        } else {
          assert(isa<TopLevelCodeDecl>(DC) || isa<Initializer>(DC) ||
                 isa<TypeAliasDecl>(DC));
          if (!isCascadingUse.hasValue())
            isCascadingUse = DC->isCascadingContextForLookup(false);
        }

        // Check the generic parameters for something with the given name.
        if (GenericParams) {
          namelookup::FindLocalVal localVal(SM, Loc, Consumer);
          localVal.checkGenericParams(GenericParams);

          if (shouldReturnBasedOnResults())
            return;
        }

        if (BaseDC && ExtendedType && !ExtendedType->hasError()) {
          NLOptions options = baseNLOptions;
          if (isCascadingUse.getValue())
            options |= NL_KnownCascadingDependency;
          else
            options |= NL_KnownNonCascadingDependency;

          SmallVector<ValueDecl *, 4> Lookup;
          DC->lookupQualified(ExtendedType, Name, options, TypeResolver, Lookup);
          bool FoundAny = false;
          auto startIndex = Results.size();
          for (auto Result : Lookup) {
            // Classify this declaration.
            FoundAny = true;

            // Types are local or metatype members.
            if (auto TD = dyn_cast<TypeDecl>(Result)) {
              if (isa<GenericTypeParamDecl>(TD))
                Results.push_back(LookupResultEntry(Result));
              else
                Results.push_back(LookupResultEntry(MetaBaseDC, Result));
              continue;
            }

            Results.push_back(LookupResultEntry(BaseDC, Result));
          }

          if (FoundAny) {
            // Predicate that determines whether a lookup result should
            // be unavailable except as a last-ditch effort.
            auto unavailableLookupResult =
              [&](const LookupResultEntry &result) {
              auto &effectiveVersion = Ctx.LangOpts.EffectiveLanguageVersion;
              return result.getValueDecl()->getAttrs()
                  .isUnavailableInSwiftVersion(effectiveVersion);
            };

            // If all of the results we found are unavailable, keep looking.
            auto begin = Results.begin() + startIndex;
            if (std::all_of(begin, Results.end(), unavailableLookupResult)) {
              UnavailableInnerResults.append(begin, Results.end());
              Results.erase(begin, Results.end());
            } else {
              if (DebugClient)
                filterForDiscriminator(Results, DebugClient);

              if (shouldReturnBasedOnResults())
                return;
            }
          }
        }

        // Check the generic parameters if our context is a generic type or
        // extension thereof.
        GenericParamList *dcGenericParams = nullptr;
        if (auto nominal = dyn_cast<NominalTypeDecl>(DC))
          dcGenericParams = nominal->getGenericParams();
        else if (auto ext = dyn_cast<ExtensionDecl>(DC))
          dcGenericParams = ext->getGenericParams();
        else if (auto subscript = dyn_cast<SubscriptDecl>(DC))
          dcGenericParams = subscript->getGenericParams();

        while (dcGenericParams) {
          namelookup::FindLocalVal localVal(SM, Loc, Consumer);
          localVal.checkGenericParams(dcGenericParams);

          if (shouldReturnBasedOnResults())
            return;

          if (!isa<ExtensionDecl>(DC))
            break;

          dcGenericParams = dcGenericParams->getOuterParameters();
        }

        DC = DC->getParentForLookup();
      }

      if (!isCascadingUse.hasValue())
        isCascadingUse = true;
    }

    if (auto SF = dyn_cast<SourceFile>(DC)) {
      if (Loc.isValid()) {
        // Look for local variables in top-level code; normally, the parser
        // resolves these for us, but it can't do the right thing for
        // local types.
        namelookup::FindLocalVal localVal(SM, Loc, Consumer);
        localVal.checkSourceFile(*SF);
        if (shouldReturnBasedOnResults())
          return;
      }
    }
  }

  // TODO: Does the debugger client care about compound names?
  if (Name.isSimpleName() && DebugClient &&
      DebugClient->lookupOverrides(Name.getBaseName(), DC, Loc,
                                   isOriginallyTypeLookup, Results))
    return;

  recordLookupOfTopLevelName(DC, Name, isCascadingUse.getValue());

  // Add private imports to the extra search list.
  SmallVector<ModuleDecl::ImportedModule, 8> extraImports;
  if (auto FU = dyn_cast<FileUnit>(DC))
    FU->getImportedModules(extraImports, ModuleDecl::ImportFilter::Private);

  using namespace namelookup;
  SmallVector<ValueDecl *, 8> CurModuleResults;
  auto resolutionKind = isOriginallyTypeLookup ? ResolutionKind::TypesOnly
                                               : ResolutionKind::Overloadable;
  lookupInModule(&M, {}, Name, CurModuleResults, NLKind::UnqualifiedLookup,
                 resolutionKind, TypeResolver, DC, extraImports);

  for (auto VD : CurModuleResults)
    Results.push_back(LookupResultEntry(VD));

  if (DebugClient)
    filterForDiscriminator(Results, DebugClient);

  // Now add any names the DebugClient knows about to the lookup.
  if (Name.isSimpleName() && DebugClient)
    DebugClient->lookupAdditions(Name.getBaseName(), DC, Loc,
                                 isOriginallyTypeLookup, Results);

  // If we've found something, we're done.
  if (shouldReturnBasedOnResults(/*noMoreOuterResults=*/true))
    return;

  // If we still haven't found anything, but we do have some
  // declarations that are "unavailable in the current Swift", drop
  // those in.
  Results = std::move(UnavailableInnerResults);
  if (shouldReturnBasedOnResults(/*noMoreOuterResults=*/true))
    return;

  if (!Name.isSimpleName())
    return;

  // Look for a module with the given name.
  if (Name.isSimpleName(M.getName())) {
    Results.push_back(LookupResultEntry(&M));
    if (shouldReturnBasedOnResults(/*noMoreOuterResults=*/true))
      return;
  }

  ModuleDecl *desiredModule = Ctx.getLoadedModule(Name.getBaseIdentifier());
  if (!desiredModule && Name == Ctx.TheBuiltinModule->getName())
    desiredModule = Ctx.TheBuiltinModule;
  if (desiredModule) {
    forAllVisibleModules(DC, [&](const ModuleDecl::ImportedModule &import) -> bool {
      if (import.second == desiredModule) {
        Results.push_back(LookupResultEntry(import.second));
        return false;
      }
      return true;
    });
  }
  // Make sure we've recorded the inner-result-boundary.
  (void)shouldReturnBasedOnResults(/*noMoreOuterResults=*/true);
}

TypeDecl* UnqualifiedLookup::getSingleTypeResult() {
  if (Results.size() != 1)
    return nullptr;
  return dyn_cast<TypeDecl>(Results.back().getValueDecl());
}

#pragma mark Member lookup table

void LazyMemberLoader::anchor() {}

void LazyConformanceLoader::anchor() {}

/// Lookup table used to store members of a nominal type (and its extensions)
/// for fast retrieval.
class swift::MemberLookupTable {
  /// The last extension that was included within the member lookup table's
  /// results.
  ExtensionDecl *LastExtensionIncluded = nullptr;

  /// The type of the internal lookup table.
  typedef llvm::DenseMap<DeclName, llvm::TinyPtrVector<ValueDecl *>>
    LookupTable;

  /// Lookup table mapping names to the set of declarations with that name.
  LookupTable Lookup;

public:
  /// Create a new member lookup table.
  explicit MemberLookupTable(ASTContext &ctx);

  /// Update a lookup table with members from newly-added extensions.
  void updateLookupTable(NominalTypeDecl *nominal);

  /// \brief Add the given member to the lookup table.
  void addMember(Decl *members);

  /// \brief Add the given members to the lookup table.
  void addMembers(DeclRange members);

  /// \brief The given extension has been extended with new members; add them
  /// if appropriate.
  void addExtensionMembers(NominalTypeDecl *nominal,
                           ExtensionDecl *ext,
                           DeclRange members);

  /// Iterator into the lookup table.
  typedef LookupTable::iterator iterator;

  iterator begin() { return Lookup.begin(); }
  iterator end() { return Lookup.end(); }

  iterator find(DeclName name) {
    return Lookup.find(name);
  }

  // \brief Mark all Decls in this table as not-resident in a table, drop
  // references to them. Should only be called when this was not fully-populated
  // from an IterableDeclContext.
  void clear() {
    // LastExtensionIncluded would only be non-null if this was populated from
    // an IterableDeclContext (though it might still be null in that case).
    assert(LastExtensionIncluded == nullptr);
    for (auto const &i : Lookup) {
      for (auto d : i.getSecond()) {
        d->setAlreadyInLookupTable(false);
      }
    }
    Lookup.clear();
  }

  // Only allow allocation of member lookup tables using the allocator in
  // ASTContext or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(MemberLookupTable)) {
    return C.Allocate(Bytes, Alignment);
  }
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
};

namespace {
  /// Stores the set of Objective-C methods with a given selector within the
  /// Objective-C method lookup table.
  struct StoredObjCMethods {
    /// The generation count at which this list was last updated.
    unsigned Generation = 0;

    /// The set of methods with the given selector.
    llvm::TinyPtrVector<AbstractFunctionDecl *> Methods;
  };
} // end anonymous namespace

/// Class member lookup table, which is a member lookup table with a second
/// table for lookup based on Objective-C selector.
class ClassDecl::ObjCMethodLookupTable
        : public llvm::DenseMap<std::pair<ObjCSelector, char>,
                                StoredObjCMethods>
{
public:
  // Only allow allocation of member lookup tables using the allocator in
  // ASTContext or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(MemberLookupTable)) {
    return C.Allocate(Bytes, Alignment);
  }
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
};

MemberLookupTable::MemberLookupTable(ASTContext &ctx) {
  // Register a cleanup with the ASTContext to call the lookup table
  // destructor.
  ctx.addCleanup([this]() {
    this->~MemberLookupTable();
  });
}

void MemberLookupTable::addMember(Decl *member) {
  // Only value declarations matter.
  auto vd = dyn_cast<ValueDecl>(member);
  if (!vd)
    return;

  // Unnamed entities cannot be found by name lookup.
  if (!vd->hasName())
    return;

  // If this declaration is already in the lookup table, don't add it
  // again.
  if (vd->isAlreadyInLookupTable()) {
    return;
  }
  vd->setAlreadyInLookupTable();

  // Add this declaration to the lookup set under its compound name and simple
  // name.
  vd->getFullName().addToLookupTable(Lookup, vd);
}

void MemberLookupTable::addMembers(DeclRange members) {
  for (auto member : members) {
    addMember(member);
  }
}

void MemberLookupTable::addExtensionMembers(NominalTypeDecl *nominal,
                                            ExtensionDecl *ext,
                                            DeclRange members) {
  // We have not processed any extensions yet, so there's nothing to do.
  if (!LastExtensionIncluded)
    return;

  // If this extension shows up in the list of extensions not yet included
  // in the lookup table, there's nothing to do.
  for (auto notIncluded = LastExtensionIncluded->NextExtension.getPointer();
       notIncluded;
       notIncluded = notIncluded->NextExtension.getPointer()) {
    if (notIncluded == ext)
      return;
  }

  // Add the new members to the lookup table.
  addMembers(members);
}

void MemberLookupTable::updateLookupTable(NominalTypeDecl *nominal) {
  // If the last extension we included is the same as the last known extension,
  // we're already up-to-date.
  if (LastExtensionIncluded == nominal->LastExtension)
    return;

  // Add members from each of the extensions that we have not yet visited.
  for (auto next = LastExtensionIncluded
                     ? LastExtensionIncluded->NextExtension.getPointer()
                     : nominal->FirstExtension;
       next;
       (LastExtensionIncluded = next,next = next->NextExtension.getPointer())) {
    addMembers(next->getMembers());
  }
}

void NominalTypeDecl::addedMember(Decl *member) {
  // If we have a lookup table, add the new member to it.
  if (LookupTable.getPointer()) {
    LookupTable.getPointer()->addMember(member);
  }
}

void ExtensionDecl::addedMember(Decl *member) {
  if (NextExtension.getInt()) {
    if (getExtendedType()->hasError())
      return;

    auto nominal = getExtendedType()->getAnyNominal();
    if (nominal->LookupTable.getPointer() &&
        nominal->LookupTable.getInt()) {
      // Make sure we have the complete list of extensions.
      // FIXME: This is completely unnecessary. We want to determine whether
      // our own extension has already been included in the lookup table.
      (void)nominal->getExtensions();

      nominal->LookupTable.getPointer()->addMember(member);
    }
  }
}

// For lack of anywhere more sensible to put it, here's a diagram of the pieces
// involved in finding members and extensions of a NominalTypeDecl.
//
// ┌────────────────────────────┬─┐
// │IterableDeclContext         │ │     ┌─────────────────────────────┐
// │-------------------         │ │     │┌───────────────┬┐           ▼
// │Decl *LastDecl   ───────────┼─┼─────┘│Decl           ││  ┌───────────────┬┐
// │Decl *FirstDecl  ───────────┼─┼─────▶│----           ││  │Decl           ││
// │                            │ │      │Decl  *NextDecl├┼─▶│----           ││
// │bool HasLazyMembers         │ │      ├───────────────┘│  │Decl *NextDecl ││
// │IterableDeclContextKind Kind│ │      │                │  ├───────────────┘│
// │                            │ │      │ValueDecl       │  │                │
// ├────────────────────────────┘ │      │---------       │  │ValueDecl       │
// │                              │      │DeclName Name   │  │---------       │
// │NominalTypeDecl               │      └────────────────┘  │DeclName Name   │
// │---------------               │               ▲          └────────────────┘
// │ExtensionDecl *FirstExtension─┼────────┐      │                   ▲
// │ExtensionDecl *LastExtension ─┼───────┐│      │                   └───┐
// │                              │       ││      └──────────────────────┐│
// │MemberLookupTable *LookupTable├─┐     ││                             ││
// │bool LookupTableComplete      │ │     ││     ┌─────────────────┐     ││
// └──────────────────────────────┘ │     ││     │ExtensionDecl    │     ││
//                                  │     ││     │-------------    │     ││
//                    ┌─────────────┘     │└────▶│ExtensionDecl    │     ││
//                    │                   │      │  *NextExtension ├──┐  ││
//                    ▼                   │      └─────────────────┘  │  ││
// ┌─────────────────────────────────────┐│      ┌─────────────────┐  │  ││
// │MemberLookupTable                    ││      │ExtensionDecl    │  │  ││
// │-----------------                    ││      │-------------    │  │  ││
// │ExtensionDecl *LastExtensionIncluded ├┴─────▶│ExtensionDecl    │◀─┘  ││
// │                                     │       │  *NextExtension │     ││
// │┌───────────────────────────────────┐│       └─────────────────┘     ││
// ││DenseMap<Declname, ...> LookupTable││                               ││
// ││-----------------------------------││  ┌──────────────────────────┐ ││
// ││[NameA] TinyPtrVector<ValueDecl *> ││  │TinyPtrVector<ValueDecl *>│ ││
// ││[NameB] TinyPtrVector<ValueDecl *> ││  │--------------------------│ ││
// ││[NameC] TinyPtrVector<ValueDecl *>─┼┼─▶│[0] ValueDecl *      ─────┼─┘│
// │└───────────────────────────────────┘│  │[1] ValueDecl *      ─────┼──┘
// └─────────────────────────────────────┘  └──────────────────────────┘
//
// The HasLazyMembers, Kind, and LookupTableComplete fields are packed into
// PointerIntPairs so don't go grepping for them; but for purposes of
// illustration they are effectively their own fields.
//
// MemberLookupTable is populated en-masse when the IterableDeclContext's
// (IDC's) list of Decls is populated. But MemberLookupTable can also be
// populated incrementally by one-name-at-a-time lookups by lookupDirect, in
// which case those Decls are _not_ added to the IDC's list. They are cached in
// the loader they come from, lifecycle-wise, and are added to the
// MemberLookupTable to accelerate subsequent retrieval, but the IDC is not
// considered populated until someone calls getMembers().
//
// If the IDC list is later populated and/or an extension is added _after_
// MemberLookupTable is constructed (and possibly has entries in it),
// MemberLookupTable is purged and reconstructed from IDC's list.
//
// In all lookup routines, the 'ignoreNewExtensions' flag means that
// lookup should only use the set of extensions already observed.

static bool
populateLookupTableEntryFromLazyIDCLoader(ASTContext &ctx,
                                          MemberLookupTable &LookupTable,
                                          DeclName name,
                                          IterableDeclContext *IDC) {
  if (IDC->isLoadingLazyMembers()) {
    return false;
  }
  IDC->setLoadingLazyMembers(true);
  auto ci = ctx.getOrCreateLazyIterableContextData(IDC,
                                                   /*lazyLoader=*/nullptr);
  if (auto res = ci->loader->loadNamedMembers(IDC, name.getBaseName(),
                                              ci->memberData)) {
    IDC->setLoadingLazyMembers(false);
    if (auto s = ctx.Stats) {
      ++s->getFrontendCounters().NamedLazyMemberLoadSuccessCount;
    }
    for (auto d : *res) {
      LookupTable.addMember(d);
    }
    return false;
  } else {
    IDC->setLoadingLazyMembers(false);
    if (auto s = ctx.Stats) {
      ++s->getFrontendCounters().NamedLazyMemberLoadFailureCount;
    }
    return true;
  }
}

static void populateLookupTableEntryFromCurrentMembersWithoutLoading(
    ASTContext &ctx, MemberLookupTable &LookupTable, DeclName name,
    IterableDeclContext *IDC) {
  for (auto m : IDC->getCurrentMembersWithoutLoading()) {
    if (auto v = dyn_cast<ValueDecl>(m)) {
      if (v->getFullName().matchesRef(name.getBaseName())) {
        LookupTable.addMember(m);
      }
    }
  }
}

static bool
populateLookupTableEntryFromExtensions(ASTContext &ctx,
                                       MemberLookupTable &table,
                                       NominalTypeDecl *nominal,
                                       DeclName name,
                                       bool ignoreNewExtensions) {
  if (!ignoreNewExtensions) {
    for (auto e : nominal->getExtensions()) {
      if (e->wasDeserialized() || e->hasClangNode()) {
        if (populateLookupTableEntryFromLazyIDCLoader(ctx, table,
                                                      name, e)) {
          return true;
        }
      } else {
        populateLookupTableEntryFromCurrentMembersWithoutLoading(ctx, table,
                                                                 name, e);
      }
    }
  }
  return false;
}

void NominalTypeDecl::prepareLookupTable(bool ignoreNewExtensions) {
  // If we haven't allocated the lookup table yet, do so now.
  if (!LookupTable.getPointer()) {
    auto &ctx = getASTContext();
    LookupTable.setPointer(new (ctx) MemberLookupTable(ctx));
  }

  if (hasLazyMembers()) {
    // Lazy members: if the table needs population, populate the table _only
    // from those members already in the IDC member list_ such as implicits or
    // globals-as-members, then update table entries from the extensions that
    // have the same names as any such initial-population members.
    if (!LookupTable.getInt()) {
      LookupTable.setInt(true);
      LookupTable.getPointer()->addMembers(getCurrentMembersWithoutLoading());
      for (auto *m : getCurrentMembersWithoutLoading()) {
        if (auto v = dyn_cast<ValueDecl>(m)) {
          populateLookupTableEntryFromExtensions(getASTContext(),
                                                 *LookupTable.getPointer(),
                                                 this, v->getBaseName(),
                                                 ignoreNewExtensions);
        }
      }
    }

  } else {
    // No lazy members: if the table needs population, populate the table
    // en-masse; and in either case update the extensions.
    if (!LookupTable.getInt()) {
      LookupTable.setInt(true);
      LookupTable.getPointer()->addMembers(getMembers());
    }
    if (!ignoreNewExtensions) {
      LookupTable.getPointer()->updateLookupTable(this);
    }
  }
}

void NominalTypeDecl::makeMemberVisible(ValueDecl *member) {
  if (!LookupTable.getPointer()) {
    auto &ctx = getASTContext();
    LookupTable.setPointer(new (ctx) MemberLookupTable(ctx));
  }
  
  LookupTable.getPointer()->addMember(member);
}

TinyPtrVector<ValueDecl *> NominalTypeDecl::lookupDirect(
                                                  DeclName name,
                                                  bool ignoreNewExtensions) {
  ASTContext &ctx = getASTContext();
  FrontendStatsTracer tracer(ctx.Stats, "lookup-direct", this);
  if (auto s = ctx.Stats) {
    ++s->getFrontendCounters().NominalTypeLookupDirectCount;
  }

  // We only use NamedLazyMemberLoading when a user opts-in and we have
  // not yet loaded all the members into the IDC list in the first place.
  bool useNamedLazyMemberLoading = (ctx.LangOpts.NamedLazyMemberLoading &&
                                    hasLazyMembers());

  // FIXME: At present, lazy member loading conflicts with a bunch of other code
  // that appears to special-case initializers (clang-imported initializer
  // sorting, implicit initializer synthesis), so for the time being we have to
  // turn it off for them entirely.
  if (name.getBaseName() == DeclBaseName::createConstructor())
    useNamedLazyMemberLoading = false;

  LLVM_DEBUG(llvm::dbgs() << getNameStr() << ".lookupDirect(" << name << ")"
        << ", lookupTable.getInt()=" << LookupTable.getInt()
        << ", hasLazyMembers()=" << hasLazyMembers()
        << ", useNamedLazyMemberLoading=" << useNamedLazyMemberLoading
        << "\n");

  // We check the LookupTable at most twice, possibly treating a miss in the
  // first try as a cache-miss that we then do a cache-fill on, and retry.
  for (int i = 0; i < 2; ++i) {

    // First, if we're _not_ doing NamedLazyMemberLoading, we make sure we've
    // populated the IDC and brought it up to date with any extensions. This
    // will flip the hasLazyMembers() flag to false as well.
    if (!useNamedLazyMemberLoading) {
      // It's possible that the lookup table exists but has information in it
      // that is either currently out of date or soon to be out of date.
      // This can happen two ways:
      //
      //   - We've not yet indexed the members we have (LookupTable.getInt()
      //     is zero).
      //
      //   - We've still got more lazy members left to load; this can happen
      //     even if we _did_ index some members.
      //
      // In either of these cases, we want to reset the table to empty and
      // mark it as needing reconstruction.
      if (LookupTable.getPointer() &&
          (hasLazyMembers() || !LookupTable.getInt())) {
        LookupTable.getPointer()->clear();
        LookupTable.setInt(false);
      }

      (void)getMembers();

      // Make sure we have the complete list of members (in this nominal and in
      // all extensions).
      if (!ignoreNewExtensions) {
        for (auto E : getExtensions())
          (void)E->getMembers();
      }
    }

    // Next, in all cases, prepare the lookup table for use, possibly
    // repopulating it from the IDC if the IDC member list has just grown.
    prepareLookupTable(ignoreNewExtensions);

    // Look for a declaration with this name.
    auto known = LookupTable.getPointer()->find(name);

    // We found something; return it.
    if (known != LookupTable.getPointer()->end())
      return known->second;

    // If we have no more second chances, stop now.
    if (!useNamedLazyMemberLoading || i > 0)
      break;

    // If we get here, we had a cache-miss and _are_ using
    // NamedLazyMemberLoading. Try to populate a _single_ entry in the
    // MemberLookupTable from both this nominal and all of its extensions, and
    // retry. Any failure to load here flips the useNamedLazyMemberLoading to
    // false, and we fall back to loading all members during the retry.
    auto &Table = *LookupTable.getPointer();
    if (populateLookupTableEntryFromLazyIDCLoader(ctx, Table,
                                                  name, this) ||
        populateLookupTableEntryFromExtensions(ctx, Table, this, name,
                                               ignoreNewExtensions)) {
      useNamedLazyMemberLoading = false;
    }
  }

  // None of our attempts found anything.
  return { };
}

void ClassDecl::createObjCMethodLookup() {
  assert(!ObjCMethodLookup && "Already have an Objective-C member table");
  auto &ctx = getASTContext();
  ObjCMethodLookup = new (ctx) ObjCMethodLookupTable();

  // Register a cleanup with the ASTContext to call the lookup table
  // destructor.
  ctx.addCleanup([this]() {
    this->ObjCMethodLookup->~ObjCMethodLookupTable();
  });
}

MutableArrayRef<AbstractFunctionDecl *>
ClassDecl::lookupDirect(ObjCSelector selector, bool isInstance) {
  if (!ObjCMethodLookup) {
    createObjCMethodLookup();
  }

  // If any modules have been loaded since we did the search last (or if we
  // hadn't searched before), look in those modules, too.
  auto &stored = (*ObjCMethodLookup)[{selector, isInstance}];
  ASTContext &ctx = getASTContext();
  if (ctx.getCurrentGeneration() > stored.Generation) {
    ctx.loadObjCMethods(this, selector, isInstance, stored.Generation,
                        stored.Methods);
    stored.Generation = ctx.getCurrentGeneration();
  }

  return { stored.Methods.begin(), stored.Methods.end() };
}

void ClassDecl::recordObjCMethod(AbstractFunctionDecl *method) {
  if (!ObjCMethodLookup) {
    createObjCMethodLookup();
  }

  // Record the method.
  bool isInstanceMethod = method->isObjCInstanceMethod();
  auto selector = method->getObjCSelector();
  auto &vec = (*ObjCMethodLookup)[{selector, isInstanceMethod}].Methods;

  // In a non-empty vector, we could have duplicates or conflicts.
  if (!vec.empty()) {
    // Check whether we have a duplicate. This only checks more than one
    // element in ill-formed code, so the linear search is acceptable.
    if (std::find(vec.begin(), vec.end(), method) != vec.end())
      return;

    if (vec.size() == 1) {
      // We have a conflict.
      getASTContext().recordObjCMethodConflict(this, selector,
                                               isInstanceMethod);
    }
  } else {
    // Record the first method that has this selector.
    getASTContext().recordObjCMethod(method);
  }

  vec.push_back(method);
}

/// Configure name lookup for the given declaration context and options.
///
/// This utility is used by qualified name lookup.
static void configureLookup(const DeclContext *dc,
                            NLOptions &options,
                            ReferencedNameTracker *&tracker,
                            bool &isLookupCascading) {
  auto &ctx = dc->getASTContext();
  if (!ctx.LangOpts.EnableAccessControl)
    options |= NL_IgnoreAccessControl;

  // Find the dependency tracker we'll need for this lookup.
  tracker = nullptr;
  if (auto containingSourceFile =
          dyn_cast<SourceFile>(dc->getModuleScopeContext())) {
    tracker = containingSourceFile->getReferencedNameTracker();
  }

  auto checkLookupCascading = [dc, options]() -> Optional<bool> {
    switch (static_cast<unsigned>(options & NL_KnownDependencyMask)) {
    case 0:
      return dc->isCascadingContextForLookup(
               /*functionsAreNonCascading=*/false);
    case NL_KnownNonCascadingDependency:
      return false;
    case NL_KnownCascadingDependency:
      return true;
    case NL_KnownNoDependency:
      return None;
    default:
      // FIXME: Use llvm::CountPopulation_64 when that's declared constexpr.
#if defined(__clang__) || defined(__GNUC__)
      static_assert(__builtin_popcountll(NL_KnownDependencyMask) == 2,
                    "mask should only include four values");
#endif
      llvm_unreachable("mask only includes four values");
    }
  };

  // Determine whether a lookup here will cascade.
  isLookupCascading = false;
  if (tracker) {
    if (auto maybeLookupCascade = checkLookupCascading())
      isLookupCascading = maybeLookupCascade.getValue();
    else
      tracker = nullptr;
  }
}

/// Determine whether the given declaration is an acceptable lookup
/// result when searching from the given DeclContext.
static bool isAcceptableLookupResult(const DeclContext *dc,
                                     NLOptions options,
                                     ValueDecl *decl,
                                     bool onlyCompleteObjectInits) {
  // Filter out designated initializers, if requested.
  if (onlyCompleteObjectInits) {
    if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
      if (isa<ClassDecl>(ctor->getDeclContext()) && !ctor->isInheritable())
        return false;
    } else {
      return false;
    }
  }

  // Ignore stub implementations.
  if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    if (ctor->hasStubImplementation())
      return false;
  }

  // Check access.
  if (!(options & NL_IgnoreAccessControl)) {
    return decl->isAccessibleFrom(dc);
  }

  return true;
}

/// Only name lookup has gathered a set of results, perform any necessary
/// steps to prune the result set before returning it to the caller.
static bool finishLookup(const DeclContext *dc, NLOptions options,
                         SmallVectorImpl<ValueDecl *> &decls) {
  // If we're supposed to remove overridden declarations, do so now.
  if (options & NL_RemoveOverridden)
    removeOverriddenDecls(decls);

  // If we're supposed to remove shadowed/hidden declarations, do so now.
  ModuleDecl *M = dc->getParentModule();
  if (options & NL_RemoveNonVisible)
    removeShadowedDecls(decls, M);

  if (auto *debugClient = M->getDebugClient())
    filterForDiscriminator(decls, debugClient);

  // We're done. Report success/failure.
  return !decls.empty();
}

/// Inspect the given type to determine which nominal type declarations it
/// directly references, to facilitate name lookup into those types.
static void extractDirectlyReferencedNominalTypes(
              Type type, SmallVectorImpl<NominalTypeDecl *> &decls) {
  if (auto nominal = type->getAnyNominal()) {
    decls.push_back(nominal);
    return;
  }

  if (auto archetypeTy = type->getAs<ArchetypeType>()) {
    // Look in the protocols to which the archetype conforms (always).
    for (auto proto : archetypeTy->getConformsTo())
      decls.push_back(proto);

    // Look into the superclasses of this archetype.
    if (auto superclass = archetypeTy->getSuperclass()) {
      if (auto superclassDecl = superclass->getClassOrBoundGenericClass())
        decls.push_back(superclassDecl);
    }

    return;
  }

  if (auto compositionTy = type->getAs<ProtocolCompositionType>()) {
    auto layout = compositionTy->getExistentialLayout();

    for (auto proto : layout.getProtocols()) {
      auto *protoDecl = proto->getDecl();
      decls.push_back(protoDecl);
    }

    if (auto superclass = layout.explicitSuperclass) {
      auto *superclassDecl = superclass->getClassOrBoundGenericClass();
      if (superclassDecl)
        decls.push_back(superclassDecl);
    }

    return;
  }

  llvm_unreachable("Not a type containing nominal types?");
}

bool DeclContext::lookupQualified(Type type,
                                  DeclName member,
                                  NLOptions options,
                                  LazyResolver *typeResolver,
                                  SmallVectorImpl<ValueDecl *> &decls) const {
  using namespace namelookup;
  assert(decls.empty() && "additive lookup not supported");

  // Handle AnyObject lookup.
  if (type->isAnyObject())
    return lookupAnyObject(member, options, decls);

  // Handle lookup in a module.
  if (auto moduleTy = type->getAs<ModuleType>())
    return lookupQualified(moduleTy->getModule(), member, options, decls);

  // Figure out which nominal types we will look into.
  SmallVector<NominalTypeDecl *, 4> nominalTypesToLookInto;
  extractDirectlyReferencedNominalTypes(type, nominalTypesToLookInto);

  return lookupQualified(nominalTypesToLookInto, member, options, decls);
}

bool DeclContext::lookupQualified(ArrayRef<NominalTypeDecl *> typeDecls,
                                  DeclName member,
                                  NLOptions options,
                                  SmallVectorImpl<ValueDecl *> &decls) const {
  using namespace namelookup;
  assert(decls.empty() && "additive lookup not supported");

  // Configure lookup and dig out the tracker.
  ReferencedNameTracker *tracker = nullptr;
  bool isLookupCascading;
  configureLookup(this, options, tracker, isLookupCascading);

  // Tracking for the nominal types we'll visit.
  SmallVector<NominalTypeDecl *, 4> stack;
  llvm::SmallPtrSet<NominalTypeDecl *, 4> visited;
  bool sawClassDecl = false;

  // Add the given nominal type to the stack.
  auto addNominalType = [&](NominalTypeDecl *nominal) {
    if (!visited.insert(nominal).second)
      return false;

    if (isa<ClassDecl>(nominal))
      sawClassDecl = true;

    stack.push_back(nominal);
    return true;
  };

  // Look through the type declarations we were given, resolving
  ASTContext &ctx = getASTContext();
  for (auto nominal : typeDecls) {
    addNominalType(nominal);
  }

  // Whether we only want to return complete object initializers.
  bool onlyCompleteObjectInits = false;

  // Visit all of the nominal types we know about, discovering any others
  // we need along the way.
  auto typeResolver = ctx.getLazyResolver();
  bool wantProtocolMembers = (options & NL_ProtocolMembers);
  while (!stack.empty()) {
    auto current = stack.back();
    stack.pop_back();

    if (tracker)
      tracker->addUsedMember({current, member.getBaseName()},isLookupCascading);

    // Make sure we've resolved implicit members, if we need them.
    if (typeResolver) {
      if (member.getBaseName() == DeclBaseName::createConstructor())
        typeResolver->resolveImplicitConstructors(current);

      typeResolver->resolveImplicitMember(current, member);
    }

    // Look for results within the current nominal type and its extensions.
    bool currentIsProtocol = isa<ProtocolDecl>(current);
    for (auto decl : current->lookupDirect(member)) {
      // If we're performing a type lookup, don't even attempt to validate
      // the decl if its not a type.
      if ((options & NL_OnlyTypes) && !isa<TypeDecl>(decl))
        continue;

      if (isAcceptableLookupResult(this, options, decl,
                                   onlyCompleteObjectInits))
        decls.push_back(decl);
    }

    // Visit superclass.
    if (auto classDecl = dyn_cast<ClassDecl>(current)) {
      // If we're looking for initializers, only look at the superclass if the
      // current class permits inheritance. Even then, only find complete
      // object initializers.
      bool visitSuperclass = true;
      if (member.getBaseName() == DeclBaseName::createConstructor()) {
        if (classDecl->inheritsSuperclassInitializers(typeResolver))
          onlyCompleteObjectInits = true;
        else
          visitSuperclass = false;
      }

      if (visitSuperclass) {
        if (auto superclassDecl = classDecl->getSuperclassDecl())
          if (visited.insert(superclassDecl).second)
            stack.push_back(superclassDecl);
      }
    }

    // If we're not looking at a protocol and we're not supposed to
    // visit the protocols that this type conforms to, skip the next
    // step.
    if (!wantProtocolMembers && !currentIsProtocol)
      continue;

    if (!sawClassDecl) {
      if (auto *protoDecl = dyn_cast<ProtocolDecl>(current)) {
        if (auto superclassDecl = protoDecl->getSuperclassDecl()) {
          visited.insert(superclassDecl);
          stack.push_back(superclassDecl);
        }
      }
    }

    SmallVector<ProtocolDecl *, 4> protocols;
    for (auto proto : current->getAllProtocols()) {
      if (visited.insert(proto).second) {
        stack.push_back(proto);
      }
    }

    // For a class, we don't need to visit the protocol members of the
    // superclass: that's already handled.
    if (isa<ClassDecl>(current))
      wantProtocolMembers = false;
  }

  return finishLookup(this, options, decls);
}

bool DeclContext::lookupQualified(ModuleDecl *module, DeclName member,
                                  NLOptions options,
                                  SmallVectorImpl<ValueDecl *> &decls) const {
  using namespace namelookup;
  assert(decls.empty() && "additive lookup not supported");

  // Configure lookup and dig out the tracker.
  ReferencedNameTracker *tracker = nullptr;
  bool isLookupCascading;
  configureLookup(this, options, tracker, isLookupCascading);

  ASTContext &ctx = getASTContext();
  auto topLevelScope = getModuleScopeContext();
  if (module == topLevelScope->getParentModule()) {
    if (tracker) {
      recordLookupOfTopLevelName(topLevelScope, member, isLookupCascading);
    }
    lookupInModule(module, /*accessPath=*/{}, member, decls,
                   NLKind::QualifiedLookup, ResolutionKind::Overloadable,
                   ctx.getLazyResolver(), topLevelScope);
  } else {
    // Note: This is a lookup into another module. Unless we're compiling
    // multiple modules at once, or if the other module re-exports this one,
    // it shouldn't be possible to have a dependency from that module on
    // anything in this one.

    // Perform the lookup in all imports of this module.
    forAllVisibleModules(this,
                         [&](const ModuleDecl::ImportedModule &import) -> bool {
      if (import.second != module)
        return true;
      lookupInModule(import.second, import.first, member, decls,
                     NLKind::QualifiedLookup, ResolutionKind::Overloadable,
                     ctx.getLazyResolver(), topLevelScope);
      // If we're able to do an unscoped lookup, we see everything. No need
      // to keep going.
      return !import.first.empty();
    });
  }

  llvm::SmallPtrSet<ValueDecl *, 4> knownDecls;
  decls.erase(std::remove_if(decls.begin(), decls.end(),
                             [&](ValueDecl *vd) -> bool {
    // If we're performing a type lookup, skip non-types.
    if ((options & NL_OnlyTypes) && !isa<TypeDecl>(vd))
      return true;

    return !knownDecls.insert(vd).second;
  }), decls.end());

  return finishLookup(this, options, decls);
}

bool DeclContext::lookupAnyObject(DeclName member, NLOptions options,
                                  SmallVectorImpl<ValueDecl *> &decls) const {
  using namespace namelookup;
  assert(decls.empty() && "additive lookup not supported");

  // Configure lookup and dig out the tracker.
  ReferencedNameTracker *tracker = nullptr;
  bool isLookupCascading;
  configureLookup(this, options, tracker, isLookupCascading);

  // Record this lookup.
  if (tracker)
    tracker->addDynamicLookupName(member.getBaseName(), isLookupCascading);

  // Type-only lookup won't find anything on AnyObject.
  if (options & NL_OnlyTypes)
    return false;

  // Collect all of the visible declarations.
  SmallVector<ValueDecl *, 4> allDecls;
  forAllVisibleModules(this, [&](ModuleDecl::ImportedModule import) {
    import.second->lookupClassMember(import.first, member, allDecls);
  });

  // For each declaration whose context is not something we've
  // already visited above, add it to the list of declarations.
  llvm::SmallPtrSet<ValueDecl *, 4> knownDecls;
  for (auto decl : allDecls) {
    // If the declaration is not @objc, it cannot be called dynamically.
    if (!decl->isObjC())
      continue;

    // If the declaration has an override, name lookup will also have
    // found the overridden method. Skip this declaration, because we
    // prefer the overridden method.
    if (decl->getOverriddenDecl())
      continue;

    auto dc = decl->getDeclContext();
    auto nominal = dc->getAsNominalTypeOrNominalTypeExtensionContext();
    assert(nominal && "Couldn't find nominal type?");

    // If we didn't see this declaration before, and it's an acceptable
    // result, add it to the list.
    // declaration to the list.
    if (knownDecls.insert(decl).second &&
        isAcceptableLookupResult(this, options, decl,
                                 /*onlyCompleteObjectInits=*/false))
      decls.push_back(decl);
  }

  return finishLookup(this, options, decls);
}

void DeclContext::lookupAllObjCMethods(
       ObjCSelector selector,
       SmallVectorImpl<AbstractFunctionDecl *> &results) const {
  // Collect all of the methods with this selector.
  forAllVisibleModules(this, [&](ModuleDecl::ImportedModule import) {
    import.second->lookupObjCMethods(selector, results);
  });

  // Filter out duplicates.
  llvm::SmallPtrSet<AbstractFunctionDecl *, 8> visited;
  results.erase(
    std::remove_if(results.begin(), results.end(),
                   [&](AbstractFunctionDecl *func) -> bool {
                     return !visited.insert(func).second;
                   }),
    results.end());
}

/// Given a set of type declarations, find all of the nominal type declarations
/// that they reference, looking through typealiases as appropriate.
static TinyPtrVector<NominalTypeDecl *>
resolveTypeDeclsToNominal(Evaluator &evaluator,
                          ASTContext &ctx,
                          ArrayRef<TypeDecl *> typeDecls,
                          SmallVectorImpl<ModuleDecl *> &modulesFound) {
  TinyPtrVector<NominalTypeDecl *> nominalDecls;

  for (auto typeDecl : typeDecls) {
    // Nominal type declarations get copied directly.
    if (auto nominalDecl = dyn_cast<NominalTypeDecl>(typeDecl)) {
      nominalDecls.push_back(nominalDecl);
      continue;
    }

    // Recursively resolve typealiases.
    if (auto typealias = dyn_cast<TypeAliasDecl>(typeDecl)) {
      auto underlyingTypeReferences
        = evaluator(UnderlyingTypeDeclsReferencedRequest{typealias});
      auto underlyingNominalReferences
        = resolveTypeDeclsToNominal(evaluator, ctx, underlyingTypeReferences,
                                    modulesFound);
      nominalDecls.insert(nominalDecls.end(),
                          underlyingNominalReferences.begin(),
                          underlyingNominalReferences.end());
      continue;
    }

    // Keep track of modules we see.
    if (auto module = dyn_cast<ModuleDecl>(typeDecl)) {
      modulesFound.push_back(module);
      continue;
    }

    // Make sure we didn't miss some interesting kind of type declaration.
    assert(isa<AbstractTypeParamDecl>(typeDecl));
  }

  return nominalDecls;
}

/// Perform unqualified name lookup for types at the given location.
static DirectlyReferencedTypeDecls
directReferencesForUnqualifiedTypeLookup(ASTContext &ctx, DeclName name,
                                         SourceLoc loc, DeclContext *dc) {
  DirectlyReferencedTypeDecls results;
  UnqualifiedLookup::Options options = UnqualifiedLookup::Flags::TypeLookup;
  UnqualifiedLookup lookup(name, dc, ctx.getLazyResolver(), loc, options);
  for (const auto &result : lookup.Results) {
    if (auto typeDecl = dyn_cast<TypeDecl>(result.getValueDecl()))
      results.push_back(typeDecl);
  }

  return results;
}

/// Perform qualified name lookup for types.
static DirectlyReferencedTypeDecls
directReferencesForQualifiedTypeLookup(Evaluator &evaluator,
                                       ASTContext &ctx,
                                       ArrayRef<TypeDecl *> baseTypes,
                                       DeclName name,
                                       DeclContext *dc) {
  // Look through the base types to find something on which we can perform
  // qualified name lookup.
  SmallVector<ModuleDecl *, 2> moduleBaseTypes;
  auto nominalBaseTypes =
      resolveTypeDeclsToNominal(evaluator, ctx, baseTypes, moduleBaseTypes);

  DirectlyReferencedTypeDecls result;
  auto addResults = [&result](ArrayRef<ValueDecl *> found){
    for (auto decl : found){
      assert(isa<TypeDecl>(decl) &&
             "Lookup should only have found type declarations");
      result.push_back(cast<TypeDecl>(decl));
    }
  };

  {
    // Look through nominal types.
    SmallVector<ValueDecl *, 4> nominalMembers;
    auto options = NL_RemoveNonVisible | NL_OnlyTypes;
    dc->lookupQualified(nominalBaseTypes, name, options, nominalMembers);
    addResults(nominalMembers);
  }

  {
    // Look through modules.
    auto options = NL_RemoveNonVisible | NL_OnlyTypes;
    for (auto module : moduleBaseTypes) {
      SmallVector<ValueDecl *, 4> moduleMembers;
      dc->lookupQualified(module, name, options, moduleMembers);
      addResults(moduleMembers);
    }
  }

  return result;
}

/// Determine the types directly referenced by the given identifier type.
static DirectlyReferencedTypeDecls
directReferencesForIdentTypeRepr(Evaluator &evaluator,
                                 ASTContext &ctx, IdentTypeRepr *ident,
                                 DeclContext *dc) {
  DirectlyReferencedTypeDecls current;

  bool firstComponent = true;
  for (const auto &component : ident->getComponentRange()) {
    // If we already set a declaration, use it.
    if (auto typeDecl = component->getBoundDecl()) {
      current = {1, typeDecl};
      continue;
    }

    // For the first component, perform unqualified name lookup.
    if (current.empty()) {
      current =
        directReferencesForUnqualifiedTypeLookup(ctx,
                                                 component->getIdentifier(),
                                                 component->getIdLoc(),
                                                 dc);

      // If we didn't find anything, fail now.
      if (current.empty())
        return current;

      firstComponent = false;
      continue;
    }

    // For subsequent components, perform qualified name lookup.
    current =
        directReferencesForQualifiedTypeLookup(evaluator, ctx, current,
                                               component->getIdentifier(), dc);
    if (current.empty())
      return current;
  }

  return current;
}

/// Retrieve the set of type declarations that are directly referenced from
/// the given parsed type representation.
static DirectlyReferencedTypeDecls
directReferencesForTypeRepr(Evaluator &evaluator,
                            ASTContext &ctx, TypeRepr *typeRepr,
                            DeclContext *dc) {
  switch (typeRepr->getKind()) {
  case TypeReprKind::Array:
    return {1, ctx.getArrayDecl()};

  case TypeReprKind::Attributed: {
    auto attributed = cast<AttributedTypeRepr>(typeRepr);
    return directReferencesForTypeRepr(evaluator, ctx,
                                       attributed->getTypeRepr(), dc);
  }

  case TypeReprKind::Composition: {
    DirectlyReferencedTypeDecls result;
    auto composition = cast<CompositionTypeRepr>(typeRepr);
    for (auto component : composition->getTypes()) {
      auto componentResult =
          directReferencesForTypeRepr(evaluator, ctx, component, dc);
      result.insert(result.end(),
                    componentResult.begin(),
                    componentResult.end());
    }
    return result;
  }

  case TypeReprKind::CompoundIdent:
  case TypeReprKind::GenericIdent:
  case TypeReprKind::SimpleIdent:
    return directReferencesForIdentTypeRepr(evaluator, ctx,
                                            cast<IdentTypeRepr>(typeRepr), dc);

  case TypeReprKind::Dictionary:
    return { 1, ctx.getDictionaryDecl()};

  case TypeReprKind::Error:
  case TypeReprKind::Function:
  case TypeReprKind::InOut:
  case TypeReprKind::Metatype:
  case TypeReprKind::Owned:
  case TypeReprKind::Protocol:
  case TypeReprKind::Shared:
  case TypeReprKind::SILBox:
  case TypeReprKind::Tuple:
    return { };

  case TypeReprKind::Fixed:
    llvm_unreachable("Cannot get fixed TypeReprs in name lookup");

  case TypeReprKind::Optional:
  case TypeReprKind::ImplicitlyUnwrappedOptional:
    return { 1, ctx.getOptionalDecl() };
  }
}

/// Retrieve the set of type declarations that are directly referenced from
/// the given type.
static DirectlyReferencedTypeDecls
directReferencesForType(Type type) {
  // If it's a typealias, return that.
  if (auto aliasType = dyn_cast<NameAliasType>(type.getPointer()))
    return { 1, aliasType->getDecl() };

  // If there is a generic declaration, return it.
  if (auto genericDecl = type->getAnyGeneric())
    return { 1, genericDecl };

  if (type->isExistentialType()) {
    DirectlyReferencedTypeDecls result;
    const auto &layout = type->getExistentialLayout();

    // Superclass.
    if (auto superclassType = layout.explicitSuperclass) {
      if (auto superclassDecl = superclassType->getAnyGeneric()) {
        result.push_back(superclassDecl);
      }
    }

    // Protocols.
    for (auto protocolTy : layout.getProtocols())
      result.push_back(protocolTy->getDecl());
    return result;
  }

  return { };
}

DirectlyReferencedTypeDecls InheritedDeclsReferencedRequest::evaluate(
    Evaluator &evaluator,
    llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
    unsigned index) const {

  // Prefer syntactic information when we have it.
  TypeLoc &typeLoc = getTypeLoc(decl, index);
  if (auto typeRepr = typeLoc.getTypeRepr()) {
    // Figure out the context in which name lookup will occur.
    DeclContext *dc;
    if (auto typeDecl = decl.dyn_cast<TypeDecl *>())
      dc = typeDecl->getInnermostDeclContext();
    else
      dc = decl.get<ExtensionDecl *>();

    return directReferencesForTypeRepr(evaluator, dc->getASTContext(), typeRepr,
                                       dc);
  }

  // Fall back to semantic types.
  // FIXME: In the long run, we shouldn't need this. Non-syntactic results
  // should be cached.
  if (auto type = typeLoc.getType()) {
    return directReferencesForType(type);
  }

  return { };
}

DirectlyReferencedTypeDecls UnderlyingTypeDeclsReferencedRequest::evaluate(
    Evaluator &evaluator,
    TypeAliasDecl *typealias) const {
  // Prefer syntactic information when we have it.
  if (auto typeRepr = typealias->getUnderlyingTypeLoc().getTypeRepr()) {
    return directReferencesForTypeRepr(evaluator, typealias->getASTContext(),
                                       typeRepr, typealias);
  }

  // Fall back to semantic types.
  // FIXME: In the long run, we shouldn't need this. Non-syntactic results
  // should be cached.
  if (auto type = typealias->getUnderlyingTypeLoc().getType()) {
    return directReferencesForType(type);
  }

  return { };
}

/// Evaluate a superclass declaration request.
ClassDecl *SuperclassDeclRequest::evaluate(Evaluator &evaluator,
                                           NominalTypeDecl *subject) const {
  for (unsigned i : indices(subject->getInherited())) {
    // Find the inherited declarations referenced at this position.
    auto inheritedTypes =
      evaluator(InheritedDeclsReferencedRequest{subject, i});

    // Resolve those type declarations to nominal type declarations.
    SmallVector<ModuleDecl *, 2> modulesFound;
    auto inheritedNominalTypes
      = resolveTypeDeclsToNominal(evaluator, subject->getASTContext(),
                                  inheritedTypes, modulesFound);

    // Look for a class declaration.
    for (auto inheritedNominal : inheritedNominalTypes) {
      if (auto classDecl = dyn_cast<ClassDecl>(inheritedNominal))
        return classDecl;
    }
  }

  return nullptr;
}

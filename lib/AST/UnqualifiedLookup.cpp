//===--- UnqualifiedLookup.cpp - Swift Name Lookup Routines ---------------===//
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
// This file implements the construction of an UnqualifiedLookup, which entails
// performing the lookup.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTScope.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/DebuggerClient.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "namelookup"

using namespace swift;
using namespace swift::namelookup;

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
  case ASTScopeKind::PatternInitializer: // lazy var 'self'
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

static void populateLookupDeclsFromContext(
    DeclContext *dc, SmallVectorImpl<NominalTypeDecl *> &lookupDecls) {
  auto nominal = dc->getSelfNominalTypeDecl();
  if (!nominal)
    return;

  lookupDecls.push_back(nominal);

  // For a protocol extension, check whether there are additional "Self"
  // constraints that can affect name lookup.
  if (dc->getExtendedProtocolDecl()) {
    auto ext = cast<ExtensionDecl>(dc);
    auto bounds = getSelfBoundsFromWhereClause(ext);
    for (auto bound : bounds.decls)
      lookupDecls.push_back(bound);
  }
}

namespace {

/// Determine whether unqualified lookup should look at the members of the
/// given nominal type or extension, vs. only looking at type parameters.
template <typename D> bool shouldLookupMembers(D *decl, SourceLoc loc) {
  // Only look at members of this type (or its inherited types) when
  // inside the body or a protocol's top-level 'where' clause. (Why the
  // 'where' clause? Because that's where you put constraints on
  // inherited associated types.)

  // When we have no source-location information, we have to perform member
  // lookup.
  if (loc.isInvalid() || decl->getBraces().isInvalid())
    return true;

  // Within the braces, always look for members.
  auto &ctx = decl->getASTContext();
  if (ctx.SourceMgr.rangeContainsTokenLoc(decl->getBraces(), loc))
    return true;

  // Within 'where' clause, we can also look for members.
  if (auto *whereClause = decl->getTrailingWhereClause()) {
    SourceRange whereClauseRange = whereClause->getSourceRange();
    if (whereClauseRange.isValid() &&
        ctx.SourceMgr.rangeContainsTokenLoc(whereClauseRange, loc)) {
      return true;
    }
  }

  // Don't look at the members.
  return false;
}
} // end anonymous namespace

namespace {
class UnqualifiedLookupFactory {
public:
  using Flags = LegacyUnqualifiedLookup::Flags;
  using Options = LegacyUnqualifiedLookup::Options;
  using PlacesToSearch = UnqualifiedLookup::PlacesToSearch;
  using PerScopeLookupState = UnqualifiedLookup::PerScopeLookupState;

  enum class LookupFinished { Yes, No };

private:
  // Inputs
  const DeclName Name;
  DeclContext *const DC;
  ModuleDecl &M;
  const ASTContext &Ctx;
  LazyResolver *const TypeResolver;
  const SourceLoc Loc;
  const SourceManager &SM;
  /// Used to find the file-local names.
  DebuggerClient *const DebugClient;
  const Options options;
  const bool isOriginallyTypeLookup;
  const NLOptions baseNLOptions;
  // Transputs
  NamedDeclConsumer Consumer;
  // Outputs
  SmallVectorImpl<LookupResultEntry> &Results;
  size_t &IndexOfFirstOuterResult;

  // For debugging:
  SourceFile const *&recordedSF;
  DeclName &recordedName;
  bool &recordedIsCascadingUse;
  std::vector<PerScopeLookupState> &breadcrumbs;

  SmallVector<LookupResultEntry, 4> UnavailableInnerResults;

public:
  // clang-format off
    UnqualifiedLookupFactory(DeclName Name,
                             DeclContext *const DC,
                             LazyResolver *TypeResolver,
                             SourceLoc Loc,
                             Options options,
                             UnqualifiedLookup &lookupToBeCreated);
  // clang-format on

  void fillInLookup();

private:
  bool shouldUseASTScopeLookup() const;

#pragma mark ASTScope-based-lookup declarations

  // TODO: better name than DC
  struct DCAndIsCascadingUse {
    DeclContext *const DC;
    const bool isCascadingUse;
  };

  Optional<DCAndIsCascadingUse>
  astScopeBasedLookup(DeclContext *dc, Optional<bool> isCascadingUse);

  std::pair<const ASTScope *, bool>
  operatorScopeForASTScopeLookup(DeclContext *dc,
                                 Optional<bool> isCascadingUse);

  std::pair<const ASTScope *, Optional<bool>>
  nonoperatorScopeForASTScopeLookup(DeclContext *dc,
                                    Optional<bool> isCascadingUse) const;

  struct LookInScopeForASTScopeLookupResult {
    bool isDone; // Searching outers
    DeclContext *selfDC;
    DeclContext *dc;
    Optional<bool> isCascadingUse;
  };

  /// Return status and new selfDC and new DC
  Optional<LookInScopeForASTScopeLookupResult>
  lookInScopeForASTScopeLookup(const ASTScope *const currentScope,
                               DeclContext *selfDC, DeclContext *dc,
                               const Optional<bool> isCascadingUse);

  /// Returns status and selfDC and DC
  Optional<LookInScopeForASTScopeLookupResult>
  lookIntoDeclarationContextForASTScopeLookup(DeclContext *dc,
                                              DeclContext *selfDC,
                                              DeclContext *wasDC,
                                              Optional<bool> isCascadingUse);

  /// Return true if done with lookup.
  bool isFinishedWithLookupNowThatIsAboutToLookForOuterResults(
      bool noMoreOuterResults = false);

#pragma mark normal (non-ASTScope-based) lookup declarations

  /// Return None if lookup done.
  Optional<DCAndIsCascadingUse> operatorLookup(DeclContext *dc,
                                               Optional<bool> isCascadingUse);

  /// Return None if lookup done.
  Optional<DCAndIsCascadingUse>
  nonASTScopeBasedLookup(DeclContext *const dc,
                         const Optional<bool> isCascadingUseArg);

  struct LookupInOneDeclContextResult {
    bool isDone;
    DeclContext *dc;
    Optional<bool> isCascadingUse;
  };

  /// Return the next context to search.
  Optional<LookupInOneDeclContextResult>
  lookupInOneDeclContext(DeclContext *dc,
                         const Optional<bool> isCascadingUseArg);

  Optional<PerScopeLookupState>
  lookupInAppropriateContext(DeclContext *dc, Optional<bool> isCascadingUse);

  // TODO: use objects & virtuals

  Optional<PerScopeLookupState>
  lookupInPatternBindingInitializer(PatternBindingInitializer *PBI,
                                    Optional<bool> isCascadingUse);

  Optional<PerScopeLookupState>
  lookupInFunctionDecl(AbstractFunctionDecl *AFD,
                       Optional<bool> isCascadingUse);

  Optional<PerScopeLookupState> lookupInClosure(AbstractClosureExpr *ACE,
                                                Optional<bool> isCascadingUse);

  template <typename NominalTypeDeclOrExtensionDecl>
  Optional<PerScopeLookupState>
  lookupInNominalTypeOrExtension(NominalTypeDeclOrExtensionDecl *D,
                                 Optional<bool> isCascadingUse);

  Optional<PerScopeLookupState>
  lookupInDefaultArgumentInitializer(DefaultArgumentInitializer *I,
                                     Optional<bool> isCascadingUse);

  bool isOutsideBodyOfFunction(const AbstractFunctionDecl *const AFD) const;

  Optional<PerScopeLookupState>
  lookupInMiscContext(DeclContext *dc, Optional<bool> isCascadingUse);

  /// Check the generic parameters of our context.
  /// Return true if done with lookup
  bool addGenericParametersHereAndInEnclosingScopes(DeclContext *dc);

  /// Consume generic parameters
  void addGenericParametersForFunction(AbstractFunctionDecl *AFD);

  static GenericParamList *getGenericParams(const DeclContext *const dc);

  /// Return true if lookup is done
  bool addLocalVariableResults(DeclContext *dc);

  /// Return true if finished with lookup
  bool handleUnavailableInnerResults(const size_t startIndexOfInnerResults);

  void recordDependencyOnTopLevelName(DeclContext *topLevelContext,
                                      DeclName name, bool isCascadingUse);

  void addPrivateImports(DeclContext *const dc);

  /// Return true if done
  bool addNamesKnownToDebugClient(DeclContext *dc);

  /// Return true if done
  bool addUnavailableInnerResults();

  bool lookForAModuleWithTheGivenName(DeclContext *const dc);

#pragma mark common helper declarations
  static NLOptions
  computeBaseNLOptions(const LegacyUnqualifiedLookup::Options options,
                       const bool isOriginallyTypeLookup);

  static bool resolveIsCascadingUse(const DeclContext *const dc,
                                    Optional<bool> isCascadingUse,
                                    bool onlyCareAboutFunctionBody);
};
} // namespace

#pragma mark UnqualifiedLookupFactory functions

// clang-format off
UnqualifiedLookupFactory::UnqualifiedLookupFactory(
                                                   DeclName Name,
                                                   DeclContext *const DC,
                                                   LazyResolver *TypeResolver,
                                                   SourceLoc Loc,
                                                   Options options,
                                                   UnqualifiedLookup &lookupToBeCreated)
:
Name(Name),
DC(DC),
M(*DC->getParentModule()),
Ctx(M.getASTContext()),
TypeResolver(TypeResolver ? TypeResolver : Ctx.getLazyResolver()),
Loc(Loc),
SM(Ctx.SourceMgr),
DebugClient(M.getDebugClient()),
options(options),
isOriginallyTypeLookup(options.contains(Flags::TypeLookup)),
baseNLOptions(computeBaseNLOptions(options, isOriginallyTypeLookup)),
Consumer(Name, lookupToBeCreated.Results, isOriginallyTypeLookup),
Results(lookupToBeCreated.Results),
IndexOfFirstOuterResult(lookupToBeCreated.IndexOfFirstOuterResult),
recordedSF(lookupToBeCreated.recordedSF),
recordedName(lookupToBeCreated.recordedName),
recordedIsCascadingUse(lookupToBeCreated.recordedIsCascadingUse),
breadcrumbs(lookupToBeCreated.breadcrumbs) {}
// clang-format on

void UnqualifiedLookupFactory::fillInLookup() {
  const Optional<bool> isCascadingUseInitial =
      options.contains(Flags::KnownPrivate) ? Optional<bool>(false) : None;
  // Never perform local lookup for operators.
  auto dcAndIsCascadingUse =
      shouldUseASTScopeLookup()
          ? astScopeBasedLookup(DC, isCascadingUseInitial)
          : Name.isOperator()
                ? operatorLookup(DC, isCascadingUseInitial)
                : nonASTScopeBasedLookup(DC, isCascadingUseInitial);

  if (!dcAndIsCascadingUse.hasValue())
    return;

  DeclContext *const DC = dcAndIsCascadingUse.getValue().DC;
  const bool isCascadingUse = dcAndIsCascadingUse.getValue().isCascadingUse;

  // TODO: Does the debugger client care about compound names?
  if (Name.isSimpleName() && DebugClient &&
      DebugClient->lookupOverrides(Name.getBaseName(), DC, Loc,
                                   isOriginallyTypeLookup, Results))
    return;

  recordDependencyOnTopLevelName(DC, Name, isCascadingUse);
  addPrivateImports(DC);
  if (addNamesKnownToDebugClient(DC))
    return;
  // If we still haven't found anything, but we do have some
  // declarations that are "unavailable in the current Swift", drop
  // those in.
  if (addUnavailableInnerResults())
    return;
  if (lookForAModuleWithTheGivenName(DC))
    return;
  // Make sure we've recorded the inner-result-boundary.
  (void)isFinishedWithLookupNowThatIsAboutToLookForOuterResults(
      /*noMoreOuterResults=*/true);
}

bool UnqualifiedLookupFactory::shouldUseASTScopeLookup() const {
  return Loc.isValid() && DC->getParentSourceFile() &&
         DC->getParentSourceFile()->Kind != SourceFileKind::REPL &&
         Ctx.LangOpts.EnableASTScopeLookup;
}

#pragma mark ASTScope-based-lookup definitions

/// Return None if lookup done
Optional<UnqualifiedLookupFactory::DCAndIsCascadingUse>
UnqualifiedLookupFactory::astScopeBasedLookup(DeclContext *const startDC,
                                              Optional<bool> isCascadingUse) {
  const std::pair<const ASTScope *, Optional<bool>>
      lookupScopeAndIsCascadingUse =
          Name.isOperator()
              ? operatorScopeForASTScopeLookup(startDC, isCascadingUse)
              : nonoperatorScopeForASTScopeLookup(startDC, isCascadingUse);
  // Walk scopes outward from the innermost scope until we find something.
  DeclContext *dc = startDC;
  DeclContext *selfDC = nullptr;
  Optional<bool> currentIsCascadingUse = lookupScopeAndIsCascadingUse.second;
  for (auto currentScope = lookupScopeAndIsCascadingUse.first; currentScope;
       currentScope = currentScope->getParent()) {
    auto r = lookInScopeForASTScopeLookup(currentScope, selfDC, dc,
                                          currentIsCascadingUse);
    if (!r.hasValue())
      return None;
    const bool isDone = r.getValue().isDone;
    selfDC = r.getValue().selfDC;
    dc = r.getValue().dc;
    currentIsCascadingUse = r.getValue().isCascadingUse;
    if (isDone)
      return DCAndIsCascadingUse{dc, currentIsCascadingUse.getValue()};
  }
  llvm_unreachable("impossible");
}

std::pair<const ASTScope *, bool>
UnqualifiedLookupFactory::operatorScopeForASTScopeLookup(
    DeclContext *dc, Optional<bool> isCascadingUse) {
  // Find the source file in which we are performing the lookup.
  SourceFile &sourceFile = *dc->getParentSourceFile();

  // Find the scope from which we will initiate unqualified name lookup.
  const ASTScope *lookupScope =
      sourceFile.getScope().findInnermostEnclosingScope(Loc);

  // Operator lookup is always at module scope.
  return std::make_pair(
      &sourceFile.getScope(),
      resolveIsCascadingUse(lookupScope->getInnermostEnclosingDeclContext(),
                            isCascadingUse,
                            /*onlyCareAboutFunctionBody*/ true));
}

std::pair<const ASTScope *, Optional<bool>>
UnqualifiedLookupFactory::nonoperatorScopeForASTScopeLookup(
    DeclContext *dc, Optional<bool> isCascadingUse) const {
  // Find the source file in which we are performing the lookup.
  SourceFile &sourceFile = *dc->getParentSourceFile();

  // Find the scope from which we will initiate unqualified name lookup.
  const ASTScope *lookupScope =
      sourceFile.getScope().findInnermostEnclosingScope(Loc);

  return std::make_pair(lookupScope, isCascadingUse);
}

Optional<UnqualifiedLookupFactory::LookInScopeForASTScopeLookupResult>
UnqualifiedLookupFactory::lookInScopeForASTScopeLookup(
    const ASTScope *const currentScope, DeclContext *selfDC, DeclContext *dc,
    const Optional<bool> isCascadingUse) {

  // Perform local lookup within this scope.
  auto localBindings = currentScope->getLocalBindings();
  for (auto local : localBindings) {
    Consumer.foundDecl(local, getLocalDeclVisibilityKind(currentScope));
  }

  // If we found anything, we're done.
  if (isFinishedWithLookupNowThatIsAboutToLookForOuterResults())
    return None;

  // When we are in the body of a method, get the 'self' declaration.
  if (currentScope->getKind() == ASTScopeKind::AbstractFunctionBody &&
      currentScope->getAbstractFunctionDecl()
          ->getDeclContext()
          ->isTypeContext()) {
    selfDC = currentScope->getAbstractFunctionDecl();
    return LookInScopeForASTScopeLookupResult{false, selfDC, dc,
                                              isCascadingUse};
  }

  // If there is a declaration context associated with this scope, we might
  // want to look in it.
  if (auto scopeDC = currentScope->getDeclContext())
    return lookIntoDeclarationContextForASTScopeLookup(scopeDC, selfDC, dc,
                                                       isCascadingUse);
  return LookInScopeForASTScopeLookupResult{false, selfDC, dc, isCascadingUse};
}

Optional<UnqualifiedLookupFactory::LookInScopeForASTScopeLookupResult>
UnqualifiedLookupFactory::lookIntoDeclarationContextForASTScopeLookup(
    DeclContext *dc, DeclContext *selfDC, DeclContext *wasDC,
    Optional<bool> isCascadingUse) {

  // If we haven't determined whether we have a cascading use, do so now.
  const bool isCascadingUseResult = resolveIsCascadingUse(
      dc, isCascadingUse, /*onlyCareAboutFunctionBody=*/false);

  // Pattern binding initializers are only interesting insofar as they
  // affect lookup in an enclosing nominal type or extension thereof.
  if (auto *bindingInit = dyn_cast<PatternBindingInitializer>(dc)) {
    // Lazy variable initializer contexts have a 'self' parameter for
    // instance member lookup.
    if (bindingInit->getImplicitSelfDecl())
      selfDC = bindingInit;

    return LookInScopeForASTScopeLookupResult{false, selfDC, wasDC,
                                              isCascadingUseResult};
  }

  // Default arguments only have 'static' access to the members of the
  // enclosing type, if there is one.
  if (isa<DefaultArgumentInitializer>(dc))
    return LookInScopeForASTScopeLookupResult{false, selfDC, wasDC,
                                              isCascadingUseResult};

  // Functions/initializers/deinitializers are only interesting insofar as
  // they affect lookup in an enclosing nominal type or extension thereof.
  if (isa<AbstractFunctionDecl>(dc))
    return LookInScopeForASTScopeLookupResult{false, selfDC, wasDC,
                                              isCascadingUseResult};

  // Subscripts have no lookup of their own.
  if (isa<SubscriptDecl>(dc))
    return LookInScopeForASTScopeLookupResult{false, selfDC, wasDC,
                                              isCascadingUseResult};

  // Closures have no lookup of their own.
  if (isa<AbstractClosureExpr>(dc))
    return LookInScopeForASTScopeLookupResult{false, selfDC, wasDC,
                                              isCascadingUseResult};

  // Top-level declarations have no lookup of their own.
  if (isa<TopLevelCodeDecl>(dc))
    return LookInScopeForASTScopeLookupResult{false, selfDC, wasDC,
                                              isCascadingUseResult};

  // Typealiases have no lookup of their own.
  if (isa<TypeAliasDecl>(dc))
    return LookInScopeForASTScopeLookupResult{false, selfDC, wasDC,
                                              isCascadingUseResult};

  // Lookup in the source file's scope marks the end.
  if (isa<SourceFile>(dc)) {
    // FIXME: A bit of a hack.
    return LookInScopeForASTScopeLookupResult{true, selfDC, dc,
                                              isCascadingUseResult};
  }

  // We have a nominal type or an extension thereof. Perform lookup into
  // the nominal type.
  auto nominal = dc->getSelfNominalTypeDecl();
  if (!nominal)
    return LookInScopeForASTScopeLookupResult{false, selfDC, wasDC,
                                              isCascadingUseResult};

  // Dig out the type we're looking into.
  using LookupDecls = SmallVector<NominalTypeDecl *, 2>;
  LookupDecls lookupDecls;
  populateLookupDeclsFromContext(dc, lookupDecls);

  NLOptions options = baseNLOptions;
  // Perform lookup into the type.
  if (isCascadingUseResult)
    options |= NL_KnownCascadingDependency;
  else
    options |= NL_KnownNonCascadingDependency;

  SmallVector<ValueDecl *, 4> lookup;
  dc->lookupQualified(lookupDecls, Name, options, lookup);

  auto startIndex = Results.size();
  for (auto result : lookup) {
    auto *baseDC = dc;
    if (!isa<TypeDecl>(result) && selfDC)
      baseDC = selfDC;
    Results.push_back(LookupResultEntry(baseDC, result));
  }

  if (!Results.empty()) {
    // Predicate that determines whether a lookup result should
    // be unavailable except as a last-ditch effort.
    auto unavailableLookupResult = [&](const LookupResultEntry &result) {
      auto &effectiveVersion = Ctx.LangOpts.EffectiveLanguageVersion;
      return result.getValueDecl()->getAttrs().isUnavailableInSwiftVersion(
          effectiveVersion);
    };

    // If all of the results we just found are unavailable, keep looking.
    auto begin = Results.begin() + startIndex;
    if (std::all_of(begin, Results.end(), unavailableLookupResult)) {
      UnavailableInnerResults.append(begin, Results.end());
      Results.erase(begin, Results.end());
    } else {
      if (DebugClient)
        filterForDiscriminator(Results, DebugClient);

      if (isFinishedWithLookupNowThatIsAboutToLookForOuterResults())
        return None;
    }
  }

  // Forget the 'self' declaration.
  selfDC = nullptr;

  return LookInScopeForASTScopeLookupResult{false, selfDC, dc,
                                            isCascadingUseResult};
}

#pragma mark normal (non-ASTScope-based) lookup declarations

Optional<UnqualifiedLookupFactory::DCAndIsCascadingUse>
UnqualifiedLookupFactory::operatorLookup(DeclContext *dc,
                                         Optional<bool> isCascadingUse) {
  auto *msc = dc->getModuleScopeContext();
  return DCAndIsCascadingUse{
      addLocalVariableResults(msc) ? nullptr : msc,
      resolveIsCascadingUse(dc, isCascadingUse,
                            /*onlyCareAboutFunctionBody*/ true)};
}

Optional<UnqualifiedLookupFactory::DCAndIsCascadingUse>
UnqualifiedLookupFactory::nonASTScopeBasedLookup(
    DeclContext *const dc, const Optional<bool> isCascadingUseArg) {
  // If we are inside of a method, check to see if there are any ivars in
  // scope, and if so, whether this is a reference to one of them.
  // FIXME: We should persist this information between lookups.
  DeclContext *nextDC = dc;
  auto isCascadingUse = isCascadingUseArg;
  DeclContext *priorDC = nullptr;
  while (!nextDC->isModuleScopeContext()) {
    auto r = lookupInOneDeclContext(nextDC, isCascadingUse);
    if (!r.hasValue())
      return None;
    const bool isDone = r.getValue().isDone;
    nextDC = r.getValue().dc;
    isCascadingUse = r.getValue().isCascadingUse;
    if (isDone)
      break;
    assert(nextDC != priorDC && "non-termination");
    priorDC = nextDC;
  }
  if (addLocalVariableResults(nextDC))
    return None;
  return DCAndIsCascadingUse{
      nextDC, isCascadingUse.hasValue() ? isCascadingUse.getValue() : true};
}

// clang-format off
Optional<UnqualifiedLookupFactory::LookupInOneDeclContextResult>
UnqualifiedLookupFactory::lookupInOneDeclContext(
                                                 DeclContext *dc,
                                                 const Optional<bool> isCascadingUseArg) {
  // clang-format on
  const auto r = lookupInAppropriateContext(dc, isCascadingUseArg);
  if (!r.hasValue())
    return None;
  const bool isDone = r.getValue().isDone;
  breadcrumbs.push_back(r.getValue());
  auto childOfNextDC = r.getValue().childOfNextDC;
  auto placesToSearch = std::move(r.getValue().placesToSearch);
  auto isCascadingUse = r.getValue().isCascadingUse;

  if (isDone)
    return LookupInOneDeclContextResult{false, childOfNextDC, isCascadingUse};

  if (addGenericParametersHereAndInEnclosingScopes(childOfNextDC))
    return None;
  if (placesToSearch.hasValue() && !placesToSearch.getValue().empty()) {
    auto startIndexOfInnerResults = Results.size();
    placesToSearch.getValue().addToResults(
        Name, isCascadingUse.getValue(), baseNLOptions, childOfNextDC, Results);
    if (handleUnavailableInnerResults(startIndexOfInnerResults))
      return None;
  }
  // TODO: What if !BaseDC && lookupDecls non-empty?
  DeclContext *nextDC = childOfNextDC->getParentForLookup();
  return LookupInOneDeclContextResult{false, nextDC, isCascadingUse};
}
// clang-format on

Optional<UnqualifiedLookupFactory::PerScopeLookupState>
UnqualifiedLookupFactory::lookupInAppropriateContext(
    DeclContext *dc, Optional<bool> isCascadingUse) {
  if (auto *PBI = dyn_cast<PatternBindingInitializer>(dc))
    return lookupInPatternBindingInitializer(PBI, isCascadingUse);
  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(dc))
    return lookupInFunctionDecl(AFD, isCascadingUse);
  if (auto *ACE = dyn_cast<AbstractClosureExpr>(dc))
    return lookupInClosure(ACE, isCascadingUse);
  if (auto *ED = dyn_cast<ExtensionDecl>(dc))
    return lookupInNominalTypeOrExtension(ED, isCascadingUse);
  if (auto *ND = dyn_cast<NominalTypeDecl>(dc))
    return lookupInNominalTypeOrExtension(ND, isCascadingUse);
  if (auto I = dyn_cast<DefaultArgumentInitializer>(dc))
    return lookupInDefaultArgumentInitializer(I, isCascadingUse);

  return lookupInMiscContext(dc, isCascadingUse);
}

Optional<UnqualifiedLookupFactory::PerScopeLookupState>
UnqualifiedLookupFactory::lookupInPatternBindingInitializer(
    PatternBindingInitializer *PBI, Optional<bool> isCascadingUse) {
  auto *PBD = PBI->getBinding();
  assert(PBD);
  // Lazy variable initializer contexts have a 'self' parameter for
  // instance member lookup.
  if (auto *selfParam = PBI->getImplicitSelfDecl()) {
    Consumer.foundDecl(selfParam, DeclVisibilityKind::FunctionParameter);
    if (isFinishedWithLookupNowThatIsAboutToLookForOuterResults())
      return None;
    DeclContext *const parent = PBI->getParent();
    // clang-format off
    return PerScopeLookupState{
      false,
      parent,
      PlacesToSearch(parent, PBI, parent, parent),
      resolveIsCascadingUse(PBI,isCascadingUse,
                            /*onlyCareAboutFunctionBody=*/false)};
    // clang-format no
  }
  // Initializers for stored properties of types perform static
  // lookup into the surrounding context.
  if (PBD->getDeclContext()->isTypeContext()) {
    DeclContext *const surroundingContext = PBI->getParent();
    // clang-format off
    return PerScopeLookupState{
      false,
      surroundingContext,
      PlacesToSearch(surroundingContext, surroundingContext,
                     surroundingContext, surroundingContext),
      resolveIsCascadingUse(surroundingContext,
                            None,
                            /*onlyCareAboutFunctionBody=*/false)};
    // clang-format on
  }
  // Otherwise, we have an initializer for a global or local property.
  // There's not much to find here, we'll keep going up to a parent
  // context.
  // clang-format off
  return PerScopeLookupState{
    false,
    PBI,
    None,
    resolveIsCascadingUse(PBI, isCascadingUse,
                          /*onlyCareAboutFunctionBody=*/false)};
  // clang-format on
}

Optional<UnqualifiedLookupFactory::PerScopeLookupState>
UnqualifiedLookupFactory::lookupInFunctionDecl(AbstractFunctionDecl *AFD,
                                               Optional<bool> isCascadingUse) {
  // Look for local variables; normally, the parser resolves these
  // for us, but it can't do the right thing inside local types.
  // FIXME: when we can parse and typecheck the function body partially
  // for code completion, AFD->getBody() check can be removed.
  if (Loc.isValid() && AFD->getBody()) {

    namelookup::FindLocalVal localVal(SM, Loc, Consumer);
    localVal.visit(AFD->getBody());
    if (isFinishedWithLookupNowThatIsAboutToLookForOuterResults())
      return None;

    if (auto *P = AFD->getImplicitSelfDecl())
      localVal.checkValueDecl(P, DeclVisibilityKind::FunctionParameter);
    localVal.checkParameterList(AFD->getParameters());
    if (isFinishedWithLookupNowThatIsAboutToLookForOuterResults())
      return None;
  }
  const bool returnValueForIsCascadingUse =
      AFD->isCascadingContextForLookup(false) &&
      (isCascadingUse.hasValue()
           ? isCascadingUse.getValue()
           : !Loc.isValid() || !AFD->getBody() ||
                 !SM.rangeContainsTokenLoc(AFD->getBodySourceRange(), Loc));

  if (AFD->getDeclContext()->isTypeContext()) {
    DeclContext *fnDeclContext = AFD->getDeclContext();
    DeclContext *fnParent = AFD->getParent();
    addGenericParametersForFunction(AFD);
    if (isFinishedWithLookupNowThatIsAboutToLookForOuterResults())
      return None;
    // clang-format off
    return PerScopeLookupState{
      false,
      fnParent,
      PlacesToSearch(
                     fnParent,
                     // If we're not in the body of the function (for example, we
                     // might be type checking a default argument expression and
                     // performing name lookup from there), the base declaration
                     // is the nominal type, not 'self'.
                     isOutsideBodyOfFunction(AFD) ? fnDeclContext : AFD,
                     fnDeclContext,
                     AFD->getDeclContext()),
      returnValueForIsCascadingUse};
    // clang-format on
  }
  // Look in the generic parameters after checking our local declaration.
  addGenericParametersForFunction(AFD);
  if (isFinishedWithLookupNowThatIsAboutToLookForOuterResults())
    return None;
  // clang-format off
  return PerScopeLookupState{
    false,
    AFD,
    None,
    returnValueForIsCascadingUse};
  // clang-format on
}

Optional<UnqualifiedLookupFactory::PerScopeLookupState>
UnqualifiedLookupFactory::lookupInClosure(AbstractClosureExpr *ACE,
                                          Optional<bool> isCascadingUse) {
  // Look for local variables; normally, the parser resolves these
  // for us, but it can't do the right thing inside local types.
  if (Loc.isValid()) {
    if (auto *CE = dyn_cast<ClosureExpr>(ACE)) {
      namelookup::FindLocalVal localVal(SM, Loc, Consumer);
      if (auto body = CE->getBody())
        localVal.visit(body);
      if (isFinishedWithLookupNowThatIsAboutToLookForOuterResults())
        return None;
      if (auto params = CE->getParameters())
        localVal.checkParameterList(params);
      if (isFinishedWithLookupNowThatIsAboutToLookForOuterResults())
        return None;
    }
  }
  // clang-format off
  return PerScopeLookupState{
    false,
    ACE,
    None,
    resolveIsCascadingUse(ACE, isCascadingUse,
                          /*onlyCareAboutFunctionBody=*/false)};
  // clang-format on
}

template <typename NominalTypeDeclOrExtensionDecl>
Optional<UnqualifiedLookupFactory::PerScopeLookupState>
UnqualifiedLookupFactory::lookupInNominalTypeOrExtension(
    NominalTypeDeclOrExtensionDecl *D, Optional<bool> isCascadingUse) {
  // clang-format off
  return PerScopeLookupState{
    false,
    D,
    shouldLookupMembers(D, Loc)
    ? Optional<PlacesToSearch>(PlacesToSearch(D, D, D, D))
    : None,
    resolveIsCascadingUse(D, isCascadingUse,
                          /*onlyCareAboutFunctionBody=*/false)};

  // clang-format on
}

Optional<UnqualifiedLookupFactory::PerScopeLookupState>
UnqualifiedLookupFactory::lookupInDefaultArgumentInitializer(
    DefaultArgumentInitializer *I, Optional<bool> isCascadingUse) {
  // In a default argument, skip immediately out of both the
  // initializer and the function.
  // clang-format off
  return PerScopeLookupState{true,
    I->getParent()->getParent(),
    None,
    false};
  // clang-format on
}

bool UnqualifiedLookupFactory::isOutsideBodyOfFunction(
    const AbstractFunctionDecl *const AFD) const {
  return !AFD->isImplicit() && Loc.isValid() &&
         AFD->getBodySourceRange().isValid() &&
         !SM.rangeContainsTokenLoc(AFD->getBodySourceRange(), Loc);
}

Optional<UnqualifiedLookupFactory::PerScopeLookupState>
UnqualifiedLookupFactory::lookupInMiscContext(DeclContext *dc,
                                              Optional<bool> isCascadingUse) {
  // clang-format off
  assert(isa<TopLevelCodeDecl>(dc) ||
         isa<Initializer>(dc) ||
         isa<TypeAliasDecl>(dc) ||
         isa<SubscriptDecl>(dc));
  return PerScopeLookupState{
    false,
    dc,
    None,
    resolveIsCascadingUse(DC, isCascadingUse,
                          /*onlyCareAboutFunctionBody=*/false)};
  // clang-format on
}

GenericParamList *
UnqualifiedLookupFactory::getGenericParams(const DeclContext *const dc) {
  if (auto nominal = dyn_cast<NominalTypeDecl>(dc))
    return nominal->getGenericParams();
  if (auto ext = dyn_cast<ExtensionDecl>(dc))
    return ext->getGenericParams();
  if (auto subscript = dyn_cast<SubscriptDecl>(dc))
    return subscript->getGenericParams();
  return nullptr;
}

bool UnqualifiedLookupFactory::addGenericParametersHereAndInEnclosingScopes(
    DeclContext *dc) {
  for (GenericParamList *dcGenericParams = getGenericParams(dc);
       dcGenericParams;
       dcGenericParams = dcGenericParams->getOuterParameters()) {
    namelookup::FindLocalVal localVal(SM, Loc, Consumer);
    localVal.checkGenericParams(dcGenericParams);

    if (isFinishedWithLookupNowThatIsAboutToLookForOuterResults())
      return true;
  }
  return false;
}

void UnqualifiedLookupFactory::addGenericParametersForFunction(
    AbstractFunctionDecl *AFD) {
  // If we're inside a function context, we've already moved to
  // the parent DC, so we have to check the function's generic
  // parameters first.
  GenericParamList *GenericParams = AFD->getGenericParams();
  if (GenericParams) {
    namelookup::FindLocalVal localVal(SM, Loc, Consumer);
    localVal.checkGenericParams(GenericParams);
  }
}

// TODO enum instead of bool?
bool UnqualifiedLookupFactory::addLocalVariableResults(DeclContext *dc) {
  if (auto SF = dyn_cast<SourceFile>(dc)) {
    if (Loc.isValid()) {
      // Look for local variables in top-level code; normally, the parser
      // resolves these for us, but it can't do the right thing for
      // local types.
      namelookup::FindLocalVal localVal(SM, Loc, Consumer);
      localVal.checkSourceFile(*SF);
      if (isFinishedWithLookupNowThatIsAboutToLookForOuterResults())
        return true;
    }
  }
  return false;
}

void UnqualifiedLookup::PlacesToSearch::addToResults(
    const DeclName &Name, bool isCascadingUse, NLOptions baseNLOptions,
    DeclContext *contextForLookup,
    SmallVectorImpl<LookupResultEntry> &results) const {
  const NLOptions options =
      baseNLOptions | (isCascadingUse ? NL_KnownCascadingDependency
                                      : NL_KnownNonCascadingDependency);

  SmallVector<ValueDecl *, 4> Lookup;
  contextForLookup->lookupQualified(places, Name, options, Lookup);
  for (auto Result : Lookup)
    results.push_back(LookupResultEntry(whereValueIsMember(Result), Result));
}

bool UnqualifiedLookupFactory::handleUnavailableInnerResults(
    const size_t startIndexOfInnerResults) {
  // An optimization:
  assert(Results.size() >= startIndexOfInnerResults);
  if (Results.size() == startIndexOfInnerResults)
    return false;
  // Predicate that determines whether a lookup result should
  // be unavailable except as a last-ditch effort.
  auto unavailableLookupResult = [&](const LookupResultEntry &result) {
    auto &effectiveVersion = Ctx.LangOpts.EffectiveLanguageVersion;
    return result.getValueDecl()->getAttrs().isUnavailableInSwiftVersion(
        effectiveVersion);
  };

  // If all of the results we found are unavailable, keep looking.
  auto begin = Results.begin() + startIndexOfInnerResults;
  if (std::all_of(begin, Results.end(), unavailableLookupResult)) {
    UnavailableInnerResults.append(begin, Results.end());
    Results.erase(begin, Results.end());
    return false;
  }
  if (DebugClient)
    filterForDiscriminator(Results, DebugClient);

  return isFinishedWithLookupNowThatIsAboutToLookForOuterResults();
}

void UnqualifiedLookupFactory::recordDependencyOnTopLevelName(
    DeclContext *topLevelContext, DeclName name, bool isCascadingUse) {
  recordLookupOfTopLevelName(topLevelContext, Name, isCascadingUse);
  recordedSF = dyn_cast<SourceFile>(topLevelContext);
  recordedName = Name;
  recordedIsCascadingUse = isCascadingUse;
}

void UnqualifiedLookupFactory::addPrivateImports(DeclContext *const dc) {
  // Add private imports to the extra search list.
  SmallVector<ModuleDecl::ImportedModule, 8> extraImports;
  if (auto FU = dyn_cast<FileUnit>(dc))
    FU->getImportedModules(extraImports, ModuleDecl::ImportFilter::Private);

  using namespace namelookup;
  SmallVector<ValueDecl *, 8> CurModuleResults;
  auto resolutionKind = isOriginallyTypeLookup ? ResolutionKind::TypesOnly
                                               : ResolutionKind::Overloadable;
  lookupInModule(&M, {}, Name, CurModuleResults, NLKind::UnqualifiedLookup,
                 resolutionKind, TypeResolver, dc, extraImports);

  // Always perform name shadowing for type lookup.
  if (options.contains(Flags::TypeLookup)) {
    removeShadowedDecls(CurModuleResults, &M);
  }

  for (auto VD : CurModuleResults)
    Results.push_back(LookupResultEntry(VD));

  if (DebugClient)
    filterForDiscriminator(Results, DebugClient);
}

bool UnqualifiedLookupFactory::addNamesKnownToDebugClient(DeclContext *dc) {
  if (Name.isSimpleName() && DebugClient)
    DebugClient->lookupAdditions(Name.getBaseName(), dc, Loc,
                                 isOriginallyTypeLookup, Results);

  // If we've found something, we're done.
  return isFinishedWithLookupNowThatIsAboutToLookForOuterResults(
      /*noMoreOuterResults=*/true);
}

bool UnqualifiedLookupFactory::addUnavailableInnerResults() {
  Results = std::move(UnavailableInnerResults);
  return isFinishedWithLookupNowThatIsAboutToLookForOuterResults(
      /*noMoreOuterResults=*/true);
}

bool UnqualifiedLookupFactory::lookForAModuleWithTheGivenName(
    DeclContext *const dc) {
  using namespace namelookup;
  if (!Name.isSimpleName())
    return true;

  // Look for a module with the given name.
  if (Name.isSimpleName(M.getName())) {
    Results.push_back(LookupResultEntry(&M));
    return isFinishedWithLookupNowThatIsAboutToLookForOuterResults(
        /*noMoreOuterResults=*/true);
  }
  ModuleDecl *desiredModule = Ctx.getLoadedModule(Name.getBaseIdentifier());
  if (!desiredModule && Name == Ctx.TheBuiltinModule->getName())
    desiredModule = Ctx.TheBuiltinModule;
  if (desiredModule) {
    forAllVisibleModules(
        dc, [&](const ModuleDecl::ImportedModule &import) -> bool {
          if (import.second == desiredModule) {
            Results.push_back(LookupResultEntry(import.second));
            return false;
          }
          return true;
        });
  }
  return false;
}

#pragma mark common helper definitions
NLOptions UnqualifiedLookupFactory::computeBaseNLOptions(
    const UnqualifiedLookup::Options options,
    const bool isOriginallyTypeLookup) {
  NLOptions baseNLOptions = NL_UnqualifiedDefault;
  if (options.contains(Flags::AllowProtocolMembers))
    baseNLOptions |= NL_ProtocolMembers;
  if (isOriginallyTypeLookup)
    baseNLOptions |= NL_OnlyTypes;
  if (options.contains(Flags::IgnoreAccessControl))
    baseNLOptions |= NL_IgnoreAccessControl;
  return baseNLOptions;
}
// TODO: elim flag?
// TODO: fold this into return None?
bool UnqualifiedLookupFactory::
    isFinishedWithLookupNowThatIsAboutToLookForOuterResults(
        bool noMoreOuterResults) {
  if (Results.empty())
    return false;

  if (IndexOfFirstOuterResult == 0)
    IndexOfFirstOuterResult = Results.size();

  return !options.contains(Flags::IncludeOuterResults) || noMoreOuterResults;
}

bool UnqualifiedLookupFactory::resolveIsCascadingUse(
    const DeclContext *const dc, Optional<bool> isCascadingUse,
    bool onlyCareAboutFunctionBody) {
  return isCascadingUse.hasValue()
             ? isCascadingUse.getValue()
             : dc->isCascadingContextForLookup(
                   /*functionsAreNonCascading=*/onlyCareAboutFunctionBody);
}

LegacyUnqualifiedLookup::LegacyUnqualifiedLookup(DeclName Name, DeclContext *DC,
                                                 LazyResolver *TypeResolver,
                                                 SourceLoc Loc, Options options)
    : IndexOfFirstOuterResult(0) {
  ModuleDecl &M = *DC->getParentModule();
  ASTContext &Ctx = M.getASTContext();
  if (!TypeResolver)
    TypeResolver = Ctx.getLazyResolver();
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

  if (Loc.isValid() && DC->getParentSourceFile() &&
      DC->getParentSourceFile()->Kind != SourceFileKind::REPL &&
      Ctx.LangOpts.EnableASTScopeLookup) {
    // Find the source file in which we are performing the lookup.
    SourceFile &sourceFile = *DC->getParentSourceFile();

    // Find the scope from which we will initiate unqualified name lookup.
    const ASTScope *lookupScope =
        sourceFile.getScope().findInnermostEnclosingScope(Loc);

    // Operator lookup is always at module scope.
    if (Name.isOperator()) {
      if (!isCascadingUse.hasValue()) {
        DeclContext *innermostDC =
            lookupScope->getInnermostEnclosingDeclContext();
        isCascadingUse = innermostDC->isCascadingContextForLookup(
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
        Consumer.foundDecl(local, getLocalDeclVisibilityKind(currentScope));
      }

      // If we found anything, we're done.
      if (shouldReturnBasedOnResults())
        return;

      // When we are in the body of a method, get the 'self' declaration.
      if (currentScope->getKind() == ASTScopeKind::AbstractFunctionBody &&
          currentScope->getAbstractFunctionDecl()
              ->getDeclContext()
              ->isTypeContext()) {
        selfDC = currentScope->getAbstractFunctionDecl();
        continue;
      }

      // If there is a declaration context associated with this scope, we might
      // want to look in it.
      if (auto dc = currentScope->getDeclContext()) {
        // If we haven't determined whether we have a cascading use, do so now.
        if (!isCascadingUse.hasValue()) {
          isCascadingUse = dc->isCascadingContextForLookup(
              /*functionsAreNonCascading=*/false);
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
        if (isa<DefaultArgumentInitializer>(dc))
          continue;

        // Functions/initializers/deinitializers are only interesting insofar as
        // they affect lookup in an enclosing nominal type or extension thereof.
        if (isa<AbstractFunctionDecl>(dc))
          continue;

        // Subscripts have no lookup of their own.
        if (isa<SubscriptDecl>(dc))
          continue;

        // Closures have no lookup of their own.
        if (isa<AbstractClosureExpr>(dc))
          continue;

        // Top-level declarations have no lookup of their own.
        if (isa<TopLevelCodeDecl>(dc))
          continue;

        // Typealiases have no lookup of their own.
        if (isa<TypeAliasDecl>(dc))
          continue;

        // Lookup in the source file's scope marks the end.
        if (isa<SourceFile>(dc)) {
          // FIXME: A bit of a hack.
          DC = dc;
          break;
        }

        // We have a nominal type or an extension thereof. Perform lookup into
        // the nominal type.
        auto nominal = dc->getSelfNominalTypeDecl();
        if (!nominal)
          continue;

        // Dig out the type we're looking into.
        SmallVector<NominalTypeDecl *, 2> lookupDecls;
        populateLookupDeclsFromContext(dc, lookupDecls);

        NLOptions options = baseNLOptions;
        // Perform lookup into the type.
        if (isCascadingUse.getValue())
          options |= NL_KnownCascadingDependency;
        else
          options |= NL_KnownNonCascadingDependency;

        SmallVector<ValueDecl *, 4> lookup;
        dc->lookupQualified(lookupDecls, Name, options, lookup);

        auto startIndex = Results.size();
        for (auto result : lookup) {
          auto *baseDC = dc;
          if (!isa<TypeDecl>(result) && selfDC)
            baseDC = selfDC;
          Results.push_back(LookupResultEntry(baseDC, result));
        }

        if (!Results.empty()) {
          // Predicate that determines whether a lookup result should
          // be unavailable except as a last-ditch effort.
          auto unavailableLookupResult = [&](const LookupResultEntry &result) {
            auto &effectiveVersion = Ctx.LangOpts.EffectiveLanguageVersion;
            return result.getValueDecl()
                ->getAttrs()
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

        SmallVector<NominalTypeDecl *, 2> lookupDecls;

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

            populateLookupDeclsFromContext(DC, lookupDecls);
            MetaBaseDC = DC;
            BaseDC = PBI;
          }
          // Initializers for stored properties of types perform static
          // lookup into the surrounding context.
          else if (PBD->getDeclContext()->isTypeContext()) {
            DC = DC->getParent();

            populateLookupDeclsFromContext(DC, lookupDecls);
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
            populateLookupDeclsFromContext(AFD->getDeclContext(), lookupDecls);
            BaseDC = AFD;
            MetaBaseDC = AFD->getDeclContext();
            DC = DC->getParent();

            // If we're not in the body of the function (for example, we
            // might be type checking a default argument expression and
            // performing name lookup from there), the base declaration
            // is the nominal type, not 'self'.
            if (!AFD->isImplicit() && Loc.isValid() &&
                AFD->getBodySourceRange().isValid() &&
                !SM.rangeContainsTokenLoc(AFD->getBodySourceRange(), Loc)) {
              BaseDC = MetaBaseDC;
            }
          }

          // Look in the generic parameters after checking our local
          // declaration.
          GenericParams = AFD->getGenericParams();
        } else if (auto *ACE = dyn_cast<AbstractClosureExpr>(DC)) {
          // Look for local variables; normally, the parser resolves these
          // for us, but it can't do the right thing inside local types.
          if (Loc.isValid()) {
            if (auto *CE = dyn_cast<ClosureExpr>(ACE)) {
              namelookup::FindLocalVal localVal(SM, Loc, Consumer);
              if (auto body = CE->getBody())
                localVal.visit(body);
              if (shouldReturnBasedOnResults())
                return;
              if (auto params = CE->getParameters())
                localVal.checkParameterList(params);
              if (shouldReturnBasedOnResults())
                return;
            }
          }
          if (!isCascadingUse.hasValue())
            isCascadingUse = ACE->isCascadingContextForLookup(false);
        } else if (auto *ED = dyn_cast<ExtensionDecl>(DC)) {
          if (shouldLookupMembers(ED, Loc))
            populateLookupDeclsFromContext(ED, lookupDecls);

          BaseDC = ED;
          MetaBaseDC = ED;
          if (!isCascadingUse.hasValue())
            isCascadingUse = ED->isCascadingContextForLookup(false);
        } else if (auto *ND = dyn_cast<NominalTypeDecl>(DC)) {
          if (shouldLookupMembers(ND, Loc))
            populateLookupDeclsFromContext(ND, lookupDecls);
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
                 isa<TypeAliasDecl>(DC) || isa<SubscriptDecl>(DC));
          if (!isCascadingUse.hasValue())
            isCascadingUse = DC->isCascadingContextForLookup(false);
        }

        // If we're inside a function context, we've already moved to
        // the parent DC, so we have to check the function's generic
        // parameters first.
        if (GenericParams) {
          namelookup::FindLocalVal localVal(SM, Loc, Consumer);
          localVal.checkGenericParams(GenericParams);

          if (shouldReturnBasedOnResults())
            return;
        }

        // Check the generic parameters of our context.
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

          dcGenericParams = dcGenericParams->getOuterParameters();
        }

        if (BaseDC && !lookupDecls.empty()) {
          NLOptions options = baseNLOptions;
          if (isCascadingUse.getValue())
            options |= NL_KnownCascadingDependency;
          else
            options |= NL_KnownNonCascadingDependency;

          SmallVector<ValueDecl *, 4> Lookup;
          DC->lookupQualified(lookupDecls, Name, options, Lookup);
          bool FoundAny = false;
          auto startIndex = Results.size();
          for (auto Result : Lookup) {
            // Classify this declaration.
            FoundAny = true;

            // Types are formally members of the metatype.
            if (auto TD = dyn_cast<TypeDecl>(Result)) {
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
                  auto &effectiveVersion =
                      Ctx.LangOpts.EffectiveLanguageVersion;
                  return result.getValueDecl()
                      ->getAttrs()
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
  recordedSF = dyn_cast<SourceFile>(DC);
  recordedName = Name;
  recordedIsCascadingUse = isCascadingUse.getValue();

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

  // Always perform name shadowing for type lookup.
  if (options.contains(Flags::TypeLookup)) {
    removeShadowedDecls(CurModuleResults, &M);
  }

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
    forAllVisibleModules(
        DC, [&](const ModuleDecl::ImportedModule &import) -> bool {
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

TypeDecl *LegacyUnqualifiedLookup::getSingleTypeResult() {
  if (Results.size() != 1)
    return nullptr;
  return dyn_cast<TypeDecl>(Results.back().getValueDecl());
}

UnqualifiedLookup::PlacesToSearch::PlacesToSearch(
    DeclContext *fromWhence, DeclContext *whereNonTypesAreMembers,
    DeclContext *whereTypesAreMembers, DeclContext *placesHolder)
    : fromWhence(fromWhence), whereNonTypesAreMembers(whereNonTypesAreMembers),
      whereTypesAreMembers(whereTypesAreMembers) {
  populateLookupDeclsFromContext(placesHolder, places);
}

void UnqualifiedLookup::PlacesToSearch::dump() const {
  llvm::errs() << "fromWhence: ";
  fromWhence->dumpContext();
  llvm::errs() << "whereNonTypesAreMembers: ";
  whereNonTypesAreMembers->dumpContext();
  llvm::errs() << "whereTypesAreMembers: ";
  whereTypesAreMembers->dumpContext();
  llvm::errs() << "places: ";
  for (const auto *D : places)
    D->dump();
  llvm::errs() << "\n";
}

#pragma mark UnqualifiedLookup functions

// clang-format off
UnqualifiedLookup::UnqualifiedLookup(DeclName Name,
                                     DeclContext *const DC,
                                     LazyResolver *TypeResolver,
                                     SourceLoc Loc,
                                     Options options)
    // clang-format on
    : IndexOfFirstOuterResult(0) {
  UnqualifiedLookupFactory(Name, DC, TypeResolver, Loc, options, *this)
      .fillInLookup();
  assert(verifyEqual(
             LegacyUnqualifiedLookup(Name, DC, TypeResolver, Loc, options)) &&
         "bad refactoring");
}

TypeDecl *UnqualifiedLookup::getSingleTypeResult() {
  if (Results.size() != 1)
    return nullptr;
  return dyn_cast<TypeDecl>(Results.back().getValueDecl());
}

bool UnqualifiedLookup::verifyEqual(
    const LegacyUnqualifiedLookup &&other) const {
  assert(Results.size() == other.Results.size());
  for (size_t i : indices(Results)) {
    const auto &e = Results[i];
    const auto &oe = other.Results[i];
    assert(e.getValueDecl() == oe.getValueDecl());
    assert(e.getDeclContext() == oe.getDeclContext());
    // unsigned printContext(llvm::raw_ostream &OS, unsigned indent = 0,
    // bool onlyAPartialLine = false) const;
    assert(e.getBaseDecl() == oe.getBaseDecl());
  }
  assert(IndexOfFirstOuterResult == other.IndexOfFirstOuterResult);
  assert(recordedSF == other.recordedSF);
  assert(recordedName == other.recordedName);
  if (recordedIsCascadingUse)
    assert(other.recordedIsCascadingUse);
  else
    assert(!other.recordedIsCascadingUse);
  return true;
}

void UnqualifiedLookup::dumpBreadcrumbs() const {
  auto &e = llvm::errs();
  for (size_t i : indices(breadcrumbs)) {
    e << i << "\n";
    const PerScopeLookupState &s = breadcrumbs[i];
    s.dump();
  }
}

void UnqualifiedLookup::PerScopeLookupState::dump() const {
  auto &e = llvm::errs();
  e << (isDone ? "done: " : "not done: ");
  e << " dc: ";
  childOfNextDC->dumpContext();
  if (placesToSearch.hasValue())
    placesToSearch.getValue().dump();
}

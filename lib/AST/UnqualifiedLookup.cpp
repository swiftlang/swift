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
class LegacyUnqualifiedLookup;

class UnqualifiedLookupFactory {
public:
  using Flags = UnqualifiedLookup::Flags;
  using Options = UnqualifiedLookup::Options;
  
private:

  // TODO: better name than DC
  struct DCAndResolvedIsCascadingUse {
    DeclContext *const DC;
    const bool isCascadingUse;
  };

  /// Finds lookup results based on the types that self conforms to.
  /// For instance, self always conforms to a struct, enum or class.
  /// But in addition, self could conform to any number of protocols.
  /// For example, when there's a protocol extension, e.g. extension P where
  /// self: P2, self also conforms to P2 so P2 must be searched.
  class ResultFinderForSelfsConformances {
    /// Nontypes are formally members of the base type, i.e. the dynamic type
    /// of the activation record.
    DeclContext *const dynamicContext;
    /// Types are formally members of the metatype, i.e. the static type of the
    /// activation record.
    // DOUG: can you help clarify the above comments?
    DeclContext *const staticContext;
    using NominalTypesThatSelfMustConformTo = SmallVector<NominalTypeDecl *, 2>;
    NominalTypesThatSelfMustConformTo nominalTypesThatSelfMustConformTo;

  public:
    /// \p staticContext is also the context from which to derive the self types
    ResultFinderForSelfsConformances(DeclContext *dynamicContext,
                                     DeclContext *staticContext);

    void dump() const;

  private:
    NominalTypesThatSelfMustConformTo
    findNominalTypesThatSelfMustConformTo(DeclContext *dc);

    // Classify this declaration.
    // Types are formally members of the metatype.
    DeclContext *whereValueIsMember(const ValueDecl *const member) const {
      return isa<TypeDecl>(member) ? staticContext : dynamicContext;
    }

  public:
    /// Do the lookups and add matches to results.
    void findResults(const DeclName &Name, bool isCascadingUse,
                     NLOptions baseNLOptions, DeclContext *contextForLookup,
                     SmallVectorImpl<LookupResultEntry> &results) const;
  };

  enum class AddGenericParameters { Yes, No };

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
  SmallVector<LookupResultEntry, 4> UnavailableInnerResults;

public: // for exp debugging
  SourceFile const *recordedSF = nullptr;
  DeclName recordedName;
  bool recordedIsCascadingUse = false;

public:
  // clang-format off
    UnqualifiedLookupFactory(DeclName Name,
                             DeclContext *const DC,
                             LazyResolver *TypeResolver,
                             SourceLoc Loc,
                             Options options,
                             UnqualifiedLookup &lookupToBeCreated);
  // clang-format on

  void performUnqualifiedLookup();

private:
  struct DCAndUnresolvedIsCascadingUse {
    DeclContext *whereToLook;
    Optional<bool> isCascadingUse;
    DCAndResolvedIsCascadingUse resolve(const bool resolution) const {
      return DCAndResolvedIsCascadingUse{whereToLook,
                                         isCascadingUse.getValueOr(resolution)};
    }
  };

  bool useASTScopesForExperimentalLookup() const;

  void lookUpTopLevelNamesInModuleScopeContext(DeclContext *);

#pragma mark ASTScope-based-lookup declarations

  void experimentallyLookInASTScopes(DCAndUnresolvedIsCascadingUse);

  std::pair<const ASTScope *, bool>
      operatorScopeForASTScopeLookup(DCAndUnresolvedIsCascadingUse);

  std::pair<const ASTScope *, Optional<bool>>
      nonoperatorScopeForASTScopeLookup(DCAndUnresolvedIsCascadingUse) const;

  struct ASTScopeLookupState {
    const ASTScope *scope;
    DeclContext *selfDC;
    DeclContext *dc;
    Optional<bool> isCascadingUse;
    
    ASTScopeLookupState withParentScope() const {
      return ASTScopeLookupState{scope->getParent(), selfDC, dc, isCascadingUse};
    }
    ASTScopeLookupState withNoScope() const {
      return ASTScopeLookupState{nullptr, selfDC, dc, isCascadingUse};
    }
    ASTScopeLookupState withSelfDC(DeclContext *selfDC) const {
      return ASTScopeLookupState{scope, selfDC, dc, isCascadingUse};
    }
    ASTScopeLookupState withDC(DeclContext *dc) const {
      return ASTScopeLookupState{scope, selfDC, dc, isCascadingUse};
    }
    ASTScopeLookupState withResolvedIsCascadingUse(bool isCascadingUse) const {
      return ASTScopeLookupState{scope, selfDC, dc, isCascadingUse};
    }
  };

  void lookInScopeForASTScopeLookup(const ASTScopeLookupState);

  void lookIntoDeclarationContextForASTScopeLookup(ASTScopeLookupState);
  /// Can lookup stop searching for results, assuming hasn't looked for outer
  /// results yet?
  bool isFirstResultEnough() const;

  /// Every time lookup finishes searching a scope, call me
  /// to record the dividing line between results from first fruitful scope and
  /// the result.
  void recordCompletionOfAScope();

  template <typename Fn> void ifNotDoneYet(Fn fn) {
    recordCompletionOfAScope();
    if (!isFirstResultEnough())
      fn();
  }

  template <typename Fn1, typename Fn2> void ifNotDoneYet(Fn1 fn1, Fn2 fn2) {
    ifNotDoneYet(fn1);
    ifNotDoneYet(fn2);
  }

#pragma mark normal (non-ASTScope-based) lookup declarations
  
  void lookupOperatorInDeclContexts(DCAndUnresolvedIsCascadingUse);

  void lookupNamesIntroducedBy(const DCAndUnresolvedIsCascadingUse);

  void finishLookingInContext(AddGenericParameters addGenericParameters,
                              DeclContext *lookupContextForThisContext,
                              Optional<ResultFinderForSelfsConformances>
                                  &&resultFinderForSelfsConformances,
                              Optional<bool> isCascadingUse);

  void lookupInModuleScopeContext(DeclContext *, Optional<bool> isCascadingUse);

  // TODO: use objects & virtuals?

  void lookupNamesIntroducedByPatternBindingInitializer(
      PatternBindingInitializer *PBI, Optional<bool> isCascadingUse);

  void
  lookupNamesIntroducedByLazyVariableInitializer(PatternBindingInitializer *PBI,
                                                 ParamDecl *selfParam,
                                                 Optional<bool> isCascadingUse);

  void lookupNamesIntroducedByInitializerOfStoredPropertyOfAType(
      PatternBindingInitializer *PBI, Optional<bool> isCascadingUse);

  /// An initializer of a global name, or a function-likelocal name.
  void lookupNamesIntroducedByInitializerOfGlobalOrLocal(
      PatternBindingInitializer *PBI, Optional<bool> isCascadingUse);

  void lookupNamesIntroducedByFunctionDecl(AbstractFunctionDecl *AFD,
                                           Optional<bool> isCascadingUse);

  void lookupNamesIntroducedByMemberFunction(AbstractFunctionDecl *AFD,
                                             bool isCascadingUse);

  void lookupNamesIntroducedByPureFunction(AbstractFunctionDecl *AFD,
                                           bool isCascadingUse);

  void lookupNamesIntroducedByClosure(AbstractClosureExpr *ACE,
                                      Optional<bool> isCascadingUse);

  template <typename NominalTypeDeclOrExtensionDecl>
  void lookupNamesIntroducedByNominalTypeOrExtension(
      NominalTypeDeclOrExtensionDecl *D, Optional<bool> isCascadingUse);

  void lookupNamesIntroducedByDefaultArgumentInitializer(
      DefaultArgumentInitializer *I, Optional<bool> isCascadingUse);

  void lookupNamesIntroducedByMiscContext(DeclContext *dc,
                                          Optional<bool> isCascadingUse);

  void lookForLocalVariablesIn(AbstractFunctionDecl *AFD,
                               Optional<bool> isCascadingUse);
  void lookForLocalVariablesIn(ClosureExpr *);
  void lookForLocalVariablesIn(SourceFile *);

  bool isOutsideBodyOfFunction(const AbstractFunctionDecl *const AFD) const;

  void addGenericParametersHereAndInEnclosingScopes(DeclContext *dc);
  void addGenericParametersHereAndInEnclosingScopes(GenericParamList *);

  /// Consume generic parameters
  void addGenericParametersForFunction(AbstractFunctionDecl *AFD);

  static GenericParamList *getGenericParams(const DeclContext *const dc);

  /// For diagnostic purposes, move aside the unavailables, and put
  /// them back as a last-ditch effort.
  /// Could be cleaner someday with a richer interface to UnqualifiedLookup.
  void setAsideUnavailableResults(size_t firstPossiblyUnavailableResult);

  void recordDependencyOnTopLevelName(DeclContext *topLevelContext,
                                      DeclName name, bool isCascadingUse);

  void addPrivateImports(DeclContext *const dc);

  void addNamesKnownToDebugClient(DeclContext *dc);

  void addUnavailableInnerResults();

  void lookForAModuleWithTheGivenName(DeclContext *const dc);

#pragma mark common helper declarations
  static NLOptions
  computeBaseNLOptions(const UnqualifiedLookup::Options options,
                       const bool isOriginallyTypeLookup);

  static bool resolveIsCascadingUse(const DeclContext *const dc,
                                    Optional<bool> isCascadingUse,
                                    bool onlyCareAboutFunctionBody);
  static bool resolveIsCascadingUse(DCAndUnresolvedIsCascadingUse x,
                                    bool onlyCareAboutFunctionBody) {
    return resolveIsCascadingUse(x.whereToLook, x.isCascadingUse,
                                 onlyCareAboutFunctionBody);
  }

  void findResultsAndSaveUnavailables(ResultFinderForSelfsConformances &&PTS,
                                      bool isCascadingUse,
                                      NLOptions baseNLOptions,
                                      DeclContext *lookupContextForThisContext);

  void dumpBreadcrumbs() const;

public:
  bool verifyEqualToLegacy(const LegacyUnqualifiedLookup &&LUL) const;
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
IndexOfFirstOuterResult(lookupToBeCreated.IndexOfFirstOuterResult)
{}
// clang-format on

void UnqualifiedLookupFactory::performUnqualifiedLookup() {
  const Optional<bool> isCascadingUseInitial =
  options.contains(Flags::KnownPrivate) ? Optional<bool>(false) : None;

  DCAndUnresolvedIsCascadingUse dcAndIsCascadingUse{DC, isCascadingUseInitial};
  if (useASTScopesForExperimentalLookup())
    experimentallyLookInASTScopes(dcAndIsCascadingUse);
  else if (Name.isOperator())
    lookupOperatorInDeclContexts(dcAndIsCascadingUse);
  else
    lookupNamesIntroducedBy(dcAndIsCascadingUse);
}

void UnqualifiedLookupFactory::lookUpTopLevelNamesInModuleScopeContext(
    DeclContext *DC) {
  // TODO: Does the debugger client care about compound names?
  if (Name.isSimpleName() && DebugClient &&
      DebugClient->lookupOverrides(Name.getBaseName(), DC, Loc,
                                   isOriginallyTypeLookup, Results))
    return;

  addPrivateImports(DC);
  addNamesKnownToDebugClient(DC);
  if (Results.empty()) {
    // If we still haven't found anything, but we do have some
    // declarations that are "unavailable in the current Swift", drop
    // those in.
    addUnavailableInnerResults();
    if (Results.empty())
      lookForAModuleWithTheGivenName(DC);
  }
  recordCompletionOfAScope();
}

bool UnqualifiedLookupFactory::useASTScopesForExperimentalLookup() const {
  return Loc.isValid() && DC->getParentSourceFile() &&
         DC->getParentSourceFile()->Kind != SourceFileKind::REPL &&
         Ctx.LangOpts.EnableASTScopeLookup;
}

#pragma mark ASTScope-based-lookup definitions

void UnqualifiedLookupFactory::experimentallyLookInASTScopes(
    const DCAndUnresolvedIsCascadingUse dcAndIsCascadingUseArg) {
  const std::pair<const ASTScope *, Optional<bool>>
      lookupScopeAndIsCascadingUse =
          Name.isOperator()
              ? operatorScopeForASTScopeLookup(dcAndIsCascadingUseArg)
              : nonoperatorScopeForASTScopeLookup(dcAndIsCascadingUseArg);
  // Walk scopes outward from the innermost scope until we find something.

  ASTScopeLookupState state{lookupScopeAndIsCascadingUse.first, nullptr,
                            dcAndIsCascadingUseArg.whereToLook,
                            lookupScopeAndIsCascadingUse.second};
  lookInScopeForASTScopeLookup(state);
}

std::pair<const ASTScope *, bool>
UnqualifiedLookupFactory::operatorScopeForASTScopeLookup(
    const DCAndUnresolvedIsCascadingUse dcAndIsCascadingUseArg) {
  // Find the source file in which we are performing the lookup.
  SourceFile &sourceFile =
      *dcAndIsCascadingUseArg.whereToLook->getParentSourceFile();

  // Find the scope from which we will initiate unqualified name lookup.
  const ASTScope *lookupScope =
      sourceFile.getScope().findInnermostEnclosingScope(Loc);

  // Operator lookup is always at module scope.
  return std::make_pair(
      &sourceFile.getScope(),
      resolveIsCascadingUse(lookupScope->getInnermostEnclosingDeclContext(),
                            dcAndIsCascadingUseArg.isCascadingUse,
                            /*onlyCareAboutFunctionBody*/ true));
}

std::pair<const ASTScope *, Optional<bool>>
UnqualifiedLookupFactory::nonoperatorScopeForASTScopeLookup(
    const DCAndUnresolvedIsCascadingUse dcAndIsCascadingUseArg) const {
  // Find the source file in which we are performing the lookup.
  SourceFile &sourceFile =
      *dcAndIsCascadingUseArg.whereToLook->getParentSourceFile();

  // Find the scope from which we will initiate unqualified name lookup.
  const ASTScope *lookupScope =
      sourceFile.getScope().findInnermostEnclosingScope(Loc);

  return std::make_pair(lookupScope, dcAndIsCascadingUseArg.isCascadingUse);
}

void UnqualifiedLookupFactory::lookInScopeForASTScopeLookup(
    const ASTScopeLookupState state) {

  // Perform local lookup within this scope.
  auto localBindings = state.scope->getLocalBindings();
  for (auto local : localBindings) {
    Consumer.foundDecl(local, getLocalDeclVisibilityKind(state.scope));
  }
  ifNotDoneYet([&] {
    // When we are in the body of a method, get the 'self' declaration.
    const bool inBody =
        state.scope->getKind() == ASTScopeKind::AbstractFunctionBody &&
        state.scope->getAbstractFunctionDecl()
            ->getDeclContext()
            ->isTypeContext();
    if (inBody)
      lookInScopeForASTScopeLookup(
          state.withSelfDC(state.scope->getAbstractFunctionDecl())
              .withParentScope());
    // If there is a declaration context associated with this scope, we might
    // want to look in it.
    else
      lookIntoDeclarationContextForASTScopeLookup(state);
  });
}

void UnqualifiedLookupFactory::lookIntoDeclarationContextForASTScopeLookup(
    ASTScopeLookupState stateArg) {

  DeclContext *scopeDC = stateArg.scope->getDeclContext();
  if (!scopeDC) {
    lookInScopeForASTScopeLookup(stateArg.withParentScope());
    return;
  }

  // If we haven't determined whether we have a cascading use, do so now.
  const bool isCascadingUseResult = resolveIsCascadingUse(
      scopeDC, stateArg.isCascadingUse, /*onlyCareAboutFunctionBody=*/false);

  const ASTScopeLookupState defaultNextState =
      stateArg.withResolvedIsCascadingUse(isCascadingUseResult)
          .withParentScope();

  // Pattern binding initializers are only interesting insofar as they
  // affect lookup in an enclosing nominal type or extension thereof.
  if (auto *bindingInit = dyn_cast<PatternBindingInitializer>(scopeDC)) {
    // Lazy variable initializer contexts have a 'self' parameter for
    // instance member lookup.
    lookInScopeForASTScopeLookup(bindingInit->getImplicitSelfDecl()
                                     ? defaultNextState.withSelfDC(bindingInit)
                                     : defaultNextState);
    return;
  }

  // Default arguments only have 'static' access to the members of the
  // enclosing type, if there is one.
  if (isa<DefaultArgumentInitializer>(scopeDC)) {
    lookInScopeForASTScopeLookup(defaultNextState);
    return;
  }
  // Functions/initializers/deinitializers are only interesting insofar as
  // they affect lookup in an enclosing nominal type or extension thereof.
  if (isa<AbstractFunctionDecl>(scopeDC)) {
    lookInScopeForASTScopeLookup(defaultNextState);
    return;
  }
  // Subscripts have no lookup of their own.
  if (isa<SubscriptDecl>(scopeDC)) {
    lookInScopeForASTScopeLookup(defaultNextState);
    return;
  }
  // Closures have no lookup of their own.
  if (isa<AbstractClosureExpr>(scopeDC)) {
    lookInScopeForASTScopeLookup(defaultNextState);
    return;
  }
  // Top-level declarations have no lookup of their own.
  if (isa<TopLevelCodeDecl>(scopeDC)) {
    lookInScopeForASTScopeLookup(defaultNextState);
    return;
  }
  // Typealiases have no lookup of their own.
  if (isa<TypeAliasDecl>(scopeDC)) {
    lookInScopeForASTScopeLookup(defaultNextState);
    return;
  }
  // Lookup in the source file's scope marks the end.
  if (isa<SourceFile>(scopeDC)) {
    recordDependencyOnTopLevelName(scopeDC, Name, isCascadingUseResult);
    lookUpTopLevelNamesInModuleScopeContext(scopeDC);
    return;
  }

  // We have a nominal type or an extension thereof. Perform lookup into
  // the nominal type.
  auto nominal = scopeDC->getSelfNominalTypeDecl();
  if (!nominal) {
    lookInScopeForASTScopeLookup(defaultNextState);
    return;
  }
  // Dig out the type we're looking into.
  // Perform lookup into the type
  findResultsAndSaveUnavailables(
      ResultFinderForSelfsConformances(
          defaultNextState.selfDC ? defaultNextState.selfDC : scopeDC, scopeDC),
      isCascadingUseResult, baseNLOptions, scopeDC);
  // Forget the 'self' declaration.
  ifNotDoneYet([&] {
    lookInScopeForASTScopeLookup(defaultNextState.withSelfDC(nullptr));
  });
}

#pragma mark context-based lookup definitions

void UnqualifiedLookupFactory::lookupOperatorInDeclContexts(
    const DCAndUnresolvedIsCascadingUse dcAndUseArg) {
  DCAndResolvedIsCascadingUse dcAndResolvedIsCascadingUse{
      // Operators are global
      dcAndUseArg.whereToLook->getModuleScopeContext(),
      resolveIsCascadingUse(dcAndUseArg, /*onlyCareAboutFunctionBody*/ true)};
  lookupInModuleScopeContext(dcAndResolvedIsCascadingUse.DC,
                             dcAndResolvedIsCascadingUse.isCascadingUse);
}

// TODO: Unify with LookupVisibleDecls.cpp::lookupVisibleDeclsImpl
void UnqualifiedLookupFactory::lookupNamesIntroducedBy(
    const DCAndUnresolvedIsCascadingUse dcAndIsCascadingUseArg) {

  DeclContext *const dc = dcAndIsCascadingUseArg.whereToLook;
  const auto isCascadingUseSoFar = dcAndIsCascadingUseArg.isCascadingUse;
  if (dc->isModuleScopeContext())
    lookupInModuleScopeContext(dc, isCascadingUseSoFar);
  else if (auto *PBI = dyn_cast<PatternBindingInitializer>(dc))
    lookupNamesIntroducedByPatternBindingInitializer(PBI, isCascadingUseSoFar);
  else if (auto *AFD = dyn_cast<AbstractFunctionDecl>(dc))
    lookupNamesIntroducedByFunctionDecl(AFD, isCascadingUseSoFar);
  else if (auto *ACE = dyn_cast<AbstractClosureExpr>(dc))
    lookupNamesIntroducedByClosure(ACE, isCascadingUseSoFar);
  else if (auto *ED = dyn_cast<ExtensionDecl>(dc))
    lookupNamesIntroducedByNominalTypeOrExtension(ED, isCascadingUseSoFar);
  else if (auto *ND = dyn_cast<NominalTypeDecl>(dc))
    lookupNamesIntroducedByNominalTypeOrExtension(ND, isCascadingUseSoFar);
  else if (auto I = dyn_cast<DefaultArgumentInitializer>(dc))
    lookupNamesIntroducedByDefaultArgumentInitializer(I, isCascadingUseSoFar);
  else
    lookupNamesIntroducedByMiscContext(dc, isCascadingUseSoFar);
}

void UnqualifiedLookupFactory::finishLookingInContext(
    const AddGenericParameters addGenericParameters,
    DeclContext *const lookupContextForThisContext,
    Optional<ResultFinderForSelfsConformances>
        &&resultFinderForSelfsConformances,
    const Optional<bool> isCascadingUse) {

  // When a generic has the same name as a member, Swift prioritizes the generic
  // because the member could still be named by qualifying it. But there is no
  // corresponding way to qualify a generic parameter.
  // So, look for genrics first.
  if (addGenericParameters == AddGenericParameters::Yes)
    addGenericParametersHereAndInEnclosingScopes(lookupContextForThisContext);

  ifNotDoneYet(
      [&] {
        if (resultFinderForSelfsConformances)
          findResultsAndSaveUnavailables(
              std::move(*resultFinderForSelfsConformances), *isCascadingUse,
              baseNLOptions, lookupContextForThisContext);
      },
      // Recurse into the next context.
      [&] {
        lookupNamesIntroducedBy(DCAndUnresolvedIsCascadingUse{
            lookupContextForThisContext->getParentForLookup(), isCascadingUse});
      });
}

void UnqualifiedLookupFactory::findResultsAndSaveUnavailables(
    ResultFinderForSelfsConformances &&PTS, bool isCascadingUse,
    NLOptions baseNLOptions, DeclContext *lookupContextForThisContext) {
  auto firstPossiblyUnavailableResult = Results.size();
  PTS.findResults(Name, isCascadingUse, baseNLOptions,
                  lookupContextForThisContext, Results);
  setAsideUnavailableResults(firstPossiblyUnavailableResult);
}

void UnqualifiedLookupFactory::lookupInModuleScopeContext(
    DeclContext *dc, Optional<bool> isCascadingUse) {
  if (auto SF = dyn_cast<SourceFile>(dc))
    lookForLocalVariablesIn(SF);
  ifNotDoneYet([&] {
    // If no result has been found yet, the dependency must be on a top-level
    // name, since up to now, the search has been for non-top-level names.
    recordDependencyOnTopLevelName(dc, Name, isCascadingUse.getValueOr(true));
    lookUpTopLevelNamesInModuleScopeContext(dc);
  });
}

void UnqualifiedLookupFactory::lookupNamesIntroducedByPatternBindingInitializer(
    PatternBindingInitializer *PBI, Optional<bool> isCascadingUse) {
  assert(PBI->getBinding());
  // Lazy variable initializer contexts have a 'self' parameter for
  // instance member lookup.
  if (auto *selfParam = PBI->getImplicitSelfDecl())
    lookupNamesIntroducedByLazyVariableInitializer(PBI, selfParam,
                                                   isCascadingUse);
  else if (PBI->getBinding()->getDeclContext()->isTypeContext())
    lookupNamesIntroducedByInitializerOfStoredPropertyOfAType(PBI,
                                                              isCascadingUse);
  else
    lookupNamesIntroducedByInitializerOfGlobalOrLocal(PBI, isCascadingUse);
  }

  void UnqualifiedLookupFactory::lookupNamesIntroducedByLazyVariableInitializer(
      PatternBindingInitializer *PBI, ParamDecl *selfParam,
      Optional<bool> isCascadingUse) {
    Consumer.foundDecl(selfParam, DeclVisibilityKind::FunctionParameter);
    ifNotDoneYet([&] {
      DeclContext *const patternContainer = PBI->getParent();
      // clang-format off
    finishLookingInContext(
      AddGenericParameters::Yes,
      patternContainer,
      ResultFinderForSelfsConformances(PBI, patternContainer),
      resolveIsCascadingUse(PBI, isCascadingUse,
                           /*onlyCareAboutFunctionBody=*/false));
      // clang-format on
    });
}

void UnqualifiedLookupFactory::
    lookupNamesIntroducedByInitializerOfStoredPropertyOfAType(
        PatternBindingInitializer *PBI, Optional<bool> isCascadingUse) {
  // Initializers for stored properties of types perform static
  // lookup into the surrounding context.
  DeclContext *const storedPropertyContainer = PBI->getParent();
  // clang-format off
  finishLookingInContext(
    AddGenericParameters::Yes,
    storedPropertyContainer,
    ResultFinderForSelfsConformances(storedPropertyContainer, storedPropertyContainer),
    resolveIsCascadingUse(storedPropertyContainer, None,
                          /*onlyCareAboutFunctionBody=*/false));
  // clang-format on
}

void UnqualifiedLookupFactory::
    lookupNamesIntroducedByInitializerOfGlobalOrLocal(
        PatternBindingInitializer *PBI, Optional<bool> isCascadingUse) {
  // There's not much to find here, we'll keep going up to a parent
  // context.
  // clang-format off
  finishLookingInContext(
                         AddGenericParameters::Yes,
                         PBI,
                         None, // not looking in the partic type
                         resolveIsCascadingUse(PBI, isCascadingUse,
                                               /*onlyCareAboutFunctionBody=*/false));
  // clang-format on
}

void UnqualifiedLookupFactory::lookupNamesIntroducedByFunctionDecl(
    AbstractFunctionDecl *AFD, Optional<bool> isCascadingUseArg) {

  // DOUG: how does this differ from isOutsideBodyOfFunction below?
  const bool isCascadingUse =
      AFD->isCascadingContextForLookup(false) &&
      (isCascadingUseArg.getValueOr(
          Loc.isInvalid() || !AFD->getBody() ||
          !SM.rangeContainsTokenLoc(AFD->getBodySourceRange(), Loc)));

  if (AFD->getDeclContext()->isTypeContext())
    lookupNamesIntroducedByMemberFunction(AFD, isCascadingUse);
  else
    lookupNamesIntroducedByPureFunction(AFD, isCascadingUse);
}

void UnqualifiedLookupFactory::lookupNamesIntroducedByMemberFunction(
    AbstractFunctionDecl *AFD, bool isCascadingUse) {
  lookForLocalVariablesIn(AFD, isCascadingUse);
  ifNotDoneYet(
      [&] {
        // If we're inside a function context, we're about to move to
        // the parent DC, so we have to check the function's generic
        // parameters first.
        // Cannot start here in finishLookingInContext because AFD's
        // getOuterParameters may be null even when AFD's parent has generics.
        addGenericParametersForFunction(AFD);
      },
      [&] {
        DeclContext *const fnDeclContext = AFD->getDeclContext();
        // If we're not in the body of the function (for example, we
        // might be type checking a default argument expression and
        // performing name lookup from there), the base declaration
        // is the nominal type, not 'self'.
        DeclContext *const BaseDC =
            isOutsideBodyOfFunction(AFD) ? fnDeclContext : AFD;
        // If we are inside of a method, check to see if there are any ivars in
        // scope, and if so, whether this is a reference to one of them.
        // FIXME: We should persist this information between lookups.
        // clang-format off
      finishLookingInContext(
                             AddGenericParameters::Yes,
                             AFD->getParent(),
                             ResultFinderForSelfsConformances(BaseDC, fnDeclContext),
                             isCascadingUse);
        // clang-format on
      });
}

void UnqualifiedLookupFactory::lookupNamesIntroducedByPureFunction(
    AbstractFunctionDecl *AFD, bool isCascadingUse) {
  lookForLocalVariablesIn(AFD, isCascadingUse);
  ifNotDoneYet([&] {
    // clang-format off
    finishLookingInContext(
                           AddGenericParameters::Yes,
                           AFD,
                           None,
                           isCascadingUse);
  });
}


void UnqualifiedLookupFactory::lookupNamesIntroducedByClosure(
    AbstractClosureExpr *ACE, Optional<bool> isCascadingUse) {
  if (auto *CE = dyn_cast<ClosureExpr>(ACE))
    lookForLocalVariablesIn(CE);
  ifNotDoneYet([&] {
    // clang-format off
    finishLookingInContext(
      AddGenericParameters::Yes,
      ACE,
      None,
      resolveIsCascadingUse(ACE, isCascadingUse,
                           /*onlyCareAboutFunctionBody=*/false));
    // clang-format on
  });
}

template <typename NominalTypeDeclOrExtensionDecl>
void UnqualifiedLookupFactory::lookupNamesIntroducedByNominalTypeOrExtension(
    NominalTypeDeclOrExtensionDecl *D, Optional<bool> isCascadingUse) {
  // clang-format off
  finishLookingInContext(
    AddGenericParameters::Yes,
    D,
    shouldLookupMembers(D, Loc)
    ? Optional<ResultFinderForSelfsConformances>(ResultFinderForSelfsConformances(D, D))
    : None,
    resolveIsCascadingUse(D, isCascadingUse,
                          /*onlyCareAboutFunctionBody=*/false));

  // clang-format on
}

void UnqualifiedLookupFactory::
    lookupNamesIntroducedByDefaultArgumentInitializer(
        DefaultArgumentInitializer *I, Optional<bool> isCascadingUse) {
  // In a default argument, skip immediately out of both the
  // initializer and the function.
  finishLookingInContext(AddGenericParameters::No, I->getParent(), None, false);
}

void UnqualifiedLookupFactory::lookupNamesIntroducedByMiscContext(
    DeclContext *dc, Optional<bool> isCascadingUse) {
  // clang-format off
  assert(isa<TopLevelCodeDecl>(dc) ||
         isa<Initializer>(dc) ||
         isa<TypeAliasDecl>(dc) ||
         isa<SubscriptDecl>(dc));
  finishLookingInContext(
    AddGenericParameters::Yes,
    dc,
    None,
    resolveIsCascadingUse(DC, isCascadingUse,
                          /*onlyCareAboutFunctionBody=*/false));
  // clang-format on
}

void UnqualifiedLookupFactory::lookForLocalVariablesIn(
    AbstractFunctionDecl *AFD, Optional<bool> isCascadingUse) {
  // Look for local variables; normally, the parser resolves these
  // for us, but it can't do the right thing inside local types.
  // FIXME: when we can parse and typecheck the function body partially
  // for code completion, AFD->getBody() check can be removed.

  if (Loc.isInvalid() || !AFD->getBody()) {
    return;
  }

  namelookup::FindLocalVal localVal(SM, Loc, Consumer);
  localVal.visit(AFD->getBody());

  ifNotDoneYet([&] {
    if (auto *P = AFD->getImplicitSelfDecl())
      localVal.checkValueDecl(P, DeclVisibilityKind::FunctionParameter);
    localVal.checkParameterList(AFD->getParameters());
  });
}

void UnqualifiedLookupFactory::lookForLocalVariablesIn(ClosureExpr *CE) {
  // Look for local variables; normally, the parser resolves these
  // for us, but it can't do the right thing inside local types.
  if (Loc.isInvalid())
    return;
  namelookup::FindLocalVal localVal(SM, Loc, Consumer);
  if (auto body = CE->getBody())
    localVal.visit(body);
  ifNotDoneYet([&] {
    if (auto params = CE->getParameters())
      localVal.checkParameterList(params);
  });
}

void UnqualifiedLookupFactory::lookForLocalVariablesIn(SourceFile *SF) {
  if (Loc.isInvalid())
    return;
  // Look for local variables in top-level code; normally, the parser
  // resolves these for us, but it can't do the right thing for
  // local types.
  namelookup::FindLocalVal localVal(SM, Loc, Consumer);
  localVal.checkSourceFile(*SF);
}

bool UnqualifiedLookupFactory::isOutsideBodyOfFunction(
    const AbstractFunctionDecl *const AFD) const {
  return !AFD->isImplicit() && Loc.isValid() &&
         AFD->getBodySourceRange().isValid() &&
         !SM.rangeContainsTokenLoc(AFD->getBodySourceRange(), Loc);
}

GenericParamList *
UnqualifiedLookupFactory::getGenericParams(const DeclContext *const dc) {
  if (auto nominal = dyn_cast<NominalTypeDecl>(dc))
    return nominal->getGenericParams();
  if (auto ext = dyn_cast<ExtensionDecl>(dc))
    return ext->getGenericParams();
  if (auto subscript = dyn_cast<SubscriptDecl>(dc))
    return subscript->getGenericParams();
  if (auto func = dyn_cast<AbstractFunctionDecl>(dc))
    return func->getGenericParams();
  return nullptr;
}

void UnqualifiedLookupFactory::addGenericParametersHereAndInEnclosingScopes(
    DeclContext *dc) {
  // Generics can be nested, so visit the generic list, innermost first.
  // Cannot use DeclContext::forEachGenericContext because this code breaks out
  // if it finds a match and isFirstResultEnough()
  addGenericParametersHereAndInEnclosingScopes(getGenericParams(dc));
}

void UnqualifiedLookupFactory::addGenericParametersHereAndInEnclosingScopes(
    GenericParamList *dcGenericParams) {
  if (!dcGenericParams)
    return;
  namelookup::FindLocalVal localVal(SM, Loc, Consumer);
  localVal.checkGenericParams(dcGenericParams);
  ifNotDoneYet([&] {
    addGenericParametersHereAndInEnclosingScopes(
        dcGenericParams->getOuterParameters());
  });
}

void UnqualifiedLookupFactory::addGenericParametersForFunction(
    AbstractFunctionDecl *AFD) {
  GenericParamList *GenericParams = AFD->getGenericParams();
  if (GenericParams) {
    namelookup::FindLocalVal localVal(SM, Loc, Consumer);
    localVal.checkGenericParams(GenericParams);
  }
}

void UnqualifiedLookupFactory::ResultFinderForSelfsConformances::findResults(
    const DeclName &Name, bool isCascadingUse, NLOptions baseNLOptions,
    DeclContext *contextForLookup,
    SmallVectorImpl<LookupResultEntry> &results) const {
  // An optimization:
  if (nominalTypesThatSelfMustConformTo.empty())
    return;
  const NLOptions options =
      baseNLOptions | (isCascadingUse ? NL_KnownCascadingDependency
                                      : NL_KnownNonCascadingDependency);

  SmallVector<ValueDecl *, 4> Lookup;
  contextForLookup->lookupQualified(nominalTypesThatSelfMustConformTo, Name,
                                    options, Lookup);
  for (auto Result : Lookup)
    results.push_back(LookupResultEntry(whereValueIsMember(Result), Result));
}

// TODO (someday): Instead of adding unavailable entries to Results,
// then later shunting them aside, just put them in the right place
// to begin with.

void UnqualifiedLookupFactory::setAsideUnavailableResults(
    const size_t firstPossiblyUnavailableResult) {
  // An optimization:
  assert(Results.size() >= firstPossiblyUnavailableResult);
  if (Results.size() == firstPossiblyUnavailableResult)
    return;
  // Predicate that determines whether a lookup result should
  // be unavailable except as a last-ditch effort.
  auto unavailableLookupResult = [&](const LookupResultEntry &result) {
    auto &effectiveVersion = Ctx.LangOpts.EffectiveLanguageVersion;
    return result.getValueDecl()->getAttrs().isUnavailableInSwiftVersion(
        effectiveVersion);
  };

  // If all of the results we found are unavailable, keep looking.
  auto begin = Results.begin() + firstPossiblyUnavailableResult;
  if (std::all_of(begin, Results.end(), unavailableLookupResult)) {
    // better to have more structure in results
    UnavailableInnerResults.append(begin, Results.end());
    Results.erase(begin, Results.end());
    return;
  }
  // The debugger may have a different private discriminator
  // in order to support lookup relative to the place where
  // execution is suspended.
  filterForDiscriminator(Results, DebugClient);
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

  filterForDiscriminator(Results, DebugClient);
}

void UnqualifiedLookupFactory::addNamesKnownToDebugClient(DeclContext *dc) {
  if (Name.isSimpleName() && DebugClient)
    DebugClient->lookupAdditions(Name.getBaseName(), dc, Loc,
                                 isOriginallyTypeLookup, Results);
}

void UnqualifiedLookupFactory::addUnavailableInnerResults() {
  Results = std::move(UnavailableInnerResults);
}

void UnqualifiedLookupFactory::lookForAModuleWithTheGivenName(
    DeclContext *const dc) {
  using namespace namelookup;
  if (!Name.isSimpleName())
    return;

  // Look for a module with the given name.
  if (Name.isSimpleName(M.getName())) {
    Results.push_back(LookupResultEntry(&M));
    return;
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

bool UnqualifiedLookupFactory::isFirstResultEnough()
    const {
  return !Results.empty() && !options.contains(Flags::IncludeOuterResults);
}

void UnqualifiedLookupFactory::recordCompletionOfAScope() {
  // OK to call (NOOP) if there are more inner results and Results is empty
  if (IndexOfFirstOuterResult == 0)
    IndexOfFirstOuterResult = Results.size();
}

bool UnqualifiedLookupFactory::resolveIsCascadingUse(
    const DeclContext *const dc, Optional<bool> isCascadingUse,
    bool onlyCareAboutFunctionBody) {
  return isCascadingUse.getValueOr(dc->isCascadingContextForLookup(
      /*functionsAreNonCascading=*/onlyCareAboutFunctionBody));
}

#pragma mark LegacyUnqualifiedLookup

namespace {
/// This class implements and represents the result of performing
/// unqualified lookup (i.e. lookup for a plain identifier).
/// It is being kept around in order to check the refactoring against the new
/// UnqualifiedLookup above.
/// TODO: after landing the UnqualifiedLookupFactory and passing tests,
/// delete this class.
class LegacyUnqualifiedLookup {
public:
  using Flags = UnqualifiedLookup::Flags;
  using Options = UnqualifiedLookup::Options;

  /// Lookup an unqualified identifier \p Name in the context.
  ///
  /// If the current DeclContext is nested in a function body, the SourceLoc
  /// is used to determine which declarations in that body are visible.
  LegacyUnqualifiedLookup(DeclName Name, DeclContext *DC,
                          LazyResolver *TypeResolver,
                          SourceLoc Loc = SourceLoc(),
                          Options options = Options());

  SmallVector<LookupResultEntry, 4> Results;
  /// The index of the first result that isn't from the innermost scope
  /// with results.
  ///
  /// That is, \c makeArrayRef(Results).take_front(IndexOfFirstOuterResults)
  /// will be Results from the innermost scope that had results, and the
  /// remaining elements of Results will be from parent scopes of this one.
  size_t IndexOfFirstOuterResult;

  /// Return true if anything was found by the name lookup.
  bool isSuccess() const { return !Results.empty(); }

  /// Get the result as a single type, or a null type if that fails.
  TypeDecl *getSingleTypeResult() const;

public: // for exp debugging
  SourceFile const *recordedSF = nullptr;
  DeclName recordedName;
  bool recordedIsCascadingUse = false;

private:
  static void populateLookupDeclsFromContext(
      DeclContext *dc, SmallVectorImpl<NominalTypeDecl *> &lookupDecls);
};
}; // namespace

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
        if (*isCascadingUse)
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
          if (*isCascadingUse)
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

  recordLookupOfTopLevelName(DC, Name, *isCascadingUse);
  recordedSF = dyn_cast<SourceFile>(DC);
  recordedName = Name;
  recordedIsCascadingUse = *isCascadingUse;

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

TypeDecl *LegacyUnqualifiedLookup::getSingleTypeResult() const {
  if (Results.size() != 1)
    return nullptr;
  return dyn_cast<TypeDecl>(Results.back().getValueDecl());
}

UnqualifiedLookupFactory::ResultFinderForSelfsConformances::
    ResultFinderForSelfsConformances(DeclContext *dynamicContext,
                                     DeclContext *staticContext)
    : dynamicContext(dynamicContext), staticContext(staticContext),
      nominalTypesThatSelfMustConformTo(
          findNominalTypesThatSelfMustConformTo(staticContext)) {}

UnqualifiedLookupFactory::ResultFinderForSelfsConformances::
    NominalTypesThatSelfMustConformTo
    UnqualifiedLookupFactory::ResultFinderForSelfsConformances::
        findNominalTypesThatSelfMustConformTo(DeclContext *dc) {
  auto nominal = dc->getSelfNominalTypeDecl();
  if (!nominal)
    return {};

  NominalTypesThatSelfMustConformTo lookupDecls;
  lookupDecls.push_back(nominal);

  // For a protocol extension, check whether there are additional "Self"
  // constraints that can affect name lookup.
  if (dc->getExtendedProtocolDecl()) {
    auto ext = cast<ExtensionDecl>(dc);
    auto bounds = getSelfBoundsFromWhereClause(ext);
    for (auto bound : bounds.decls)
      lookupDecls.push_back(bound);
  }
  return lookupDecls;
}

void LegacyUnqualifiedLookup::populateLookupDeclsFromContext(
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

void UnqualifiedLookupFactory::ResultFinderForSelfsConformances::dump() const {
  llvm::errs() << "dynamicContext: ";
  dynamicContext->dumpContext();
  llvm::errs() << "staticContext: ";
  staticContext->dumpContext();
  llvm::errs() << "nominalTypesThatSelfMustConformTo: ";
  for (const auto *D : nominalTypesThatSelfMustConformTo)
    D->dump(llvm::errs(), 1);
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
  UnqualifiedLookupFactory factory(Name, DC, TypeResolver, Loc, options, *this);
  factory.performUnqualifiedLookup();
  assert(factory.verifyEqualToLegacy(
             LegacyUnqualifiedLookup(Name, DC, TypeResolver, Loc, options)) &&
         "bad refactoring");
}

TypeDecl *UnqualifiedLookup::getSingleTypeResult() const {
  if (Results.size() != 1)
    return nullptr;
  return dyn_cast<TypeDecl>(Results.back().getValueDecl());
}

bool UnqualifiedLookupFactory::verifyEqualToLegacy(
    const LegacyUnqualifiedLookup &&LUL) const {
  assert(Results.size() == LUL.Results.size());
  for (size_t i : indices(Results)) {
    const auto &e = Results[i];
    const auto &oe = LUL.Results[i];
    assert(e.getValueDecl() == oe.getValueDecl());
    assert(e.getDeclContext() == oe.getDeclContext());
    // unsigned printContext(llvm::raw_ostream &OS, unsigned indent = 0,
    // bool onlyAPartialLine = false) const;
    assert(e.getBaseDecl() == oe.getBaseDecl());
  }
  assert(IndexOfFirstOuterResult == LUL.IndexOfFirstOuterResult);
  assert(recordedSF == LUL.recordedSF);
  assert(recordedName == LUL.recordedName);
  if (recordedIsCascadingUse)
    assert(LUL.recordedIsCascadingUse);
  else
    assert(!LUL.recordedIsCascadingUse);
  return true;
}

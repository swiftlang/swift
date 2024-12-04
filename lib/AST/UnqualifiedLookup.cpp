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
///
/// This file implements unqualified lookup, which searches for an identifier
/// from a given context.
///
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/DebuggerClient.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "namelookup"

using namespace swift;
using namespace swift::namelookup;

namespace {
  class UnqualifiedLookupFactory {

    friend class ASTScopeDeclConsumerForUnqualifiedLookup;

  public:
    using Flags = UnqualifiedLookupFlags;
    using Options = UnqualifiedLookupOptions;
    using ResultsVector = SmallVector<LookupResultEntry, 4>;
    
  private:
    // Inputs
    const DeclNameRef Name;
    DeclContext *const DC;
    ModuleDecl &M;
    const ASTContext &Ctx;
    const SourceLoc Loc;

    /// Used to find the file-local names.
    DebuggerClient *const DebugClient;
    
    const Options options;
    const bool isOriginallyTypeLookup;
    const bool isOriginallyMacroLookup;
    const NLOptions baseNLOptions;

    // Outputs
    SmallVectorImpl<LookupResultEntry> &Results;
    size_t &IndexOfFirstOuterResult;
    ResultsVector UnavailableInnerResults;

#ifndef NDEBUG
    static unsigned lookupCounter;
    static const unsigned targetLookup;
#endif

  public:
    // clang-format off
    UnqualifiedLookupFactory(DeclNameRef Name,
                             DeclContext *const DC,
                             SourceLoc Loc,
                             Options options,
                             SmallVectorImpl<LookupResultEntry> &Results,
                             size_t &IndexOfFirstOuterResult);
    // clang-format on
    
    void performUnqualifiedLookup();
    
  private:
    void lookUpTopLevelNamesInModuleScopeContext(const DeclContext *);

    void lookInASTScopes();

    /// Can lookup stop searching for results, assuming hasn't looked for outer
    /// results yet?
    bool isFirstResultEnough() const;

    /// Do we want precise scoping of VarDecls? If IncludeOuterResults is on,
    /// this is true, which allows us to resolve forward references to
    /// local VarDecls from inside local function and closure bodies.
    bool hasPreciseScopingOfVarDecls() const;

    /// Every time lookup finishes searching a scope, call me
    /// to record the dividing line between results from first fruitful scope and
    /// the result.
    void recordCompletionOfAScope();

#pragma mark context-based lookup declarations

    ValueDecl *lookupBaseDecl(const DeclContext *baseDC) const;

    ValueDecl *getBaseDeclForResult(const DeclContext *baseDC) const;

    /// For diagnostic purposes, move aside the unavailables, and put
    /// them back as a last-ditch effort.
    /// Could be cleaner someday with a richer interface to UnqualifiedLookup.
    void setAsideUnavailableResults(size_t firstPossiblyUnavailableResult);

    void addImportedResults(const DeclContext *const dc);

    void addNamesKnownToDebugClient(const DeclContext *dc);

    void addUnavailableInnerResults();

    void lookForAModuleWithTheGivenName(const DeclContext *dc);

#pragma mark common helper declarations
    static NLOptions
    computeBaseNLOptions(const UnqualifiedLookupOptions options,
                         const bool isOriginallyTypeLookup,
                         const bool isOriginallyMacroLookup);

    void findResultsAndSaveUnavailables(
        const DeclContext *dynamicContext,
        const DeclContext *staticContext,
        SmallVector<NominalTypeDecl *, 2> selfBounds,
        NLOptions baseNLOptions);

  public:
    SWIFT_DEBUG_DUMP;
    SWIFT_DEBUG_DUMPER(dumpResults());
    SWIFT_DEBUG_DUMPER(dumpScopes());

    void printScopes(raw_ostream &OS) const;
    void print(raw_ostream &OS) const;
    void printResults(raw_ostream &OS) const;

#ifndef NDEBUG
    bool isTargetLookup() const;
    void stopForDebuggingIfStartingTargetLookup(bool isASTScopeLookup) const;
    void stopForDebuggingIfDuringTargetLookup(bool isASTScopeLookup) const;
    void
    stopForDebuggingIfAddingTargetLookupResult(const LookupResultEntry &) const;
    void addedResult(const LookupResultEntry &) const;
#endif
  };

} // namespace

namespace {
  /// Used to gather lookup results
class ASTScopeDeclConsumerForUnqualifiedLookup
    : public AbstractASTScopeDeclConsumer {
private:
  UnqualifiedLookupFactory &factory;

  /// The 'self' parameter from the innermost scope containing the lookup
  /// location to be used when an instance member of a type is accessed,
  /// or \c nullptr if instance members should not be 'self' qualified.
  ///
  // FIXME: This field is currently reset to \c nullptr by `lookInMembers` as
  // part of the lookup traversal of scopes. If, instead, consumers were
  // created at each point in the traversal, this field would no longer need
  // to be marked \c mutable.
  mutable const DeclContext *candidateSelfDC;

  void maybeUpdateSelfDC(VarDecl *var);

public:
  ASTScopeDeclConsumerForUnqualifiedLookup(UnqualifiedLookupFactory &factory)
      : factory(factory), candidateSelfDC(nullptr) {}

  virtual ~ASTScopeDeclConsumerForUnqualifiedLookup() = default;

  bool consume(ArrayRef<ValueDecl *> values,
               NullablePtr<DeclContext> baseDC = nullptr) override;

  bool consumePossiblyNotInScope(ArrayRef<VarDecl *> vars) override;

  bool lookInMembers(const DeclContext *) const override;

#ifndef NDEBUG
  void startingNextLookupStep() override {
    factory.stopForDebuggingIfDuringTargetLookup(true);
  }
  bool isTargetLookup() const override { return factory.isTargetLookup(); }

  void finishingLookup(std::string msg) const override {
    if (isTargetLookup())
      llvm::errs() << "Finishing lookup: " << msg << "\n";
  }
#endif
  };
} // namespace

#pragma mark UnqualifiedLookupFactory functions

// clang-format off
UnqualifiedLookupFactory::UnqualifiedLookupFactory(
                            DeclNameRef Name,
                            DeclContext *const DC,
                            SourceLoc Loc,
                            Options options,
                            SmallVectorImpl<LookupResultEntry> &Results,
                            size_t &IndexOfFirstOuterResult)
:
  Name(Name),
  DC(DC),
  M(*DC->getParentModule()),
  Ctx(M.getASTContext()),
  Loc(Loc),
  DebugClient(M.getDebugClient()),
  options(options),
  isOriginallyTypeLookup(options.contains(Flags::TypeLookup)),
  isOriginallyMacroLookup(options.contains(Flags::MacroLookup)),
  baseNLOptions(
      computeBaseNLOptions(
        options, isOriginallyTypeLookup, isOriginallyMacroLookup)),
  Results(Results),
  IndexOfFirstOuterResult(IndexOfFirstOuterResult)
{}
// clang-format on

void UnqualifiedLookupFactory::performUnqualifiedLookup() {
#ifndef NDEBUG
  ++lookupCounter;
  auto localCounter = lookupCounter;
  (void)localCounter; // for debugging
#endif

  if (options.contains(UnqualifiedLookupFlags::ModuleLookup)) {
    lookForAModuleWithTheGivenName(DC->getModuleScopeContext());
    recordCompletionOfAScope();
    return;
  }

  if (Loc.isValid() && DC->getParentSourceFile()) {
    // Operator lookup is always global, for the time being.
    if (!Name.isOperator())
      lookInASTScopes();
  } else {
    assert((DC->isModuleScopeContext() || !DC->getParentSourceFile()) &&
           "Unqualified lookup without a source location must start from "
           "a module-scope context");

#ifndef NDEBUG
    stopForDebuggingIfStartingTargetLookup(false);
#endif
  }

  recordCompletionOfAScope();
  if (!isFirstResultEnough()) {
    // If no result has been found yet, the dependency must be on a top-level
    // name, since up to now, the search has been for non-top-level names.
    auto *moduleScopeContext = getModuleScopeLookupContext(DC);

    lookUpTopLevelNamesInModuleScopeContext(moduleScopeContext);
  }
}

void UnqualifiedLookupFactory::lookUpTopLevelNamesInModuleScopeContext(
    const DeclContext *DC) {
  // TODO: Does the debugger client care about compound names?
  if (Name.isSimpleName() && !Name.isSpecial() && DebugClient &&
      DebugClient->lookupOverrides(Name.getBaseName(),
                                   const_cast<DeclContext *>(DC), Loc,
                                   isOriginallyTypeLookup, Results)) {
    return;
  }

  addImportedResults(DC);
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

#pragma mark context-based lookup definitions

ValueDecl *
UnqualifiedLookupFactory::getBaseDeclForResult(const DeclContext *baseDC) const {
  if (baseDC == nullptr) {
    return nullptr;
  }

  if (auto localBaseDecl = lookupBaseDecl(baseDC)) {
    return localBaseDecl;
  }

  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(baseDC)) {
    return const_cast<ParamDecl *>(AFD->getImplicitSelfDecl());
  }

  if (auto *PBI = dyn_cast<PatternBindingInitializer>(baseDC)) {
    auto *selfDecl = PBI->getImplicitSelfDecl();
    assert(selfDecl);
    return selfDecl;
  }

  else if (auto *CE = dyn_cast<ClosureExpr>(baseDC)) {
    auto *selfDecl = CE->getCapturedSelfDecl();
    assert(selfDecl);
    assert(selfDecl->isSelfParamCapture());
    return selfDecl;
  }

  auto *nominalDecl = baseDC->getSelfNominalTypeDecl();
  assert(nominalDecl);
  return nominalDecl;
}

/// Whether or not the given self decl is defined in an optional
/// unwrapping condition (e.g. `guard let self else { return }`).
/// If this is true, then we know any implicit self reference in the
/// following scope is guaranteed to be non-optional.
static bool implicitSelfReferenceIsUnwrapped(const ValueDecl *selfDecl) {
  ASTContext &Ctx = selfDecl->getASTContext();

  // Check if the implicit self decl refers to a var in a conditional stmt
  LabeledConditionalStmt *conditionalStmt = nullptr;
  if (auto var = dyn_cast<VarDecl>(selfDecl)) {
    if (auto parentStmt = var->getParentPatternStmt()) {
      conditionalStmt = dyn_cast<LabeledConditionalStmt>(parentStmt);
    }
  }

  if (!conditionalStmt) {
    return false;
  }

  return conditionalStmt->rebindsSelf(Ctx);
}

ValueDecl *UnqualifiedLookupFactory::lookupBaseDecl(const DeclContext *baseDC) const {
  // If the member was not in local context, we're not going to need the
  // 'self' declaration, so skip all this work to avoid request cycles.
  if (!baseDC->isLocalContext())
    return nullptr;

  // If we're only interested in type declarations, we can also skip everything.
  if (isOriginallyTypeLookup)
    return nullptr;

  // Perform an unqualified lookup for the base decl of this result. This
  // handles cases where self was rebound (e.g. `guard let self = self`)
  // earlier in this closure or some outer closure.
  auto closureExpr = DC->getInnermostClosureForCaptures();
  if (!closureExpr) {
    return nullptr;
  }

  bool capturesSelfWeakly = false;
  if (auto decl = closureExpr->getCapturedSelfDecl()) {
    if (auto a = decl->getAttrs().getAttribute<ReferenceOwnershipAttr>()) {
      capturesSelfWeakly = a->get() == ReferenceOwnership::Weak;
    }
  }

  // Previously we didn't perform the lookup of 'self' for anything outside
  // of a '[weak self]' closure, maintain that behavior until Swift 6 mode.
  if (!Ctx.LangOpts.isSwiftVersionAtLeast(6) && !capturesSelfWeakly)
    return nullptr;

  auto selfDecl = ASTScope::lookupSingleLocalDecl(DC->getParentSourceFile(),
                                                  DeclName(Ctx.Id_self), Loc);
  if (!selfDecl) {
    return nullptr;
  }

  // In Swift 5 mode, implicit self is allowed within non-escaping
  // `weak self` closures even before self is unwrapped.
  // For example, this is allowed:
  //
  //   doVoidStuffNonEscaping { [weak self] in
  //     method() // implicitly `self.method()`
  //   }
  //
  // To support this, we have to preserve the lookup behavior from
  // Swift 5.7 and earlier where implicit self defaults to the closure's
  // `ParamDecl`. This causes the closure to capture self strongly,
  // which is not acceptable for escaping closures.
  //
  // Escaping closures, however, only need to permit implicit self once
  // it has been unwrapped to be non-optional:
  //
  //   doVoidStuffEscaping { [weak self] in
  //     guard let self else { return }
  //     method()
  //   }
  //
  // In these cases, using the Swift 6 lookup behavior doesn't affect
  // how the body is type-checked, so it can be used in Swift 5 mode
  // without breaking source compatibility for non-escaping closures.
  if (!Ctx.LangOpts.isSwiftVersionAtLeast(6) &&
      !implicitSelfReferenceIsUnwrapped(selfDecl)) {
    return nullptr;
  }

  // Closures are only allowed to rebind self in specific circumstances:
  //  1. In a capture list with an explicit capture.
  //  2. In a `guard let self = self` / `if let self = self` condition
  //     in a closure that captures self weakly.
  //
  // These rebindings can be done by any parent closure, and apply
  // to all nested closures. We only need to check these structural
  // requirements loosely here -- more extensive validation happens
  // later in MiscDiagnostics::diagnoseImplicitSelfUseInClosure.
  //
  // Other types of rebindings, like an arbitrary "let `self` = foo",
  // are never allowed to rebind self.
  auto selfVD = dyn_cast<VarDecl>(selfDecl);
  if (!selfVD) {
    return nullptr;
  }

  if (!(selfVD->isCaptureList() || selfVD->getParentPatternStmt())) {
    return nullptr;
  }

  return selfDecl;
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
    return result.getValueDecl()->isUnavailableInCurrentSwiftVersion();
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

void UnqualifiedLookupFactory::addImportedResults(const DeclContext *const dc) {
  using namespace namelookup;
  SmallVector<ValueDecl *, 8> CurModuleResults;
  auto resolutionKind = isOriginallyTypeLookup ? ResolutionKind::TypesOnly
                      : isOriginallyMacroLookup ? ResolutionKind::MacrosOnly
                      : ResolutionKind::Overloadable;
  auto nlOptions = NL_UnqualifiedDefault;
  if (options.contains(Flags::IncludeUsableFromInline))
    nlOptions |= NL_IncludeUsableFromInline;
  if (options.contains(Flags::ExcludeMacroExpansions))
    nlOptions |= NL_ExcludeMacroExpansions;
  lookupInModule(dc, Name.getFullName(), CurModuleResults,
                 NLKind::UnqualifiedLookup, resolutionKind, dc,
                 Loc, nlOptions);

  // Always perform name shadowing for type lookup.
  if (options.contains(Flags::TypeLookup)) {
    removeShadowedDecls(CurModuleResults, dc);
  }

  for (auto VD : CurModuleResults) {
    Results.push_back(LookupResultEntry(VD));
#ifndef NDEBUG
    addedResult(Results.back());
#endif
  }

  filterForDiscriminator(Results, DebugClient);
}

void UnqualifiedLookupFactory::addNamesKnownToDebugClient(
    const DeclContext *dc) {
  if (Name.isSimpleName() && DebugClient)
    DebugClient->lookupAdditions(Name.getBaseName(),
                                 const_cast<DeclContext *>(dc), Loc,
                                 isOriginallyTypeLookup, Results);
}

void UnqualifiedLookupFactory::addUnavailableInnerResults() {
  Results = std::move(UnavailableInnerResults);
}

void UnqualifiedLookupFactory::lookForAModuleWithTheGivenName(
    const DeclContext *const dc) {
  using namespace namelookup;
  if (!Name.isSimpleName() || Name.isSpecial())
    return;

  // Look for a module with the given name.
  if (Name.isSimpleName(M.getName())) {
    Results.push_back(LookupResultEntry(&M));
#ifndef NDEBUG
    addedResult(Results.back());
#endif
    return;
  }

  ModuleDecl *desiredModule = nullptr;
  auto givenName = Name.getBaseIdentifier();
  // Check if the given name appearing in the source file is a module
  // real name or alias; for example, if `-module-alias Foo=Bar` was
  // passed, the alias 'Foo' should appear in source files, not 'Bar'.
  // If the real name 'Bar' was used, looking up getRealModuleName with
  // the real name 'Bar' and realNameFromAlias option should return
  // an empty Identifier.
  if (!Ctx.getRealModuleName(givenName, ASTContext::ModuleAliasLookupOption::realNameFromAlias).empty()) {
    // Only load the module if the lookup value is not empty, i.e. given
    // name is a module alias, not a real module name.
    desiredModule = Ctx.getLoadedModule(givenName);
  }

  if (!desiredModule && Name.getFullName() == Ctx.TheBuiltinModule->getName())
    desiredModule = Ctx.TheBuiltinModule;
  if (desiredModule) {
    // Make sure the desired module is actually visible from the current
    // context.
    if (Ctx.getImportCache().isImportedBy(desiredModule, dc)) {
      Results.push_back(LookupResultEntry(desiredModule));
#ifndef NDEBUG
      addedResult(Results.back());
#endif
    }
  }
}

#pragma mark common helper definitions

void UnqualifiedLookupFactory::findResultsAndSaveUnavailables(
    const DeclContext *dynamicContext,
    const DeclContext *staticContext,
    SmallVector<NominalTypeDecl *, 2> selfBounds,
    NLOptions baseNLOptions) {
  if (selfBounds.empty())
    return;

  auto firstPossiblyUnavailableResult = Results.size();
  SmallVector<ValueDecl *, 4> Lookup;
  staticContext->lookupQualified(selfBounds, Name, Loc, baseNLOptions, Lookup);
  for (auto Result : Lookup) {
    auto baseDC = isa<TypeDecl>(Result) ? staticContext : dynamicContext;
    auto baseDecl = getBaseDeclForResult(baseDC);
    Results.emplace_back(const_cast<DeclContext *>(baseDC), baseDecl, Result);
#ifndef NDEBUG
    addedResult(Results.back());
#endif
  }

  setAsideUnavailableResults(firstPossiblyUnavailableResult);
}

NLOptions UnqualifiedLookupFactory::computeBaseNLOptions(
    const UnqualifiedLookupOptions options,
    const bool isOriginallyTypeLookup,
    const bool isOriginallyMacroLookup) {
  NLOptions baseNLOptions = NL_UnqualifiedDefault;
  if (options.contains(Flags::AllowProtocolMembers))
    baseNLOptions |= NL_ProtocolMembers;
  if (isOriginallyTypeLookup)
    baseNLOptions |= NL_OnlyTypes;
  if (isOriginallyMacroLookup)
    baseNLOptions |= NL_OnlyMacros;
  if (options.contains(Flags::IgnoreAccessControl))
    baseNLOptions |= NL_IgnoreAccessControl;
  if (options.contains(Flags::IgnoreMissingImports))
    baseNLOptions |= NL_IgnoreMissingImports;
  return baseNLOptions;
}

bool UnqualifiedLookupFactory::isFirstResultEnough() const {
  return !Results.empty() && !options.contains(Flags::IncludeOuterResults);
}

bool UnqualifiedLookupFactory::hasPreciseScopingOfVarDecls() const {
  return !options.contains(Flags::IncludeOuterResults);
}

void UnqualifiedLookupFactory::recordCompletionOfAScope() {
  // OK to call (NOOP) if there are more inner results and Results is empty
  if (IndexOfFirstOuterResult == 0)
    IndexOfFirstOuterResult = Results.size();
}

#pragma mark ASTScopeImpl support

void UnqualifiedLookupFactory::lookInASTScopes() {

  ASTScopeDeclConsumerForUnqualifiedLookup consumer(*this);

#ifndef NDEBUG
  stopForDebuggingIfStartingTargetLookup(true);
#endif

  ASTScope::unqualifiedLookup(DC->getParentSourceFile(), Loc, consumer);
}

void ASTScopeDeclConsumerForUnqualifiedLookup::maybeUpdateSelfDC(VarDecl *var) {
  // We have a binding named 'self'.
  //
  // There are three possibilities:
  //
  // 1) This binding is the 'self' parameter of a method,
  // 2) This binding is a bona-fide 'self' capture, meaning a capture
  //    list entry named 'self' with initial value expression 'self',
  // 3) None of the above.
  //
  // How we handle these cases depends on whether we've already seen
  // another 'self' binding.
  if (candidateSelfDC == nullptr) {
    // We haven't seen one yet, so record it.
    if (var->isSelfParameter())
      candidateSelfDC = var->getDeclContext();
    else if (var->isSelfParamCapture())
      candidateSelfDC = var->getParentCaptureList()->getClosureBody();
  } else {
    // If we see a binding named 'self' that is not a bona-fide
    // 'self', we have to forget about the previous 'self' capture
    // because it's not going to be the right one for accessing
    // instance members of the innermost nominal type. Eg,
    //
    // class C {
    //   func bar() {}
    //   func foo() {
    //     _ { [self=12] { [self] bar() } }
    //   }
    // }
    //
    // Instead, we're going to move on and look for the next-innermost
    // 'self' binding.
    if (!var->isSelfParameter() &&
        !var->isSelfParamCapture())
      candidateSelfDC = nullptr;
  }
}

bool ASTScopeDeclConsumerForUnqualifiedLookup::consume(
    ArrayRef<ValueDecl *> values, NullablePtr<DeclContext> baseDC) {
  for (auto *value: values) {
    if (factory.isOriginallyTypeLookup && !isa<TypeDecl>(value))
      continue;

    if (auto *var = dyn_cast<VarDecl>(value)) {
      // Try to resolve the base for unqualified instance member
      // references. This is used by lookInMembers().
      if (var->getName() == factory.Ctx.Id_self) {
        maybeUpdateSelfDC(var);
      }

      // Local VarDecls with a pattern binding are visited as part of their
      // BraceStmt when hasPreciseScopingOfVarDecls() is off.
      if (var->getParentPatternBinding() &&
          !factory.hasPreciseScopingOfVarDecls())
        continue;
    }

    auto fullName = factory.Name.getFullName();
    if (!value->getName().matchesRef(fullName)) {
      bool foundMatch = false;
      if (auto *varDecl = dyn_cast<VarDecl>(value)) {
        // Check if the name matches any auxiliary decls not in the AST
        varDecl->visitAuxiliaryDecls([&](VarDecl *auxiliaryVar) {
          if (auxiliaryVar->ValueDecl::getName().matchesRef(fullName)) {
            value = auxiliaryVar;
            foundMatch = true;
          }
        });
      }

      if (!foundMatch)
        continue;
    }

    factory.Results.push_back(LookupResultEntry(value));
#ifndef NDEBUG
    factory.stopForDebuggingIfAddingTargetLookupResult(factory.Results.back());
#endif
  }
  factory.recordCompletionOfAScope();
  return factory.isFirstResultEnough();
}

bool ASTScopeDeclConsumerForUnqualifiedLookup::consumePossiblyNotInScope(
    ArrayRef<VarDecl *> vars) {
  if (factory.hasPreciseScopingOfVarDecls())
    return false;

  for (auto *var : vars) {
    if (!factory.Name.getFullName().isSimpleName(var->getName()))
      continue;

    factory.Results.push_back(LookupResultEntry(var));
  }

  return false;
}

bool ASTScopeDeclGatherer::consume(ArrayRef<ValueDecl *> valuesArg,
                                   NullablePtr<DeclContext>) {
  for (auto *v: valuesArg)
    values.push_back(v);
  return false;
}

// TODO: in future, migrate this functionality into ASTScopes
bool ASTScopeDeclConsumerForUnqualifiedLookup::lookInMembers(
    const DeclContext *scopeDC) const {
  auto nominal = scopeDC->getSelfNominalTypeDecl();
  if (!nominal)
    return false;

  SmallVector<NominalTypeDecl *, 2> selfBounds;
  selfBounds.push_back(nominal);

  // For a protocol extension, check whether there are additional "Self"
  // constraints that can affect name lookup.
  if (!factory.options.contains(UnqualifiedLookupFlags::DisregardSelfBounds)) {
    if (scopeDC->getExtendedProtocolDecl()) {
      auto ext = cast<ExtensionDecl>(scopeDC);
      auto bounds = getSelfBoundsFromWhereClause(ext);
      for (auto bound : bounds.decls)
        selfBounds.push_back(bound);
    }
  }

  // We're looking for members of a type.
  //
  // If we started the looking from inside a scope where a 'self' parameter
  // is visible, instance members are returned with the 'self' parameter's
  // DeclContext as the base, which is how the expression checker knows to
  // convert the unqualified reference into a self member access.
  factory.findResultsAndSaveUnavailables(candidateSelfDC ? candidateSelfDC : scopeDC,
                                         scopeDC, selfBounds, factory.baseNLOptions);
  factory.recordCompletionOfAScope();

  // We're done looking inside a nominal type declaration. It is possible
  // that this nominal type is nested inside of another type, in which case
  // we will visit the outer type next. Make sure to clear out the known
  // 'self' parameter context, since any members of the outer type are
  // not accessed via the innermost 'self' parameter.
  candidateSelfDC = nullptr;

  return factory.isFirstResultEnough();
}

LookupResult
UnqualifiedLookupRequest::evaluate(Evaluator &evaluator,
                                   UnqualifiedLookupDescriptor desc) const {
  SmallVector<LookupResultEntry, 4> results;
  size_t indexOfFirstOuterResult = 0;
  UnqualifiedLookupFactory factory(desc.Name, desc.DC, desc.Loc, desc.Options,
                                   results, indexOfFirstOuterResult);
  factory.performUnqualifiedLookup();
  return LookupResult(results, indexOfFirstOuterResult);
}

#pragma mark debugging

void UnqualifiedLookupFactory::dump() const { print(llvm::errs()); }
void UnqualifiedLookupFactory::dumpScopes() const { printScopes(llvm::errs()); }
void UnqualifiedLookupFactory::dumpResults() const {
  printResults(llvm::errs());
}

void UnqualifiedLookupFactory::printScopes(raw_ostream &out) const {
  out << "\n\nScopes:\n";
  DC->getParentSourceFile()->getScope().print(out);
  out << "\n";
}

void UnqualifiedLookupFactory::printResults(raw_ostream &out) const {
  for (auto i : indices(Results)) {
    out << i << ": ";
    Results[i].print(out);
    out << "\n";
  }
}

void UnqualifiedLookupFactory::print(raw_ostream &OS) const {
  OS << "Look up";
#ifndef NDEBUG
  OS << " (" << lookupCounter << ")";
#endif
  OS << " '" << Name << "' at: ";
  Loc.print(OS, DC->getASTContext().SourceMgr);
  OS << "\nStarting in: ";
  DC->printContext(OS);
  OS << "\n";
}

#pragma mark breakpointing
#ifndef NDEBUG

bool UnqualifiedLookupFactory::isTargetLookup() const {
  return lookupCounter == targetLookup;
}

void UnqualifiedLookupFactory::stopForDebuggingIfStartingTargetLookup(
    const bool isASTScopeLookup) const {
  if (!isTargetLookup())
    return;
  if (isASTScopeLookup)
    llvm::errs() << "starting target ASTScopeImpl lookup\n";
  else
    llvm::errs() << "starting target context-based lookup\n";
}

void UnqualifiedLookupFactory::stopForDebuggingIfDuringTargetLookup(
    const bool isASTScopeLookup) const {
  if (!isTargetLookup())
    return;
  if (isASTScopeLookup)
    llvm::errs() << "during target ASTScopeImpl lookup\n";
  else
    llvm::errs() << "during target context-based lookup\n";
}

void UnqualifiedLookupFactory::stopForDebuggingIfAddingTargetLookupResult(
    const LookupResultEntry &e) const {
  if (!isTargetLookup())
    return;
  auto &out = llvm::errs();
  out << "\nresult for Target lookup:\n";
  e.print(out);
  out << "\n";
}

void UnqualifiedLookupFactory::addedResult(const LookupResultEntry &e) const {
  stopForDebuggingIfAddingTargetLookupResult(e);
}

unsigned UnqualifiedLookupFactory::lookupCounter = 0;

// set to ~0 when not debugging
const unsigned UnqualifiedLookupFactory::targetLookup = ~0;

#endif // NDEBUG

namespace {

class ASTScopeDeclConsumerForLocalLookup
    : public AbstractASTScopeDeclConsumer {
  DeclName name;
  bool stopAfterInnermostBraceStmt;
  SmallVectorImpl<ValueDecl *> &results;

public:
  ASTScopeDeclConsumerForLocalLookup(
      DeclName name, bool stopAfterInnermostBraceStmt,
      SmallVectorImpl<ValueDecl *> &results)
    : name(name), stopAfterInnermostBraceStmt(stopAfterInnermostBraceStmt),
      results(results) {}

  bool consume(ArrayRef<ValueDecl *> values,
               NullablePtr<DeclContext> baseDC) override {
    for (auto *value: values) {
      bool foundMatch = false;
      if (auto *varDecl = dyn_cast<VarDecl>(value)) {
        // Check if the name matches any auxiliary decls not in the AST
        varDecl->visitAuxiliaryDecls([&](VarDecl *auxiliaryVar) {
          if (name.isSimpleName(auxiliaryVar->getName())) {
            results.push_back(auxiliaryVar);
            foundMatch = true;
          }
        });
      }

      if (!foundMatch && value->getName().matchesRef(name))
        results.push_back(value);
    }

    return (!stopAfterInnermostBraceStmt && !results.empty());
  }

  bool lookInMembers(const DeclContext *) const override {
    return true;
  }

  bool finishLookupInBraceStmt(BraceStmt *stmt) override {
    return stopAfterInnermostBraceStmt;
  }

#ifndef NDEBUG
  void startingNextLookupStep() override {}
  void finishingLookup(std::string) const override {}
  bool isTargetLookup() const override { return false; }
#endif
};

}

/// Lookup that only finds local declarations and does not trigger
/// interface type computation.
void ASTScope::lookupLocalDecls(SourceFile *sf, DeclName name, SourceLoc loc,
                                bool stopAfterInnermostBraceStmt,
                                SmallVectorImpl<ValueDecl *> &results) {
  ASTScopeDeclConsumerForLocalLookup consumer(name, stopAfterInnermostBraceStmt,
                                              results);
  ASTScope::unqualifiedLookup(sf, loc, consumer);
}

ValueDecl *ASTScope::lookupSingleLocalDecl(SourceFile *sf, DeclName name,
                                           SourceLoc loc) {
  SmallVector<ValueDecl *, 1> result;
  ASTScope::lookupLocalDecls(sf, name, loc,
                             /*finishLookupInBraceStmt=*/false,
                             result);
  if (result.size() != 1)
    return nullptr;
  return result[0];
}

DeclContext *swift::getModuleScopeLookupContext(DeclContext *dc) {
  auto moduleScopeContext = dc->getModuleScopeContext();

  // When the module scope context is in a Clang module but we actually
  // have a parent source file, it's because we are within a macro
  // expansion triggered for the imported declaration. In such cases,
  // we want to use the parent source file as the lookup context, becauae
  // it has the appropriate resolved imports.
  if (isa<ClangModuleUnit>(moduleScopeContext)) {
    if (auto parentSF = dc->getParentSourceFile())
      return parentSF;
  }

  return moduleScopeContext;
}

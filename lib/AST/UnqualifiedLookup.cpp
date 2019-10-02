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
/// This file implements the construction of an UnqualifiedLookup, which entails
/// performing the lookup.
///
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/DebuggerClient.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/SourceFile.h"
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
    auto braces = decl->getBraces();
    if (braces.Start != braces.End &&
        ctx.SourceMgr.rangeContainsTokenLoc(braces, loc))
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
  /// Because UnqualifiedLookup does all of its work in the constructor,
  /// a factory class is needed to hold all of the inputs and outputs so
  /// that the construction code can be decomposed into bite-sized pieces.
  
  class UnqualifiedLookupFactory {

    friend class ASTScopeDeclConsumerForUnqualifiedLookup;

  public:
    using Flags = UnqualifiedLookup::Flags;
    using Options = UnqualifiedLookup::Options;
    using ResultsVector = UnqualifiedLookup::ResultsVector;
    
  private:
    struct ContextAndResolvedIsCascadingUse {
      DeclContext *const DC;
      const bool isCascadingUse;
    };
    
    /// Finds lookup results based on the types that self conforms to.
    /// For instance, self always conforms to a struct, enum or class.
    /// But in addition, self could conform to any number of protocols.
    /// For example, when there's a protocol extension, e.g. extension P where
    /// self: P2, self also conforms to P2 so P2 must be searched.
    class ResultFinderForTypeContext {
      UnqualifiedLookupFactory *const factory;
      /// Nontypes are formally members of the base type, i.e. the dynamic type
      /// of the activation record.
      DeclContext *const dynamicContext;
      /// Types are formally members of the metatype, i.e. the static type of the
      /// activation record.
      DeclContext *const staticContext;
      using SelfBounds = SmallVector<NominalTypeDecl *, 2>;
      SelfBounds selfBounds;
      
    public:
      /// \p staticContext is also the context from which to derive the self types
      ResultFinderForTypeContext(UnqualifiedLookupFactory *factory,
                                 DeclContext *dynamicContext,
                                 DeclContext *staticContext);
      
      void dump() const;
      
    private:
      SelfBounds findSelfBounds(DeclContext *dc);
      
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

#ifndef NDEBUG
    /// A consumer for debugging that lets the UnqualifiedLookupFactory know when
    /// finding something.
    class InstrumentedNamedDeclConsumer : public NamedDeclConsumer {
      virtual void anchor() override;
      UnqualifiedLookupFactory *factory;
      
    public:
      InstrumentedNamedDeclConsumer(UnqualifiedLookupFactory *factory,
                                    DeclName name,
                                    SmallVectorImpl<LookupResultEntry> &results,
                                    bool isTypeLookup)
      : NamedDeclConsumer(name, results, isTypeLookup), factory(factory) {}
      
      virtual void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason,
                             DynamicLookupInfo dynamicLookupInfo = {}) override {
        unsigned before = results.size();
        NamedDeclConsumer::foundDecl(VD, Reason, dynamicLookupInfo);
        unsigned after = results.size();
        if (after > before)
          factory->addedResult(results.back());
      }
    };
#endif
    // Inputs
    const DeclName Name;
    DeclContext *const DC;
    ModuleDecl &M;
    const ASTContext &Ctx;
    const SourceLoc Loc;
    const SourceManager &SM;
    
    /// Used to find the file-local names.
    DebuggerClient *const DebugClient;
    
    const Options options;
    const bool isOriginallyTypeLookup;
    const NLOptions baseNLOptions;
    // Transputs
#ifndef NDEBUG
    InstrumentedNamedDeclConsumer Consumer;
#else
    NamedDeclConsumer Consumer;
#endif
    // Outputs
    SmallVectorImpl<LookupResultEntry> &Results;
    size_t &IndexOfFirstOuterResult;
    ResultsVector UnavailableInnerResults;

#ifndef NDEBUG
    static unsigned lookupCounter;
    static const unsigned targetLookup;
#endif

  public: // for exp debugging
    SourceFile const *recordedSF = nullptr;
    bool recordedIsCascadingUse = false;
    unsigned resultsSizeBeforeLocalsPass = ~0;
    
  public:
    // clang-format off
    UnqualifiedLookupFactory(DeclName Name,
                             DeclContext *const DC,
                             SourceLoc Loc,
                             Options options,
                             UnqualifiedLookup &lookupToBeCreated);
    
    UnqualifiedLookupFactory(DeclName Name,
                             DeclContext *const DC,
                             SourceLoc Loc,
                             Options options,
                             SmallVectorImpl<LookupResultEntry> &Results,
                             size_t &IndexOfFirstOuterResult);
    // clang-format on
    
    void performUnqualifiedLookup();
    
  private:
    struct ContextAndUnresolvedIsCascadingUse {
      DeclContext *whereToLook;
      Optional<bool> isCascadingUse;
      ContextAndResolvedIsCascadingUse resolve(const bool resolution) const {
        return ContextAndResolvedIsCascadingUse{
          whereToLook, isCascadingUse.getValueOr(resolution)};
      }
    };

    bool useASTScopesForLookup() const;

    /// For testing, assume this lookup is enabled:
    bool useASTScopesForLookupIfEnabled() const;

    void lookUpTopLevelNamesInModuleScopeContext(DeclContext *);

    void lookInASTScopes();

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
    
#pragma mark context-based lookup declarations
    
    void lookupOperatorInDeclContexts(ContextAndUnresolvedIsCascadingUse);
    
    void lookupNamesIntroducedBy(const ContextAndUnresolvedIsCascadingUse);
    
    void finishLookingInContext(
                                AddGenericParameters addGenericParameters,
                                DeclContext *lookupContextForThisContext,
                                Optional<ResultFinderForTypeContext> &&resultFinderForTypeContext,
                                Optional<bool> isCascadingUse);
    
    void lookupInModuleScopeContext(DeclContext *, Optional<bool> isCascadingUse);
    
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
    
    void addGenericParametersForContext(DeclContext *dc);
    void addGenericParametersForContext(GenericParamList *);
    
    /// Consume generic parameters
    void addGenericParametersForFunction(AbstractFunctionDecl *AFD);
    
    static GenericParamList *getGenericParams(const DeclContext *const dc);
    
    /// For diagnostic purposes, move aside the unavailables, and put
    /// them back as a last-ditch effort.
    /// Could be cleaner someday with a richer interface to UnqualifiedLookup.
    void setAsideUnavailableResults(size_t firstPossiblyUnavailableResult);
    
    void recordDependencyOnTopLevelName(DeclContext *topLevelContext,
                                        DeclName name, bool isCascadingUse);
    
    void addImportedResults(DeclContext *const dc);
    
    void addNamesKnownToDebugClient(DeclContext *dc);
    
    void addUnavailableInnerResults();
    
    void lookForAModuleWithTheGivenName(DeclContext *const dc);
    
#pragma mark common helper declarations
    static NLOptions
    computeBaseNLOptions(const UnqualifiedLookup::Options options,
                         const bool isOriginallyTypeLookup);

    Optional<bool> getInitialIsCascadingUse() const {
      return options.contains(Flags::KnownPrivate) ? Optional<bool>(false)
                                                   : None;
    }

    static bool resolveIsCascadingUse(const DeclContext *const dc,
                                      Optional<bool> isCascadingUse,
                                      bool onlyCareAboutFunctionBody) {
      return isCascadingUse.getValueOr(dc->isCascadingContextForLookup(
                                                                       /*functionsAreNonCascading=*/onlyCareAboutFunctionBody));
    }
    
    static bool resolveIsCascadingUse(ContextAndUnresolvedIsCascadingUse x,
                                      bool onlyCareAboutFunctionBody) {
      return resolveIsCascadingUse(x.whereToLook, x.isCascadingUse,
                                   onlyCareAboutFunctionBody);
    }
    
    void findResultsAndSaveUnavailables(
                                        DeclContext *lookupContextForThisContext,
                                        ResultFinderForTypeContext &&resultFinderForTypeContext,
                                        bool isCascadingUse, NLOptions baseNLOptions);
    
  public:
    void dump() const;
    void dumpScopes() const;
    void print(raw_ostream &OS) const;
    
    void dumpResults() const;
    
    bool verifyEqualTo(const UnqualifiedLookupFactory &&, const char *thisLabel,
                       const char *otherLabel) const;
    
    /// Legacy lookup is wrong here; we should NOT find this symbol.
    bool shouldDiffer() const;
    StringRef getSourceFileName() const;

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
  UnqualifiedLookupFactory &factory;

public:
  ASTScopeDeclConsumerForUnqualifiedLookup(UnqualifiedLookupFactory &factory)
      : factory(factory) {}

  virtual ~ASTScopeDeclConsumerForUnqualifiedLookup() = default;

  bool consume(ArrayRef<ValueDecl *> values, DeclVisibilityKind vis,
               NullablePtr<DeclContext> baseDC = nullptr) override;

  /// returns true if finished and new value for isCascadingUse
  bool lookInMembers(NullablePtr<DeclContext> selfDC,
                     DeclContext *const scopeDC, NominalTypeDecl *const nominal,
                     function_ref<bool(Optional<bool>)>) override;

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
                            DeclName Name,
                            DeclContext *const DC,
                            SourceLoc Loc,
                            Options options,
                            UnqualifiedLookup &lookupToBeCreated)
: UnqualifiedLookupFactory(Name, DC, Loc, options,
    lookupToBeCreated.Results,
    lookupToBeCreated.IndexOfFirstOuterResult)

{}

UnqualifiedLookupFactory::UnqualifiedLookupFactory(
                            DeclName Name,
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
  SM(Ctx.SourceMgr),
  DebugClient(M.getDebugClient()),
  options(options),
  isOriginallyTypeLookup(options.contains(Flags::TypeLookup)),
  baseNLOptions(computeBaseNLOptions(options, isOriginallyTypeLookup)),
  #ifdef NDEBUG
  Consumer(Name, Results, isOriginallyTypeLookup),
  #else
  Consumer(this, Name, Results, isOriginallyTypeLookup),
  #endif
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
  FrontendStatsTracer StatsTracer(Ctx.Stats, "performUnqualifedLookup",
                                  DC->getParentSourceFile());

  const Optional<bool> initialIsCascadingUse = getInitialIsCascadingUse();

  ContextAndUnresolvedIsCascadingUse contextAndIsCascadingUse{
      DC, initialIsCascadingUse};
  const bool crosscheckUnqualifiedLookup =
      Ctx.LangOpts.CrosscheckUnqualifiedLookup;
  if (useASTScopesForLookup()) {
    static bool haveWarned = false;
    if (!haveWarned && Ctx.LangOpts.WarnIfASTScopeLookup) {
      haveWarned = true;
      llvm::errs() << "WARNING: TRYING Scope exclusively\n";
    }
    lookInASTScopes();
  } else {
#ifndef NDEBUG
    stopForDebuggingIfStartingTargetLookup(false);
#endif

    if (Name.isOperator())
      lookupOperatorInDeclContexts(contextAndIsCascadingUse);
    else
      lookupNamesIntroducedBy(contextAndIsCascadingUse);
  }

  if (crosscheckUnqualifiedLookup && useASTScopesForLookupIfEnabled()) {
    ResultsVector results;
    size_t indexOfFirstOuterResult = 0;
    UnqualifiedLookupFactory altLookup(Name, DC, Loc, options, results,
                                       indexOfFirstOuterResult);
    if (!useASTScopesForLookup())
      altLookup.lookInASTScopes();
    else if (Name.isOperator())
      altLookup.lookupOperatorInDeclContexts(contextAndIsCascadingUse);
    else
      altLookup.lookupNamesIntroducedBy(contextAndIsCascadingUse);

    assert(
        verifyEqualTo(std::move(altLookup), "main lookup", "alternate lookup"));
  }
}

void UnqualifiedLookupFactory::lookUpTopLevelNamesInModuleScopeContext(
    DeclContext *DC) {
  // TODO: Does the debugger client care about compound names?
  if (Name.isSimpleName() && !Name.isSpecial() && DebugClient &&
      DebugClient->lookupOverrides(Name.getBaseName(), DC, Loc,
                                   isOriginallyTypeLookup, Results))
    return;

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

bool UnqualifiedLookupFactory::useASTScopesForLookup() const {
  return Ctx.LangOpts.EnableASTScopeLookup && useASTScopesForLookupIfEnabled();
}

bool UnqualifiedLookupFactory::useASTScopesForLookupIfEnabled() const {
  if (!Loc.isValid())
    return false;
  const auto *const SF = DC->getParentSourceFile();
  return SF && SF->isSuitableForASTScopes();
}

#pragma mark context-based lookup definitions

void UnqualifiedLookupFactory::lookupOperatorInDeclContexts(
    const ContextAndUnresolvedIsCascadingUse contextAndUseArg) {
  ContextAndResolvedIsCascadingUse contextAndResolvedIsCascadingUse{
      // Operators are global
      contextAndUseArg.whereToLook->getModuleScopeContext(),
      resolveIsCascadingUse(contextAndUseArg,
                            /*onlyCareAboutFunctionBody*/ true)};
  lookupInModuleScopeContext(contextAndResolvedIsCascadingUse.DC,
                             contextAndResolvedIsCascadingUse.isCascadingUse);
}

// TODO: Unify with LookupVisibleDecls.cpp::lookupVisibleDeclsImpl
void UnqualifiedLookupFactory::lookupNamesIntroducedBy(
    const ContextAndUnresolvedIsCascadingUse contextAndIsCascadingUseArg) {
#ifndef NDEBUG
  stopForDebuggingIfDuringTargetLookup(false);
#endif
  DeclContext *const dc = contextAndIsCascadingUseArg.whereToLook;
  const auto isCascadingUseSoFar = contextAndIsCascadingUseArg.isCascadingUse;
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

void UnqualifiedLookupFactory::lookupInModuleScopeContext(
    DeclContext *dc, Optional<bool> isCascadingUse) {
  if (auto SF = dyn_cast<SourceFile>(dc)) {
    resultsSizeBeforeLocalsPass = Results.size();
    lookForLocalVariablesIn(SF);
  }
  ifNotDoneYet([&] {
    // If no result has been found yet, the dependency must be on a top-level
    // name, since up to now, the search has been for non-top-level names.
    recordDependencyOnTopLevelName(dc, Name, isCascadingUse.getValueOr(true));
    lookUpTopLevelNamesInModuleScopeContext(dc);
  });
}

void UnqualifiedLookupFactory::lookupNamesIntroducedByPatternBindingInitializer(
    PatternBindingInitializer *PBI, Optional<bool> isCascadingUse) {
  // Lazy variable initializer contexts have a 'self' parameter for
  // instance member lookup.
  if (auto *selfParam = PBI->getImplicitSelfDecl())
    lookupNamesIntroducedByLazyVariableInitializer(PBI, selfParam,
                                                   isCascadingUse);
  else if (PBI->getParent()->isTypeContext())
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
      ResultFinderForTypeContext(this, PBI, patternContainer),
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
    ResultFinderForTypeContext(
      this, storedPropertyContainer, storedPropertyContainer),
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
          Loc.isInvalid() || AFD->getBodySourceRange().isInvalid() ||
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
        ResultFinderForTypeContext(this, BaseDC, fnDeclContext),
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
    ? Optional<ResultFinderForTypeContext>(
                ResultFinderForTypeContext(this, D, D))
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


void UnqualifiedLookupFactory::finishLookingInContext(
       const AddGenericParameters addGenericParameters,
       DeclContext *const lookupContextForThisContext,
       Optional<ResultFinderForTypeContext> &&resultFinderForTypeContext,
       const Optional<bool> isCascadingUse) {
#ifndef NDEBUG
  stopForDebuggingIfDuringTargetLookup(false);
#endif
  // When a generic has the same name as a member, Swift prioritizes the generic
  // because the member could still be named by qualifying it. But there is no
  // corresponding way to qualify a generic parameter.
  // So, look for generics first.
  if (addGenericParameters == AddGenericParameters::Yes)
    addGenericParametersForContext(lookupContextForThisContext);
  
  ifNotDoneYet(
    [&] {
      if (resultFinderForTypeContext)
        findResultsAndSaveUnavailables(lookupContextForThisContext,
                                      std::move(*resultFinderForTypeContext),
                                      *isCascadingUse, baseNLOptions);
    },
    // Recurse into the next context.
    [&] {
      lookupNamesIntroducedBy(ContextAndUnresolvedIsCascadingUse{
        lookupContextForThisContext->getParentForLookup(), isCascadingUse});
    });
}


void UnqualifiedLookupFactory::lookForLocalVariablesIn(
    AbstractFunctionDecl *AFD, Optional<bool> isCascadingUse) {
  // Look for local variables; normally, the parser resolves these
  // for us, but it can't do the right thing inside local types.
  // FIXME: when we can parse and typecheck the function body partially
  // for code completion, AFD->getBody() check can be removed.

  if (Loc.isInvalid() || AFD->getBodySourceRange().isInvalid() ||
      !SM.rangeContainsTokenLoc(AFD->getBodySourceRange(), Loc) ||
      !AFD->getBody()) {
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

void UnqualifiedLookupFactory::addGenericParametersForContext(
    DeclContext *dc) {
  // Generics can be nested, so visit the generic list, innermost first.
  // Cannot use DeclContext::forEachGenericContext because this code breaks out
  // if it finds a match and isFirstResultEnough()
  addGenericParametersForContext(getGenericParams(dc));
}

void UnqualifiedLookupFactory::addGenericParametersForContext(
    GenericParamList *dcGenericParams) {
  if (!dcGenericParams)
    return;
  namelookup::FindLocalVal localVal(SM, Loc, Consumer);
  localVal.checkGenericParams(dcGenericParams);
  ifNotDoneYet([&] {
    addGenericParametersForContext(
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

void UnqualifiedLookupFactory::ResultFinderForTypeContext::findResults(
    const DeclName &Name, bool isCascadingUse, NLOptions baseNLOptions,
    DeclContext *contextForLookup,
    SmallVectorImpl<LookupResultEntry> &results) const {
  // An optimization:
  if (selfBounds.empty())
    return;
  const NLOptions options =
      baseNLOptions | (isCascadingUse ? NL_KnownCascadingDependency
                                      : NL_KnownNonCascadingDependency);

  SmallVector<ValueDecl *, 4> Lookup;
  contextForLookup->lookupQualified(selfBounds, Name, options, Lookup);
  for (auto Result : Lookup) {
    results.push_back(LookupResultEntry(whereValueIsMember(Result), Result));
#ifndef NDEBUG
    factory->addedResult(results.back());
#endif
  }
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
  recordedIsCascadingUse = isCascadingUse;
}

void UnqualifiedLookupFactory::addImportedResults(DeclContext *const dc) {
  using namespace namelookup;
  SmallVector<ValueDecl *, 8> CurModuleResults;
  auto resolutionKind = isOriginallyTypeLookup ? ResolutionKind::TypesOnly
                                               : ResolutionKind::Overloadable;
  lookupInModule(dc, Name, CurModuleResults, NLKind::UnqualifiedLookup,
                 resolutionKind, dc);

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
  ModuleDecl *desiredModule = Ctx.getLoadedModule(Name.getBaseIdentifier());
  if (!desiredModule && Name == Ctx.TheBuiltinModule->getName())
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
       DeclContext *lookupContextForThisContext,
       ResultFinderForTypeContext &&resultFinderForTypeContext,
       bool isCascadingUse, NLOptions baseNLOptions) {
  auto firstPossiblyUnavailableResult = Results.size();
  resultFinderForTypeContext.findResults(Name, isCascadingUse, baseNLOptions,
                                         lookupContextForThisContext, Results);
  setAsideUnavailableResults(firstPossiblyUnavailableResult);
}


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

UnqualifiedLookupFactory::ResultFinderForTypeContext::
    ResultFinderForTypeContext(UnqualifiedLookupFactory *factory,
                               DeclContext *dynamicContext,
                               DeclContext *staticContext)
    : factory(factory),
      dynamicContext(dynamicContext), staticContext(staticContext),
      selfBounds(findSelfBounds(staticContext)) {}

UnqualifiedLookupFactory::ResultFinderForTypeContext::SelfBounds
UnqualifiedLookupFactory::ResultFinderForTypeContext::findSelfBounds(
    DeclContext *dc) {
  auto nominal = dc->getSelfNominalTypeDecl();
  if (!nominal)
    return {};

  SelfBounds selfBounds;
  selfBounds.push_back(nominal);

  // For a protocol extension, check whether there are additional "Self"
  // constraints that can affect name lookup.
  if (dc->getExtendedProtocolDecl()) {
    auto ext = cast<ExtensionDecl>(dc);
    auto bounds = getSelfBoundsFromWhereClause(ext);
    for (auto bound : bounds.decls)
      selfBounds.push_back(bound);
  }
  return selfBounds;
}

#pragma mark ASTScopeImpl support

void UnqualifiedLookupFactory::lookInASTScopes() {

  ASTScopeDeclConsumerForUnqualifiedLookup consumer(*this);

#ifndef NDEBUG
  stopForDebuggingIfStartingTargetLookup(true);
#endif

  const auto history = ASTScope::unqualifiedLookup(DC->getParentSourceFile(),
                                                   Name, Loc, DC, consumer);

  ifNotDoneYet([&] {
    // Copied from lookupInModuleScopeContext
    // If no result has been found yet, the dependency must be on a top-level
    // name, since up to now, the search has been for non-top-level names.
    auto *const moduleScopeContext = DC->getParentSourceFile();

    const Optional<bool> isCascadingUseAtStartOfLookup =
        !Name.isOperator()
            ? getInitialIsCascadingUse()
            : resolveIsCascadingUse(DC, getInitialIsCascadingUse(),
                                    /*onlyCareAboutFunctionBody*/ true);

    const Optional<bool> isCascadingUseAfterLookup =
        ASTScope::computeIsCascadingUse(history, isCascadingUseAtStartOfLookup);

    recordDependencyOnTopLevelName(moduleScopeContext, Name,
                                   isCascadingUseAfterLookup.getValueOr(true));
    lookUpTopLevelNamesInModuleScopeContext(moduleScopeContext);
  });
}

bool ASTScopeDeclConsumerForUnqualifiedLookup::consume(
    ArrayRef<ValueDecl *> values, DeclVisibilityKind vis,
    NullablePtr<DeclContext> baseDC) {
  for (auto *value: values) {
    if (factory.isOriginallyTypeLookup && !isa<TypeDecl>(value))
      continue;
    if (!value->getFullName().matchesRef(factory.Name))
      continue;

    // In order to preserve the behavior of the existing context-based lookup,
    // which finds all results for non-local variables at the top level instead
    // of stopping at the first one, ignore results at the top level that are
    // not local variables. The caller \c lookInASTScopes will
    // then do the appropriate work when the scope lookup fails. In
    // FindLocalVal::visitBraceStmt, it sees PatternBindingDecls, not VarDecls,
    // so a VarDecl at top level would not be found by the context-based lookup.
    if (isa<SourceFile>(value->getDeclContext()) &&
        (vis != DeclVisibilityKind::LocalVariable || isa<VarDecl>(value)))
      return false;

    factory.Results.push_back(LookupResultEntry(value));
#ifndef NDEBUG
    factory.stopForDebuggingIfAddingTargetLookupResult(factory.Results.back());
#endif
  }
  factory.recordCompletionOfAScope();
  return factory.isFirstResultEnough();
}

bool ASTScopeDeclGatherer::consume(ArrayRef<ValueDecl *> valuesArg,
                                   DeclVisibilityKind,
                                   NullablePtr<DeclContext>) {
  for (auto *v: valuesArg)
    values.push_back(v);
  return false;
}

// TODO: in future, migrate this functionality into ASTScopes
bool ASTScopeDeclConsumerForUnqualifiedLookup::lookInMembers(
    NullablePtr<DeclContext> selfDC, DeclContext *const scopeDC,
    NominalTypeDecl *const nominal,
    function_ref<bool(Optional<bool>)> calculateIsCascadingUse) {
  if (selfDC) {
    if (auto *d = selfDC.get()->getAsDecl()) {
      if (auto *afd = dyn_cast<AbstractFunctionDecl>(d))
        assert(!factory.isOutsideBodyOfFunction(afd) && "Should be inside");
    }
  }
  auto resultFinder = UnqualifiedLookupFactory::ResultFinderForTypeContext(
      &factory, selfDC ? selfDC.get() : scopeDC, scopeDC);
  const bool isCascadingUse =
      calculateIsCascadingUse(factory.getInitialIsCascadingUse());
  factory.findResultsAndSaveUnavailables(scopeDC, std::move(resultFinder),
                                         isCascadingUse, factory.baseNLOptions);
  factory.recordCompletionOfAScope();
  return factory.isFirstResultEnough();
}


#pragma mark UnqualifiedLookup functions

// clang-format off
UnqualifiedLookup::UnqualifiedLookup(DeclName Name,
                                     DeclContext *const DC,
                                     SourceLoc Loc,
                                     Options options)
    // clang-format on
    : IndexOfFirstOuterResult(0) {

  auto *stats = DC->getASTContext().Stats;
  if (stats)
    stats->getFrontendCounters().NumUnqualifiedLookup++;

  UnqualifiedLookupFactory factory(Name, DC, Loc, options, *this);
  factory.performUnqualifiedLookup();
}

TypeDecl *UnqualifiedLookup::getSingleTypeResult() const {
  if (Results.size() != 1)
    return nullptr;
  return dyn_cast<TypeDecl>(Results.back().getValueDecl());
}

#pragma mark debugging
#ifndef NDEBUG
void UnqualifiedLookupFactory::InstrumentedNamedDeclConsumer::anchor() {}
#endif

void UnqualifiedLookupFactory::ResultFinderForTypeContext::dump() const {
  (void)factory;
  llvm::errs() << "dynamicContext: ";
  dynamicContext->dumpContext();
  llvm::errs() << "staticContext: ";
  staticContext->dumpContext();
  llvm::errs() << "selfBounds: ";
  for (const auto *D : selfBounds)
    D->dump(llvm::errs(), 1);
  llvm::errs() << "\n";
}

void UnqualifiedLookupFactory::dumpScopes() const {
  llvm::errs() << "\n\nScopes:\n";
  DC->getParentSourceFile()->getScope().print(llvm::errs());
  llvm::errs() << "\n";
}

void UnqualifiedLookupFactory::dump() const { print(llvm::errs()); }

void UnqualifiedLookupFactory::dumpResults() const {
  auto &out = llvm::errs();
  for (auto i : indices(Results)) {
    if (i == resultsSizeBeforeLocalsPass)
      out << "============== next pass ============\n";
    out << i << ": ";
    Results[i].print(out);
    out << "\n";
  }
  if (resultsSizeBeforeLocalsPass == Results.size())
    out << "============== next pass ============\n";
  if (resultsSizeBeforeLocalsPass == ~0u)
    out << "never tried locals\n\n";
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

#pragma mark debugging: output utilities for grepping

static void writeLine(std::string s) {
  llvm::errs() << "\n+-+-+-+-  " << s << "\n";
}

StringRef UnqualifiedLookupFactory::getSourceFileName() const {
  return DC->getParentSourceFile()->getFilename();
}

static void writeFirstLine(const UnqualifiedLookupFactory &ul, StringRef s) {
  std::string line =
      std::string("In file: ") + ul.getSourceFileName().str() + ", " + s.str();
  writeLine(line);
}

static void writeInconsistent(const UnqualifiedLookupFactory &me,
                              const char *thisLabel,
                              const UnqualifiedLookupFactory &other,
                              const char *otherLabel, StringRef s) {
  writeFirstLine(me, s);
  other.dump();
  llvm::errs() << "\n" << thisLabel << " Results:\n";
  me.dumpResults();
  llvm::errs() << "\n" << otherLabel << " Results:\n";
  other.dumpResults();
  me.dumpScopes();
}

#pragma mark comparing results

bool UnqualifiedLookupFactory::verifyEqualTo(
    const UnqualifiedLookupFactory &&other, const char *thisLabel,
    const char *otherLabel) const {
  if (shouldDiffer()) {
     return true;
  }
  auto writeErr = [&](StringRef s) {
    writeInconsistent(*this, thisLabel, other, otherLabel, s);
  };
  if (Results.size() != other.Results.size()) {
    const bool tooMany = Results.size() < other.Results.size();
    writeErr(std::string(tooMany ? "Found too many: " : "Found too few: ") +
             std::to_string(Results.size()) + " vs " +
             std::to_string(other.Results.size()));
    if (tooMany)
      assert(false && "ASTScopeImpl found too many");
    else
      assert(false && "ASTScopeImpl found too few");
  }
  for (size_t i : indices(Results)) {
    const auto &e = Results[i];
    const auto &oe = other.Results[i];
    if (e.getValueDecl() != oe.getValueDecl()) {
      // print_ast_tc_function_bodies.swift generic from subscript vs get fn
      std::string a; llvm::raw_string_ostream as(a);
      std::string b; llvm::raw_string_ostream bs(b);
      e.getValueDecl()->print(as);
      oe.getValueDecl()->print(bs);
      if (a == b)
        llvm::errs() << "ValueDecls differ but print same\n";
      else {
        writeErr(std::string( "ValueDecls differ at ") + std::to_string(i));
        assert(false && "ASTScopeImpl found different Decl");
      }
    }
    if (e.getDeclContext() != oe.getDeclContext()) {
      writeErr((std::string("Contexts differ at ")) + std::to_string(i));
      assert(false && "ASTScopeImpl found different context");
    }
    // unsigned printContext(llvm::raw_ostream &OS, unsigned indent = 0,
    // bool onlyAPartialLine = false) const;
  }
  if (IndexOfFirstOuterResult != other.IndexOfFirstOuterResult) {
    writeErr( std::string("IndexOfFirstOuterResult differs, should be: ")
             + std::to_string(IndexOfFirstOuterResult)
             + std::string( ", is: ")
             + std::to_string(other.IndexOfFirstOuterResult));
    assert(false && "ASTScopeImpl IndexOfFirstOuterResult differs");
  }
  if (recordedSF != other.recordedSF) {
    writeErr(std::string("recordedSF differs: shouldBe: ") +
             (recordedSF ? recordedSF->getFilename().str()
                         : std::string("<no name>")) +
             std::string(" is: ") +
             (other.recordedSF ? other.recordedSF->getFilename().str()
                               : std::string("<no name>")));
    assert(false && "ASTScopeImpl recordedSF differs");
  }
  if (recordedSF && recordedIsCascadingUse != other.recordedIsCascadingUse) {
    writeErr(std::string("recordedIsCascadingUse differs: shouldBe: ") +
             std::to_string(recordedIsCascadingUse) + std::string(" is: ") +
             std::to_string(other.recordedIsCascadingUse));
    assert(false && "ASTScopeImpl recordedIsCascadingUse differs");
  }
  return true;
}

bool UnqualifiedLookupFactory::shouldDiffer() const {
  auto *SF = dyn_cast<SourceFile>(DC->getModuleScopeContext());
  if (!SF)
    return false;
  
  static std::vector<const char*> testsThatShouldDiffer {
    "swift/test/Constraints/diagnostics.swift",
    "swift/test/Constraints/enum_cases.swift",
    "swift/test/Constraints/rdar39401774.swift",
    "swift/test/Constraints/rdar39401774-astscope.swift",
    "swift/test/Interpreter/repl.swift",
    "swift/test/Sema/diag_defer_captures.swift",
    "swift/test/Sema/diag_use_before_declaration.swift",
    "swift/test/SourceKit/CodeFormat/indent-closure.swift",
    "swift/test/TypeCoercion/overload_noncall.swift",
    "swift/test/expr/capture/nested_class.swift",
    "swift/test/expr/capture/order.swift",
    "swift/test/NameBinding/name-binding.swift"
  };
  StringRef fileName = SF->getFilename();
  return llvm::any_of(testsThatShouldDiffer, [&](const char *testFile) {
    return fileName.endswith(testFile);
  });
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

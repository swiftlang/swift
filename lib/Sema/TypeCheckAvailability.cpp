//===--- TypeCheckAvailability.cpp - Availability Diagnostics -------------===//
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
// This file implements availability diagnostics.
//
//===----------------------------------------------------------------------===//

#include "TypeCheckAvailability.h"
#include "MiscDiagnostics.h"
#include "TypeCheckConcurrency.h"
#include "TypeCheckObjC.h"
#include "TypeCheckType.h"
#include "TypeCheckUnsafe.h"
#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/AvailabilityConstraint.h"
#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/AvailabilityInference.h"
#include "swift/AST/AvailabilityScope.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeDeclFinder.h"
#include "swift/AST/UnsafeUse.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/ParseDeclName.h"
#include "swift/Sema/IDETypeChecking.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace swift;

static const Decl *
concreteSyntaxDeclForAvailableAttribute(const Decl *AbstractSyntaxDecl);

/// Emit a diagnostic for references to declarations that have been
/// marked as unavailable, either through "unavailable" or "obsoleted:".
static bool diagnoseExplicitUnavailability(
    SourceLoc loc, const AvailabilityConstraint &constraint,
    const RootProtocolConformance *rootConf, const ExtensionDecl *ext,
    const ExportContext &where,
    bool warnIfConformanceUnavailablePreSwift6 = false,
    bool preconcurrency = false);

/// Emit a diagnostic for references to declarations that have been
/// marked as unavailable, either through "unavailable" or "obsoleted:".
static bool diagnoseExplicitUnavailability(
    const ValueDecl *D, SourceRange R, const AvailabilityConstraint &constraint,
    const ExportContext &Where, DeclAvailabilityFlags Flags,
    llvm::function_ref<void(InFlightDiagnostic &, StringRef)>
        attachRenameFixIts);

static bool diagnoseSubstitutionMapAvailability(
    SourceLoc loc, SubstitutionMap subs, const ExportContext &where,
    Type depTy = Type(), Type replacementTy = Type(),
    bool warnIfConformanceUnavailablePreSwift6 = false,
    bool suppressParameterizationCheckForOptional = false,
    bool preconcurrency = false);

/// Diagnose uses of unavailable declarations in types.
static bool
diagnoseTypeReprAvailability(const TypeRepr *T, const ExportContext &where,
                             DeclAvailabilityFlags flags = std::nullopt);

ExportContext::ExportContext(DeclContext *DC,
                             AvailabilityContext availability,
                             FragileFunctionKind kind,
                             llvm::SmallVectorImpl<UnsafeUse> *unsafeUses,
                             bool spi, bool exported,
                             bool implicit)
    : DC(DC), Availability(availability), FragileKind(kind),
      UnsafeUses(unsafeUses) {
  SPI = spi;
  Exported = exported;
  Implicit = implicit;
  Reason = unsigned(ExportabilityReason::General);
}

bool swift::isExported(const ValueDecl *VD) {
  if (VD->getAttrs().hasAttribute<ImplementationOnlyAttr>())
    return false;
  if (VD->isObjCMemberImplementation())
    return false;

  // Is this part of the module's API or ABI?
  AccessScope accessScope =
      VD->getFormalAccessScope(nullptr,
                               /*treatUsableFromInlineAsPublic*/true);
  if (accessScope.isPublic())
    return true;

  // Is this a stored property in a @frozen struct or class?
  if (auto *property = dyn_cast<VarDecl>(VD))
    if (property->isLayoutExposedToClients())
      return true;

  return false;
}

static bool hasConformancesToPublicProtocols(const ExtensionDecl *ED) {
  auto nominal = ED->getExtendedNominal();
  if (!nominal)
    return false;

  // Extensions of protocols cannot introduce additional conformances.
  if (isa<ProtocolDecl>(nominal))
    return false;

  auto protocols = ED->getLocalProtocols(ConformanceLookupKind::OnlyExplicit);
  for (const ProtocolDecl *PD : protocols) {
    AccessScope scope =
        PD->getFormalAccessScope(/*useDC*/ nullptr,
                                 /*treatUsableFromInlineAsPublic*/ true);
    if (scope.isPublic())
      return true;
  }

  return false;
}

bool swift::isExported(const ExtensionDecl *ED) {
  // An extension can only be exported if it extends an exported type.
  if (auto *NTD = ED->getExtendedNominal()) {
    if (!isExported(NTD))
      return false;
  }

  // If there are any exported members then the extension is exported.
  for (const Decl *D : ED->getMembers()) {
    if (isExported(D))
      return true;
  }

  // If the extension declares a conformance to a public protocol then the
  // extension is exported.
  if (hasConformancesToPublicProtocols(ED))
    return true;

  return false;
}

bool swift::isExported(const Decl *D) {
  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    return isExported(VD);
  }
  if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
    for (unsigned i = 0, e = PBD->getNumPatternEntries(); i < e; ++i) {
      if (auto *VD = PBD->getAnchoringVarDecl(i))
        return isExported(VD);
    }

    return false;
  }
  if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
    return isExported(ED);
  }

  return true;
}

template<typename Fn>
static void forEachOuterDecl(DeclContext *DC, Fn fn) {
  for (; !DC->isModuleScopeContext(); DC = DC->getParent()) {
    switch (DC->getContextKind()) {
    case DeclContextKind::AbstractClosureExpr:
    case DeclContextKind::SerializedAbstractClosure:
    case DeclContextKind::TopLevelCodeDecl:
    case DeclContextKind::SerializedTopLevelCodeDecl:
    case DeclContextKind::Package:
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
    case DeclContextKind::MacroDecl:
      break;

    case DeclContextKind::Initializer:
      if (auto *PBI = dyn_cast<PatternBindingInitializer>(DC))
        fn(PBI->getBinding());
      else if (auto *I = dyn_cast<PropertyWrapperInitializer>(DC))
        fn(I->getWrappedVar());
      break;

    case DeclContextKind::SubscriptDecl:
      fn(cast<SubscriptDecl>(DC));
      break;

    case DeclContextKind::EnumElementDecl:
      fn(cast<EnumElementDecl>(DC));
      break;

    case DeclContextKind::AbstractFunctionDecl:
      fn(cast<AbstractFunctionDecl>(DC));

      if (auto *AD = dyn_cast<AccessorDecl>(DC))
        fn(AD->getStorage());
      break;

    case DeclContextKind::GenericTypeDecl:
      fn(cast<GenericTypeDecl>(DC));
      break;

    case DeclContextKind::ExtensionDecl:
      fn(cast<ExtensionDecl>(DC));
      break;
    }
  }
}

static void computeExportContextBits(ASTContext &Ctx, Decl *D, bool *spi,
                                     bool *implicit) {
  if (D->isSPI() ||
      D->isAvailableAsSPI())
    *spi = true;

  // Defer bodies are desugared to an implicit closure expression. We need to
  // dilute the meaning of "implicit" to make sure we're still checking
  // availability inside of defer statements.
  const auto isDeferBody = isa<FuncDecl>(D) && cast<FuncDecl>(D)->isDeferBody();
  if (D->isImplicit() && !isDeferBody)
    *implicit = true;

  if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
    for (unsigned i = 0, e = PBD->getNumPatternEntries(); i < e; ++i) {
      if (auto *VD = PBD->getAnchoringVarDecl(i))
        computeExportContextBits(Ctx, VD, spi, implicit);
    }
  }
}

ExportContext ExportContext::forDeclSignature(Decl *D) {
  auto &Ctx = D->getASTContext();

  auto *DC = D->getInnermostDeclContext();
  auto fragileKind = DC->getFragileFunctionKind();
  auto loc = D->getLoc();
  auto availabilityContext = TypeChecker::availabilityAtLocation(loc, DC);
  bool spi = Ctx.LangOpts.LibraryLevel == LibraryLevel::SPI;
  bool implicit = false;
  computeExportContextBits(Ctx, D, &spi, &implicit);
  forEachOuterDecl(D->getDeclContext(), [&](Decl *D) {
    computeExportContextBits(Ctx, D, &spi, &implicit);
  });

  bool exported = ::isExported(D);

  return ExportContext(DC, availabilityContext, fragileKind, nullptr,
                       spi, exported, implicit);
}

ExportContext ExportContext::forFunctionBody(DeclContext *DC, SourceLoc loc) {
  auto &Ctx = DC->getASTContext();

  auto fragileKind = DC->getFragileFunctionKind();
  auto availabilityContext = TypeChecker::availabilityAtLocation(loc, DC);
  bool spi = Ctx.LangOpts.LibraryLevel == LibraryLevel::SPI;
  bool implicit = false;
  forEachOuterDecl(
      DC, [&](Decl *D) { computeExportContextBits(Ctx, D, &spi, &implicit); });

  bool exported = false;

  return ExportContext(DC, availabilityContext, fragileKind, nullptr,
                       spi, exported, implicit);
}

ExportContext ExportContext::forConformance(DeclContext *DC,
                                            ProtocolDecl *proto) {
  assert(isa<ExtensionDecl>(DC) || isa<NominalTypeDecl>(DC));
  auto where = forDeclSignature(DC->getInnermostDeclarationDeclContext());

  where.Exported &= proto->getFormalAccessScope(
      DC, /*usableFromInlineAsPublic*/true).isPublic();

  return where;
}

ExportContext ExportContext::withReason(ExportabilityReason reason) const {
  auto copy = *this;
  copy.Reason = unsigned(reason);
  return copy;
}

ExportContext ExportContext::withExported(bool exported) const {
  auto copy = *this;
  copy.Exported = isExported() && exported;
  return copy;
}

ExportContext ExportContext::withRefinedAvailability(
    const AvailabilityRange &availability) const {
  auto copy = *this;
  copy.Availability.constrainWithPlatformRange(availability,
                                               DC->getASTContext());
  return copy;
}

bool ExportContext::mustOnlyReferenceExportedDecls() const {
  return Exported || FragileKind.kind != FragileFunctionKind::None;
}

std::optional<ExportabilityReason>
ExportContext::getExportabilityReason() const {
  if (Exported)
    return ExportabilityReason(Reason);
  return std::nullopt;
}

/// Returns true if there is any availability attribute on the declaration
/// that is active.
static bool hasActiveAvailableAttribute(const Decl *D, ASTContext &ctx) {
  D = abstractSyntaxDeclForAvailableAttribute(D);

  for (auto Attr : D->getSemanticAvailableAttrs()) {
    if (Attr.isActive(ctx))
      return true;
  }

  return false;
}

static bool computeContainedByDeploymentTarget(AvailabilityScope *scope,
                                               ASTContext &ctx) {
  return scope->getPlatformAvailabilityRange().isContainedIn(
      AvailabilityRange::forDeploymentTarget(ctx));
}

static bool shouldAllowReferenceToUnavailableInSwiftDeclaration(
    const Decl *D, const ExportContext &where) {
  auto *DC = where.getDeclContext();
  auto *SF = DC->getParentSourceFile();

  // Unavailable-in-Swift declarations shouldn't be referenced directly in
  // source. However, they can be referenced in implicit declarations that are
  // printed in .swiftinterfaces.
  if (!SF || SF->Kind != SourceFileKind::Interface)
    return false;

  if (auto constructor = dyn_cast_or_null<ConstructorDecl>(DC->getAsDecl())) {
    // Designated initializers inherited from an Obj-C superclass may have
    // parameters that are unavailable-in-Swift.
    if (constructor->isObjC())
      return true;
  }

  return false;
}

// Utility function to help determine if noasync diagnostics are still
// appropriate even if a `DeclContext` returns `false` from `isAsyncContext()`.
static bool shouldTreatDeclContextAsAsyncForDiagnostics(const DeclContext *DC) {
  if (auto *D = DC->getAsDecl())
    if (auto *FD = dyn_cast<FuncDecl>(D))
      if (FD->isDeferBody())
        // If this is a defer body, we should delegate to its parent.
        return shouldTreatDeclContextAsAsyncForDiagnostics(DC->getParent());

  return DC->isAsyncContext();
}

namespace {

/// A class to walk the AST to build the availability scope tree.
class AvailabilityScopeBuilder : private ASTWalker {

  ASTContext &Context;

  /// Represents an entry in a stack of active availability scopes. The stack is
  /// used to facilitate building the availability scope tree structure. A new
  /// scope is pushed onto this stack before visiting children whenever the
  /// current AST node requires a new context and the scope is then popped
  /// post-visitation.
  struct ContextInfo {
    AvailabilityScope *Scope;

    /// The AST node. This node can be null (ParentTy()),
    /// indicating that custom logic elsewhere will handle removing
    /// the context when needed.
    ParentTy ScopeNode;

    bool ContainedByDeploymentTarget;
  };
  std::vector<ContextInfo> ContextStack;

  llvm::SmallVector<const Decl *, 4> ConcreteDeclStack;

  /// Represents an entry in a stack of pending decl body availability scopes.
  /// Scopes in this stack should be pushed onto \p ContextStack when
  /// \p BodyStmt is encountered.
  struct DeclBodyContextInfo {
    Decl *Decl;
    llvm::DenseMap<ASTNode, AvailabilityScope *> BodyScopes;
  };
  std::vector<DeclBodyContextInfo> DeclBodyContextStack;

  std::vector<const DeclContext *> DeclContextStack;

  AvailabilityScope *getCurrentScope() {
    return ContextStack.back().Scope;
  }

  const DeclContext *getCurrentDeclContext() const {
    assert(!DeclContextStack.empty());
    return DeclContextStack.back();
  }

  bool isCurrentScopeContainedByDeploymentTarget() {
    return ContextStack.back().ContainedByDeploymentTarget;
  }

  const AvailabilityContext constrainCurrentAvailabilityWithPlatformRange(
      const AvailabilityRange &platformRange) {
    auto availability = getCurrentScope()->getAvailabilityContext();
    availability.constrainWithPlatformRange(platformRange, Context);
    return availability;
  }

  void pushContext(AvailabilityScope *Scope, ParentTy PopAfterNode) {
    ContextInfo Info;
    Info.Scope = Scope;
    Info.ScopeNode = PopAfterNode;

    if (!ContextStack.empty() && isCurrentScopeContainedByDeploymentTarget()) {
      assert(computeContainedByDeploymentTarget(Scope, Context) &&
             "incorrectly skipping computeContainedByDeploymentTarget()");
      Info.ContainedByDeploymentTarget = true;
    } else {
      Info.ContainedByDeploymentTarget =
          computeContainedByDeploymentTarget(Scope, Context);
    }

    ContextStack.push_back(Info);
  }

  void pushDeclBodyContext(
      Decl *D, llvm::SmallVector<std::pair<ASTNode, AvailabilityScope *>, 4>
                   NodesAndScopes) {
    DeclBodyContextInfo Info;
    Info.Decl = D;
    for (auto NodeAndScope : NodesAndScopes) {
      Info.BodyScopes.insert(NodeAndScope);
    }

    DeclBodyContextStack.push_back(Info);
  }

  const char *stackTraceAction() const {
    return "building availabilty scope for";
  }

  friend class swift::ExpandChildAvailabilityScopesRequest;

public:
  AvailabilityScopeBuilder(AvailabilityScope *Scope, ASTContext &Context)
      : Context(Context) {
    assert(Scope);
    pushContext(Scope, ParentTy());
    DeclContextStack.push_back(Scope->getIntroductionNode().getDeclContext());
  }

  void build(Decl *D) {
    PrettyStackTraceDecl trace(stackTraceAction(), D);
    unsigned StackHeight = ContextStack.size();
    D->walk(*this);
    assert(ContextStack.size() == StackHeight);
    (void)StackHeight;
  }

  void build(Stmt *S) {
    PrettyStackTraceStmt trace(Context, stackTraceAction(), S);
    unsigned StackHeight = ContextStack.size();
    S->walk(*this);
    assert(ContextStack.size() == StackHeight);
    (void)StackHeight;
  }

  void build(Expr *E) {
    PrettyStackTraceExpr trace(Context, stackTraceAction(), E);
    unsigned StackHeight = ContextStack.size();
    E->walk(*this);
    assert(ContextStack.size() == StackHeight);
    (void)StackHeight;
  }

private:
  MacroWalking getMacroWalkingBehavior() const override {
    // Expansion buffers will have their type availability scopes built lazily.
    return MacroWalking::Arguments;
  }

  SequenceWalking getSequenceWalkingBehavior() const override {
    // Since availability scopes may be built at arbitrary times, the builder
    // may encounter ASTs where SequenceExprs still exist and have not been
    // folded, or it may encounter folded SequenceExprs that have not been
    // removed from the AST. When folded exprs are encountered, its important
    // to avoid walking into the same AST nodes twice.
    return SequenceWalking::OnlyWalkFirstOperatorWhenFolded;
  }

  /// Check whether this declaration is within a macro expansion buffer that
  /// will have its own availability scope that will be lazily expanded.
  bool isDeclInMacroExpansion(Decl *decl) const override {
    // If it's not in a macro expansion relative to its context, it's not
    // considered to be in a macro expansion.
    if (!decl->isInMacroExpansionInContext())
      return false;

    auto module = decl->getDeclContext()->getParentModule();
    auto *declFile = module->getSourceFileContainingLocation(decl->getLoc());
    if (!declFile)
      return false;

    // Look for a parent context that implies that we are producing an
    // availability scope for this expansion.
    for (auto iter = ContextStack.rbegin(), endIter = ContextStack.rend();
         iter != endIter; ++iter) {
      const auto &context = *iter;
      if (auto scope = context.Scope) {
        // If the context is the same source file, don't treat it as an
        // expansion.
        auto introNode = scope->getIntroductionNode();
        switch (scope->getReason()) {
        case AvailabilityScope::Reason::Root:
          if (auto contextFile = introNode.getAsSourceFile())
            if (declFile == contextFile)
              return false;

          break;

        case AvailabilityScope::Reason::Decl:
        case AvailabilityScope::Reason::DeclImplicit:
          // If the context is a declaration, check whether the declaration
          // is in the same source file as this declaration.
          if (auto contextDecl = introNode.getAsDecl()) {
            if (decl == contextDecl)
              return false;

            auto contextModule =
                contextDecl->getDeclContext()->getParentModule();
            SourceLoc contextDeclLoc = contextDecl->getLoc();
            auto contextDeclFile =
                contextModule->getSourceFileContainingLocation(contextDeclLoc);
            if (declFile == contextDeclFile)
              return false;
          }
          break;

        case AvailabilityScope::Reason::IfStmtThenBranch:
        case AvailabilityScope::Reason::IfStmtElseBranch:
        case AvailabilityScope::Reason::ConditionFollowingAvailabilityQuery:
        case AvailabilityScope::Reason::GuardStmtFallthrough:
        case AvailabilityScope::Reason::GuardStmtElseBranch:
        case AvailabilityScope::Reason::WhileStmtBody:
          // Nothing to check here.
          break;
        }
      }
    }

    return true;
  }

  bool shouldSkipDecl(Decl *D) const {
    // Only visit a node that has a corresponding concrete syntax node if we are
    // already walking that concrete syntax node.
    auto *concreteDecl = concreteSyntaxDeclForAvailableAttribute(D);
    if (concreteDecl != D) {
      if (ConcreteDeclStack.empty() || ConcreteDeclStack.back() != concreteDecl)
        return true;
    }

    return false;
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    PrettyStackTraceDecl trace(stackTraceAction(), D);

    // Implicit decls don't have source locations so they cannot have a scope.
    // However, some implicit nodes contain non-implicit nodes (e.g. defer
    // blocks) so we must continue through them.
    if (!D->isImplicit()) {
      if (shouldSkipDecl(D))
        return Action::SkipNode();

      // The AST of this decl may not be ready to traverse yet if it hasn't been
      // full typechecked. If that's the case, we leave a placeholder node in
      // the tree to indicate that the subtree should be expanded lazily when it
      // needs to be traversed.
      if (buildLazyContextForDecl(D))
        return Action::SkipNode();

      // Adds in a scope that covers the entire declaration.
      if (auto DeclScope = getNewContextForSignatureOfDecl(D)) {
        pushContext(DeclScope, D);
      }

      // Create scopes that cover only the body of the declaration.
      buildContextsForBodyOfDecl(D);
    }

    if (auto *DC = dyn_cast<DeclContext>(D)) {
      DeclContextStack.push_back(DC);
    }

    // If this decl is the concrete syntax decl for some abstract syntax decl,
    // push it onto the stack so that the abstract syntax decls may be visited.
    auto *abstractDecl = abstractSyntaxDeclForAvailableAttribute(D);
    if (abstractDecl != D) {
      ConcreteDeclStack.push_back(D);
    }
    return Action::Continue();
  }

  PostWalkAction walkToDeclPost(Decl *D) override {
    if (!ConcreteDeclStack.empty() && ConcreteDeclStack.back() == D) {
      ConcreteDeclStack.pop_back();
    }

    if (auto *DC = dyn_cast<DeclContext>(D)) {
      assert(DeclContextStack.back() == DC);
      DeclContextStack.pop_back();
    }

    while (ContextStack.back().ScopeNode.getAsDecl() == D) {
      ContextStack.pop_back();
    }

    while (!DeclBodyContextStack.empty() &&
           DeclBodyContextStack.back().Decl == D) {
      // All pending body scopes should have been consumed.
      assert(DeclBodyContextStack.back().BodyScopes.empty());
      DeclBodyContextStack.pop_back();
    }

    return Action::Continue();
  }

  bool shouldBuildLazyContextForDecl(Decl *D) {
    // Skip functions that have unparsed bodies on an initial descent to avoid
    // eagerly parsing bodies unnecessarily.
    if (auto *afd = dyn_cast<AbstractFunctionDecl>(D)) {
      if (afd->hasBody() && !afd->isBodySkipped() &&
          !afd->getBody(/*canSynthesize=*/false))
        return true;
    }

    // Pattern binding declarations may have attached property wrappers that
    // get expanded from macros attached to the parent declaration. We must
    // not eagerly expand the attached property wrappers to avoid request
    // cycles.
    if (isa<PatternBindingDecl>(D)) {
      return true;
    }

    if (isa<ExtensionDecl>(D)) {
      return true;
    }

    return false;
  }

  /// For declarations that were previously skipped prepare the AST before
  /// building out scopes.
  void prepareDeclForLazyExpansion(Decl *D) {
    if (auto AFD = dyn_cast<AbstractFunctionDecl>(D)) {
      (void)AFD->getBody(/*canSynthesize*/ true);
    }
  }

  /// Constructs a placeholder scope that should be expanded later. This is
  /// useful for postponing unnecessary work (and request triggers) when
  /// initally building out the scope subtree under a declaration. Lazy nodes
  /// constructed here will be expanded by ExpandChildAvailabilityScopesRequest.
  /// Returns true if a node was created.
  bool buildLazyContextForDecl(Decl *D) {
    // Check whether the current scope is already a lazy placeholder. If it is,
    // we should try to expand it rather than creating a new placeholder.
    auto currentScope = getCurrentScope();
    if (currentScope->getNeedsExpansion() && currentScope->getDeclOrNull() == D)
      return false;

    if (!shouldBuildLazyContextForDecl(D))
      return false;

    // If we've made it this far then we've identified a declaration that
    // requires lazy expansion later.
    auto lazyScope = AvailabilityScope::createForDeclImplicit(
        Context, D, currentScope, currentScope->getAvailabilityContext(),
        refinementSourceRangeForDecl(D));
    lazyScope->setNeedsExpansion(true);
    return true;
  }

  /// Returns a new context to be introduced for the declaration, or nullptr
  /// if no new context should be introduced.
  AvailabilityScope *getNewContextForSignatureOfDecl(Decl *D) {
    if (!isa<ValueDecl>(D) &&
        !isa<EnumCaseDecl>(D) &&
        !isa<ExtensionDecl>(D) &&
        !isa<MacroExpansionDecl>(D) &&
        !isa<PatternBindingDecl>(D))
      return nullptr;

    // Only introduce for an AbstractStorageDecl if it is not local. We
    // introduce for the non-local case because these may have getters and
    // setters (and these may be synthesized, so they might not even exist yet).
    if (isa<AbstractStorageDecl>(D) && D->getDeclContext()->isLocalContext())
      return nullptr;

    // Don't introduce for abstract syntax nodes that have separate concrete
    // syntax nodes. The scope will be introduced for the concrete node instead.
    if (concreteSyntaxDeclForAvailableAttribute(D) != D)
      return nullptr;

    // Declarations with explicit availability attributes always get a scope.
    if (hasActiveAvailableAttribute(D, Context)) {
      return AvailabilityScope::createForDecl(
          Context, D, getCurrentScope(),
          getEffectiveAvailabilityForDeclSignature(D),
          refinementSourceRangeForDecl(D));
    }

    // Declarations without explicit availability attributes get a scope if they
    // are effectively less available than the surrounding context. For example,
    // an internal property in a public struct can be effectively less available
    // than the containing struct decl because the internal property will only
    // be accessed by code running at the deployment target or later.
    auto CurrentAvailability = getCurrentScope()->getAvailabilityContext();
    auto EffectiveAvailability = getEffectiveAvailabilityForDeclSignature(D);
    if (CurrentAvailability != EffectiveAvailability)
      return AvailabilityScope::createForDeclImplicit(
          Context, D, getCurrentScope(), EffectiveAvailability,
          refinementSourceRangeForDecl(D));

    return nullptr;
  }

  const AvailabilityContext
  getEffectiveAvailabilityForDeclSignature(const Decl *D) {
    auto EffectiveIntroduction = AvailabilityRange::alwaysAvailable();

    // Availability attributes are found abstract syntax decls.
    D = abstractSyntaxDeclForAvailableAttribute(D);

    // As a special case, extension decls are treated as effectively as
    // available as the nominal type they extend, up to the deployment target.
    // This rule is a convenience for library authors who have written
    // extensions without specifying availabilty on the extension itself.
    if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
      auto ET = ED->getExtendedType();
      if (ET && !hasActiveAvailableAttribute(D, Context)) {
        EffectiveIntroduction.intersectWith(
            swift::AvailabilityInference::inferForType(ET));

        // We want to require availability to be specified on extensions of
        // types that would be potentially unavailable to the module containing
        // the extension, so limit the effective availability to the deployment
        // target.
        EffectiveIntroduction.unionWith(
            AvailabilityRange::forDeploymentTarget(Context));
      }
    }

    if (shouldConstrainSignatureToDeploymentTarget(D))
      EffectiveIntroduction.intersectWith(
          AvailabilityRange::forDeploymentTarget(Context));

    auto availability = getCurrentScope()->getAvailabilityContext();
    availability.constrainWithDeclAndPlatformRange(D, EffectiveIntroduction);
    return availability;
  }

  /// Checks whether the entire declaration, including its signature, should be
  /// constrained to the deployment target. Generally public API declarations
  /// are not constrained since they appear in the interface of the module and
  /// may be consumed by clients with lower deployment targets, but there are
  /// some exceptions.
  bool shouldConstrainSignatureToDeploymentTarget(const Decl *D) {
    if (isCurrentScopeContainedByDeploymentTarget())
      return false;

    // A declaration inside of a local context always inherits the availability
    // of the parent.
    if (D->getDeclContext()->isLocalContext())
      return false;

    // As a convenience, explicitly unavailable decls are constrained to the
    // deployment target. There's not much benefit to checking these decls at a
    // lower availability version floor since they can't be invoked by clients.
    if (getCurrentScope()->getAvailabilityContext().isUnavailable() ||
        D->isUnavailable())
      return true;

    // To remain compatible with a lot of existing SPIs that are declared
    // without availability attributes, constrain them to the deployment target
    // too.
    if (D->isSPI())
      return true;

    return !::isExported(D);
  }

  /// Returns the source range which should be refined by declaration. This
  /// provides a convenient place to specify the refined range when it is
  /// different than the declaration's source range.
  SourceRange refinementSourceRangeForDecl(Decl *D) {
    // We require a valid range in order to be able to query for the scope
    // corresponding to a given SourceLoc.
    // If this assert fires, it means we have probably synthesized an implicit
    // declaration without location information. The appropriate fix is
    // probably to gin up a source range for the declaration when synthesizing
    // it.
    assert(D->getSourceRange().isValid());

    auto &Context = D->getASTContext();
    SourceRange Range;
    if (auto *storageDecl = dyn_cast<AbstractStorageDecl>(D)) {
      // Use the declaration's availability for the context when checking
      // the bodies of its accessors.
      Range = storageDecl->getSourceRange();

      // HACK: For synthesized trivial accessors we may have not a valid
      // location for the end of the braces, so in that case we will fall back
      // to using the range for the storage declaration. The right fix here is
      // to update AbstractStorageDecl::addTrivialAccessors() to take brace
      // locations and have callers of that method provide appropriate source
      // locations.
      SourceRange BracesRange = storageDecl->getBracesRange();
      if (BracesRange.isValid()) {
        Range.widen(BracesRange);
      }
    } else {
      Range = D->getSourceRangeIncludingAttrs();
    }

    Range.End = Lexer::getLocForEndOfToken(Context.SourceMgr, Range.End);
    return Range;
  }

  /// Enumerate the AST nodes and their corresponding source ranges for
  /// the body (or bodies) of the given declaration.
  void enumerateBodyRanges(
     Decl *decl,
     llvm::function_ref<void(Decl *decl, ASTNode body, SourceRange)> acceptBody
  ) {
    // Top level code always uses the deployment target.
    if (auto tlcd = dyn_cast<TopLevelCodeDecl>(decl)) {
      if (auto bodyStmt = tlcd->getBody()) {
        acceptBody(tlcd, bodyStmt, refinementSourceRangeForDecl(tlcd));
      }
      return;
    }

    // For functions, provide the body source range.
    if (auto afd = dyn_cast<AbstractFunctionDecl>(decl)) {
      if (!afd->isImplicit()) {
        if (auto body = afd->getBody(/*canSynthesize*/ false)) {
          acceptBody(afd, body, afd->getBodySourceRange());
        }
      }
      return;
    }

    // Pattern binding declarations have initial values that are their
    // bodies.
    if (auto *pbd = dyn_cast<PatternBindingDecl>(decl)) {
      for (unsigned index : range(pbd->getNumPatternEntries())) {
        auto var = pbd->getAnchoringVarDecl(index);
        if (!var)
          continue;

        auto *initExpr = pbd->getInit(index);
        if (initExpr && !initExpr->isImplicit()) {
          assert(initExpr->getSourceRange().isValid());

          // Create a scope for the init written in the source.
          acceptBody(var, initExpr, initExpr->getSourceRange());
        }
      }
      return;
    }
  }

  // Creates an implicit decl scope specifying the deployment target for
  // `range` in decl `D`.
  AvailabilityScope *
  createImplicitDeclContextForDeploymentTarget(Decl *D, SourceRange range) {
    const AvailabilityContext Availability =
        constrainCurrentAvailabilityWithPlatformRange(
            AvailabilityRange::forDeploymentTarget(Context));
    return AvailabilityScope::createForDeclImplicit(
        Context, D, getCurrentScope(), Availability, range);
  }

  /// Determine whether the body of the given declaration has
  /// deployment-target availability.
  static bool bodyIsDeploymentTarget(Decl *decl) {
    if (auto afd = dyn_cast<AbstractFunctionDecl>(decl)) {
      return afd->getResilienceExpansion() != ResilienceExpansion::Minimal;
    }

    if (auto var = dyn_cast<VarDecl>(decl)) {
      // Var decls may have associated pattern binding decls or property
      // wrappers with init expressions. Those expressions need to be
      // constrained to the deployment target unless they are exposed to
      // clients.
      return var->hasInitialValue() && !var->isInitExposedToClients();
    }

    return true;
  }

  void buildContextsForBodyOfDecl(Decl *D) {
    // Are we already constrained by the deployment target and the declaration
    // doesn't explicitly allow unsafe constructs in its definition, adding
    // new contexts won't change availability.
    if (isCurrentScopeContainedByDeploymentTarget())
      return;

    // Enumerate all of the body scopes to apply availability.
    llvm::SmallVector<std::pair<ASTNode, AvailabilityScope *>, 4>
        nodesAndScopes;
    enumerateBodyRanges(D, [&](Decl *decl, ASTNode body, SourceRange range) {
      auto availability = getCurrentScope()->getAvailabilityContext();

      // Apply deployment-target availability if appropriate for this body.
      if (!isCurrentScopeContainedByDeploymentTarget() &&
          bodyIsDeploymentTarget(decl)) {
        availability.constrainWithPlatformRange(
             AvailabilityRange::forDeploymentTarget(Context), Context);
      }

      nodesAndScopes.push_back({
          body,
          AvailabilityScope::createForDeclImplicit(
              Context, decl, getCurrentScope(), availability, range)
      });
    });

    if (nodesAndScopes.size() > 0)
      pushDeclBodyContext(D, nodesAndScopes);

    if (!isCurrentScopeContainedByDeploymentTarget()) {
      // Pattern binding declarations can have children corresponding to property
      // wrappers, which we handle separately.
      if (auto *pbd = dyn_cast<PatternBindingDecl>(D)) {
        // Ideally any init expression would be returned by `getInit()` above.
        // However, for property wrappers it doesn't get populated until
        // typechecking completes (which is too late). Instead, we find the
        // the property wrapper attribute and use its source range to create a
        // scope for the initializer expression.
        //
        // FIXME: Since we don't have an expression here, we can't build out its
        // scope. If the Expr that will eventually be created contains a closure
        // expression, then it might have AST nodes that need to be refined. For
        // example, property wrapper initializers that takes block arguments
        // are not handled correctly because of this (rdar://77841331).
        if (auto firstVar = pbd->getAnchoringVarDecl(0)) {
          if (firstVar->hasInitialValue() &&
              !firstVar->isInitExposedToClients()) {
            for (auto *wrapper : firstVar->getAttachedPropertyWrappers()) {
              createImplicitDeclContextForDeploymentTarget(firstVar,
                                                           wrapper->getRange());
            }
          }
        }
      }
    }
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    PrettyStackTraceStmt trace(Context, stackTraceAction(), S);

    if (consumeDeclBodyContextIfNecessary(S)) {
      return Action::Continue(S);
    }

    if (auto *IS = dyn_cast<IfStmt>(S)) {
      buildIfStmtRefinementContext(IS);
      return Action::SkipNode(S);
    }

    if (auto *RS = dyn_cast<GuardStmt>(S)) {
      buildGuardStmtRefinementContext(RS);
      return Action::SkipNode(S);
    }

    if (auto *WS = dyn_cast<WhileStmt>(S)) {
      buildWhileStmtRefinementContext(WS);
      return Action::SkipNode(S);
    }

    return Action::Continue(S);
  }

  PostWalkResult<Stmt *> walkToStmtPost(Stmt *S) override {
    // If we have multiple guard statements in the same block
    // then we may have multiple availability scopes to pop
    // after walking that block.
    while (!ContextStack.empty() &&
           ContextStack.back().ScopeNode.getAsStmt() == S) {
      ContextStack.pop_back();
    }

    return Action::Continue(S);
  }

  /// Attempts to consume a scope from the `BodyScopes` of the top of
  /// `DeclBodyContextStack`. Returns \p true if a scope was pushed.
  template <typename T>
  bool consumeDeclBodyContextIfNecessary(T Body) {
    if (DeclBodyContextStack.empty())
      return false;

    auto &Info = DeclBodyContextStack.back();
    auto Iter = Info.BodyScopes.find(Body);
    if (Iter == Info.BodyScopes.end())
      return false;

    pushContext(Iter->getSecond(), Body);
    Info.BodyScopes.erase(Iter);
    return true;
  }

  /// Builds the availability scope hierarchy for the IfStmt if the guard
  /// introduces a new scope for the Then branch.
  /// There is no need for the caller to explicitly traverse the children
  /// of this node.
  void buildIfStmtRefinementContext(IfStmt *IS) {
    std::optional<AvailabilityRange> ThenRange;
    std::optional<AvailabilityRange> ElseRange;
    std::tie(ThenRange, ElseRange) =
        buildStmtConditionRefinementContext(IS->getCond());

    if (ThenRange.has_value()) {
      // Create a new context for the Then branch and traverse it in that new
      // context.
      auto AvailabilityContext =
          constrainCurrentAvailabilityWithPlatformRange(ThenRange.value());
      auto *ThenScope = AvailabilityScope::createForIfStmtThen(
          Context, IS, getCurrentDeclContext(), getCurrentScope(),
          AvailabilityContext);
      AvailabilityScopeBuilder(ThenScope, Context).build(IS->getThenStmt());
    } else {
      build(IS->getThenStmt());
    }

    Stmt *ElseStmt = IS->getElseStmt();
    if (!ElseStmt)
      return;

    // Refine the else branch if we're given a version range for that branch.
    // For now, if present, this will only be the empty range, indicating
    // that the branch is dead. We use it to suppress potential unavailability
    // and deprecation diagnostics on code that definitely will not run with
    // the current platform and minimum deployment target.
    // If we add a more precise version range lattice (i.e., one that can
    // support "<") we should create non-empty contexts for the Else branch.
    if (ElseRange.has_value()) {
      // Create a new context for the Then branch and traverse it in that new
      // context.
      auto AvailabilityContext =
          constrainCurrentAvailabilityWithPlatformRange(ElseRange.value());
      auto *ElseScope = AvailabilityScope::createForIfStmtElse(
          Context, IS, getCurrentDeclContext(), getCurrentScope(),
          AvailabilityContext);
      AvailabilityScopeBuilder(ElseScope, Context).build(ElseStmt);
    } else {
      build(IS->getElseStmt());
    }
  }

  /// Builds the availability scopes for the WhileStmt if the guard
  /// introduces a new availability scope for the body branch.
  /// There is no need for the caller to explicitly traverse the children
  /// of this node.
  void buildWhileStmtRefinementContext(WhileStmt *WS) {
    std::optional<AvailabilityRange> BodyRange =
        buildStmtConditionRefinementContext(WS->getCond()).first;

    if (BodyRange.has_value()) {
      // Create a new context for the body and traverse it in the new
      // context.
      auto AvailabilityContext =
          constrainCurrentAvailabilityWithPlatformRange(BodyRange.value());
      auto *BodyScope = AvailabilityScope::createForWhileStmtBody(
          Context, WS, getCurrentDeclContext(), getCurrentScope(),
          AvailabilityContext);
      AvailabilityScopeBuilder(BodyScope, Context).build(WS->getBody());
    } else {
      build(WS->getBody());
    }
  }

  /// Builds the availability scopes for the GuardStmt and pushes
  /// the fallthrough scope onto the scope stack so that subsequent
  /// AST elements in the same scope are analyzed in the context of the
  /// fallthrough scope.
  void buildGuardStmtRefinementContext(GuardStmt *GS) {
    // 'guard' statements fall through if all of the
    // guard conditions are true, so we refine the range after the require
    // until the end of the enclosing block.
    // if ... {
    //   guard available(...) else { return } <-- Refined range starts here
    //   ...
    // } <-- Refined range ends here
    //
    // This is slightly tricky because, unlike our other control constructs,
    // the refined region is not lexically contained inside the construct
    // introducing the availability scope.
    std::optional<AvailabilityRange> FallthroughRange;
    std::optional<AvailabilityRange> ElseRange;
    std::tie(FallthroughRange, ElseRange) =
        buildStmtConditionRefinementContext(GS->getCond());

    if (Stmt *ElseBody = GS->getBody()) {
      if (ElseRange.has_value()) {
        auto AvailabilityContext =
            constrainCurrentAvailabilityWithPlatformRange(ElseRange.value());
        auto *TrueScope = AvailabilityScope::createForGuardStmtElse(
            Context, GS, getCurrentDeclContext(), getCurrentScope(),
            AvailabilityContext);

        AvailabilityScopeBuilder(TrueScope, Context).build(ElseBody);
      } else {
        build(ElseBody);
      }
    }

    auto *ParentBrace = dyn_cast<BraceStmt>(Parent.getAsStmt());
    assert(ParentBrace && "Expected parent of GuardStmt to be BraceStmt");
    if (!FallthroughRange.has_value())
      return;

    // Create a new context for the fallthrough.
    auto FallthroughAvailability =
        constrainCurrentAvailabilityWithPlatformRange(FallthroughRange.value());
    auto *FallthroughScope = AvailabilityScope::createForGuardStmtFallthrough(
        Context, GS, ParentBrace, getCurrentDeclContext(), getCurrentScope(),
        FallthroughAvailability);

    pushContext(FallthroughScope, ParentBrace);
  }

  /// Build the availability scopes for a StmtCondition and return a pair
  /// of optional version ranges, the first for the true branch and the second
  /// for the false branch. A value of None for a given branch indicates that
  /// the branch does not introduce a new scope.
  std::pair<std::optional<AvailabilityRange>,
            std::optional<AvailabilityRange>>
  buildStmtConditionRefinementContext(StmtCondition Cond) {
    if (Context.LangOpts.DisableAvailabilityChecking)
      return {};

    // Any availability scopes introduced in the statement condition
    // will end at the end of the last condition element.
    StmtConditionElement LastElement = Cond.back();

    // Keep track of how many nested availability scopes we have pushed on
    // the scope stack so we can pop them when we're done building the
    // scope for the StmtCondition.
    unsigned NestedCount = 0;

    // Tracks the potential version range when the condition is false.
    auto FalseFlow = AvailabilityRange::neverAvailable();

    AvailabilityScope *StartingScope = getCurrentScope();

    // Tracks if we're refining for availability or unavailability.
    std::optional<bool> isUnavailability = std::nullopt;

    for (StmtConditionElement Element : Cond) {
      AvailabilityScope *CurrentScope = getCurrentScope();
      AvailabilityRange CurrentInfo =
          CurrentScope->getPlatformAvailabilityRange();

      // If the element is not a condition, walk it in the current scope.
      if (Element.getKind() != StmtConditionElement::CK_Availability) {

        // Assume any condition element that is not a #available() can
        // potentially be false, so conservatively combine the version
        // range of the current context with the accumulated false flow
        // of all other conjuncts.
        FalseFlow.unionWith(CurrentInfo);

        Element.walk(*this);
        continue;
      }

      // #available query: introduce a new availability scope for the statement
      // condition elements following it.
      auto *Query = Element.getAvailability();

      if (isUnavailability == std::nullopt) {
        isUnavailability = Query->isUnavailability();
      } else if (isUnavailability != Query->isUnavailability()) {
        // Mixing availability with unavailability in the same statement will
        // cause the false flow's version range to be ambiguous. Report it.
        //
        // Technically we can support this by not refining ambiguous flows,
        // but there are currently no legitimate cases where one would have
        // to mix availability with unavailability.
        Context.Diags.diagnose(Query->getLoc(),
                               diag::availability_cannot_be_mixed);
        break;
      }

      // If this query expression has no queries, we will not introduce a new
      // availability scope. We do not diagnose here: a diagnostic will already
      // have been emitted by the parser.
      // For #unavailable, empty queries are valid as wildcards are implied.
      if (!Query->isUnavailability() && Query->getQueries().empty())
        continue;

      auto Spec = bestActiveSpecForQuery(Query);
      if (!Spec) {
        // We couldn't find an appropriate spec for the current platform,
        // so rather than refining, emit a diagnostic and just use the current
        // scope.
        Context.Diags.diagnose(
            Query->getLoc(), diag::availability_query_required_for_platform,
            platformString(targetPlatform(Context.LangOpts)));

        continue;
      }

      AvailabilityRange NewConstraint = contextForSpec(*Spec, false);
      Query->setAvailableRange(
          contextForSpec(*Spec, true).getRawVersionRange());

      // When compiling zippered for macCatalyst, we need to collect both
      // a macOS version (the target version) and an iOS/macCatalyst version
      // (the target-variant). These versions will both be passed to a runtime
      // entrypoint that will check either the macOS version or the iOS
      // version depending on the kind of process this code is loaded into.
      if (Context.LangOpts.TargetVariant) {
        auto VariantSpec =
            bestActiveSpecForQuery(Query, /*ForTargetVariant*/ true);
        if (VariantSpec) {
          VersionRange VariantRange =
              contextForSpec(*VariantSpec, true).getRawVersionRange();
          Query->setVariantAvailableRange(VariantRange);
        }
      }

      if (Spec->isWildcard()) {
        // The wildcard spec '*' represents the minimum deployment target, so
        // there is no need to create an availability scope for this query.
        // Further, we won't diagnose for useless #available() conditions
        // where * matched on this platform -- presumably those conditions are
        // needed for some other platform.
        continue;
      }

      // If the explicitly-specified (via #availability) version range for the
      // current scope is completely contained in the range for the spec, then
      // a version query can never be false, so the spec is useless.
      // If so, report this.
      auto ExplicitRange = CurrentScope->getExplicitAvailabilityRange();
      if (ExplicitRange && ExplicitRange->isContainedIn(NewConstraint)) {
        // Unavailability scopes are always "useless" from a symbol
        // availability point of view, so only useless availability specs are
        // reported.
        if (isUnavailability.value()) {
          continue;
        }
        DiagnosticEngine &Diags = Context.Diags;
        if (CurrentScope->getReason() != AvailabilityScope::Reason::Root) {
          PlatformKind BestPlatform = targetPlatform(Context.LangOpts);

          auto Domain = Spec->getDomain();
          // If possible, try to report the diagnostic in terms for the
          // platform the user uttered in the '#available()'. For a platform
          // that inherits availability from another platform it may be
          // different from the platform specified in the target triple.
          if (Domain.getPlatformKind() != PlatformKind::none)
            BestPlatform = Domain.getPlatformKind();
          Diags.diagnose(Query->getLoc(),
                         diag::availability_query_useless_enclosing_scope,
                         platformString(BestPlatform));
          Diags.diagnose(CurrentScope->getIntroductionLoc(),
                         diag::availability_query_useless_enclosing_scope_here);
        }
      }

      if (CurrentInfo.isContainedIn(NewConstraint)) {
        // No need to actually create the availability scope if we know it is
        // useless.
        continue;
      }

      // If the #available() is not useless then there is potential false flow,
      // so join the false flow with the potential versions of the current
      // context.
      // We could be more precise here if we enriched the lattice to include
      // ranges of the form [x, y).
      FalseFlow.unionWith(CurrentInfo);

      auto ConstrainedAvailability =
          constrainCurrentAvailabilityWithPlatformRange(NewConstraint);
      auto *Scope = AvailabilityScope::createForConditionFollowingQuery(
          Context, Query, LastElement, getCurrentDeclContext(), CurrentScope,
          ConstrainedAvailability);

      pushContext(Scope, ParentTy());
      ++NestedCount;
    }

    std::optional<AvailabilityRange> FalseRefinement = std::nullopt;
    // The version range for the false branch should never have any versions
    // that weren't possible when the condition started evaluating.
    assert(
        FalseFlow.isContainedIn(StartingScope->getPlatformAvailabilityRange()));

    // If the starting version range is not completely contained in the
    // false flow version range then it must be the case that false flow range
    // is strictly smaller than the starting range (because the false flow
    // range *is* contained in the starting range), so we should introduce a
    // new availability scope for the false flow.
    if (!StartingScope->getPlatformAvailabilityRange().isContainedIn(
            FalseFlow)) {
      FalseRefinement = FalseFlow;
    }

    auto makeResult =
        [isUnavailability](std::optional<AvailabilityRange> TrueRefinement,
                           std::optional<AvailabilityRange> FalseRefinement) {
          if (isUnavailability.has_value() && isUnavailability.value()) {
            // If this is an unavailability check, invert the result.
            return std::make_pair(FalseRefinement, TrueRefinement);
          }
          return std::make_pair(TrueRefinement, FalseRefinement);
        };

    if (NestedCount == 0)
      return makeResult(std::nullopt, FalseRefinement);

    AvailabilityScope *NestedScope = getCurrentScope();
    while (NestedCount-- > 0)
      ContextStack.pop_back();

    assert(getCurrentScope() == StartingScope);

    return makeResult(NestedScope->getPlatformAvailabilityRange(),
                      FalseRefinement);
  }

  /// Return the best active spec for the target platform or nullptr if no
  /// such spec exists.
  std::optional<SemanticAvailabilitySpec>
  bestActiveSpecForQuery(PoundAvailableInfo *available,
                         bool forTargetVariant = false) {
    std::optional<SemanticAvailabilitySpec> FoundWildcardSpec;
    std::optional<SemanticAvailabilitySpec> BestSpec;

    for (auto Spec :
         available->getSemanticAvailabilitySpecs(getCurrentDeclContext())) {
      if (Spec.isWildcard()) {
        FoundWildcardSpec = Spec;
        continue;
      }

      auto Domain = Spec.getDomain();
      if (!Domain.isPlatform())
        continue;

      // FIXME: This is not quite right: we want to handle AppExtensions
      // properly. For example, on the OSXApplicationExtension platform
      // we want to chose the OS X spec unless there is an explicit
      // OSXApplicationExtension spec.
      auto Platform = Domain.getPlatformKind();
      if (isPlatformActive(Platform, Context.LangOpts, forTargetVariant,
                           /* ForRuntimeQuery */ true)) {

        if (!BestSpec ||
            inheritsAvailabilityFromPlatform(
                Platform, BestSpec->getDomain().getPlatformKind())) {
          BestSpec = Spec;
        }
      }
    }

    if (BestSpec)
      return BestSpec;

    // If we have reached this point, we found no spec for our target, so
    // we return the other spec ('*'), if we found it, or nullptr, if not.
    if (FoundWildcardSpec) {
      return FoundWildcardSpec;
    } else if (available->isUnavailability()) {
      // For #unavailable, imply the presence of a wildcard.
      SourceLoc Loc = available->getRParenLoc();
      return AvailabilitySpec::createWildcard(Context, Loc);
    } else {
      return std::nullopt;
    }
  }

  /// Return the availability context for the given spec.
  AvailabilityRange contextForSpec(SemanticAvailabilitySpec Spec,
                                   bool GetRuntimeContext) {
    if (Spec.isWildcard()) {
      return AvailabilityRange::alwaysAvailable();
    }

    llvm::VersionTuple Version =
        (GetRuntimeContext ? Spec.getRuntimeVersion() : Spec.getVersion());

    return AvailabilityRange(VersionRange::allGTE(Version));
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    (void)consumeDeclBodyContextIfNecessary(E);

    if (auto CE = dyn_cast<ClosureExpr>(E)) {
      DeclContextStack.push_back(CE);
    }

    return Action::Continue(E);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
    if (ContextStack.back().ScopeNode.getAsExpr() == E) {
      ContextStack.pop_back();
    }

    if (auto *CE = dyn_cast<ClosureExpr>(E)) {
      assert(DeclContextStack.back() == CE);
      DeclContextStack.pop_back();
    }

    return Action::Continue(E);
  }
};

} // end anonymous namespace

void TypeChecker::buildAvailabilityScopes(SourceFile &SF) {
  switch (SF.Kind) {
  case SourceFileKind::SIL:
    // SIL doesn't support availability queries.
    return;
  case SourceFileKind::MacroExpansion:
  case SourceFileKind::DefaultArgument:
  case SourceFileKind::Library:
  case SourceFileKind::Main:
  case SourceFileKind::Interface:
    break;
  }
  ASTContext &Context = SF.getASTContext();

  // If there's already a root node, then we're done.
  if (SF.getAvailabilityScope())
    return;

  // The root availability scope reflects the fact that all parts of
  // the source file are guaranteed to be executing on at least the minimum
  // platform version for inlining.
  auto AvailabilityContext = AvailabilityContext::forInliningTarget(Context);
  AvailabilityScope *RootScope =
      AvailabilityScope::createForSourceFile(&SF, AvailabilityContext);
  SF.setAvailabilityScope(RootScope);

  // Build availability scopes, if necessary, for all declarations starting
  // with StartElem.
  AvailabilityScopeBuilder Builder(RootScope, Context);
  for (auto item : SF.getTopLevelItems()) {
    if (auto decl = item.dyn_cast<Decl *>())
      Builder.build(decl);
    else if (auto expr = item.dyn_cast<Expr *>())
      Builder.build(expr);
    else if (auto stmt = item.dyn_cast<Stmt *>())
      Builder.build(stmt);
  }
}

AvailabilityScope *TypeChecker::getOrBuildAvailabilityScope(SourceFile *SF) {
  AvailabilityScope *scope = SF->getAvailabilityScope();
  if (!scope) {
    buildAvailabilityScopes(*SF);
    scope = SF->getAvailabilityScope();
  }

  return scope;
}

evaluator::SideEffect ExpandChildAvailabilityScopesRequest::evaluate(
    Evaluator &evaluator, AvailabilityScope *parentScope) const {
  assert(parentScope->getNeedsExpansion());
  if (auto decl = parentScope->getDeclOrNull()) {
    ASTContext &ctx = decl->getASTContext();
    AvailabilityScopeBuilder builder(parentScope, ctx);
    builder.prepareDeclForLazyExpansion(decl);
    builder.build(decl);
  }
  return evaluator::SideEffect();
}

AvailabilityContext
TypeChecker::availabilityAtLocation(SourceLoc loc, const DeclContext *DC,
                                    const AvailabilityScope **MostRefined) {
  SourceFile *SF;
  if (loc.isValid())
    SF = DC->getParentModule()->getSourceFileContainingLocation(loc);
  else
    SF = DC->getParentSourceFile();
  auto &Context = DC->getASTContext();

  // If our source location is invalid (this may be synthesized code), climb
  // the decl context hierarchy until we find a location that is valid,
  // collecting availability ranges on the way up.
  // We will combine the version ranges from these annotations
  // with the scope for the valid location to overapproximate the running
  // OS versions at the original source location.
  // Because we are climbing DeclContexts we will miss availability scopes in
  // synthesized code that are introduced by AST elements that are themselves
  // not DeclContexts, such as  #available(..) and property declarations.
  // That is, a reference with an invalid location that is contained
  // inside a #available() and with no intermediate DeclContext will not be
  // refined. For now, this is fine -- but if we ever synthesize #available(),
  // this will be a real problem.

  // We can assume we are running on at least the minimum inlining target.
  auto baseAvailability = AvailabilityContext::forInliningTarget(Context);
  auto isInvalidLoc = [SF](SourceLoc loc) {
    return SF ? loc.isInvalid() : true;
  };
  while (DC && isInvalidLoc(loc)) {
    const Decl *D = DC->getInnermostDeclarationDeclContext();
    if (!D)
      break;

    baseAvailability.constrainWithDecl(D);
    loc = D->getLoc();
    DC = D->getDeclContext();
  }

  if (!SF || loc.isInvalid())
    return baseAvailability;

  AvailabilityScope *rootScope = getOrBuildAvailabilityScope(SF);
  if (!rootScope)
    return baseAvailability;

  AvailabilityScope *scope = rootScope->findMostRefinedSubContext(loc, Context);
  if (!scope)
    return baseAvailability;

  if (MostRefined) {
    *MostRefined = scope;
  }

  auto availability = scope->getAvailabilityContext();
  availability.constrainWithContext(baseAvailability, Context);
  return availability;
}

AvailabilityContext
TypeChecker::availabilityForDeclSignature(const Decl *decl) {
  return TypeChecker::availabilityAtLocation(decl->getLoc(),
                                             decl->getInnermostDeclContext());
}

AvailabilityRange TypeChecker::overApproximateAvailabilityAtLocation(
    SourceLoc loc, const DeclContext *DC,
    const AvailabilityScope **MostRefined) {
  return availabilityAtLocation(loc, DC, MostRefined).getPlatformRange();
}

/// A class that walks the AST to find the innermost (i.e., deepest) node that
/// contains a target SourceRange and matches a particular criterion.
/// This class finds the innermost nodes of interest by walking
/// down the root until it has found the target range (in a Pre-visitor)
/// and then recording the innermost node on the way back up in the
/// Post-visitors. It does its best to not search unnecessary subtrees,
/// although this is complicated by the fact that not all nodes have
/// source range information.
class InnermostAncestorFinder : private ASTWalker {
public:

  /// The type of a match predicate, which takes as input a node and its
  /// parent and returns a bool indicating whether the node matches.
  using MatchPredicate = std::function<bool(ASTNode, ASTWalker::ParentTy)>;

private:
  const SourceRange TargetRange;
  const SourceManager &SM;
  const MatchPredicate Predicate;

  bool FoundTarget = false;
  std::optional<ASTNode> InnermostMatchingNode;

public:
  InnermostAncestorFinder(SourceRange TargetRange, const SourceManager &SM,
                          ASTNode SearchNode, const MatchPredicate &Predicate)
      : TargetRange(TargetRange), SM(SM), Predicate(Predicate) {
    assert(TargetRange.isValid());

    SearchNode.walk(*this);
  }

  /// Returns the innermost node containing the target range that matches
  /// the predicate.
  std::optional<ASTNode> getInnermostMatchingNode() {
    return InnermostMatchingNode;
  }

  MacroWalking getMacroWalkingBehavior() const override {
    // This is SourceRange based finder. 'SM.rangeContains()' fails anyway when
    // crossing source buffers.
    return MacroWalking::Arguments;
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    return getPreWalkActionFor(E);
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    return getPreWalkActionFor(S);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    return getPreWalkActionFor(D).Action;
  }

  PreWalkResult<Pattern *> walkToPatternPre(Pattern *P) override {
    return getPreWalkActionFor(P);
  }

  PreWalkAction walkToTypeReprPre(TypeRepr *T) override {
    return getPreWalkActionFor(T).Action;
  }

  /// Retrieve the pre-walk action for a given node, which determines whether
  /// or not it should be walked into.
  template <typename T>
  PreWalkResult<T> getPreWalkActionFor(T Node) {
    // When walking down the tree, we traverse until we have found a node
    // inside the target range. Once we have found such a node, there is no
    // need to traverse any deeper.
    if (FoundTarget)
      return Action::SkipNode(Node);

    // If we haven't found our target yet and the node we are pre-visiting
    // doesn't have a valid range, we still have to traverse it because its
    // subtrees may have valid ranges.
    auto Range = Node->getSourceRange();
    if (Range.isInvalid())
      return Action::Continue(Node);

    // We have found our target if the range of the node we are visiting
    // is contained in the range we are looking for.
    FoundTarget = SM.rangeContains(TargetRange, Range);

    if (FoundTarget) {
      walkToNodePost(Node);
      return Action::SkipNode(Node);
    }

    // Search the subtree if the target range is inside its range.
    if (!SM.rangeContains(Range, TargetRange))
      return Action::SkipNode(Node);

    return Action::Continue(Node);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
    return walkToNodePost(E);
  }

  PostWalkResult<Stmt *> walkToStmtPost(Stmt *S) override {
    return walkToNodePost(S);
  }

  PostWalkAction walkToDeclPost(Decl *D) override {
    return walkToNodePost(D).Action;
  }

  /// Once we have found the target node, look for the innermost ancestor
  /// matching our criteria on the way back up the spine of the tree.
  template <typename T>
  PostWalkResult<T> walkToNodePost(T Node) {
    if (!InnermostMatchingNode.has_value() && Predicate(Node, Parent)) {
      assert(Node->getSourceRange().isInvalid() ||
             SM.rangeContains(Node->getSourceRange(), TargetRange));

      InnermostMatchingNode = Node;
      return Action::Stop();
    }

    return Action::Continue(Node);
  }
};

/// Starting from SearchRoot, finds the innermost node containing ChildRange
/// for which Predicate returns true. Returns None if no such root is found.
static std::optional<ASTNode> findInnermostAncestor(
    SourceRange ChildRange, const SourceManager &SM, ASTNode SearchRoot,
    const InnermostAncestorFinder::MatchPredicate &Predicate) {
  InnermostAncestorFinder Finder(ChildRange, SM, SearchRoot, Predicate);
  return Finder.getInnermostMatchingNode();
}

/// Given a reference range and a declaration context containing the range,
/// attempt to find a declaration containing the reference. This may not
/// be the innermost declaration containing the range.
/// Returns null if no such declaration can be found.
static const Decl *findContainingDeclaration(SourceRange ReferenceRange,
                                             const DeclContext *ReferenceDC,
                                             const SourceManager &SM) {
  auto ContainsReferenceRange = [&](const Decl *D) -> bool {
    if (ReferenceRange.isInvalid())
      return false;

    return SM.rangeContains(D->getSourceRange(), ReferenceRange);
  };

  if (const Decl *D = ReferenceDC->getInnermostDeclarationDeclContext()) {
    // If we have an inner declaration context, see if we can narrow the search
    // down to one of its members. This is important for properties, which don't
    // count as DeclContexts of their own but which can still introduce
    // availability.
    if (auto *IDC = dyn_cast<IterableDeclContext>(D)) {
      auto BestMember = llvm::find_if(IDC->getMembers(),
                                      ContainsReferenceRange);
      if (BestMember != IDC->getMembers().end())
        return *BestMember;
    }
    return D;
  }

  // We couldn't find a suitable node by climbing the DeclContext hierarchy, so
  // fall back to looking for a top-level declaration that contains the
  // reference range. We will hit this case for top-level elements that do not
  // themselves introduce DeclContexts, such as global variables. If we don't
  // have a reference range, there is nothing we can do, so return null.
  if (ReferenceRange.isInvalid())
    return nullptr;

  SourceFile *SF = ReferenceDC->getParentSourceFile();
  if (!SF)
    return nullptr;

  auto BestTopLevelDecl = llvm::find_if(SF->getTopLevelDecls(),
                                        ContainsReferenceRange);
  if (BestTopLevelDecl != SF->getTopLevelDecls().end())
    return *BestTopLevelDecl;

  return nullptr;
}

/// Given a declaration that allows availability attributes in the abstract
/// syntax tree, return the declaration upon which the declaration would
/// appear in concrete syntax. This function is necessary because for semantic
/// analysis, the parser attaches attributes to declarations other
/// than those on which they, concretely, appear. For these declarations (enum
/// cases and variable declarations) a Fix-It for an added availability
/// attribute should be suggested for the appropriate concrete location.
static const Decl *
concreteSyntaxDeclForAvailableAttribute(const Decl *AbstractSyntaxDecl) {
  // This function needs to be kept in sync with its counterpart,
  // abstractSyntaxDeclForAvailableAttribute().

  // The source range for VarDecls does not include 'var ' (and, in any
  // event, multiple variables can be introduced with a single 'var'),
  // so suggest adding an attribute to the PatterningBindingDecl instead.
  if (auto *VD = dyn_cast<VarDecl>(AbstractSyntaxDecl)) {
    if (auto *PBD = VD->getParentPatternBinding())
      return PBD;
  }

  // Similarly suggest applying the Fix-It to the parent enum case rather than
  // the enum element.
  if (auto *EE = dyn_cast<EnumElementDecl>(AbstractSyntaxDecl)) {
    return EE->getParentCase();
  }

  return AbstractSyntaxDecl;
}

/// Given a declaration, return a better related declaration for which
/// to suggest an @available fixit, or the original declaration
/// if no such related declaration exists.
static const Decl *relatedDeclForAvailabilityFixit(const Decl *D) {
  if (auto *accessor = dyn_cast<AccessorDecl>(D)) {
    // Suggest @available Fix-Its on property rather than individual
    // accessors.
    D = accessor->getStorage();
  }

  return abstractSyntaxDeclForAvailableAttribute(D);
}

/// Walk the DeclContext hierarchy starting from D to find a declaration
/// at the member level (i.e., declared in a type context) on which to provide
/// an @available() Fix-It.
static const Decl *ancestorMemberLevelDeclForAvailabilityFixit(const Decl *D) {
  while (D) {
    D = relatedDeclForAvailabilityFixit(D);

    if (!D->isImplicit() && D->getDeclContext()->isTypeContext() &&
        DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::Available, D)) {
      break;
    }

    D = cast_or_null<AbstractFunctionDecl>(
        D->getDeclContext()->getInnermostMethodContext());
  }

  return D;
}

/// Returns true if the declaration is at the type level (either a nominal
/// type, an extension, or a global function) and can support an @available
/// attribute.
static bool isTypeLevelDeclForAvailabilityFixit(const Decl *D) {
  if (!DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::Available, D)) {
    return false;
  }

  if (isa<ExtensionDecl>(D) || isa<NominalTypeDecl>(D)) {
    return true;
  }

  bool IsModuleScopeContext = D->getDeclContext()->isModuleScopeContext();

  // We consider global functions, type aliases, and macros to be "type level"
  if (isa<FuncDecl>(D) || isa<MacroDecl>(D) || isa<TypeAliasDecl>(D)) {
    return IsModuleScopeContext;
  }

  if (auto *VD = dyn_cast<VarDecl>(D)) {
    if (!IsModuleScopeContext)
      return false;

    if (PatternBindingDecl *PBD = VD->getParentPatternBinding()) {
      return PBD->getDeclContext()->isModuleScopeContext();
    }
  }

  return false;
}

/// Walk the DeclContext hierarchy starting from D to find a declaration
/// at a member level (i.e., declared in a type context) on which to provide an
/// @available() Fix-It.
static const Decl *ancestorTypeLevelDeclForAvailabilityFixit(const Decl *D) {
  assert(D);

  D = relatedDeclForAvailabilityFixit(D);

  while (D && !isTypeLevelDeclForAvailabilityFixit(D)) {
    D = D->getDeclContext()->getInnermostDeclarationDeclContext();
  }

  return D;
}

/// Given the range of a reference to an unavailable symbol and the
/// declaration context containing the reference, make a best effort find up to
/// three locations for potential fixits.
///
/// \param FoundVersionCheckNode Returns a node that can be wrapped in a
/// if #available(...) { ... } version check to fix the unavailable reference,
/// or None if such a node cannot be found.
///
/// \param FoundMemberLevelDecl Returns member-level declaration (i.e., the
///  child of a type DeclContext) for which an @available attribute would
/// fix the unavailable reference.
///
/// \param FoundTypeLevelDecl returns a type-level declaration (a
/// a nominal type, an extension, or a global function) for which an
/// @available attribute would fix the unavailable reference.
static void findAvailabilityFixItNodes(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC,
    const SourceManager &SM, std::optional<ASTNode> &FoundVersionCheckNode,
    const Decl *&FoundMemberLevelDecl, const Decl *&FoundTypeLevelDecl) {
  FoundVersionCheckNode = std::nullopt;
  FoundMemberLevelDecl = nullptr;
  FoundTypeLevelDecl = nullptr;

  // Limit tree to search based on the DeclContext of the reference.
  const Decl *DeclarationToSearch =
      findContainingDeclaration(ReferenceRange, ReferenceDC, SM);
  if (!DeclarationToSearch)
    return;

  // Const-cast to inject into ASTNode. This search will not modify
  // the declaration.
  ASTNode SearchRoot = const_cast<Decl *>(DeclarationToSearch);

  // The node to wrap in if #available(...) { ... } is the innermost node in
  // SearchRoot that (1) can be guarded with an if statement and (2)
  // contains the ReferenceRange.
  // We make no guarantee that the Fix-It, when applied, will result in
  // semantically valid code -- but, at a minimum, it should parse. So,
  // for example, we may suggest wrapping a variable declaration in a guard,
  // which would not be valid if the variable is later used. The goal
  // is discoverability of #os() (via the diagnostic and Fix-It) rather than
  // magically fixing the code in all cases.

  InnermostAncestorFinder::MatchPredicate IsGuardable =
      [](ASTNode Node, ASTWalker::ParentTy Parent) {
        if (Expr *ParentExpr = Parent.getAsExpr()) {
          if (!isa<ClosureExpr>(ParentExpr))
            return false;
        } else if (auto *ParentStmt = Parent.getAsStmt()) {
          if (!isa<BraceStmt>(ParentStmt)) {
            return false;
          }
        } else {
          return false;
        }

        return true;
      };

  FoundVersionCheckNode =
      findInnermostAncestor(ReferenceRange, SM, SearchRoot, IsGuardable);

  // Try to find declarations on which @available attributes can be added.
  // The heuristics for finding these declarations are biased towards deeper
  // nodes in the AST to limit the scope of suggested availability regions
  // and provide a better IDE experience (it can get jumpy if Fix-It locations
  // are far away from the error needing the Fix-It).
  if (DeclarationToSearch) {
    FoundMemberLevelDecl =
        ancestorMemberLevelDeclForAvailabilityFixit(DeclarationToSearch);

    FoundTypeLevelDecl =
        ancestorTypeLevelDeclForAvailabilityFixit(DeclarationToSearch);
  }
}

/// Emit a diagnostic note and Fix-It to add an @available attribute
/// on the given declaration for the given version range.
static void
fixAvailabilityForDecl(SourceRange ReferenceRange, const Decl *D,
                       const AvailabilityRange &RequiredAvailability,
                       ASTContext &Context) {
  assert(D);

  // Don't suggest adding an @available() to a declaration where we would
  // emit a diagnostic saying it is not allowed.
  if (TypeChecker::diagnosticIfDeclCannotBePotentiallyUnavailable(D).has_value())
    return;

  if (hasActiveAvailableAttribute(D, Context)) {
    // For QoI, in future should emit a fixit to update the existing attribute.
    return;
  }

  // For some declarations (variables, enum elements), the location in concrete
  // syntax to suggest the Fix-It may differ from the declaration to which
  // we attach availability attributes in the abstract syntax tree during
  // parsing.
  const Decl *ConcDecl = concreteSyntaxDeclForAvailableAttribute(D);

  // To avoid exposing the pattern binding declaration to the user, get the
  // descriptive kind from one of the VarDecls.
  DescriptiveDeclKind KindForDiagnostic = ConcDecl->getDescriptiveKind();
  if (KindForDiagnostic == DescriptiveDeclKind::PatternBinding) {
    KindForDiagnostic = D->getDescriptiveKind();
  }

  SourceLoc InsertLoc =
      ConcDecl->getAttributeInsertionLoc(/*forModifier=*/false);
  if (InsertLoc.isInvalid())
    return;

  StringRef OriginalIndent =
      Lexer::getIndentationForLine(Context.SourceMgr, InsertLoc);
  PlatformKind Target = targetPlatform(Context.LangOpts);

  D->diagnose(diag::availability_add_attribute, KindForDiagnostic)
      .fixItInsert(InsertLoc, diag::insert_available_attr,
                   platformString(Target),
                   RequiredAvailability.getVersionString(), OriginalIndent);
}

/// In the special case of being in an existing, nontrivial availability scope
/// that's close but not quite narrow enough to satisfy requirements
/// (i.e. requirements are contained-in the existing scope but off by a subminor
/// version), emit a diagnostic and fixit that narrows the existing scope
/// condition to the required range.
static bool fixAvailabilityByNarrowingNearbyVersionCheck(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC,
    const AvailabilityRange &RequiredAvailability, ASTContext &Context,
    InFlightDiagnostic &Err) {
  const AvailabilityScope *scope = nullptr;
  (void)TypeChecker::overApproximateAvailabilityAtLocation(ReferenceRange.Start,
                                                           ReferenceDC, &scope);
  if (!scope)
    return false;

  auto ExplicitAvailability = scope->getExplicitAvailabilityRange();
  if (ExplicitAvailability && !RequiredAvailability.isAlwaysAvailable() &&
      scope->getReason() != AvailabilityScope::Reason::Root &&
      RequiredAvailability.isContainedIn(*ExplicitAvailability)) {

    // Only fix situations that are "nearby" versions, meaning
    // disagreement on a minor-or-less version (subminor-or-less version for
    // macOS 10.x.y).
    auto RunningVers = ExplicitAvailability->getRawMinimumVersion();
    auto RequiredVers = RequiredAvailability.getRawMinimumVersion();
    auto Platform = targetPlatform(Context.LangOpts);
    if (RunningVers.getMajor() != RequiredVers.getMajor())
      return false;
    if ((Platform == PlatformKind::macOS ||
         Platform == PlatformKind::macOSApplicationExtension) &&
        RunningVers.getMajor() == 10 &&
        !(RunningVers.getMinor().has_value() &&
          RequiredVers.getMinor().has_value() &&
          RunningVers.getMinor().value() ==
          RequiredVers.getMinor().value()))
      return false;

    auto FixRange = scope->getAvailabilityConditionVersionSourceRange(
        AvailabilityDomain::forPlatform(Platform), RunningVers);
    if (!FixRange.isValid())
      return false;
    // Have found a nontrivial availability scope-introducer to narrow.
    Err.fixItReplace(FixRange, RequiredAvailability.getVersionString());
    return true;
  }
  return false;
}

/// Emit a diagnostic note and Fix-It to add an if #available(...) { } guard
/// that checks for the given version range around the given node.
static void fixAvailabilityByAddingVersionCheck(
    ASTNode NodeToWrap, const AvailabilityRange &RequiredAvailability,
    SourceRange ReferenceRange, ASTContext &Context) {
  // If this is an implicit variable that wraps an expression,
  // let's point to it's initializer. For example, result builder
  // transform captures expressions into implicit variables.
  if (auto *PB =
          dyn_cast_or_null<PatternBindingDecl>(NodeToWrap.dyn_cast<Decl *>())) {
    if (PB->isImplicit() && PB->getSingleVar()) {
      if (auto *init = PB->getInit(0))
        NodeToWrap = init;
    }
  }

  SourceRange RangeToWrap = NodeToWrap.getSourceRange();
  if (RangeToWrap.isInvalid())
    return;

  SourceLoc ReplaceLocStart = RangeToWrap.Start;
  StringRef ExtraIndent;
  StringRef OriginalIndent = Lexer::getIndentationForLine(
      Context.SourceMgr, ReplaceLocStart, &ExtraIndent);

  std::string IfText;
  {
    llvm::raw_string_ostream Out(IfText);

    SourceLoc ReplaceLocEnd =
        Lexer::getLocForEndOfToken(Context.SourceMgr, RangeToWrap.End);

    std::string GuardedText =
        Context.SourceMgr.extractText(CharSourceRange(Context.SourceMgr,
                                                      ReplaceLocStart,
                                                      ReplaceLocEnd)).str();

    std::string NewLine = "\n";
    std::string NewLineReplacement = (NewLine + ExtraIndent).str();

    // Indent the body of the Fix-It if. Because the body may be a compound
    // statement, we may have to indent multiple lines.
    size_t StartAt = 0;
    while ((StartAt = GuardedText.find(NewLine, StartAt)) !=
           std::string::npos) {
      GuardedText.replace(StartAt, NewLine.length(), NewLineReplacement);
      StartAt += NewLine.length();
    }

    PlatformKind Target = targetPlatform(Context.LangOpts);

    // Runtime availability checks that specify app extension platforms don't
    // work, so only suggest checks against the base platform.
    if (auto TargetRemovingAppExtension =
            basePlatformForExtensionPlatform(Target))
      Target = *TargetRemovingAppExtension;

    Out << "if #available(" << platformString(Target) << " "
        << RequiredAvailability.getVersionString() << ", *) {\n";

    Out << OriginalIndent << ExtraIndent << GuardedText << "\n";

    // We emit an empty fallback case with a comment to encourage the developer
    // to think explicitly about whether fallback on earlier versions is needed.
    Out << OriginalIndent << "} else {\n";
    Out << OriginalIndent << ExtraIndent << "// Fallback on earlier versions\n";
    Out << OriginalIndent << "}";
  }

  Context.Diags.diagnose(
      ReferenceRange.Start, diag::availability_guard_with_version_check)
      .fixItReplace(RangeToWrap, IfText);
}

/// Emit suggested Fix-Its for a reference with to an unavailable symbol
/// requiting the given OS version range.
static void fixAvailability(SourceRange ReferenceRange,
                            const DeclContext *ReferenceDC,
                            const AvailabilityRange &RequiredAvailability,
                            ASTContext &Context) {
  if (ReferenceRange.isInvalid())
    return;

  std::optional<ASTNode> NodeToWrapInVersionCheck;
  const Decl *FoundMemberDecl = nullptr;
  const Decl *FoundTypeLevelDecl = nullptr;

  findAvailabilityFixItNodes(ReferenceRange, ReferenceDC, Context.SourceMgr,
                             NodeToWrapInVersionCheck, FoundMemberDecl,
                             FoundTypeLevelDecl);

  // Suggest wrapping in if #available(...) { ... } if possible.
  if (NodeToWrapInVersionCheck.has_value()) {
    fixAvailabilityByAddingVersionCheck(NodeToWrapInVersionCheck.value(),
                                        RequiredAvailability, ReferenceRange,
                                        Context);
  }

  // Suggest adding availability attributes.
  if (FoundMemberDecl) {
    fixAvailabilityForDecl(ReferenceRange, FoundMemberDecl,
                           RequiredAvailability, Context);
  }

  if (FoundTypeLevelDecl) {
    fixAvailabilityForDecl(ReferenceRange, FoundTypeLevelDecl,
                           RequiredAvailability, Context);
  }
}

static void diagnosePotentialUnavailability(
    SourceRange ReferenceRange,
    llvm::function_ref<InFlightDiagnostic(StringRef, llvm::VersionTuple)>
        Diagnose,
    const DeclContext *ReferenceDC, const AvailabilityRange &Availability) {
  ASTContext &Context = ReferenceDC->getASTContext();

  {
    auto Err = Diagnose(Context.getTargetPlatformStringForDiagnostics(),
                        Availability.getRawMinimumVersion());

    // Direct a fixit to the error if an existing guard is nearly-correct
    if (fixAvailabilityByNarrowingNearbyVersionCheck(
            ReferenceRange, ReferenceDC, Availability, Context, Err))
      return;
  }
  fixAvailability(ReferenceRange, ReferenceDC, Availability, Context);
}

bool TypeChecker::checkAvailability(
    SourceRange ReferenceRange, AvailabilityRange RequiredAvailability,
    const DeclContext *ReferenceDC,
    llvm::function_ref<InFlightDiagnostic(StringRef, llvm::VersionTuple)>
        Diagnose) {
  ASTContext &ctx = ReferenceDC->getASTContext();
  if (ctx.LangOpts.DisableAvailabilityChecking)
    return false;

  auto availabilityAtLocation =
      TypeChecker::overApproximateAvailabilityAtLocation(ReferenceRange.Start,
                                                         ReferenceDC);
  if (!availabilityAtLocation.isContainedIn(RequiredAvailability)) {
    diagnosePotentialUnavailability(ReferenceRange, Diagnose, ReferenceDC,
                                    RequiredAvailability);
    return true;
  }

  return false;
}

bool TypeChecker::checkAvailability(SourceRange ReferenceRange,
                                    AvailabilityRange RequiredAvailability,
                                    Diag<StringRef, llvm::VersionTuple> Diag,
                                    const DeclContext *ReferenceDC) {
  auto &Diags = ReferenceDC->getASTContext().Diags;
  return TypeChecker::checkAvailability(
      ReferenceRange, RequiredAvailability, ReferenceDC,
      [&](StringRef platformName, llvm::VersionTuple version) {
        return Diags.diagnose(ReferenceRange.Start, Diag, platformName,
                              version);
      });
}

void TypeChecker::checkConcurrencyAvailability(SourceRange ReferenceRange,
                                               const DeclContext *ReferenceDC) {
  checkAvailability(
      ReferenceRange,
      ReferenceDC->getASTContext().getBackDeployedConcurrencyAvailability(),
      diag::availability_concurrency_only_version_newer,
      ReferenceDC);
}

static bool
requiresDeploymentTargetOrEarlier(const AvailabilityRange &availability,
                                  ASTContext &ctx) {
  auto deploymentTarget = AvailabilityRange::forDeploymentTarget(ctx);
  return deploymentTarget.isContainedIn(availability);
}

/// Returns the diagnostic to emit for the potentially unavailable decl and sets
/// \p IsError accordingly.
static Diagnostic getPotentialUnavailabilityDiagnostic(
    const ValueDecl *D, const DeclContext *ReferenceDC,
    const AvailabilityRange &Availability, bool WarnBeforeDeploymentTarget,
    bool &IsError) {
  ASTContext &Context = ReferenceDC->getASTContext();
  auto Platform = Context.getTargetPlatformStringForDiagnostics();

  if (requiresDeploymentTargetOrEarlier(Availability, Context)) {
    // The required OS version is at or before the deployment target so this
    // diagnostic should indicate that the decl could be unavailable to clients
    // of the module containing the reference.
    IsError = !WarnBeforeDeploymentTarget;

    return Diagnostic(
        IsError ? diag::availability_decl_only_version_newer_for_clients
                : diag::availability_decl_only_version_newer_for_clients_warn,
        D, Platform, Availability.getRawMinimumVersion(),
        ReferenceDC->getParentModule());
  }

  IsError = true;
  return Diagnostic(diag::availability_decl_only_version_newer, D, Platform,
                    Availability.getRawMinimumVersion());
}

// Emits a diagnostic for a reference to a declaration that is potentially
// unavailable at the given source location. Returns true if an error diagnostic
// was emitted.
static bool
diagnosePotentialUnavailability(const ValueDecl *D, SourceRange ReferenceRange,
                                const DeclContext *ReferenceDC,
                                const AvailabilityRange &Availability,
                                bool WarnBeforeDeploymentTarget = false) {
  ASTContext &Context = ReferenceDC->getASTContext();
  if (Context.LangOpts.DisableAvailabilityChecking)
    return false;

  bool IsError;
  {
    auto Diag = Context.Diags.diagnose(
        ReferenceRange.Start,
        getPotentialUnavailabilityDiagnostic(
            D, ReferenceDC, Availability, WarnBeforeDeploymentTarget, IsError));

    // Direct a fixit to the error if an existing guard is nearly-correct
    if (fixAvailabilityByNarrowingNearbyVersionCheck(
            ReferenceRange, ReferenceDC, Availability, Context, Diag))
      return IsError;
  }

  fixAvailability(ReferenceRange, ReferenceDC, Availability, Context);
  return IsError;
}

/// Emits a diagnostic for a reference to a storage accessor that is
/// potentially unavailable.
static void diagnosePotentialAccessorUnavailability(
    const AccessorDecl *Accessor, SourceRange ReferenceRange,
    const DeclContext *ReferenceDC, const AvailabilityRange &Availability,
    bool ForInout) {
  ASTContext &Context = ReferenceDC->getASTContext();

  assert(Accessor->isGetterOrSetter());

  auto &diag = ForInout ? diag::availability_inout_accessor_only_version_newer
                        : diag::availability_decl_only_version_newer;

  {
    auto Err = Context.Diags.diagnose(
        ReferenceRange.Start, diag, Accessor,
        Context.getTargetPlatformStringForDiagnostics(),
        Availability.getRawMinimumVersion());

    // Direct a fixit to the error if an existing guard is nearly-correct
    if (fixAvailabilityByNarrowingNearbyVersionCheck(
            ReferenceRange, ReferenceDC, Availability, Context, Err))
      return;
  }

  fixAvailability(ReferenceRange, ReferenceDC, Availability, Context);
}

static DiagnosticBehavior
behaviorLimitForExplicitUnavailability(
    const RootProtocolConformance *rootConf,
    const DeclContext *fromDC) {
  auto protoDecl = rootConf->getProtocol();

  // Soften errors about unavailable `Sendable` conformances depending on the
  // concurrency checking mode.
  if (protoDecl->isSpecificProtocol(KnownProtocolKind::Sendable)) {
    SendableCheckContext checkContext(fromDC);
    if (auto nominal = rootConf->getType()->getAnyNominal())
      return checkContext.diagnosticBehavior(nominal);

    return checkContext.defaultDiagnosticBehavior();
  }

  return DiagnosticBehavior::Unspecified;
}

/// Emits a diagnostic for a protocol conformance that is potentially
/// unavailable at the given source location.
static bool
diagnosePotentialUnavailability(const RootProtocolConformance *rootConf,
                                const ExtensionDecl *ext, SourceLoc loc,
                                const DeclContext *dc,
                                const AvailabilityRange &availability) {
  ASTContext &ctx = dc->getASTContext();
  if (ctx.LangOpts.DisableAvailabilityChecking)
    return false;

  {
    auto type = rootConf->getType();
    auto proto = rootConf->getProtocol()->getDeclaredInterfaceType();
    auto err = ctx.Diags.diagnose(
        loc, diag::conformance_availability_only_version_newer, type, proto,
        ctx.getTargetPlatformStringForDiagnostics(),
        availability.getRawMinimumVersion());

    auto behaviorLimit = behaviorLimitForExplicitUnavailability(rootConf, dc);
    if (behaviorLimit >= DiagnosticBehavior::Warning)
      err.limitBehavior(behaviorLimit);
    else
      err.warnUntilSwiftVersion(6);

    // Direct a fixit to the error if an existing guard is nearly-correct
    if (fixAvailabilityByNarrowingNearbyVersionCheck(loc, dc, availability, ctx,
                                                     err))
      return true;
  }

  fixAvailability(loc, dc, availability, ctx);
  return true;
}

/// Returns the availability attribute indicating deprecation of the
/// declaration is deprecated or null otherwise.
static std::optional<SemanticAvailableAttr> getDeprecated(const Decl *D) {
  auto &Ctx = D->getASTContext();
  if (auto Attr = D->getDeprecatedAttr())
    return Attr;

  if (Ctx.LangOpts.WarnSoftDeprecated) {
    // When -warn-soft-deprecated is specified, treat any declaration that is
    // deprecated in the future as deprecated.
    if (auto Attr = D->getSoftDeprecatedAttr())
      return Attr;
  }

  // Treat extensions methods as deprecated if their extension
  // is deprecated.
  DeclContext *DC = D->getDeclContext();
  if (auto *ED = dyn_cast<ExtensionDecl>(DC)) {
    return getDeprecated(ED);
  }

  return std::nullopt;
}

static void fixItAvailableAttrRename(InFlightDiagnostic &diag,
                                     SourceRange referenceRange,
                                     const ValueDecl *renamedDecl,
                                     StringRef newName, const Expr *call) {
  if (isa<AccessorDecl>(renamedDecl))
    return;

  ParsedDeclName parsed = swift::parseDeclName(newName);
  if (!parsed)
    return;

  bool originallyWasKnownOperatorExpr = false;
  if (call) {
    originallyWasKnownOperatorExpr =
        isa<BinaryExpr>(call) ||
        isa<PrefixUnaryExpr>(call) ||
        isa<PostfixUnaryExpr>(call);
  }
  if (parsed.isOperator() != originallyWasKnownOperatorExpr)
    return;

  auto &ctx = renamedDecl->getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;
  if (parsed.isInstanceMember()) {
    auto *CE = dyn_cast_or_null<CallExpr>(call);
    if (!CE)
      return;

    // Replace the base of the call with the "self argument".
    // We can only do a good job with the fix-it if we have the whole call
    // expression.
    // FIXME: Should we be validating the ContextName in some way?
    unsigned selfIndex = parsed.SelfIndex.value();
    const Expr *selfExpr = nullptr;
    SourceLoc removeRangeStart;
    SourceLoc removeRangeEnd;

    auto *originalArgs = CE->getArgs()->getOriginalArgs();
    size_t numElementsWithinParens = originalArgs->size();
    numElementsWithinParens -= originalArgs->getNumTrailingClosures();
    if (selfIndex >= numElementsWithinParens)
      return;

    if (parsed.IsGetter) {
      if (numElementsWithinParens != 1)
        return;
    } else if (parsed.IsSetter) {
      if (numElementsWithinParens != 2)
        return;
    } else {
      if (parsed.ArgumentLabels.size() != originalArgs->size() - 1)
        return;
    }

    selfExpr = originalArgs->getExpr(selfIndex);

    if (selfIndex + 1 == numElementsWithinParens) {
      if (selfIndex > 0) {
        // Remove from the previous comma to the close-paren (half-open).
        removeRangeStart = originalArgs->getExpr(selfIndex - 1)->getEndLoc();
        removeRangeStart = Lexer::getLocForEndOfToken(sourceMgr,
                                                      removeRangeStart);
      } else {
        // Remove from after the open paren to the close paren (half-open).
        removeRangeStart =
            Lexer::getLocForEndOfToken(sourceMgr, originalArgs->getStartLoc());
      }

      // Prefer the r-paren location, so that we get the right behavior when
      // there's a trailing closure, but handle some implicit cases too.
      removeRangeEnd = originalArgs->getRParenLoc();
      if (removeRangeEnd.isInvalid())
        removeRangeEnd = originalArgs->getEndLoc();

    } else {
      // Remove from the label to the start of the next argument (half-open).
      SourceLoc labelLoc = originalArgs->getLabelLoc(selfIndex);
      if (labelLoc.isValid())
        removeRangeStart = labelLoc;
      else
        removeRangeStart = selfExpr->getStartLoc();

      SourceLoc nextLabelLoc = originalArgs->getLabelLoc(selfIndex + 1);
      if (nextLabelLoc.isValid())
        removeRangeEnd = nextLabelLoc;
      else
        removeRangeEnd = originalArgs->getExpr(selfIndex + 1)->getStartLoc();
    }

    // Avoid later argument label fix-its for this argument.
    if (!parsed.isPropertyAccessor()) {
      Identifier oldLabel = originalArgs->getLabel(selfIndex);
      StringRef oldLabelStr;
      if (!oldLabel.empty())
        oldLabelStr = oldLabel.str();
      parsed.ArgumentLabels.insert(parsed.ArgumentLabels.begin() + selfIndex,
                                    oldLabelStr);
    }

    if (auto *inoutSelf = dyn_cast<InOutExpr>(selfExpr))
      selfExpr = inoutSelf->getSubExpr();

    CharSourceRange selfExprRange =
        Lexer::getCharSourceRangeFromSourceRange(sourceMgr,
                                                 selfExpr->getSourceRange());
    bool needsParens = !selfExpr->canAppendPostfixExpression();

    SmallString<64> selfReplace;
    if (needsParens)
      selfReplace.push_back('(');

    // If the base is contextual member lookup and we know the type,
    // let's just prepend it, otherwise we'll end up with an incorrect fix-it.
    auto base = sourceMgr.extractText(selfExprRange);
    if (!base.empty() && base.front() == '.') {
      // If this is not a rename, let's not
      // even try to emit a fix-it because
      // it's going to be invalid.
      if (newName.empty())
        return;

      auto parts = newName.split('.');
      auto nominalName = parts.first;
      assert(!nominalName.empty());

      selfReplace += nominalName;
    }

    selfReplace += base;
    if (needsParens)
      selfReplace.push_back(')');

    selfReplace.push_back('.');
    selfReplace += parsed.BaseName;

    diag.fixItReplace(CE->getFn()->getSourceRange(), selfReplace);

    if (!parsed.isPropertyAccessor())
      diag.fixItRemoveChars(removeRangeStart, removeRangeEnd);

    // Continue on to diagnose any argument label renames.

  } else if (parsed.BaseName == "init" && isa_and_nonnull<CallExpr>(call)) {
    auto *CE = cast<CallExpr>(call);

    // If it is a call to an initializer (rather than a first-class reference):

    if (parsed.isMember()) {
      // replace with a "call" to the type (instead of writing `.init`)
      diag.fixItReplace(CE->getFn()->getSourceRange(), parsed.ContextName);
    } else if (auto *dotCall = dyn_cast<DotSyntaxCallExpr>(CE->getFn())) {
      // if it's a dot call, and the left side is a type (and not `self` or 
      // `super`, for example), just remove the dot and the right side, again 
      // in order to make it a "call" to the type
      if (isa<TypeExpr>(dotCall->getBase())) {
        SourceLoc removeLoc = dotCall->getDotLoc();
        if (removeLoc.isInvalid())
          return;

        diag.fixItRemove(SourceRange(removeLoc, dotCall->getFn()->getEndLoc()));
      }
    } else if (!isa<ConstructorRefCallExpr>(CE->getFn())) {
      return;
    }

    // Continue on to diagnose any constructor argument label renames.

  } else if (parsed.IsSubscript) {
    if (auto *CE = dyn_cast_or_null<CallExpr>(call)) {
      // Renaming from CallExpr to SubscriptExpr. Remove function name and
      // replace parens with square brackets.

      if (auto *DSCE = dyn_cast<DotSyntaxCallExpr>(CE->getFn())) {
        if (DSCE->getBase()->isImplicit()) {
          // If self is implicit, self must be inserted before subscript syntax.
          diag.fixItInsert(CE->getStartLoc(), "self");
        }
      }

      diag.fixItReplace(CE->getFn()->getEndLoc(), "[");
      diag.fixItReplace(CE->getEndLoc(), "]");
    }
  } else {
    // Just replace the base name.
    SmallString<64> baseReplace;

    if (!parsed.ContextName.empty()) {
      baseReplace += parsed.ContextName;
      baseReplace += '.';
    }
    baseReplace += parsed.BaseName;

    if (parsed.IsFunctionName && isa_and_nonnull<SubscriptExpr>(call)) {
      auto *SE = cast<SubscriptExpr>(call);

      // Renaming from SubscriptExpr to CallExpr. Insert function name and
      // replace square brackets with parens.
      diag.fixItReplace(SE->getArgs()->getStartLoc(),
                        ("." + baseReplace.str() + "(").str());
      diag.fixItReplace(SE->getEndLoc(), ")");
    } else {
      bool shouldEmitRenameFixit = true;
      if (auto *CE = dyn_cast_or_null<CallExpr>(call)) {
        SmallString<64> callContextName;
        llvm::raw_svector_ostream name(callContextName);
        if (auto *DCE = dyn_cast<DotSyntaxCallExpr>(CE->getDirectCallee())) {
          if (auto *TE = dyn_cast<TypeExpr>(DCE->getBase())) {
            TE->getTypeRepr()->print(name);
            if (!parsed.ContextName.empty()) {
              // If there is a context in rename function e.g.
              // `Context.function()` and call context is a `DotSyntaxCallExpr`
              // adjust the range so it replaces the base as well.
              referenceRange =
                  SourceRange(TE->getStartLoc(), referenceRange.End);
            }
          }
        }
        // Function names are the same (including context if applicable), so
        // renaming fix-it doesn't need do be produced.
        auto calledValue = CE->getCalledValue(/*skipFunctionConversions=*/true);
        if ((parsed.ContextName.empty() ||
             parsed.ContextName == callContextName) &&
            calledValue && calledValue->getBaseName() == parsed.BaseName) {
          shouldEmitRenameFixit = false;
        }
      }
      if (shouldEmitRenameFixit) {
        if (parsed.IsFunctionName && parsed.ArgumentLabels.empty() &&
            isa<VarDecl>(renamedDecl)) {
          // If we're going from a var to a function with no arguments, emit an
          // empty parameter list.
          baseReplace += "()";
        }

        diag.fixItReplace(referenceRange, baseReplace);
      }
    }
  }

  if (!call || !call->getArgs())
    return;

  auto *originalArgs = call->getArgs()->getOriginalArgs();
  if (parsed.IsGetter) {
    diag.fixItRemove(originalArgs->getSourceRange());
    return;
  }

  if (parsed.IsSetter) {
    const Expr *newValueExpr = nullptr;

    if (originalArgs->size() >= 1) {
      size_t newValueIndex = 0;
      if (parsed.isInstanceMember()) {
        assert(parsed.SelfIndex.value() == 0 ||
               parsed.SelfIndex.value() == 1);
        newValueIndex = !parsed.SelfIndex.value();
      }
      newValueExpr = originalArgs->getExpr(newValueIndex);
    } else {
      newValueExpr = originalArgs->getExpr(0);
    }

    diag.fixItReplaceChars(originalArgs->getStartLoc(),
                           newValueExpr->getStartLoc(), " = ");
    diag.fixItRemoveChars(
        Lexer::getLocForEndOfToken(sourceMgr, newValueExpr->getEndLoc()),
        Lexer::getLocForEndOfToken(sourceMgr, originalArgs->getEndLoc()));
    return;
  }

  if (!parsed.IsFunctionName)
    return;

  SmallVector<Identifier, 4> argumentLabelIDs;
  llvm::transform(parsed.ArgumentLabels, std::back_inserter(argumentLabelIDs),
                  [&ctx](StringRef labelStr) -> Identifier {
                    return labelStr.empty() ? Identifier()
                                            : ctx.getIdentifier(labelStr);
                  });

  // Coerce the `argumentLabelIDs` to the user supplied arguments.
  // e.g:
  //   @available(.., renamed: "new(w:x:y:z:)")
  //   func old(a: Int, b: Int..., c: String="", d: Int=0){}
  //   old(a: 1, b: 2, 3, 4, d: 5)
  // coerce
  //   argumentLabelIDs = {"w", "x", "y", "z"}
  // to
  //   argumentLabelIDs = {"w", "x", "", "", "z"}
  auto I = argumentLabelIDs.begin();

  auto updateLabelsForArg = [&](Expr *expr) -> bool {
    if (I == argumentLabelIDs.end())
      return true;

    if (isa<DefaultArgumentExpr>(expr)) {
      // Defaulted: remove param label of it.
      I = argumentLabelIDs.erase(I);
      return false;
    }

    if (auto *varargExpr = dyn_cast<VarargExpansionExpr>(expr)) {
      if (auto *arrayExpr = dyn_cast<ArrayExpr>(varargExpr->getSubExpr())) {
        auto variadicArgsNum = arrayExpr->getNumElements();
        if (variadicArgsNum == 0) {
          // No arguments: Remove param label of it.
          I = argumentLabelIDs.erase(I);
        } else if (variadicArgsNum == 1) {
          // One argument: Just advance.
          ++I;
        } else {
          ++I;

          // Two or more arguments: Insert empty labels after the first one.
          --variadicArgsNum;
          I = argumentLabelIDs.insert(I, variadicArgsNum, Identifier());
          I += variadicArgsNum;
        }
        return false;
      }
    }

    // Normal: Just advance.
    ++I;
    return false;
  };

  for (auto arg : *call->getArgs()) {
    if (updateLabelsForArg(arg.getExpr()))
      return;
  }

  if (argumentLabelIDs.size() != originalArgs->size()) {
    // Mismatched lengths; give up.
    return;
  }

  // If any of the argument labels are mismatched, perform label correction.
  for (auto i : indices(*originalArgs)) {
    // The argument label of an unlabeled trailing closure is ignored.
    if (originalArgs->isUnlabeledTrailingClosureIndex(i))
      continue;
    if (argumentLabelIDs[i] != originalArgs->getLabel(i)) {
      auto paramContext = parsed.IsSubscript ? ParameterContext::Subscript
                                             : ParameterContext::Call;
      diagnoseArgumentLabelError(ctx, originalArgs, argumentLabelIDs,
                                 paramContext, &diag);
      return;
    }
  }
}

// Must be kept in sync with diag::availability_decl_unavailable_rename and
// others.
namespace {
  enum class ReplacementDeclKind : unsigned {
    None,
    InstanceMethod,
    Property,
  };
} // end anonymous namespace

static std::optional<ReplacementDeclKind>
describeRename(ASTContext &ctx, StringRef newName, const ValueDecl *D,
               SmallVectorImpl<char> &nameBuf) {
  ParsedDeclName parsed = swift::parseDeclName(newName);
  if (!parsed)
    return std::nullopt;

  // Only produce special descriptions for renames to
  // - instance members
  // - properties (or global bindings)
  // - class/static methods
  // - initializers, unless the original was known to be an initializer
  // Leave non-member renames alone, as well as renames from top-level types
  // and bindings to member types and class/static properties.
  if (!(parsed.isInstanceMember() || parsed.isPropertyAccessor() ||
        (parsed.isMember() && parsed.IsFunctionName) ||
        (parsed.BaseName == "init" &&
         !dyn_cast_or_null<ConstructorDecl>(D)))) {
    return std::nullopt;
  }

  llvm::raw_svector_ostream name(nameBuf);

  if (!parsed.ContextName.empty())
    name << parsed.ContextName << '.';

  if (parsed.IsFunctionName) {
    name << parsed.formDeclName(ctx, (D && isa<SubscriptDecl>(D)));
  } else {
    name << parsed.BaseName;
  }

  if (parsed.isMember() && parsed.isPropertyAccessor())
    return ReplacementDeclKind::Property;
  if (parsed.isInstanceMember() && parsed.IsFunctionName)
    return ReplacementDeclKind::InstanceMethod;

  // We don't have enough information.
  return ReplacementDeclKind::None;
}

/// Emits a diagnostic for a reference to a declaration that is deprecated.
static void diagnoseIfDeprecated(SourceRange ReferenceRange,
                                 const ExportContext &Where,
                                 const ValueDecl *DeprecatedDecl,
                                 const Expr *Call) {
  auto Attr = getDeprecated(DeprecatedDecl);
  if (!Attr)
    return;

  // We match the behavior of clang to not report deprecation warnings
  // inside declarations that are themselves deprecated on all deployment
  // targets.
  if (Where.isDeprecated()) {
    return;
  }

  auto *ReferenceDC = Where.getDeclContext();
  auto &Context = ReferenceDC->getASTContext();
  if (!Context.LangOpts.DisableAvailabilityChecking) {
    AvailabilityRange RunningOSVersions = Where.getAvailabilityRange();
    if (RunningOSVersions.isKnownUnreachable()) {
      // Suppress a deprecation warning if the availability checking machinery
      // thinks the reference program location will not execute on any
      // deployment target for the current platform.
      return;
    }
  }

  if (shouldIgnoreDeprecationOfConcurrencyDecl(DeprecatedDecl, ReferenceDC))
    return;

  StringRef Platform = Attr->getDomain().getNameForDiagnostics();
  llvm::VersionTuple DeprecatedVersion;
  if (Attr->getDeprecated())
    DeprecatedVersion = Attr->getDeprecated().value();

  auto Message = Attr->getMessage();
  auto NewName = Attr->getRename();
  if (Message.empty() && NewName.empty()) {
    Context.Diags
        .diagnose(ReferenceRange.Start, diag::availability_deprecated,
                  DeprecatedDecl, Attr->isPlatformSpecific(), Platform,
                  Attr->getDeprecated().has_value(), DeprecatedVersion,
                  /*message*/ StringRef())
        .highlight(Attr->getParsedAttr()->getRange());
    return;
  }

  // FIXME: [availability] Remap before emitting diagnostic above.
  llvm::VersionTuple RemappedDeprecatedVersion;
  if (AvailabilityInference::updateDeprecatedPlatformForFallback(
          *Attr, Context, Platform, RemappedDeprecatedVersion))
    DeprecatedVersion = RemappedDeprecatedVersion;

  SmallString<32> newNameBuf;
  std::optional<ReplacementDeclKind> replacementDeclKind =
      describeRename(Context, NewName, /*decl*/ nullptr, newNameBuf);
  StringRef newName = replacementDeclKind ? newNameBuf.str() : NewName;

  if (!Message.empty()) {
    EncodedDiagnosticMessage EncodedMessage(Message);
    Context.Diags
        .diagnose(ReferenceRange.Start, diag::availability_deprecated,
                  DeprecatedDecl, Attr->isPlatformSpecific(), Platform,
                  Attr->getDeprecated().has_value(), DeprecatedVersion,
                  EncodedMessage.Message)
        .highlight(Attr->getParsedAttr()->getRange());
  } else {
    unsigned rawReplaceKind = static_cast<unsigned>(
        replacementDeclKind.value_or(ReplacementDeclKind::None));
    Context.Diags
        .diagnose(ReferenceRange.Start, diag::availability_deprecated_rename,
                  DeprecatedDecl, Attr->isPlatformSpecific(), Platform,
                  Attr->getDeprecated().has_value(), DeprecatedVersion,
                  replacementDeclKind.has_value(), rawReplaceKind, newName)
        .highlight(Attr->getParsedAttr()->getRange());
  }

  if (!NewName.empty() && !isa<AccessorDecl>(DeprecatedDecl)) {
    auto renameDiag = Context.Diags.diagnose(
                               ReferenceRange.Start,
                               diag::note_deprecated_rename,
                               newName);
    fixItAvailableAttrRename(renameDiag, ReferenceRange, DeprecatedDecl,
                             NewName, Call);
  }
}

/// Emits a diagnostic for a reference to a conformance that is deprecated.
static bool diagnoseIfDeprecated(SourceLoc loc,
                                 const RootProtocolConformance *rootConf,
                                 const ExtensionDecl *ext,
                                 const ExportContext &where) {
  auto attr = getDeprecated(ext);
  if (!attr)
    return false;

  // We match the behavior of clang to not report deprecation warnings
  // inside declarations that are themselves deprecated on all deployment
  // targets.
  if (where.isDeprecated()) {
    return false;
  }

  auto *dc = where.getDeclContext();
  auto &ctx = dc->getASTContext();
  if (!ctx.LangOpts.DisableAvailabilityChecking) {
    AvailabilityRange runningOSVersion = where.getAvailabilityRange();
    if (runningOSVersion.isKnownUnreachable()) {
      // Suppress a deprecation warning if the availability checking machinery
      // thinks the reference program location will not execute on any
      // deployment target for the current platform.
      return false;
    }
  }

  auto type = rootConf->getType();
  auto proto = rootConf->getProtocol()->getDeclaredInterfaceType();

  StringRef platform = attr->getDomain().getNameForDiagnostics();
  llvm::VersionTuple deprecatedVersion;
  if (attr->getDeprecated())
    deprecatedVersion = attr->getDeprecated().value();

  llvm::VersionTuple remappedDeprecatedVersion;
  if (AvailabilityInference::updateDeprecatedPlatformForFallback(
          *attr, ctx, platform, remappedDeprecatedVersion))
    deprecatedVersion = remappedDeprecatedVersion;

  auto message = attr->getMessage();
  if (message.empty()) {
    ctx.Diags
        .diagnose(loc, diag::conformance_availability_deprecated, type, proto,
                  attr->isPlatformSpecific(), platform,
                  attr->getDeprecated().has_value(), deprecatedVersion,
                  /*message*/ StringRef())
        .highlight(attr->getParsedAttr()->getRange());
    return true;
  }

  EncodedDiagnosticMessage encodedMessage(message);
  ctx.Diags
      .diagnose(loc, diag::conformance_availability_deprecated, type, proto,
                attr->isPlatformSpecific(), platform,
                attr->getDeprecated().has_value(), deprecatedVersion,
                encodedMessage.Message)
      .highlight(attr->getParsedAttr()->getRange());
  return true;
}

void swift::diagnoseOverrideOfUnavailableDecl(ValueDecl *override,
                                              const ValueDecl *base,
                                              SemanticAvailableAttr attr) {
  ASTContext &ctx = override->getASTContext();
  auto &diags = ctx.Diags;
  if (attr.getRename().empty()) {
    EncodedDiagnosticMessage EncodedMessage(attr.getMessage());
    diags.diagnose(override, diag::override_unavailable,
                   override->getBaseName(), EncodedMessage.Message);

    diags.diagnose(base, diag::availability_marked_unavailable, base);
    return;
  }

  // FIXME: [availability] Take an unsatisfied constraint as input instead of
  // recomputing it.
  ExportContext where = ExportContext::forDeclSignature(override);
  auto constraint =
      getAvailabilityConstraintsForDecl(base, where.getAvailability())
          .getPrimaryConstraint();
  if (!constraint)
    return;

  diagnoseExplicitUnavailability(
      base, override->getLoc(), *constraint, where,
      /*Flags*/ std::nullopt,
      [&override, &ctx](InFlightDiagnostic &diag, StringRef rename) {
        ParsedDeclName parsedName = parseDeclName(rename);
        if (!parsedName || parsedName.isPropertyAccessor() ||
            parsedName.isMember() || parsedName.isOperator()) {
          return;
        }

        // Only initializers should be named 'init'.
        if (isa<ConstructorDecl>(override) ^ (parsedName.BaseName == "init")) {
          return;
        }

        if (!parsedName.IsFunctionName) {
          diag.fixItReplace(override->getNameLoc(), parsedName.BaseName);
          return;
        }

        DeclName newName = parsedName.formDeclName(ctx);
        size_t numArgs = override->getName().getArgumentNames().size();
        if (!newName || newName.getArgumentNames().size() != numArgs)
          return;

        fixDeclarationName(diag, override, newName);
      });
}

/// Emit a diagnostic for references to declarations that have been
/// marked as unavailable, either through "unavailable" or "obsoleted:".
static bool diagnoseExplicitUnavailability(
    const ValueDecl *D, SourceRange R, const AvailabilityConstraint &constraint,
    const ExportContext &Where, const Expr *call, DeclAvailabilityFlags Flags) {
  return diagnoseExplicitUnavailability(
      D, R, constraint, Where, Flags,
      [=](InFlightDiagnostic &diag, StringRef rename) {
        fixItAvailableAttrRename(diag, R, D, rename, call);
      });
}

bool shouldHideDomainNameForConstraintDiagnostic(
    const AvailabilityConstraint &constraint) {
  switch (constraint.getDomain().getKind()) {
  case AvailabilityDomain::Kind::Universal:
  case AvailabilityDomain::Kind::Embedded:
  case AvailabilityDomain::Kind::Custom:
  case AvailabilityDomain::Kind::PackageDescription:
    return true;
  case AvailabilityDomain::Kind::Platform:
    return false;
  case AvailabilityDomain::Kind::SwiftLanguage:
    switch (constraint.getReason()) {
    case AvailabilityConstraint::Reason::UnconditionallyUnavailable:
    case AvailabilityConstraint::Reason::IntroducedInLaterVersion:
      return false;
    case AvailabilityConstraint::Reason::IntroducedInLaterDynamicVersion:
    case AvailabilityConstraint::Reason::Obsoleted:
      return true;
    }
  }
}

bool diagnoseExplicitUnavailability(SourceLoc loc,
                                    const AvailabilityConstraint &constraint,
                                    const RootProtocolConformance *rootConf,
                                    const ExtensionDecl *ext,
                                    const ExportContext &where,
                                    bool warnIfConformanceUnavailablePreSwift6,
                                    bool preconcurrency) {
  if (!constraint.isUnavailable())
    return false;

  // Invertible protocols are never unavailable.
  if (rootConf->getProtocol()->getInvertibleProtocolKind())
    return false;

  ASTContext &ctx = ext->getASTContext();
  auto &diags = ctx.Diags;

  auto type = rootConf->getType();
  auto proto = rootConf->getProtocol()->getDeclaredInterfaceType();
  auto domain = constraint.getDomain();
  StringRef versionedPlatform = domain.getNameForDiagnostics();
  StringRef platform = shouldHideDomainNameForConstraintDiagnostic(constraint)
                           ? ""
                           : versionedPlatform;
  auto attr = constraint.getAttr();

  // Downgrade unavailable Sendable conformance diagnostics where
  // appropriate.
  auto behavior =
      behaviorLimitForExplicitUnavailability(rootConf, where.getDeclContext());

  EncodedDiagnosticMessage EncodedMessage(attr.getMessage());
  diags
      .diagnose(loc, diag::conformance_availability_unavailable, type, proto,
                platform.empty(), platform, EncodedMessage.Message)
      .limitBehaviorWithPreconcurrency(behavior, preconcurrency)
      .warnUntilSwiftVersionIf(warnIfConformanceUnavailablePreSwift6, 6);

  switch (constraint.getReason()) {
  case AvailabilityConstraint::Reason::UnconditionallyUnavailable:
    diags
        .diagnose(ext, diag::conformance_availability_marked_unavailable, type,
                  proto)
        .highlight(attr.getParsedAttr()->getRange());
    break;
  case AvailabilityConstraint::Reason::IntroducedInLaterVersion:
    diags.diagnose(ext, diag::conformance_availability_introduced_in_version,
                   type, proto, versionedPlatform, *attr.getIntroduced());
    break;
  case AvailabilityConstraint::Reason::Obsoleted:
    diags
        .diagnose(ext, diag::conformance_availability_obsoleted, type, proto,
                  versionedPlatform, *attr.getObsoleted())
        .highlight(attr.getParsedAttr()->getRange());
    break;
  case AvailabilityConstraint::Reason::IntroducedInLaterDynamicVersion:
    llvm_unreachable("unexpected constraint");
  }
  return true;
}

std::optional<AvailabilityConstraint>
swift::getUnsatisfiedAvailabilityConstraint(const Decl *decl,
                                            const DeclContext *referenceDC,
                                            SourceLoc referenceLoc) {
  return getAvailabilityConstraintsForDecl(
             decl,
             TypeChecker::availabilityAtLocation(referenceLoc, referenceDC))
      .getPrimaryConstraint();
}

/// Check if this is a subscript declaration inside String or
/// Substring that returns String, and if so return true.
bool isSubscriptReturningString(const ValueDecl *D, ASTContext &Context) {
  // Is this a subscript?
  if (!isa<SubscriptDecl>(D))
    return false;

  // Is the subscript declared in String or Substring?
  auto *declContext = D->getDeclContext();
  assert(declContext && "Expected decl context!");

  auto *stringDecl = Context.getStringDecl();
  auto *substringDecl = Context.getSubstringDecl();

  auto *typeDecl = declContext->getSelfNominalTypeDecl();
  if (!typeDecl)
    return false;

  if (typeDecl != stringDecl && typeDecl != substringDecl)
    return false;

  // Is the subscript index one we want to emit a special diagnostic
  // for, and the return type String?
  auto fnTy = D->getInterfaceType()->getAs<AnyFunctionType>();
  assert(fnTy && "Expected function type for subscript decl!");

  // We're only going to warn for BoundGenericStructType with a single
  // type argument that is not Int!
  auto params = fnTy->getParams();
  if (params.size() != 1)
    return false;

  const auto &param = params.front();
  if (param.hasLabel() || param.isVariadic() || param.isInOut())
    return false;

  auto inputTy = param.getPlainType()->getAs<BoundGenericStructType>();
  if (!inputTy)
    return false;

  auto genericArgs = inputTy->getGenericArgs();
  if (genericArgs.size() != 1)
    return false;

  // The subscripts taking T<Int> do not return Substring, and our
  // special fixit does not help here.
  auto nominalTypeParam = genericArgs[0]->getAs<NominalType>();
  if (!nominalTypeParam)
    return false;

  if (nominalTypeParam->isInt())
    return false;

  auto resultTy = fnTy->getResult()->getAs<NominalType>();
  if (!resultTy)
    return false;

  return resultTy->isString();
}

static bool diagnoseParameterizedProtocolAvailability(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC) {
  return TypeChecker::checkAvailability(
      ReferenceRange,
      ReferenceDC->getASTContext().getParameterizedExistentialAvailability(),
      diag::availability_parameterized_protocol_only_version_newer,
      ReferenceDC);
}

static bool diagnoseIsolatedAnyAvailability(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC) {
  return TypeChecker::checkAvailability(
      ReferenceRange,
      ReferenceDC->getASTContext().getIsolatedAnyAvailability(),
      diag::availability_isolated_any_only_version_newer,
      ReferenceDC);
}

static bool diagnoseTypedThrowsAvailability(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC) {
  return TypeChecker::checkAvailability(
      ReferenceRange,
      ReferenceDC->getASTContext().getTypedThrowsAvailability(),
      diag::availability_typed_throws_only_version_newer,
      ReferenceDC);
}

/// Make sure the generic arguments conform to all known invertible protocols.
/// Runtimes prior to NoncopyableGenerics do not check if any of the
/// generic arguments conform to Copyable/Escapable during dynamic casts.
/// But a dynamic cast *needs* to check if the generic arguments conform,
/// to determine if the cast should be permitted at all. For example:
///
///    struct X<T> {}
///    extension X: P where T: Y {}
///
///     func f<Y: ~Copyable>(...) {
///       let x: X<Y> = ...
///       _ = x as? any P   // <- cast should fail
///     }
///
/// The dynamic cast here must fail because Y does not conform to Copyable,
/// thus X<Y> doesn't conform to P!
///
/// \param boundTy The generic type with its generic arguments.
/// \returns the invertible protocol for which a conformance is missing in
///          one of the generic arguments, or none if all are present for
///          every generic argument.
static std::optional<InvertibleProtocolKind> checkGenericArgsForInvertibleReqs(
    BoundGenericType *boundTy) {
  for (auto arg : boundTy->getGenericArgs()) {
    for (auto ip : InvertibleProtocolSet::allKnown()) {
      switch (ip) {
      case InvertibleProtocolKind::Copyable:
        if (arg->isNoncopyable())
          return ip;
        break;
      case InvertibleProtocolKind::Escapable:
        if (!arg->isEscapable())
          return ip;
      }
    }
  }
  return std::nullopt;
}

/// Older runtimes won't check for required invertible protocol conformances
/// at runtime during a cast.
///
/// \param srcType the source or initial type of the cast
/// \param refLoc source location of the cast
/// \param refDC decl context in which the cast occurs
/// \return true if diagnosed
static bool checkInverseGenericsCastingAvailability(Type srcType,
                                                    SourceRange refLoc,
                                                    const DeclContext *refDC) {
  if (!srcType) return false;

  auto type = srcType->getCanonicalType();

  if (auto boundTy = dyn_cast<BoundGenericType>(type)) {
    if (auto missing = checkGenericArgsForInvertibleReqs(boundTy)) {
      std::optional<Diag<StringRef, llvm::VersionTuple>> diag;
      switch (*missing) {
      case InvertibleProtocolKind::Copyable:
        diag =
            diag::availability_copyable_generics_casting_only_version_newer;
        break;
      case InvertibleProtocolKind::Escapable:
        diag =
            diag::availability_escapable_generics_casting_only_version_newer;
        break;
      }

      // Enforce the availability restriction.
      return TypeChecker::checkAvailability(
          refLoc,
          refDC->getASTContext().getNoncopyableGenericsAvailability(),
          *diag,
          refDC);
    }
  }
  return false;
}

static bool checkTypeMetadataAvailabilityInternal(CanType type,
                                                  SourceRange refLoc,
                                                  const DeclContext *refDC) {
  return type.findIf([&](CanType type) {
    if (isa<ParameterizedProtocolType>(type)) {
      return diagnoseParameterizedProtocolAvailability(refLoc, refDC);
    } else if (auto fnType = dyn_cast<AnyFunctionType>(type)) {
      auto isolation = fnType->getIsolation();
      if (isolation.isErased())
        return diagnoseIsolatedAnyAvailability(refLoc, refDC);
      if (fnType.getThrownError())
        return diagnoseTypedThrowsAvailability(refLoc, refDC);
    }
    return false;
  });
}

/// Check whether type metadata is available for the given type (and its
/// component types).
bool swift::checkTypeMetadataAvailability(Type type,
                                          SourceRange refLoc,
                                          const DeclContext *refDC) {
  if (!type) return false;
  return checkTypeMetadataAvailabilityInternal(type->getCanonicalType(),
                                               refLoc, refDC);
}

/// Check whether type metadata is available for the given type, given that
/// it is the operand of a dynamic cast or existential conversion.
static bool checkTypeMetadataAvailabilityForConverted(Type refType,
                                                      SourceRange refLoc,
                                                      const DeclContext *refDC) {
  if (!refType) return false;

  auto type = refType->getCanonicalType();

  // SILGen emits these conversions by opening the outermost level of
  // existential, so we never need to emit type metadata for an
  // existential in such a position.  We necessarily have type metadata
  // for the dynamic type of the existential, so there's nothing to check
  // there.
  if (type.isAnyExistentialType()) return false;

  if (checkTypeMetadataAvailabilityInternal(type, refLoc, refDC))
    return true;

  if (checkInverseGenericsCastingAvailability(type, refLoc, refDC))
    return true;

  return false;
}

namespace {

class CheckConversionAvailability {
  SourceRange refLoc;
  const DeclContext *refDC;

public:
  CheckConversionAvailability(SourceRange refLoc, const DeclContext *refDC)
    : refLoc(refLoc), refDC(refDC) {}

  void check(CanType srcType, CanType destType);
  void checkFunction(CanAnyFunctionType srcType, CanAnyFunctionType destType);

private:
  void checkTuple(CanTupleType srcType, CanTupleType destType);
};

} // end anonymous namespace

void CheckConversionAvailability::check(CanType srcType, CanType destType) {
  if (srcType == destType)
    return;

  // We care about specific optionality structure here: converting
  // `(any P<T>)?` to `Any?` doesn't require metadata for `any P<T>`,
  // but converting `(any P<T>)?` to non-optional `Any` does.
  if (auto destObjectType = destType.getOptionalObjectType()) {
    // optional -> optional conversion
    if (auto srcObjectType = srcType.getOptionalObjectType()) {
      check(srcObjectType, destObjectType);
    // optional injection
    } else {
      check(srcType, destObjectType);
    }

  // Conversions to existential types require type metadata for the
  // source type, except that we look into existentials.
  } else if (destType.isAnyExistentialType()) {
    checkTypeMetadataAvailabilityForConverted(srcType, refLoc, refDC);

  // Conversions between function types perform a bunch of recursive
  // conversions.
  } else if (auto destFnType = dyn_cast<AnyFunctionType>(destType)) {
    if (auto srcFnType = dyn_cast<AnyFunctionType>(srcType)) {
      checkFunction(srcFnType, destFnType);
    }

  // Conversions between tuple types perform a bunch of recursive
  // conversions.
  } else if (auto destTupleType = dyn_cast<TupleType>(destType)) {
    if (auto srcTupleType = dyn_cast<TupleType>(srcType)) {
      checkTuple(srcTupleType, destTupleType);
    }

  // Conversions of things containing pack expansions convert the
  // expansion patterns.  We won't print the types we get here, so
  // we can ignore them.
  } else if (auto destExpType = dyn_cast<PackExpansionType>(destType)) {
    if (auto srcExpType = dyn_cast<PackExpansionType>(srcType)) {
      check(srcExpType.getPatternType(), destExpType.getPatternType());
    }
  }
}

void CheckConversionAvailability::checkFunction(CanAnyFunctionType srcType,
                                                CanAnyFunctionType destType) {
  // Results are covariantly converted.
  check(srcType.getResult(), destType.getResult());

  // Defensively ignored invalid conversion structure.
  if (srcType->getNumParams() != destType->getNumParams())
    return;

  // Parameters are contravariantly converted.
  for (auto i : range(srcType->getNumParams())) {
    const auto &srcParam = srcType.getParams()[i];
    const auto &destParam = destType.getParams()[i];

    // Note the reversal for contravariance.
    check(destParam.getParameterType(), srcParam.getParameterType());
  }
}

void CheckConversionAvailability::checkTuple(CanTupleType srcType,
                                             CanTupleType destType) {
  // Handle invalid structure appropriately.
  if (srcType->getNumElements() != destType->getNumElements())
    return;

  for (auto i : range(srcType->getNumElements())) {
    check(srcType.getElementType(i), destType.getElementType(i));
  }
}

static void checkFunctionConversionAvailability(Type srcType, Type destType,
                                                SourceRange refLoc,
                                                const DeclContext *refDC) {
  if (srcType && destType) {
    auto srcFnType = cast<AnyFunctionType>(srcType->getCanonicalType());
    auto destFnType = cast<AnyFunctionType>(destType->getCanonicalType());

    CheckConversionAvailability(refLoc, refDC)
      .checkFunction(srcFnType, destFnType);
  }
}

bool diagnoseExplicitUnavailability(
    const ValueDecl *D, SourceRange R, const AvailabilityConstraint &constraint,
    const ExportContext &Where, DeclAvailabilityFlags Flags,
    llvm::function_ref<void(InFlightDiagnostic &, StringRef)>
        attachRenameFixIts) {
  if (!constraint.isUnavailable())
    return false;

  auto Attr = constraint.getAttr();
  if (Attr.getDomain().isSwiftLanguage() && !Attr.isVersionSpecific()) {
    if (shouldAllowReferenceToUnavailableInSwiftDeclaration(D, Where))
      return false;
  }

  SourceLoc Loc = R.Start;
  ASTContext &ctx = D->getASTContext();
  auto &diags = ctx.Diags;
  auto domain = constraint.getDomain();
  StringRef versionedPlatform = domain.getNameForDiagnostics();
  StringRef platform = shouldHideDomainNameForConstraintDiagnostic(constraint)
                           ? ""
                           : versionedPlatform;

  // TODO: Consider removing this.
  // ObjC keypaths components weren't checked previously, so errors are demoted
  // to warnings to avoid source breakage. In some cases unavailable or
  // obsolete decls still map to valid ObjC runtime names, so behave correctly
  // at runtime, even though their use would produce an error outside of a
  // #keyPath expression.
  auto limit = Flags.contains(DeclAvailabilityFlag::ForObjCKeyPath)
                  ? DiagnosticBehavior::Warning
                  : DiagnosticBehavior::Unspecified;

  auto message = Attr.getMessage();
  auto rename = Attr.getRename();
  if (!rename.empty()) {
    SmallString<32> newNameBuf;
    std::optional<ReplacementDeclKind> replaceKind =
        describeRename(ctx, Attr.getRename(), D, newNameBuf);
    unsigned rawReplaceKind = static_cast<unsigned>(
        replaceKind.value_or(ReplacementDeclKind::None));
    StringRef newName = replaceKind ? newNameBuf.str() : rename;
    EncodedDiagnosticMessage EncodedMessage(message);
    auto diag = diags.diagnose(Loc, diag::availability_decl_unavailable_rename,
                               D, replaceKind.has_value(), rawReplaceKind,
                               newName, EncodedMessage.Message);
    diag.limitBehavior(limit);
    attachRenameFixIts(diag, rename);
  } else if (isSubscriptReturningString(D, ctx)) {
    diags.diagnose(Loc, diag::availability_string_subscript_migration)
      .highlight(R)
      .fixItInsert(R.Start, "String(")
      .fixItInsertAfter(R.End, ")");

    // Skip the note emitted below.
    return true;
  } else {
    auto unavailableDiagnosticPlatform = platform;
    AvailabilityInference::updatePlatformStringForFallback(
        Attr, ctx, unavailableDiagnosticPlatform);
    EncodedDiagnosticMessage EncodedMessage(message);
    diags
        .diagnose(Loc, diag::availability_decl_unavailable, D, platform.empty(),
                  unavailableDiagnosticPlatform, EncodedMessage.Message)
        .highlight(R)
        .limitBehavior(limit);
  }

  auto sourceRange = Attr.getParsedAttr()->getRange();
  switch (constraint.getReason()) {
  case AvailabilityConstraint::Reason::UnconditionallyUnavailable:
    diags.diagnose(D, diag::availability_marked_unavailable, D)
        .highlight(sourceRange);
    break;
  case AvailabilityConstraint::Reason::IntroducedInLaterVersion:
    diags
        .diagnose(D, diag::availability_introduced_in_version, D,
                  versionedPlatform, *Attr.getIntroduced())
        .highlight(sourceRange);
    break;
  case AvailabilityConstraint::Reason::Obsoleted:
    diags
        .diagnose(D, diag::availability_obsoleted, D, versionedPlatform,
                  *Attr.getObsoleted())
        .highlight(sourceRange);
    break;
  case AvailabilityConstraint::Reason::IntroducedInLaterDynamicVersion:
    llvm_unreachable("unexpected constraint");
    break;
  }
  return true;
}

namespace {
class ExprAvailabilityWalker : public BaseDiagnosticWalker {
  /// Models how member references will translate to accessor usage. This is
  /// used to diagnose the availability of individual accessors that may be
  /// called by the expression being checked.
  enum class MemberAccessContext : unsigned {
    /// The starting access context for the root of any expression tree. In this
    /// context, a member access will call the get accessor only.
    Default,

    /// The access context for expressions rooted in a LoadExpr. A LoadExpr
    /// coerces l-values to r-values and thus member access inside of a LoadExpr
    /// will only invoke get accessors.
    Load,

    /// The access context for the outermost member accessed in the expression
    /// tree on the left-hand side of an assignment. Only the set accessor will
    /// be invoked on this member.
    Assignment,

    /// The access context for expressions in which member is being read and
    /// then written back to. For example, a writeback will occur inside of an
    /// InOutExpr. Both the get and set accessors may be called in this context.
    Writeback
  };

  ASTContext &Context;
  MemberAccessContext AccessContext = MemberAccessContext::Default;
  SmallVector<const Expr *, 16> ExprStack;
  SmallVector<bool, 4> PreconcurrencyCalleeStack;
  const ExportContext &Where;

public:
  explicit ExprAvailabilityWalker(const ExportContext &Where)
    : Context(Where.getDeclContext()->getASTContext()), Where(Where) {}

  PreWalkAction walkToArgumentPre(const Argument &Arg) override {
    // Arguments should be walked in their own member access context which
    // starts out read-only by default.
    walkInContext(Arg.getExpr(), MemberAccessContext::Default);
    return Action::SkipChildren();
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    auto *DC = Where.getDeclContext();

    ExprStack.push_back(E);

    if (isa<ApplyExpr>(E)) {
      PreconcurrencyCalleeStack.push_back(
          hasReferenceToPreconcurrencyDecl(E));
    }

    if (auto DR = dyn_cast<DeclRefExpr>(E)) {
      diagnoseDeclRefAvailability(DR->getDeclRef(), DR->getSourceRange(),
                                  getEnclosingApplyExpr(), std::nullopt);
      maybeDiagStorageAccess(DR->getDecl(), DR->getSourceRange(), DC);
    }
    if (auto MR = dyn_cast<MemberRefExpr>(E)) {
      walkMemberRef(MR);
      return Action::SkipChildren(E);
    }
    if (auto OCDR = dyn_cast<OtherConstructorDeclRefExpr>(E))
      diagnoseDeclRefAvailability(OCDR->getDeclRef(),
                                  OCDR->getConstructorLoc().getSourceRange(),
                                  getEnclosingApplyExpr());
    if (auto DMR = dyn_cast<DynamicMemberRefExpr>(E))
      diagnoseDeclRefAvailability(DMR->getMember(),
                                  DMR->getNameLoc().getSourceRange(),
                                  getEnclosingApplyExpr());
    if (auto DS = dyn_cast<DynamicSubscriptExpr>(E))
      diagnoseDeclRefAvailability(DS->getMember(), DS->getSourceRange());
    if (auto S = dyn_cast<SubscriptExpr>(E)) {
      if (S->hasDecl()) {
        diagnoseDeclRefAvailability(S->getDecl(), S->getSourceRange(), S);
        maybeDiagStorageAccess(S->getDecl().getDecl(), S->getSourceRange(), DC);
        PreconcurrencyCalleeStack.push_back(
            hasReferenceToPreconcurrencyDecl(S));
      }
    }

    if (auto *LE = dyn_cast<LiteralExpr>(E)) {
      if (auto literalType = LE->getType()) {
        // Check availability of the type produced by implicit literal
        // initializer.
        if (auto *nominalDecl = literalType->getAnyNominal()) {
          diagnoseDeclAvailability(nominalDecl, LE->getSourceRange(),
                                   /*call=*/nullptr, Where);
        }
      }
      diagnoseDeclRefAvailability(LE->getInitializer(), LE->getSourceRange());
    }

    // Diagnose availability for any features used in a regex literal.
    if (auto *RE = dyn_cast<RegexLiteralExpr>(E)) {
      for (auto &feature : RE->getPatternFeatures()) {
        auto featureKind = feature.getKind();
        TypeChecker::checkAvailability(
            RE->getSourceRange(), featureKind.getAvailability(Context),
            Where.getDeclContext(),
            [&](StringRef platformName, llvm::VersionTuple version) {
              auto range = feature.getRange();
              auto diag = Context.Diags.diagnose(
                  range.getStart(), diag::regex_feature_unavailable,
                  featureKind.getDescription(Context), platformName, version);
              diag.highlightChars(range);
              return diag;
            });
      }
    }

    if (auto *CE = dyn_cast<CollectionExpr>(E)) {
      // Diagnose availability of implicit collection literal initializers.
      diagnoseDeclRefAvailability(CE->getInitializer(), CE->getSourceRange());
    }

    if (auto *FCE = dyn_cast<FunctionConversionExpr>(E)) {
      checkFunctionConversionAvailability(FCE->getSubExpr()->getType(),
                                          FCE->getType(),
                                          FCE->getLoc(),
                                          Where.getDeclContext());
    }
    if (auto KP = dyn_cast<KeyPathExpr>(E)) {
      maybeDiagKeyPath(KP);
    }
    if (auto A = dyn_cast<AssignExpr>(E)) {
      // Attempting to assign to a @preconcurrency declaration should
      // downgrade Sendable conformance mismatches to warnings.
      PreconcurrencyCalleeStack.push_back(
        hasReferenceToPreconcurrencyDecl(A->getDest()));

      walkAssignExpr(A);
      return Action::SkipChildren(E);
    }
    if (auto IO = dyn_cast<InOutExpr>(E)) {
      walkInOutExpr(IO);
      return Action::SkipChildren(E);
    }
    if (auto T = dyn_cast<TypeExpr>(E)) {
      if (!T->isImplicit()) {
        diagnoseTypeAvailability(T->getTypeRepr(), T->getType(), E->getLoc(),
                                 Where);
      }
    }
    if (auto CE = dyn_cast<ClosureExpr>(E)) {
      for (auto *param : *CE->getParameters()) {
        diagnoseTypeAvailability(param->getTypeRepr(), param->getInterfaceType(),
                                 E->getLoc(), Where);
      }
      diagnoseTypeAvailability(CE->hasExplicitResultType()
                               ? CE->getExplicitResultTypeRepr()
                               : nullptr,
                               CE->getResultType(), E->getLoc(), Where);
    }
    if (AbstractClosureExpr *closure = dyn_cast<AbstractClosureExpr>(E)) {
      walkAbstractClosure(closure);
      return Action::SkipChildren(E);
    }

    if (auto CE = dyn_cast<ExplicitCastExpr>(E)) {
      if (!isa<CoerceExpr>(CE)) {
        SourceLoc loc = CE->getCastTypeRepr() ? CE->getCastTypeRepr()->getLoc()
                                              : E->getLoc();
        checkTypeMetadataAvailability(CE->getCastType(), loc,
                                      Where.getDeclContext());
        checkTypeMetadataAvailabilityForConverted(CE->getSubExpr()->getType(),
                                                  loc, Where.getDeclContext());
      }

      diagnoseTypeAvailability(CE->getCastTypeRepr(), CE->getCastType(),
                               E->getLoc(), Where);
    }
    
    if (auto EE = dyn_cast<ErasureExpr>(E)) {
      checkTypeMetadataAvailability(EE->getSubExpr()->getType(),
                                    EE->getLoc(), Where.getDeclContext());
      checkInverseGenericsCastingAvailability(EE->getSubExpr()->getType(),
                                              EE->getLoc(),
                                              Where.getDeclContext());

      bool preconcurrency = false;
      if (!PreconcurrencyCalleeStack.empty()) {
        preconcurrency = PreconcurrencyCalleeStack.back();
      }

      for (ProtocolConformanceRef C : EE->getConformances()) {
        diagnoseConformanceAvailability(E->getLoc(), C, Where, Type(), Type(),
                                        /*useConformanceAvailabilityErrorsOpt=*/true,
                                        /*preconcurrency=*/preconcurrency);
      }
    }

    if (auto UTO = dyn_cast<UnderlyingToOpaqueExpr>(E)) {
      diagnoseSubstitutionMapAvailability(
          UTO->getLoc(), UTO->substitutions, Where);
    }

    if (auto ME = dyn_cast<MacroExpansionExpr>(E)) {
      diagnoseDeclRefAvailability(
          ME->getMacroRef(), ME->getMacroNameLoc().getSourceRange());
    }

    if (auto LE = dyn_cast<LoadExpr>(E)) {
      walkLoadExpr(LE);
      return Action::SkipChildren(E);
    }

    return Action::Continue(E);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
    assert(ExprStack.back() == E);
    ExprStack.pop_back();

    if (isa<ApplyExpr>(E)) {
      PreconcurrencyCalleeStack.pop_back();
    }

    return Action::Continue(E);
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    // We need to recursively call diagnoseExprAvailability for any
    // sub-expressions in the statement since the availability context may
    // differ, e.g for things like `guard #available(...)`.
    class StmtRecurseWalker : public BaseDiagnosticWalker {
      DeclContext *DC;

    public:
      StmtRecurseWalker(DeclContext *DC) : DC(DC) {}

      PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
        diagnoseExprAvailability(E, DC);
        return Action::SkipNode(E);
      }
    };
    StmtRecurseWalker W(Where.getDeclContext());
    S->walk(W);
    return Action::SkipNode(S);
  }

  bool
  diagnoseDeclRefAvailability(ConcreteDeclRef declRef, SourceRange R,
                              const Expr *call = nullptr,
                              DeclAvailabilityFlags flags = std::nullopt) const;

private:
  bool diagnoseIncDecRemoval(const ValueDecl *D, SourceRange R) const;
  bool diagnoseMemoryLayoutMigration(const ValueDecl *D, SourceRange R,
                                     SemanticAvailableAttr,
                                     const ApplyExpr *call) const;

  /// Walks up from a potential callee to the enclosing ApplyExpr.
  const ApplyExpr *getEnclosingApplyExpr() const {
    ArrayRef<const Expr *> parents = ExprStack;
    assert(!parents.empty() && "must be called while visiting an expression");
    size_t idx = parents.size() - 1;

    do {
      if (idx == 0)
        return nullptr;
      --idx;
    } while (isa<DotSyntaxBaseIgnoredExpr>(parents[idx]) || // Mod.f(a)
             isa<SelfApplyExpr>(parents[idx]) || // obj.f(a)
             isa<IdentityExpr>(parents[idx]) || // (f)(a)
             isa<ForceValueExpr>(parents[idx]) || // f!(a)
             isa<BindOptionalExpr>(parents[idx]) || // f?(a)
             isa<FunctionConversionExpr>(parents[idx]));

    auto *call = dyn_cast<ApplyExpr>(parents[idx]);
    if (!call || call->getFn() != parents[idx+1])
      return nullptr;
    return call;
  }

  /// Walk an assignment expression, checking for availability.
  void walkAssignExpr(AssignExpr *E) {
    // We take over recursive walking of assignment expressions in order to
    // walk the destination and source expressions in different member
    // access contexts.
    Expr *Dest = E->getDest();
    if (!Dest) {
      return;
    }

    // Check the Dest expression in a setter context.
    // We have an implicit assumption here that the first MemberRefExpr
    // encountered walking (pre-order) is the Dest is the destination of the
    // write. For the moment this is fine -- but future syntax might violate
    // this assumption.
    walkInContext(Dest, MemberAccessContext::Assignment);

    // Check RHS in getter context
    Expr *Source = E->getSrc();
    if (!Source) {
      return;
    }
    walkInContext(Source, MemberAccessContext::Default);
  }

  /// Walk a load expression, checking for availability.
  void walkLoadExpr(LoadExpr *E) {
    walkInContext(E->getSubExpr(), MemberAccessContext::Load);
  }

  /// Walk a member reference expression, checking for availability.
  void walkMemberRef(MemberRefExpr *E) {
    // Walk the base. If the access context is currently `Assignment`, then we
    // must be diagnosing the destination of an assignment. When recursing,
    // diagnose any remaining member refs in a `Writeback` context, since
    // there is a writeback occurring through them as a result of the
    // assignment.
    //
    //   someVar.x.y = 1
    //           │ ╰─ MemberAccessContext::Assignment
    //           ╰─── MemberAccessContext::Writeback
    //
    MemberAccessContext accessContext =
        (AccessContext == MemberAccessContext::Assignment)
            ? MemberAccessContext::Writeback
            : AccessContext;
    walkInContext(E->getBase(), accessContext);

    ConcreteDeclRef DR = E->getMember();
    // Diagnose for the member declaration itself.
    if (diagnoseDeclRefAvailability(DR, E->getNameLoc().getSourceRange(),
                                    getEnclosingApplyExpr(), std::nullopt))
      return;

    // Diagnose for appropriate accessors, given the access context.
    auto *DC = Where.getDeclContext();
    maybeDiagStorageAccess(DR.getDecl(), E->getSourceRange(), DC);
  }

  /// Walk a keypath expression, checking all of its components for
  /// availability.
  void maybeDiagKeyPath(KeyPathExpr *KP) {
    auto flags = DeclAvailabilityFlags();
    auto declContext = Where.getDeclContext();
    if (KP->isObjC())
      flags = DeclAvailabilityFlag::ForObjCKeyPath;

    for (auto &component : KP->getComponents()) {
      switch (component.getKind()) {
      case KeyPathExpr::Component::Kind::Property:
      case KeyPathExpr::Component::Kind::Subscript: {
        auto decl = component.getDeclRef();
        auto loc = component.getLoc();
        auto range = component.getSourceRange();
        if (diagnoseDeclRefAvailability(decl, loc, nullptr, flags))
          break;
        maybeDiagStorageAccess(decl.getDecl(), range, declContext);
        break;
      }

      case KeyPathExpr::Component::Kind::TupleElement:
        break;

      case KeyPathExpr::Component::Kind::Invalid:
      case KeyPathExpr::Component::Kind::UnresolvedProperty:
      case KeyPathExpr::Component::Kind::UnresolvedSubscript:
      case KeyPathExpr::Component::Kind::OptionalChain:
      case KeyPathExpr::Component::Kind::OptionalWrap:
      case KeyPathExpr::Component::Kind::OptionalForce:
      case KeyPathExpr::Component::Kind::Identity:
      case KeyPathExpr::Component::Kind::DictionaryKey:
      case KeyPathExpr::Component::Kind::CodeCompletion:
        break;
      }
    }
  }

  /// Walk an inout expression, checking for availability.
  void walkInOutExpr(InOutExpr *E) {
    // Typically an InOutExpr should begin a `Writeback` context. However,
    // inside a LoadExpr this transition is suppressed since the entire
    // expression is being coerced to an r-value.
    auto accessContext = AccessContext != MemberAccessContext::Load
                             ? MemberAccessContext::Writeback
                             : AccessContext;
    walkInContext(E->getSubExpr(), accessContext);
  }

  /// Walk an abstract closure expression, checking for availability
  void walkAbstractClosure(AbstractClosureExpr *closure) {
    // Do the walk with the closure set as the decl context of the 'where'
    auto where = ExportContext::forFunctionBody(closure, closure->getStartLoc());
    if (where.isImplicit())
      return;
    ExprAvailabilityWalker walker(where);

    // Manually dive into the body
    closure->getBody()->walk(walker);

    return;
  }

  /// Walk the given expression in the member access context.
  void walkInContext(Expr *E, MemberAccessContext AccessContext) {
    llvm::SaveAndRestore<MemberAccessContext>
      C(this->AccessContext, AccessContext);
    E->walk(*this);
  }

  /// Emit diagnostics, if necessary, for accesses to storage where
  /// the accessor for the AccessContext is not available.
  void maybeDiagStorageAccess(const ValueDecl *VD,
                              SourceRange ReferenceRange,
                              const DeclContext *ReferenceDC) const {
    if (Context.LangOpts.DisableAvailabilityChecking)
      return;

    auto *D = dyn_cast<AbstractStorageDecl>(VD);
    if (!D)
      return;

    if (!D->requiresOpaqueAccessors()) {
      return;
    }

    // Check availability of accessor functions.
    // TODO: if we're talking about an inlineable storage declaration,
    // this probably needs to be refined to not assume that the accesses are
    // specifically using the getter/setter.
    switch (AccessContext) {
    case MemberAccessContext::Default:
    case MemberAccessContext::Load:
      diagAccessorAvailability(D->getOpaqueAccessor(AccessorKind::Get),
                               ReferenceRange, ReferenceDC, std::nullopt);
      break;

    case MemberAccessContext::Assignment:
      diagAccessorAvailability(D->getOpaqueAccessor(AccessorKind::Set),
                               ReferenceRange, ReferenceDC, std::nullopt);
      break;

    case MemberAccessContext::Writeback:
      diagAccessorAvailability(D->getOpaqueAccessor(AccessorKind::Get),
                               ReferenceRange, ReferenceDC,
                               DeclAvailabilityFlag::ForInout);

      diagAccessorAvailability(D->getOpaqueAccessor(AccessorKind::Set),
                               ReferenceRange, ReferenceDC,
                               DeclAvailabilityFlag::ForInout);
      break;
    }
  }

  /// Emit a diagnostic, if necessary for a potentially unavailable accessor.
  void diagAccessorAvailability(AccessorDecl *D, SourceRange ReferenceRange,
                                const DeclContext *ReferenceDC,
                                DeclAvailabilityFlags Flags) const {
    if (!D)
      return;

    Flags &= DeclAvailabilityFlag::ForInout;
    Flags |= DeclAvailabilityFlag::ContinueOnPotentialUnavailability;
    if (diagnoseDeclAvailability(D, ReferenceRange, /*call*/ nullptr, Where,
                                 Flags))
      return;
  }

  /// Check whether the given expression references any
  /// @preconcurrency declarations.
  /// Calls, subscripts, member references can have @preconcurrency
  /// declarations at any point in their base chain.
  bool hasReferenceToPreconcurrencyDecl(Expr *expr) {
    if (auto declRef = expr->getReferencedDecl()) {
      if (declRef.getDecl()->preconcurrency())
        return true;
    }

    if (auto *selfApply = dyn_cast<SelfApplyExpr>(expr)) {
      if (hasReferenceToPreconcurrencyDecl(selfApply->getFn()))
        return true;

      // Base could be a preconcurrency declaration i.e.
      //
      // @preconcurrency var x: [any Sendable]
      // x.append(...)
      //
      // If thought `append` might not be `@preconcurrency`
      // the "base" is.
      return hasReferenceToPreconcurrencyDecl(selfApply->getBase());
    }

    if (auto *LE = dyn_cast<LookupExpr>(expr)) {
      // If subscript itself is not @preconcurrency, it's base could be.
      return hasReferenceToPreconcurrencyDecl(LE->getBase());
    }

    if (auto *apply = dyn_cast<ApplyExpr>(expr))
      return hasReferenceToPreconcurrencyDecl(apply->getFn());

    return false;
  }
};
} // end anonymous namespace

/// Diagnose uses of unsafe declarations.
static void
diagnoseDeclUnsafe(ConcreteDeclRef declRef, SourceRange R,
                   const Expr *call, const ExportContext &Where) {
  auto unsafeUses = Where.getUnsafeUses();
  if (!unsafeUses)
    return;

  SourceLoc diagLoc = call ? call->getLoc() : R.Start;
  enumerateUnsafeUses(declRef, diagLoc, call != nullptr,
                      [&](UnsafeUse unsafeUse) {
    unsafeUses->push_back(unsafeUse);
    return false;
  });
}

/// Diagnose uses of unavailable declarations. Returns true if a diagnostic
/// was emitted.
bool ExprAvailabilityWalker::diagnoseDeclRefAvailability(
    ConcreteDeclRef declRef, SourceRange R, const Expr *call,
    DeclAvailabilityFlags Flags) const {
  if (!declRef)
    return false;
  const ValueDecl *D = declRef.getDecl();

  // Suppress availability diagnostics for uses of builtins.  We don't
  // synthesize availability for builtin functions anyway, so this really
  // means to not check availability for the substitution maps.  This is
  // abstractly reasonable, since calls to generic builtins usually do not
  // require metadata for generic arguments the same way that calls to
  // generic functions might.  More importantly, the stdlib has to get the
  // availability right anyway, and diagnostics from builtin usage are not
  // likely to be of significant assistance in that.
  if (D->getModuleContext()->isBuiltinModule())
    return false;

  if (auto attr = D->getUnavailableAttr()) {
    if (diagnoseIncDecRemoval(D, R))
      return true;
    if (isa_and_nonnull<ApplyExpr>(call) &&
        diagnoseMemoryLayoutMigration(D, R, *attr, cast<ApplyExpr>(call)))
      return true;
  }

  if (diagnoseDeclAvailability(
          D, R, call, Where,
          Flags | DeclAvailabilityFlag::DisableUnsafeChecking))
    return true;

  diagnoseDeclUnsafe(declRef, R, call, Where);

  if (R.isValid()) {
    if (diagnoseSubstitutionMapAvailability(
            R.Start, declRef.getSubstitutions(), Where,
            Type(), Type(),
            /*warnIfConformanceUnavailablePreSwift6*/false,
            /*suppressParameterizationCheckForOptional*/false,
            /*preconcurrency*/D->preconcurrency())) {
      return true;
    }
  }

  return false;
}

/// Diagnose misuses of API in asynchronous contexts.
/// Returns true if a fatal diagnostic was emitted, false otherwise.
static bool
diagnoseDeclAsyncAvailability(const ValueDecl *D, SourceRange R,
                              const Expr *call, const ExportContext &Where) {
  // If we are not in an (effective) async context, don't check it
  if (!shouldTreatDeclContextAsAsyncForDiagnostics(Where.getDeclContext()))
    return false;

  ASTContext &ctx = Where.getDeclContext()->getASTContext();

  // Only suggest async alternatives if the DeclContext is truly async
  if (Where.getDeclContext()->isAsyncContext()) {
    if (const AbstractFunctionDecl *afd = dyn_cast<AbstractFunctionDecl>(D)) {
      if (const AbstractFunctionDecl *asyncAlt = afd->getAsyncAlternative()) {
        SourceLoc diagLoc = call ? call->getLoc() : R.Start;
        ctx.Diags.diagnose(diagLoc, diag::warn_use_async_alternative);
        asyncAlt->diagnose(diag::decl_declared_here, asyncAlt);
      }
    }
  }

  // @available(noasync) spelling
  if (auto attr = D->getNoAsyncAttr()) {
    SourceLoc diagLoc = call ? call->getLoc() : R.Start;
    auto diag = ctx.Diags.diagnose(diagLoc, diag::async_unavailable_decl, D,
                                   attr->getMessage());
    diag.warnUntilSwiftVersion(6);
    diag.limitBehaviorWithPreconcurrency(DiagnosticBehavior::Warning,
                                         D->preconcurrency());

    if (!attr->getRename().empty()) {
      fixItAvailableAttrRename(diag, R, D, attr->getRename(), call);
    }
    return true;
  }

  const bool hasUnavailableAttr =
      D->getAttrs().hasAttribute<UnavailableFromAsyncAttr>();

  if (!hasUnavailableAttr)
    return false;
  // @_unavailableFromAsync spelling
  const UnavailableFromAsyncAttr *attr =
      D->getAttrs().getAttribute<UnavailableFromAsyncAttr>();
  SourceLoc diagLoc = call ? call->getLoc() : R.Start;
  ctx.Diags
      .diagnose(diagLoc, diag::async_unavailable_decl, D, attr->Message)
      .warnUntilSwiftVersion(6);
  D->diagnose(diag::decl_declared_here, D);
  return true;
}

/// Diagnose uses of unavailable declarations. Returns true if a diagnostic
/// was emitted.
bool swift::diagnoseDeclAvailability(const ValueDecl *D, SourceRange R,
                                     const Expr *call,
                                     const ExportContext &Where,
                                     DeclAvailabilityFlags Flags) {
  // Generic parameters are always available.
  if (isa<GenericTypeParamDecl>(D))
    return false;

  if (R.isValid()) {
    if (TypeChecker::diagnoseInlinableDeclRefAccess(R.Start, D, Where))
      return true;

    if (TypeChecker::diagnoseDeclRefExportability(R.Start, D, Where))
      return true;
  }

  // Keep track if this is an accessor.
  auto accessor = dyn_cast<AccessorDecl>(D);

  if (accessor) {
    // If the property/subscript is unconditionally unavailable, don't bother
    // with any of the rest of this.
    if (accessor->getStorage()->isUnavailable())
      return false;
  }

  auto *DC = Where.getDeclContext();
  auto &ctx = DC->getASTContext();

  auto constraint =
      getAvailabilityConstraintsForDecl(D, Where.getAvailability())
          .getPrimaryConstraint();

  if (constraint) {
    if (diagnoseExplicitUnavailability(D, R, *constraint, Where, call, Flags))
      return true;
  }

  if (diagnoseDeclAsyncAvailability(D, R, call, Where))
    return true;

  if (!Flags.contains(DeclAvailabilityFlag::DisableUnsafeChecking))
    diagnoseDeclUnsafe(const_cast<ValueDecl *>(D), R, call, Where);

  // Make sure not to diagnose an accessor's deprecation if we already
  // complained about the property/subscript.
  bool isAccessorWithDeprecatedStorage =
      accessor && getDeprecated(accessor->getStorage());

  // Diagnose for deprecation
  if (!isAccessorWithDeprecatedStorage)
    diagnoseIfDeprecated(R, Where, D, call);

  if (Flags.contains(DeclAvailabilityFlag::AllowPotentiallyUnavailableProtocol)
        && isa<ProtocolDecl>(D))
    return false;

  if (!constraint)
    return false;

  auto requiredRange = constraint->getRequiredNewerAvailabilityRange(ctx);

  // Diagnose (and possibly signal) for potential unavailability
  if (!requiredRange)
    return false;

  if (Flags.contains(
          DeclAvailabilityFlag::
              AllowPotentiallyUnavailableAtOrBelowDeploymentTarget) &&
      requiresDeploymentTargetOrEarlier(*requiredRange, ctx))
    return false;

  if (accessor) {
    bool forInout = Flags.contains(DeclAvailabilityFlag::ForInout);
    diagnosePotentialAccessorUnavailability(accessor, R, DC, *requiredRange,
                                            forInout);
  } else {
    if (!diagnosePotentialUnavailability(D, R, DC, *requiredRange))
      return false;
  }

  return !Flags.contains(
      DeclAvailabilityFlag::ContinueOnPotentialUnavailability);
}

/// Return true if the specified type looks like an integer of floating point
/// type.
static bool isIntegerOrFloatingPointType(Type ty) {
  return (TypeChecker::conformsToKnownProtocol(
            ty, KnownProtocolKind::ExpressibleByIntegerLiteral) ||
          TypeChecker::conformsToKnownProtocol(
            ty, KnownProtocolKind::ExpressibleByFloatLiteral));
}


/// If this is a call to an unavailable ++ / -- operator, try to diagnose it
/// with a fixit hint and return true.  If not, or if we fail, return false.
bool
ExprAvailabilityWalker::diagnoseIncDecRemoval(const ValueDecl *D, SourceRange R) const {
  // We can only produce a fixit if we're talking about ++ or --.
  bool isInc = D->getBaseName() == "++";
  if (!isInc && D->getBaseName() != "--")
    return false;

  // We can only handle the simple cases of lvalue++ and ++lvalue.  This is
  // always modeled as:
  //   (postfix_unary_expr (declrefexpr ++), (inoutexpr (lvalue)))
  // if not, bail out.
  if (ExprStack.size() != 2 ||
      !isa<DeclRefExpr>(ExprStack[1]) ||
      !(isa<PostfixUnaryExpr>(ExprStack[0]) ||
        isa<PrefixUnaryExpr>(ExprStack[0])))
    return false;

  auto call = cast<ApplyExpr>(ExprStack[0]);

  // If the expression type is integer or floating point, then we can rewrite it
  // to "lvalue += 1".
  std::string replacement;
  if (isIntegerOrFloatingPointType(call->getType()))
    replacement = isInc ? " += 1" : " -= 1";
  else {
    // Otherwise, it must be an index type.  Rewrite to:
    // "lvalue = lvalue.successor()".
    auto &SM = Context.SourceMgr;
    auto CSR = Lexer::getCharSourceRangeFromSourceRange(
        SM, call->getArgs()->getSourceRange());
    replacement = " = " + SM.extractText(CSR).str();
    replacement += isInc ? ".successor()" : ".predecessor()";
  }
  
  if (!replacement.empty()) {
    // If we emit a deprecation diagnostic, produce a fixit hint as well.
    auto diag = Context.Diags.diagnose(
        R.Start, diag::availability_decl_unavailable, D, true, "",
        "it has been removed in Swift 3");
    if (isa<PrefixUnaryExpr>(call)) {
      // Prefix: remove the ++ or --.
      diag.fixItRemove(call->getFn()->getSourceRange());
      diag.fixItInsertAfter(call->getArgs()->getEndLoc(), replacement);
    } else {
      // Postfix: replace the ++ or --.
      diag.fixItReplace(call->getFn()->getSourceRange(), replacement);
    }

    return true;
  }


  return false;
}

/// If this is a call to an unavailable sizeof family function, diagnose it
/// with a fixit hint and return true. If not, or if we fail, return false.
bool
ExprAvailabilityWalker::diagnoseMemoryLayoutMigration(const ValueDecl *D,
                                                      SourceRange R,
                                                      SemanticAvailableAttr Attr,
                                                      const ApplyExpr *call) const {

  if (!D->getModuleContext()->isStdlibModule())
    return false;

  StringRef Property;
  if (D->getBaseName() == "sizeof") {
    Property = "size";
  } else if (D->getBaseName() == "alignof") {
    Property = "alignment";
  } else if (D->getBaseName() == "strideof") {
    Property = "stride";
  }

  if (Property.empty())
    return false;

  auto *args = call->getArgs();
  auto *subject = args->getUnlabeledUnaryExpr();
  if (!subject)
    return false;

  EncodedDiagnosticMessage EncodedMessage(Attr.getMessage());
  auto diag =
      Context.Diags.diagnose(
          R.Start, diag::availability_decl_unavailable, D, true, "",
          EncodedMessage.Message);
  diag.highlight(R);

  StringRef Prefix = "MemoryLayout<";
  StringRef Suffix = ">.";

  if (auto DTE = dyn_cast<DynamicTypeExpr>(subject)) {
    // Replace `sizeof(type(of: x))` with `MemoryLayout<X>.size`, where `X` is
    // the static type of `x`. The previous spelling misleadingly hinted that
    // `sizeof(_:)` might return the size of the *dynamic* type of `x`, when
    // it is not the case.
    auto valueType = DTE->getBase()->getType()->getRValueType();
    if (!valueType || valueType->hasError()) {
      // If we don't have a suitable argument, we can't emit a fixit.
      return true;
    }
    // Note that in rare circumstances we may be destructively replacing the
    // source text. For example, we'd replace `sizeof(type(of: doSomething()))`
    // with `MemoryLayout<T>.size`, if T is the return type of `doSomething()`.
    diag.fixItReplace(call->getSourceRange(),
                   (Prefix + valueType->getString() + Suffix + Property).str());
  } else {
    SourceRange PrefixRange(call->getStartLoc(), args->getLParenLoc());
    SourceRange SuffixRange(args->getRParenLoc());

    // We must remove `.self`.
    if (auto *DSE = dyn_cast<DotSelfExpr>(subject))
      SuffixRange.Start = DSE->getDotLoc();

    diag
      .fixItReplace(PrefixRange, Prefix)
      .fixItReplace(SuffixRange, (Suffix + Property).str());
  }

  return true;
}

/// Diagnose uses of unavailable declarations.
void swift::diagnoseExprAvailability(const Expr *E, DeclContext *DC) {
  auto where = ExportContext::forFunctionBody(DC, E->getStartLoc());
  if (where.isImplicit())
    return;
  ExprAvailabilityWalker walker(where);
  const_cast<Expr*>(E)->walk(walker);
}

namespace {

class StmtAvailabilityWalker : public BaseDiagnosticWalker {
  const Stmt *TopLevelStmt;
  DeclContext *DC;

public:
  explicit StmtAvailabilityWalker(const Stmt *S, DeclContext *dc)
    : TopLevelStmt(S), DC(dc) {}

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    // `diagnoseStmtAvailability` is called for every statement, so we don't
    // want to walk into any nested statements.
    return Action::VisitNodeIf(S == TopLevelStmt, S);
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    // Handled by ExprAvailabilityWalker.
    return Action::SkipNode(E);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    // Handled by DeclAvailabilityChecker.
    return Action::SkipNode();
  }

  PreWalkAction walkToTypeReprPre(TypeRepr *T) override {
    auto where = ExportContext::forFunctionBody(DC, T->getStartLoc());
    diagnoseTypeReprAvailability(T, where);
    return Action::SkipNode();
  }

  PreWalkResult<Pattern *> walkToPatternPre(Pattern *P) override {
    if (auto *IP = dyn_cast<IsPattern>(P)) {
      auto where = ExportContext::forFunctionBody(DC, P->getLoc());
      diagnoseTypeAvailability(IP->getCastTypeRepr(), IP->getCastType(),
                               P->getLoc(), where, std::nullopt);
    }

    return Action::Continue(P);
  }
};
}

void swift::diagnoseStmtAvailability(const Stmt *S, DeclContext *DC) {
  StmtAvailabilityWalker walker(S, DC);
  const_cast<Stmt*>(S)->walk(walker);
}

namespace {

class TypeReprAvailabilityWalker : public ASTWalker {
  const ExportContext &where;
  DeclAvailabilityFlags flags;

  bool checkDeclRefTypeRepr(DeclRefTypeRepr *declRefTR) const {
    ArrayRef<AssociatedTypeDecl *> primaryAssociatedTypes;

    if (auto *qualIdentTR = dyn_cast<QualifiedIdentTypeRepr>(declRefTR)) {
      // If the base is unavailable, don't go on to diagnose
      // the member since that will just produce a redundant
      // diagnostic.
      if (diagnoseTypeReprAvailability(qualIdentTR->getBase(), where, flags)) {
        return true;
      }
    }

    if (auto *typeDecl = declRefTR->getBoundDecl()) {
      auto range = declRefTR->getNameLoc().getSourceRange();
      if (diagnoseDeclAvailability(typeDecl, range, nullptr, where, flags))
        return true;

      if (auto protocol = dyn_cast<ProtocolDecl>(typeDecl)) {
        primaryAssociatedTypes = protocol->getPrimaryAssociatedTypes();
      }
    }

    bool foundAnyIssues = false;

    if (declRefTR->hasGenericArgList()) {
      auto genericFlags = flags;
      genericFlags -= DeclAvailabilityFlag::AllowPotentiallyUnavailableProtocol;

      for (auto *genericArg : declRefTR->getGenericArgs()) {
        if (diagnoseTypeReprAvailability(genericArg, where, genericFlags))
          foundAnyIssues = true;

        // The associated type that is being specified must be available as
        // well.
        if (!primaryAssociatedTypes.empty()) {
          auto primaryAssociatedType = primaryAssociatedTypes.front();
          primaryAssociatedTypes = primaryAssociatedTypes.drop_front();
          if (diagnoseDeclAvailability(
                  primaryAssociatedType, genericArg->getSourceRange(),
                  nullptr, where, genericFlags))
            foundAnyIssues = true;
        }
      }
    }

    return foundAnyIssues;
  }

public:
  bool foundAnyIssues = false;

  TypeReprAvailabilityWalker(const ExportContext &where,
                             DeclAvailabilityFlags flags)
      : where(where), flags(flags) {}

  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::ArgumentsAndExpansion;
  }

  PreWalkAction walkToTypeReprPre(TypeRepr *T) override {
    auto *declRefTR = dyn_cast<DeclRefTypeRepr>(T);
    if (!declRefTR)
      return Action::Continue();

    if (checkDeclRefTypeRepr(declRefTR)) {
      foundAnyIssues = true;
    }

    // We've already visited all the children above, so we don't
    // need to recurse.
    return Action::SkipNode();
  }
};

}

/// Diagnose uses of unavailable declarations in types.
bool diagnoseTypeReprAvailability(const TypeRepr *T, const ExportContext &where,
                                  DeclAvailabilityFlags flags) {
  if (!T)
    return false;
  TypeReprAvailabilityWalker walker(where, flags);
  const_cast<TypeRepr*>(T)->walk(walker);
  return walker.foundAnyIssues;
}

namespace {

class ProblematicTypeFinder : public TypeDeclFinder {
  SourceLoc Loc;
  const ExportContext &Where;
  DeclAvailabilityFlags Flags;

public:
  ProblematicTypeFinder(SourceLoc Loc, const ExportContext &Where,
                        DeclAvailabilityFlags Flags)
      : Loc(Loc), Where(Where), Flags(Flags) {}

  void visitTypeDecl(TypeDecl *decl) {
    // We only need to diagnose exportability here. Availability was
    // already checked on the TypeRepr.
    if (Where.mustOnlyReferenceExportedDecls())
      TypeChecker::diagnoseDeclRefExportability(Loc, decl, Where);
  }

  Action visitNominalType(NominalType *ty) override {
    visitTypeDecl(ty->getDecl());

    // If some generic parameters are missing, don't check conformances.
    if (ty->hasUnboundGenericType())
      return Action::Continue;

    // When the DeclContext parameter to getContextSubstitutionMap()
    // is a protocol declaration, the receiver must be a concrete
    // type, so it doesn't make sense to perform this check on
    // protocol types.
    if (isa<ProtocolType>(ty))
      return Action::Continue;

    auto subs = ty->getContextSubstitutionMap();
    (void) diagnoseSubstitutionMapAvailability(Loc, subs, Where);
    return Action::Continue;
  }

  Action visitBoundGenericType(BoundGenericType *ty) override {
    visitTypeDecl(ty->getDecl());

    auto subs = ty->getContextSubstitutionMap();
    (void)diagnoseSubstitutionMapAvailability(
        Loc, subs, Where,
        /*depTy=*/Type(),
        /*replacementTy=*/Type(),
        /*warnIfConformanceUnavailablePreSwift6=*/false,
        /*suppressParameterizationCheckForOptional=*/ty->isOptional(),
        /*preconcurrency*/ty->getAnyNominal()->preconcurrency());
    return Action::Continue;
  }

  Action visitTypeAliasType(TypeAliasType *ty) override {
    visitTypeDecl(ty->getDecl());

    auto subs = ty->getSubstitutionMap();
    (void) diagnoseSubstitutionMapAvailability(Loc, subs, Where);
    return Action::Continue;
  }

  // We diagnose unserializable Clang function types in the
  // post-visitor so that we diagnose any unexportable component
  // types first.
  Action walkToTypePost(Type T) override {
    if (Where.mustOnlyReferenceExportedDecls()) {
      if (auto fnType = T->getAs<AnyFunctionType>()) {
        if (auto clangType = fnType->getClangTypeInfo().getType()) {
          auto *DC = Where.getDeclContext();
          auto &ctx = DC->getASTContext();
          auto loader = ctx.getClangModuleLoader();
          // Serialization will serialize the sugared type if it can,
          // but we need the canonical type to be serializable or else
          // canonicalization (e.g. in SIL) might break things.
          if (!loader->isSerializable(clangType, /*check canonical*/ true)) {
            ctx.Diags.diagnose(Loc, diag::unexportable_clang_function_type, T);
          }
        }
      }
    }

    return TypeDeclFinder::walkToTypePost(T);
  }
};

}

void swift::diagnoseTypeAvailability(const TypeRepr *TR, Type T, SourceLoc loc,
                                     const ExportContext &where,
                                     DeclAvailabilityFlags flags) {
  if (diagnoseTypeReprAvailability(TR, where, flags))
    return;

  if (!T)
    return;
  T.walk(ProblematicTypeFinder(loc, where, flags));
}

static void diagnoseMissingConformance(
    SourceLoc loc, Type type, ProtocolDecl *proto, const DeclContext *fromDC,
    bool preconcurrency) {
  assert(proto->isSpecificProtocol(KnownProtocolKind::Sendable));
  diagnoseMissingSendableConformance(loc, type, fromDC, preconcurrency);
}

bool
swift::diagnoseConformanceAvailability(SourceLoc loc,
                                       ProtocolConformanceRef conformance,
                                       const ExportContext &where,
                                       Type depTy, Type replacementTy,
                                       bool warnIfConformanceUnavailablePreSwift6,
                                       bool preconcurrency) {
  assert(!where.isImplicit());

  if (conformance.isInvalid() || conformance.isAbstract())
    return false;

  if (conformance.isPack()) {
    bool diagnosed = false;
    auto *pack = conformance.getPack();
    for (auto patternConf : pack->getPatternConformances()) {
      diagnosed |= diagnoseConformanceAvailability(
          loc, patternConf, where, depTy, replacementTy,
          warnIfConformanceUnavailablePreSwift6,
          preconcurrency);
    }
    return diagnosed;
  }

  const ProtocolConformance *concreteConf = conformance.getConcrete();
  const RootProtocolConformance *rootConf = concreteConf->getRootConformance();

  // Conformance to Copyable and Escapable doesn't have its own availability
  // independent of the type.
  if (rootConf->getProtocol()->getInvertibleProtocolKind())
    return false;

  // Diagnose "missing" conformances where we needed a conformance but
  // didn't have one.
  auto *DC = where.getDeclContext();
  auto &ctx = DC->getASTContext();
  if (auto builtinConformance = dyn_cast<BuiltinProtocolConformance>(rootConf)){
    if (builtinConformance->isMissing()) {
      diagnoseMissingConformance(loc, builtinConformance->getType(),
                                 builtinConformance->getProtocol(), DC,
                                 preconcurrency);
    }
  }

  // Strict memory safety checking.
  if (auto unsafeUses = where.getUnsafeUses()) {
    if (auto normalConf = dyn_cast<NormalProtocolConformance>(rootConf)) {
      // @unsafe conformances are considered... unsafe.
      if (normalConf->getExplicitSafety() == ExplicitSafety::Unsafe) {
        unsafeUses->push_back(
            UnsafeUse::forConformance(
              concreteConf->getType(), conformance, loc));
      }
    }
  }

  auto maybeEmitAssociatedTypeNote = [&]() {
    if (!depTy && !replacementTy)
      return;

    Type selfTy = rootConf->getProtocol()->getSelfInterfaceType();
    if (!depTy->isEqual(selfTy)) {
      ctx.Diags.diagnose(
          loc,
          diag::assoc_conformance_from_implementation_only_module,
          depTy, replacementTy->getCanonicalType());
    }
  };

  if (auto *ext = dyn_cast<ExtensionDecl>(rootConf->getDeclContext())) {
    if (TypeChecker::diagnoseConformanceExportability(loc, rootConf, ext, where,
                                                      warnIfConformanceUnavailablePreSwift6)) {
      maybeEmitAssociatedTypeNote();
      return true;
    }

    auto constraint =
        getAvailabilityConstraintsForDecl(ext, where.getAvailability())
            .getPrimaryConstraint();
    if (constraint) {
      if (diagnoseExplicitUnavailability(loc, *constraint, rootConf, ext, where,
                                         warnIfConformanceUnavailablePreSwift6,
                                         preconcurrency)) {
        maybeEmitAssociatedTypeNote();
        return true;
      }

      // Diagnose (and possibly signal) for potential unavailability
      if (auto requiredRange =
              constraint->getRequiredNewerAvailabilityRange(ctx)) {
        if (diagnosePotentialUnavailability(rootConf, ext, loc, DC,
                                            *requiredRange)) {
          maybeEmitAssociatedTypeNote();
          return true;
        }
      }
    }

    // Diagnose for deprecation
    if (diagnoseIfDeprecated(loc, rootConf, ext, where)) {
      maybeEmitAssociatedTypeNote();

      // Deprecation is just a warning, so keep going with checking the
      // substitution map below.
    }
  }

  // Now, check associated conformances.
  SubstitutionMap subConformanceSubs = concreteConf->getSubstitutionMap();
  if (diagnoseSubstitutionMapAvailability(loc, subConformanceSubs, where,
                                          depTy, replacementTy,
                                          warnIfConformanceUnavailablePreSwift6,
                                          preconcurrency))
    return true;

  return false;
}

bool diagnoseSubstitutionMapAvailability(
    SourceLoc loc, SubstitutionMap subs, const ExportContext &where, Type depTy,
    Type replacementTy, bool warnIfConformanceUnavailablePreSwift6,
    bool suppressParameterizationCheckForOptional,
    bool preconcurrency) {
  bool hadAnyIssues = false;
  for (ProtocolConformanceRef conformance : subs.getConformances()) {
    if (diagnoseConformanceAvailability(loc, conformance, where,
                                        depTy, replacementTy,
                                        warnIfConformanceUnavailablePreSwift6,
                                        preconcurrency))
      hadAnyIssues = true;
  }

  // If we're looking at \c (any P)? (or any other depth of optional) then
  // there's no availability problem.
  if (suppressParameterizationCheckForOptional)
    return hadAnyIssues;

  for (auto replacement : subs.getReplacementTypes()) {
    if (checkTypeMetadataAvailability(replacement, loc, where.getDeclContext()))
      hadAnyIssues = true;
  }
  return hadAnyIssues;
}

/// Should we warn that \p decl needs an explicit availability annotation
/// in -require-explicit-availability mode?
static bool declNeedsExplicitAvailability(const Decl *decl) {
  auto &ctx = decl->getASTContext();

  // Don't require an introduced version on platforms that don't support
  // versioned availability.
  if (!ctx.supportsVersionedAvailability())
    return false;

  // Skip non-public decls.
  if (auto valueDecl = dyn_cast<const ValueDecl>(decl)) {
    AccessScope scope =
      valueDecl->getFormalAccessScope(/*useDC*/nullptr,
                                      /*treatUsableFromInlineAsPublic*/true);
    if (!scope.isPublic())
      return false;
  }

  // Skip functions emitted into clients, SPI or implicit.
  if (decl->getAttrs().hasAttribute<AlwaysEmitIntoClientAttr>() ||
      decl->isSPI() ||
      decl->isImplicit())
    return false;

  // Skip unavailable decls.
  if (decl->isUnavailable())
    return false;

  // Warn on decls without an introduction version.
  auto safeRangeUnderApprox = AvailabilityInference::availableRange(decl);
  return safeRangeUnderApprox.isAlwaysAvailable();
}

void swift::checkExplicitAvailability(Decl *decl) {
  // Skip if the command line option was not set and
  // accessors as we check the pattern binding decl instead.
  auto &ctx = decl->getASTContext();
  if (isa<AccessorDecl>(decl))
    return;

  DiagnosticBehavior DiagLevel;
  switch (ctx.LangOpts.RequireExplicitAvailabilityBehavior) {
  case LangOptions::RequireExplicitAvailabilityDiagnosticBehavior::Ignore:
    return;
  case LangOptions::RequireExplicitAvailabilityDiagnosticBehavior::Warning:
    DiagLevel = DiagnosticBehavior::Warning;
    break;
  case LangOptions::RequireExplicitAvailabilityDiagnosticBehavior::Error:
    DiagLevel = DiagnosticBehavior::Error;
    break;
  }

  // Only look at decls at module level or in extensions.
  // This could be changed to force having attributes on all decls.
  if (!decl->getDeclContext()->isModuleScopeContext() &&
      !isa<ExtensionDecl>(decl->getDeclContext())) return;

  if (auto extension = dyn_cast<ExtensionDecl>(decl)) {
    // decl should be either a ValueDecl or an ExtensionDecl.
    auto extended = extension->getExtendedNominal();
    if (!extended || !extended->getFormalAccessScope().isPublic())
      return;

    // Skip extensions without public members or conformances.
    auto members = extension->getMembers();
    auto hasMembers = std::any_of(members.begin(), members.end(),
                                  [](const Decl *D) -> bool {
      if (auto VD = dyn_cast<ValueDecl>(D))
        if (declNeedsExplicitAvailability(VD))
          return true;
      return false;
    });

    auto hasProtocols = hasConformancesToPublicProtocols(extension);

    if (!hasMembers && !hasProtocols) return;

  } else if (auto pbd = dyn_cast<PatternBindingDecl>(decl)) {
    // Check the first var instead.
    if (pbd->getNumPatternEntries() == 0)
      return;

    llvm::SmallVector<VarDecl *, 2> vars;
    pbd->getPattern(0)->collectVariables(vars);
    if (vars.empty())
      return;

    decl = vars.front();
  }

  if (declNeedsExplicitAvailability(decl)) {
    auto diag = decl->diagnose(diag::public_decl_needs_availability);
    diag.limitBehavior(DiagLevel);

    auto suggestPlatform = ctx.LangOpts.RequireExplicitAvailabilityTarget;
    if (!suggestPlatform.empty()) {
      auto InsertLoc = decl->getAttributeInsertionLoc(/*forModifiers=*/false);
      if (InsertLoc.isInvalid())
        return;

      std::string AttrText;
      {
         llvm::raw_string_ostream Out(AttrText);

         StringRef OriginalIndent = Lexer::getIndentationForLine(
           ctx.SourceMgr, InsertLoc);
         Out << "@available(" << suggestPlatform << ", *)\n"
             << OriginalIndent;
      }

      diag.fixItInsert(InsertLoc, AttrText);
    }
  }
}

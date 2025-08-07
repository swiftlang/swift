//===--- AvailabilityScopeBuilder.cpp - Swift Availability Scope Builder --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the AvailabilityScope::buildForSourceFile() function.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AvailabilityScope.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/AvailabilityInference.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclExportabilityVisitor.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Parse/Lexer.h"

using namespace swift;

static bool computeContainedByDeploymentTarget(AvailabilityScope *scope,
                                               ASTContext &ctx) {
  return scope->getPlatformAvailabilityRange().isContainedIn(
      AvailabilityRange::forDeploymentTarget(ctx));
}

namespace {
/// A class that walks the AST to build the availability scope tree.
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

  AvailabilityScope *getCurrentScope() { return ContextStack.back().Scope; }

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

  const AvailabilityContext constrainCurrentAvailabilityWithContext(
      const AvailabilityContext &otherContext) {
    auto availability = getCurrentScope()->getAvailabilityContext();
    availability.constrainWithContext(otherContext, Context);
    return availability;
  }

  void pushContext(AvailabilityScope *scope, ParentTy popAfterNode) {
    ContextInfo info;
    info.Scope = scope;
    info.ScopeNode = popAfterNode;

    if (!ContextStack.empty() && isCurrentScopeContainedByDeploymentTarget()) {
      assert(computeContainedByDeploymentTarget(scope, Context) &&
             "incorrectly skipping computeContainedByDeploymentTarget()");
      info.ContainedByDeploymentTarget = true;
    } else {
      info.ContainedByDeploymentTarget =
          computeContainedByDeploymentTarget(scope, Context);
    }

    ContextStack.push_back(info);
  }

  void pushDeclBodyContext(
      Decl *decl, llvm::SmallVector<std::pair<ASTNode, AvailabilityScope *>, 4>
                      nodesAndScopes) {
    DeclBodyContextInfo info;
    info.Decl = decl;
    for (auto nodeAndScope : nodesAndScopes) {
      info.BodyScopes.insert(nodeAndScope);
    }

    DeclBodyContextStack.push_back(info);
  }

  const char *stackTraceAction() const {
    return "building availability scope for";
  }

  friend class swift::ExpandChildAvailabilityScopesRequest;

public:
  AvailabilityScopeBuilder(AvailabilityScope *scope, ASTContext &ctx)
      : Context(ctx) {
    assert(scope);
    pushContext(scope, ParentTy());
    DeclContextStack.push_back(scope->getIntroductionNode().getDeclContext());
  }

  void build(Decl *decl) {
    PrettyStackTraceDecl trace(stackTraceAction(), decl);
    unsigned stackHeight = ContextStack.size();
    decl->walk(*this);
    assert(ContextStack.size() == stackHeight);
    (void)stackHeight;
  }

  void build(Stmt *stmt) {
    PrettyStackTraceStmt trace(Context, stackTraceAction(), stmt);
    unsigned stackHeight = ContextStack.size();
    stmt->walk(*this);
    assert(ContextStack.size() == stackHeight);
    (void)stackHeight;
  }

  void build(Expr *expr) {
    PrettyStackTraceExpr trace(Context, stackTraceAction(), expr);
    unsigned stackHeight = ContextStack.size();
    expr->walk(*this);
    assert(ContextStack.size() == stackHeight);
    (void)stackHeight;
  }

private:
  MacroWalking getMacroWalkingBehavior() const override {
    // Expansion buffers will have their type availability scopes built lazily.
    return MacroWalking::Arguments;
  }

  /// Check whether this declaration is within a macro expansion buffer that
  /// will have its own availability scope that will be lazily expanded.
  bool isDeclInMacroExpansion(Decl *decl) const override {
    // If it's not in a macro expansion relative to its context, it's not
    // considered to be in a macro expansion.
    if (!decl->isInMacroExpansionInContext())
      return false;

    auto parentModule = decl->getDeclContext()->getParentModule();
    auto *declFile =
        parentModule->getSourceFileContainingLocation(decl->getLoc());
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

  bool shouldSkipDecl(Decl *decl) const {
    // Only visit a node that has a corresponding concrete syntax node if we are
    // already walking that concrete syntax node.
    auto *concreteDecl = decl->getConcreteSyntaxDeclForAttributes();
    if (concreteDecl != decl) {
      if (ConcreteDeclStack.empty() || ConcreteDeclStack.back() != concreteDecl)
        return true;
    }

    return false;
  }

  PreWalkAction walkToDeclPre(Decl *decl) override {
    PrettyStackTraceDecl trace(stackTraceAction(), decl);

    // Implicit decls don't have source locations so they cannot have a scope.
    // However, some implicit nodes contain non-implicit nodes (e.g. defer
    // blocks) so we must continue through them.
    if (!decl->isImplicit()) {
      if (shouldSkipDecl(decl))
        return Action::SkipNode();

      // The AST of this decl may not be ready to traverse yet if it hasn't been
      // full typechecked. If that's the case, we leave a placeholder node in
      // the tree to indicate that the subtree should be expanded lazily when it
      // needs to be traversed.
      if (buildLazyContextForDecl(decl))
        return Action::SkipNode();

      // Adds in a scope that covers the entire declaration.
      if (auto declScope = getNewContextForSignatureOfDecl(decl)) {
        pushContext(declScope, decl);
      }

      // Create scopes that cover only the body of the declaration.
      buildContextsForBodyOfDecl(decl);
    }

    if (auto *declContext = dyn_cast<DeclContext>(decl)) {
      DeclContextStack.push_back(declContext);
    }

    // If this decl is the concrete syntax decl for some abstract syntax decl,
    // push it onto the stack so that the abstract syntax decls may be visited.
    auto *abstractDecl = decl->getAbstractSyntaxDeclForAttributes();
    if (abstractDecl != decl) {
      ConcreteDeclStack.push_back(decl);
    }
    return Action::Continue();
  }

  PostWalkAction walkToDeclPost(Decl *decl) override {
    if (!ConcreteDeclStack.empty() && ConcreteDeclStack.back() == decl) {
      ConcreteDeclStack.pop_back();
    }

    if (auto *declContext = dyn_cast<DeclContext>(decl)) {
      assert(DeclContextStack.back() == declContext);
      DeclContextStack.pop_back();
    }

    while (ContextStack.back().ScopeNode.getAsDecl() == decl) {
      ContextStack.pop_back();
    }

    while (!DeclBodyContextStack.empty() &&
           DeclBodyContextStack.back().Decl == decl) {
      // All pending body scopes should have been consumed.
      assert(DeclBodyContextStack.back().BodyScopes.empty());
      DeclBodyContextStack.pop_back();
    }

    return Action::Continue();
  }

  bool shouldBuildLazyContextForDecl(Decl *decl) {
    // Skip functions that have unparsed bodies on an initial descent to avoid
    // eagerly parsing bodies unnecessarily.
    if (auto *afd = dyn_cast<AbstractFunctionDecl>(decl)) {
      if (afd->hasBody() && !afd->isBodySkipped() &&
          !afd->getBody(/*canSynthesize=*/false))
        return true;
    }

    // Pattern binding declarations may have attached property wrappers that
    // get expanded from macros attached to the parent declaration. We must
    // not eagerly expand the attached property wrappers to avoid request
    // cycles.
    if (isa<PatternBindingDecl>(decl))
      return true;

    if (isa<ExtensionDecl>(decl))
      return true;

    return false;
  }

  /// For declarations that were previously skipped prepare the AST before
  /// building out scopes.
  void prepareDeclForLazyExpansion(Decl *decl) {
    if (auto afd = dyn_cast<AbstractFunctionDecl>(decl))
      (void)afd->getBody(/*canSynthesize=*/true);
  }

  /// Constructs a placeholder scope that should be expanded later. This is
  /// useful for postponing unnecessary work (and request triggers) when
  /// initally building out the scope subtree under a declaration. Lazy nodes
  /// constructed here will be expanded by ExpandChildAvailabilityScopesRequest.
  /// Returns true if a node was created.
  bool buildLazyContextForDecl(Decl *decl) {
    // Check whether the current scope is already a lazy placeholder. If it is,
    // we should try to expand it rather than creating a new placeholder.
    auto currentScope = getCurrentScope();
    if (currentScope->getNeedsExpansion() &&
        currentScope->getDeclOrNull() == decl)
      return false;

    if (!shouldBuildLazyContextForDecl(decl))
      return false;

    // If we've made it this far then we've identified a declaration that
    // requires lazy expansion later.
    auto lazyScope = AvailabilityScope::createForDeclImplicit(
        Context, decl, currentScope, currentScope->getAvailabilityContext(),
        refinementSourceRangeForDecl(decl));
    lazyScope->setNeedsExpansion(true);
    return true;
  }

  /// Returns a new context to be introduced for the declaration, or nullptr
  /// if no new context should be introduced.
  AvailabilityScope *getNewContextForSignatureOfDecl(Decl *decl) {
    if (!isa<ValueDecl>(decl) && !isa<EnumCaseDecl>(decl) &&
        !isa<ExtensionDecl>(decl) && !isa<MacroExpansionDecl>(decl) &&
        !isa<PatternBindingDecl>(decl))
      return nullptr;

    // Only introduce for an AbstractStorageDecl if it is not local. We
    // introduce for the non-local case because these may have getters and
    // setters (and these may be synthesized, so they might not even exist yet).
    if (isa<AbstractStorageDecl>(decl) &&
        decl->getDeclContext()->isLocalContext())
      return nullptr;

    // Don't introduce for abstract syntax nodes that have separate concrete
    // syntax nodes. The scope will be introduced for the concrete node instead.
    if (decl->getConcreteSyntaxDeclForAttributes() != decl)
      return nullptr;

    // Declarations with explicit availability attributes always get a scope.
    if (decl->hasAnyActiveAvailableAttr()) {
      return AvailabilityScope::createForDecl(
          Context, decl, getCurrentScope(),
          getEffectiveAvailabilityForDeclSignature(decl),
          refinementSourceRangeForDecl(decl));
    }

    // Declarations without explicit availability attributes get a scope if they
    // are effectively less available than the surrounding context. For example,
    // an internal property in a public struct can be effectively less available
    // than the containing struct decl because the internal property will only
    // be accessed by code running at the deployment target or later.
    auto currentAvailability = getCurrentScope()->getAvailabilityContext();
    auto effectiveAvailability = getEffectiveAvailabilityForDeclSignature(decl);
    if (currentAvailability != effectiveAvailability)
      return AvailabilityScope::createForDeclImplicit(
          Context, decl, getCurrentScope(), effectiveAvailability,
          refinementSourceRangeForDecl(decl));

    return nullptr;
  }

  const AvailabilityContext
  getEffectiveAvailabilityForDeclSignature(const Decl *decl) {
    auto effectiveIntroduction = AvailabilityRange::alwaysAvailable();

    // Availability attributes are found on abstract syntax decls.
    decl = decl->getAbstractSyntaxDeclForAttributes();

    // As a special case, extension decls are treated as effectively as
    // available as the nominal type they extend, up to the deployment target.
    // This rule is a convenience for library authors who have written
    // extensions without specifying platform availability on the extension
    // itself.
    if (auto *extension = dyn_cast<ExtensionDecl>(decl)) {
      auto extendedType = extension->getExtendedType();
      if (extendedType && !decl->hasAnyMatchingActiveAvailableAttr(
                              [](SemanticAvailableAttr attr) -> bool {
                                return attr.getDomain().isPlatform();
                              })) {
        effectiveIntroduction.intersectWith(
            swift::AvailabilityInference::inferForType(extendedType));

        // We want to require availability to be specified on extensions of
        // types that would be potentially unavailable to the module containing
        // the extension, so limit the effective availability to the deployment
        // target.
        effectiveIntroduction.unionWith(
            AvailabilityRange::forDeploymentTarget(Context));
      }
    }

    if (shouldConstrainSignatureToDeploymentTarget(decl))
      effectiveIntroduction.intersectWith(
          AvailabilityRange::forDeploymentTarget(Context));

    auto availability = getCurrentScope()->getAvailabilityContext();
    availability.constrainWithDeclAndPlatformRange(decl, effectiveIntroduction);
    return availability;
  }

  /// Checks whether the entire declaration, including its signature, should be
  /// constrained to the deployment target. Generally public API declarations
  /// are not constrained since they appear in the interface of the module and
  /// may be consumed by clients with lower deployment targets, but there are
  /// some exceptions.
  bool shouldConstrainSignatureToDeploymentTarget(const Decl *decl) {
    if (isCurrentScopeContainedByDeploymentTarget())
      return false;

    // A declaration inside of a local context always inherits the availability
    // of the parent.
    if (decl->getDeclContext()->isLocalContext())
      return false;

    // As a convenience, explicitly unavailable decls are constrained to the
    // deployment target. There's not much benefit to checking these decls at a
    // lower availability version floor since they can't be invoked by clients.
    if (getCurrentScope()->getAvailabilityContext().isUnavailable() ||
        decl->isUnavailable())
      return true;

    // To remain compatible with a lot of existing SPIs that are declared
    // without availability attributes, constrain them to the deployment target
    // too.
    if (decl->isSPI())
      return true;

    return !isExported(decl);
  }

  /// Returns the source range which should be refined by declaration. This
  /// provides a convenient place to specify the refined range when it is
  /// different than the declaration's source range.
  SourceRange refinementSourceRangeForDecl(Decl *decl) {
    // We require a valid range in order to be able to query for the scope
    // corresponding to a given SourceLoc.
    // If this assert fires, it means we have probably synthesized an implicit
    // declaration without location information. The appropriate fix is
    // probably to gin up a source range for the declaration when synthesizing
    // it.
    assert(decl->getSourceRange().isValid());

    auto &ctx = decl->getASTContext();
    SourceRange range;
    if (auto *storageDecl = dyn_cast<AbstractStorageDecl>(decl)) {
      // Use the declaration's availability for the context when checking
      // the bodies of its accessors.
      range = storageDecl->getSourceRange();

      // HACK: For synthesized trivial accessors we may have not a valid
      // location for the end of the braces, so in that case we will fall back
      // to using the range for the storage declaration. The right fix here is
      // to update AbstractStorageDecl::addTrivialAccessors() to take brace
      // locations and have callers of that method provide appropriate source
      // locations.
      SourceRange bracesRange = storageDecl->getBracesRange();
      if (bracesRange.isValid()) {
        range.widen(bracesRange);
      }
    } else {
      range = decl->getSourceRangeIncludingAttrs();
    }

    range.End = Lexer::getLocForEndOfToken(ctx.SourceMgr, range.End);
    return range;
  }

  /// Enumerate the AST nodes and their corresponding source ranges for
  /// the body (or bodies) of the given declaration.
  void enumerateBodyRanges(
      Decl *decl,
      llvm::function_ref<void(Decl *decl, ASTNode body, SourceRange)>
          acceptBody) {
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
        if (auto body = afd->getBody(/*canSynthesize=*/false)) {
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

  /// Creates an implicit decl scope specifying the deployment target for
  /// `range` in `decl`.
  AvailabilityScope *
  createImplicitDeclContextForDeploymentTarget(Decl *decl, SourceRange range) {
    auto availability = constrainCurrentAvailabilityWithPlatformRange(
        AvailabilityRange::forDeploymentTarget(Context));
    return AvailabilityScope::createForDeclImplicit(
        Context, decl, getCurrentScope(), availability, range);
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

  void buildContextsForBodyOfDecl(Decl *decl) {
    // Are we already constrained by the deployment target and the declaration
    // doesn't explicitly allow unsafe constructs in its definition, adding
    // new contexts won't change availability.
    if (isCurrentScopeContainedByDeploymentTarget())
      return;

    // Enumerate all of the body scopes to apply availability.
    llvm::SmallVector<std::pair<ASTNode, AvailabilityScope *>, 4>
        nodesAndScopes;
    enumerateBodyRanges(decl, [&](Decl *decl, ASTNode body, SourceRange range) {
      auto availability = getCurrentScope()->getAvailabilityContext();

      // Apply deployment-target availability if appropriate for this body.
      if (!isCurrentScopeContainedByDeploymentTarget() &&
          bodyIsDeploymentTarget(decl)) {
        availability.constrainWithPlatformRange(
            AvailabilityRange::forDeploymentTarget(Context), Context);
      }

      nodesAndScopes.push_back(
          {body, AvailabilityScope::createForDeclImplicit(
                     Context, decl, getCurrentScope(), availability, range)});
    });

    if (nodesAndScopes.size() > 0)
      pushDeclBodyContext(decl, nodesAndScopes);

    if (!isCurrentScopeContainedByDeploymentTarget()) {
      // Pattern binding declarations can have children corresponding to
      // property wrappers, which we handle separately.
      if (auto *pbd = dyn_cast<PatternBindingDecl>(decl)) {
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

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *stmt) override {
    PrettyStackTraceStmt trace(Context, stackTraceAction(), stmt);

    if (consumeDeclBodyContextIfNecessary(stmt)) {
      return Action::Continue(stmt);
    }

    if (auto *ifStmt = dyn_cast<IfStmt>(stmt)) {
      buildIfStmtRefinementContext(ifStmt);
      return Action::SkipNode(stmt);
    }

    if (auto *guardStmt = dyn_cast<GuardStmt>(stmt)) {
      buildGuardStmtRefinementContext(guardStmt);
      return Action::SkipNode(stmt);
    }

    if (auto *whileStmt = dyn_cast<WhileStmt>(stmt)) {
      buildWhileStmtRefinementContext(whileStmt);
      return Action::SkipNode(stmt);
    }

    return Action::Continue(stmt);
  }

  PostWalkResult<Stmt *> walkToStmtPost(Stmt *stmt) override {
    // If we have multiple guard statements in the same block
    // then we may have multiple availability scopes to pop
    // after walking that block.
    while (!ContextStack.empty() &&
           ContextStack.back().ScopeNode.getAsStmt() == stmt) {
      ContextStack.pop_back();
    }

    return Action::Continue(stmt);
  }

  /// Attempts to consume a scope from the `BodyScopes` of the top of
  /// `DeclBodyContextStack`. Returns \p true if a scope was pushed.
  template <typename T>
  bool consumeDeclBodyContextIfNecessary(T body) {
    if (DeclBodyContextStack.empty())
      return false;

    auto &info = DeclBodyContextStack.back();
    auto iter = info.BodyScopes.find(body);
    if (iter == info.BodyScopes.end())
      return false;

    pushContext(iter->getSecond(), body);
    info.BodyScopes.erase(iter);
    return true;
  }

  /// Builds the availability scope hierarchy for the IfStmt if the guard
  /// introduces a new scope for the Then branch.
  /// There is no need for the caller to explicitly traverse the children
  /// of this node.
  void buildIfStmtRefinementContext(IfStmt *ifStmt) {
    std::optional<AvailabilityContext> thenContext;
    std::optional<AvailabilityContext> elseContext;
    std::tie(thenContext, elseContext) =
        buildStmtConditionRefinementContext(ifStmt->getCond());

    if (thenContext.has_value()) {
      // Create a new context for the Then branch and traverse it in that new
      // context.
      auto availabilityContext =
          constrainCurrentAvailabilityWithContext(*thenContext);
      auto *thenScope = AvailabilityScope::createForIfStmtThen(
          Context, ifStmt, getCurrentDeclContext(), getCurrentScope(),
          availabilityContext);
      AvailabilityScopeBuilder(thenScope, Context).build(ifStmt->getThenStmt());
    } else {
      build(ifStmt->getThenStmt());
    }

    Stmt *elseStmt = ifStmt->getElseStmt();
    if (!elseStmt)
      return;

    // Refine the else branch if we're given a version range for that branch.
    // For now, if present, this will only be the empty range, indicating
    // that the branch is dead. We use it to suppress potential unavailability
    // and deprecation diagnostics on code that definitely will not run with
    // the current platform and minimum deployment target.
    // If we add a more precise version range lattice (i.e., one that can
    // support "<") we should create non-empty contexts for the Else branch.
    if (elseContext.has_value()) {
      // Create a new context for the Then branch and traverse it in that new
      // context.
      auto availabilityContext =
          constrainCurrentAvailabilityWithContext(*elseContext);
      auto *elseScope = AvailabilityScope::createForIfStmtElse(
          Context, ifStmt, getCurrentDeclContext(), getCurrentScope(),
          availabilityContext);
      AvailabilityScopeBuilder(elseScope, Context).build(elseStmt);
    } else {
      build(ifStmt->getElseStmt());
    }
  }

  /// Builds the availability scopes for the WhileStmt if the guard
  /// introduces a new availability scope for the body branch.
  /// There is no need for the caller to explicitly traverse the children
  /// of this node.
  void buildWhileStmtRefinementContext(WhileStmt *whileStmt) {
    std::optional<AvailabilityContext> bodyContext =
        buildStmtConditionRefinementContext(whileStmt->getCond()).first;

    if (bodyContext.has_value()) {
      // Create a new context for the body and traverse it in the new
      // context.
      auto availabilityContext =
          constrainCurrentAvailabilityWithContext(*bodyContext);
      auto *bodyScope = AvailabilityScope::createForWhileStmtBody(
          Context, whileStmt, getCurrentDeclContext(), getCurrentScope(),
          availabilityContext);
      AvailabilityScopeBuilder(bodyScope, Context).build(whileStmt->getBody());
    } else {
      build(whileStmt->getBody());
    }
  }

  /// Builds the availability scopes for the GuardStmt and pushes
  /// the fallthrough scope onto the scope stack so that subsequent
  /// AST elements in the same scope are analyzed in the context of the
  /// fallthrough scope.
  void buildGuardStmtRefinementContext(GuardStmt *guardStmt) {
    // 'guard' statements fall through if all of the guard conditions are true,
    // so we refine the range after the require until the end of the enclosing
    // block:
    //
    // if ... {
    //   guard available(...) else { return } <-- Refined range starts here
    //   ...
    // } <-- Refined range ends here
    //
    // This is slightly tricky because, unlike our other control constructs,
    // the refined region is not lexically contained inside the construct
    // introducing the availability scope.
    std::optional<AvailabilityContext> fallthroughContext;
    std::optional<AvailabilityContext> elseContext;
    std::tie(fallthroughContext, elseContext) =
        buildStmtConditionRefinementContext(guardStmt->getCond());

    if (Stmt *elseBody = guardStmt->getBody()) {
      if (elseContext.has_value()) {
        auto availabilityContext =
            constrainCurrentAvailabilityWithContext(*elseContext);
        auto *trueScope = AvailabilityScope::createForGuardStmtElse(
            Context, guardStmt, getCurrentDeclContext(), getCurrentScope(),
            availabilityContext);

        AvailabilityScopeBuilder(trueScope, Context).build(elseBody);
      } else {
        build(elseBody);
      }
    }

    auto *parentBrace = dyn_cast<BraceStmt>(Parent.getAsStmt());
    assert(parentBrace && "Expected parent of GuardStmt to be BraceStmt");
    if (!fallthroughContext.has_value())
      return;

    // Create a new context for the fallthrough.
    auto fallthroughAvailability =
        constrainCurrentAvailabilityWithContext(*fallthroughContext);
    auto *fallthroughScope = AvailabilityScope::createForGuardStmtFallthrough(
        Context, guardStmt, parentBrace, getCurrentDeclContext(),
        getCurrentScope(), fallthroughAvailability);

    pushContext(fallthroughScope, parentBrace);
  }

  AvailabilityQuery buildAvailabilityQuery(
      const SemanticAvailabilitySpec spec,
      const std::optional<SemanticAvailabilitySpec> &variantSpec) {
    auto domain = spec.getDomain();

    // Variant availability specfications are only supported for platform
    // domains when compiling with a -target-variant.
    if (!Context.LangOpts.TargetVariant)
      ASSERT(!variantSpec);

    auto runtimeRangeForSpec =
        [](const std::optional<SemanticAvailabilitySpec> &spec)
        -> std::optional<AvailabilityRange> {
      if (!spec || spec->isWildcard() || !spec->getDomain().isVersioned())
        return std::nullopt;

      return AvailabilityRange(spec->getRuntimeVersion());
    };

    auto primaryRange = runtimeRangeForSpec(spec);
    auto variantRange = runtimeRangeForSpec(variantSpec);

    switch (domain.getKind()) {
    case AvailabilityDomain::Kind::Embedded:
    case AvailabilityDomain::Kind::SwiftLanguage:
    case AvailabilityDomain::Kind::PackageDescription:
      // These domains don't support queries.
      llvm::report_fatal_error("unsupported domain");

    case AvailabilityDomain::Kind::Universal:
      DEBUG_ASSERT(spec.isWildcard());

      // If all of the specs that matched are '*', then the query trivially
      // evaluates to "true" at compile time.
      if (!variantRange)
        return AvailabilityQuery::constant(domain, true);

      // Otherwise, generate a dynamic query for the variant spec. For example,
      // when compiling zippered for macOS, this should generate a query that
      // just checks the iOS version at runtime:
      //
      //    if #available(iOS 18, *) { ... }
      //
      return AvailabilityQuery::dynamic(variantSpec->getDomain(), primaryRange,
                                        variantRange);

    case AvailabilityDomain::Kind::Platform:
      // Platform checks are always dynamic. The SIL optimizer is responsible
      // eliminating these checks when it can prove that they can never fail
      // (due to the deployment target). We can't perform that analysis here
      // because it may depend on inlining.
      return AvailabilityQuery::dynamic(domain, primaryRange, variantRange);
    case AvailabilityDomain::Kind::Custom:
      auto customDomain = domain.getCustomDomain();
      ASSERT(customDomain);

      switch (customDomain->getKind()) {
      case CustomAvailabilityDomain::Kind::Enabled:
        return AvailabilityQuery::constant(domain, true);
      case CustomAvailabilityDomain::Kind::Disabled:
        return AvailabilityQuery::constant(domain, false);
      case CustomAvailabilityDomain::Kind::Dynamic:
        return AvailabilityQuery::dynamic(domain, primaryRange, variantRange);
      }
    }
  }

  /// Build the availability scopes for a StmtCondition and return a pair of
  /// optional availability contexts, the first for the true branch and the
  /// second for the false branch. A value of `nullopt` for a given branch
  /// indicates that the branch does not introduce a new scope.
  std::pair<std::optional<AvailabilityContext>,
            std::optional<AvailabilityContext>>
  buildStmtConditionRefinementContext(StmtCondition cond) {
    if (Context.LangOpts.DisableAvailabilityChecking)
      return {};

    // Any availability scopes introduced in the statement condition will end
    // at the end of the last condition element.
    StmtConditionElement lastElement = cond.back();

    // Keep track of how many nested availability scopes we have pushed on
    // the scope stack so we can pop them when we're done building the scope
    // for the StmtCondition.
    unsigned nestedCount = 0;

    /// Tracks the state that is necessary to produce the `AvailabilityContext`
    /// for the false flow of the StmtCondition. The builder starts in a state
    /// where the false flow is assumed to be unreachable at runtime and then is
    /// refined by expanding the availability domain range it covers.
    class FalseFlowContextBuilder {
      enum class State {
        // The flow doesn't refine anything yet.
        Empty,
        // The flow has a valid refinement.
        Refined,
        // There is no possible refinement.
        Undefined,
      };

      State state;
      AvailabilityDomain refinedDomain;
      AvailabilityRange availableRange;
      const ASTContext &ctx;

    public:
      FalseFlowContextBuilder(const ASTContext &ctx)
          : state(State::Empty),
            refinedDomain(AvailabilityDomain::forUniversal()),
            availableRange(AvailabilityRange::neverAvailable()), ctx(ctx) {}

      /// Attempts to union the flow's existing domain and range with the given
      /// domain and range. If the given domain is compatible with the flow's
      /// existing domain then the refinement's range will be expanded as
      /// needed. Otherwise, the flow's availability context becomes "undefined"
      /// since it cannot be represented.
      void unionWithRange(const AvailabilityRange &range,
                          AvailabilityDomain domain) {
        switch (state) {
        case State::Empty:
          // There false flow doesn't refine any domain or range yet.
          refinedDomain = domain;
          availableRange = range;
          state = State::Refined;
          return;

        case State::Refined:
          // There's an existing domain and range. As long as the new domains is
          // compatible, then its available range can be expanded if needed.
          if (refinedDomain == domain || (refinedDomain.isActivePlatform(ctx) &&
                                          domain.isActivePlatform(ctx))) {
            availableRange.unionWith(range);
            return;
          }

          // The domains aren't compatible so the availability of the false flow
          // can't be represented.
          state = State::Undefined;
          return;

        case State::Undefined:
          // The availability of the false flow can't be represented.
          return;
        }
      }

      /// Force the availability context of the false flow to be undefined.
      void setUndefined() { state = State::Undefined; }

      /// Constrains the given context using the refined availability that has
      /// been built up.
      AvailabilityContext constrainContext(AvailabilityContext context) {
        auto contextCopy = context;
        switch (state) {
        case State::Empty:
          contextCopy.constrainWithPlatformRange(availableRange, ctx);
          break;
        case State::Refined:
          contextCopy.constrainWithAvailabilityRange(availableRange,
                                                     refinedDomain, ctx);
          break;
        case State::Undefined:
          break;
        }

        return contextCopy;
      }
    };

    AvailabilityScope *startingScope = getCurrentScope();
    FalseFlowContextBuilder falseFlowBuilder(Context);

    // Tracks if we're refining for availability or unavailability.
    std::optional<bool> isUnavailability = std::nullopt;

    for (StmtConditionElement element : cond) {
      auto *currentScope = getCurrentScope();
      auto currentContext = currentScope->getAvailabilityContext();

      // If the element is not a condition, walk it in the current scope.
      if (element.getKind() != StmtConditionElement::CK_Availability) {
        // Assume any condition element that is not a #available() can
        // potentially be false, so conservatively make the false flow's
        // refinement undefined since there is nothing we can prove about it.
        falseFlowBuilder.setUndefined();
        element.walk(*this);
        continue;
      }

      // #available query: introduce a new availability scope for the statement
      // condition elements following it.
      auto *query = element.getAvailability();

      if (isUnavailability == std::nullopt) {
        isUnavailability = query->isUnavailability();
      } else if (isUnavailability != query->isUnavailability()) {
        // Mixing availability with unavailability in the same statement will
        // cause the false flow's version range to be ambiguous. Report it.
        //
        // Technically we can support this by not refining ambiguous flows,
        // but there are currently no legitimate cases where one would have
        // to mix availability with unavailability.
        Context.Diags.diagnose(query->getLoc(),
                               diag::availability_cannot_be_mixed);
        break;
      }

      // If this query expression has no queries, we will not introduce a new
      // availability scope. We do not diagnose here: a diagnostic will already
      // have been emitted by the parser.
      // For #unavailable, empty queries are valid as wildcards are implied.
      if (!query->isUnavailability() && query->getQueries().empty())
        continue;

      auto spec = bestActiveSpecForQuery(query);
      if (!spec) {
        // We couldn't find an active spec so rather than refining, emit a
        // diagnostic and just use the current scope.
        Context.Diags.diagnose(
            query->getLoc(), diag::availability_query_required_for_platform,
            platformString(targetPlatform(Context.LangOpts)));
        falseFlowBuilder.setUndefined();
        continue;
      }

      // When compiling zippered for macCatalyst, we need to collect both
      // a macOS version (the target version) and an iOS/macCatalyst version
      // (the target-variant). These versions will both be passed to a runtime
      // entrypoint that will check either the macOS version or the iOS
      // version depending on the kind of process this code is loaded into.
      std::optional<SemanticAvailabilitySpec> variantSpec =
          (Context.LangOpts.TargetVariant)
              ? bestActiveSpecForQuery(query, /*ForTargetVariant*/ true)
              : std::nullopt;

      query->setAvailabilityQuery(
          buildAvailabilityQuery(*spec, variantSpec)
              .asUnavailable(query->isUnavailability()));

      // Wildcards are expected to be "useless". There may be other specs in
      // this query that are useful when compiling for other platforms.
      if (spec->isWildcard())
        continue;

      auto domain = spec->getDomain();
      auto newContext = currentContext;
      std::optional<AvailabilityRange> trueRange, falseRange;
      std::tie(trueRange, falseRange) =
          statementConditionRangesForSpec(*spec, currentContext);

      if (trueRange)
        newContext.constrainWithAvailabilityRange(*trueRange, domain, Context);

      // Check whether the new context refines availability. If it doesn't, the
      // query is useless and should potentially be diagnosed.
      if (currentContext.isContainedIn(newContext)) {
        // If the explicitly-specified (via #availability) version range for the
        // current scope is completely contained in the range for the spec, then
        // a version query can never be false, so the spec is useless.
        // If so, report this.
        auto explicitRange =
            currentScope->getExplicitAvailabilityRange(domain, Context);
        if (explicitRange && trueRange &&
            explicitRange->isContainedIn(*trueRange)) {
          // Platform unavailability queries never refine availability so don't
          // diangose them.
          if (isUnavailability.value())
            continue;

          DiagnosticEngine &diags = Context.Diags;
          if (currentScope->getReason() != AvailabilityScope::Reason::Root) {
            diags.diagnose(query->getLoc(),
                           diag::availability_query_useless_enclosing_scope,
                           domain.getNameForAttributePrinting());
            diags.diagnose(
                currentScope->getIntroductionLoc(),
                diag::availability_query_useless_enclosing_scope_here);
          }
        }

        continue;
      }

      // If the #available() is not useless then there is a potential false
      // flow and we need to potentially expand the range covered by the false
      // flow branch.
      if (falseRange)
        falseFlowBuilder.unionWithRange(*falseRange, domain);

      auto *scope = AvailabilityScope::createForConditionFollowingQuery(
          Context, query, lastElement, getCurrentDeclContext(), currentScope,
          newContext);

      pushContext(scope, ParentTy());
      ++nestedCount;
    }

    auto startingContext = startingScope->getAvailabilityContext();
    auto falseFlowContext = falseFlowBuilder.constrainContext(startingContext);

    // The version range for the false branch should never have any versions
    // that weren't possible when the condition started evaluating.
    DEBUG_ASSERT(falseFlowContext.isContainedIn(startingContext));

    // If the starting availability context is not completely contained in the
    // false flow context then it must be the case that false flow context
    // is strictly smaller than the starting context (because the false flow
    // context *is* contained in the starting context), so we should introduce a
    // new availability scope for the false flow.
    std::optional<AvailabilityContext> falseRefinement = std::nullopt;
    if (!startingScope->getAvailabilityContext().isContainedIn(
            falseFlowContext)) {
      falseRefinement = falseFlowContext;
    }

    auto makeResult =
        [isUnavailability](std::optional<AvailabilityContext> trueRefinement,
                           std::optional<AvailabilityContext> falseRefinement) {
          if (isUnavailability.has_value() && *isUnavailability) {
            // If this is an unavailability check, invert the result.
            return std::make_pair(falseRefinement, trueRefinement);
          }
          return std::make_pair(trueRefinement, falseRefinement);
        };

    if (nestedCount == 0)
      return makeResult(std::nullopt, falseRefinement);

    AvailabilityScope *nestedScope = getCurrentScope();
    while (nestedCount-- > 0)
      ContextStack.pop_back();

    assert(getCurrentScope() == startingScope);

    return makeResult(nestedScope->getAvailabilityContext(), falseRefinement);
  }

  /// Return the best active spec for the target platform or nullptr if no
  /// such spec exists.
  std::optional<SemanticAvailabilitySpec>
  bestActiveSpecForQuery(PoundAvailableInfo *available,
                         bool forTargetVariant = false) {
    std::optional<SemanticAvailabilitySpec> foundWildcardSpec;
    std::optional<SemanticAvailabilitySpec> bestSpec;

    for (auto spec :
         available->getSemanticAvailabilitySpecs(getCurrentDeclContext())) {
      if (spec.isWildcard()) {
        foundWildcardSpec = spec;
        continue;
      }

      auto domain = spec.getDomain();
      if (!domain.supportsQueries())
        continue;

      if (!domain.isPlatform()) {
        // We found an active, non-platform constraint. It should be the only
        // one, so just return it.
        return spec;
      }

      // FIXME: This is not quite right: we want to handle AppExtensions
      // properly. For example, on the OSXApplicationExtension platform
      // we want to chose the OS X spec unless there is an explicit
      // OSXApplicationExtension spec.
      auto platform = domain.getPlatformKind();
      if (isPlatformActive(platform, Context.LangOpts, forTargetVariant,
                           /* ForRuntimeQuery */ true)) {

        if (!bestSpec ||
            inheritsAvailabilityFromPlatform(
                platform, bestSpec->getDomain().getPlatformKind())) {
          bestSpec = spec;
        }
      }
    }

    if (bestSpec)
      return bestSpec;

    // If we have reached this point, we found no spec for our target, so
    // we return the other spec ('*'), if we found it, or nullptr, if not.
    if (foundWildcardSpec) {
      return foundWildcardSpec;
    } else if (available->isUnavailability()) {
      // For #unavailable, imply the presence of a wildcard.
      // FIXME: [availability] Creating a new wildcard spec here is wasteful.
      SourceLoc loc = available->getRParenLoc();
      return AvailabilitySpec::createWildcard(Context, loc);
    } else {
      return std::nullopt;
    }
  }

  /// For the given spec, returns a pair of availability ranges. The first range
  /// is for the if/then flow and the second is for the if/else flow.
  std::pair<std::optional<AvailabilityRange>, std::optional<AvailabilityRange>>
  statementConditionRangesForSpec(SemanticAvailabilitySpec spec,
                                  const AvailabilityContext &currentContext) {
    static auto always = AvailabilityRange::alwaysAvailable();
    static auto never = AvailabilityRange::neverAvailable();
    ASSERT(!spec.isWildcard());

    auto domain = spec.getDomain();
    if (!domain.isVersioned())
      return {always, never};

    return {AvailabilityRange(spec.getVersion()),
            currentContext.getAvailabilityRange(domain, Context)};
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
    (void)consumeDeclBodyContextIfNecessary(expr);

    if (auto closureExpr = dyn_cast<ClosureExpr>(expr))
      DeclContextStack.push_back(closureExpr);

    return Action::Continue(expr);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *expr) override {
    if (ContextStack.back().ScopeNode.getAsExpr() == expr)
      ContextStack.pop_back();

    if (auto *closureExpr = dyn_cast<ClosureExpr>(expr)) {
      assert(DeclContextStack.back() == closureExpr);
      DeclContextStack.pop_back();
    }

    return Action::Continue(expr);
  }
};

} // end anonymous namespace

AvailabilityScope *AvailabilityScope::getOrBuildForSourceFile(SourceFile &SF) {
  switch (SF.Kind) {
  case SourceFileKind::SIL:
    // SIL doesn't support availability queries.
    return nullptr;
  case SourceFileKind::MacroExpansion:
  case SourceFileKind::DefaultArgument:
  case SourceFileKind::Library:
  case SourceFileKind::Main:
  case SourceFileKind::Interface:
    break;
  }
  ASTContext &ctx = SF.getASTContext();

  // If there's already a root node, then we're done.
  if (auto scope = SF.getAvailabilityScope())
    return scope;

  // The root availability scope reflects the fact that all parts of
  // the source file are guaranteed to be executing on at least the minimum
  // platform version for inlining.
  auto availabilityContext = AvailabilityContext::forInliningTarget(ctx);
  AvailabilityScope *rootScope =
      AvailabilityScope::createForSourceFile(&SF, availabilityContext);
  SF.setAvailabilityScope(rootScope);

  // Build availability scopes, if necessary, for all declarations starting
  // with StartElem.
  AvailabilityScopeBuilder builder(rootScope, ctx);
  for (auto item : SF.getTopLevelItems()) {
    if (auto decl = item.dyn_cast<Decl *>())
      builder.build(decl);
    else if (auto expr = item.dyn_cast<Expr *>())
      builder.build(expr);
    else if (auto stmt = item.dyn_cast<Stmt *>())
      builder.build(stmt);
  }

  return rootScope;
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

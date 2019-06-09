//===--- ASTScopeCreation.cpp - Swift Object-Oriented AST Scope -----------===//
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
///
/// This file implements the creation methods of the ASTScopeImpl ontology.
///
//===----------------------------------------------------------------------===//
#include "swift/AST/ASTScope.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/Support/Compiler.h"
#include <algorithm>

using namespace swift;
using namespace ast_scope;

namespace swift {
namespace ast_scope {

#pragma mark ScopeCreator

class ScopeCreator final {
  /// For allocating scopes.
  ASTContext &ctx;

  /// \c SourceFiles and \c BraceStmts have \c Decls and \c ASTNodes in them
  /// that must be scoped in distant descendants. As scopes are created, pass
  /// down the \c ASTNodes in enclosing \c Decls that really need to be in
  /// subscopes.
  ///
  ///  /// The nodes that must be passed down to deeper levels in the scope
  ///  tree.
  /// In reverse order!
  std::vector<ASTNode> deferredNodesInReverse;

  /// The number of \c Decls in the \c SourceFile that were already seen.
  /// Since parsing can be interleaved with type-checking, on every
  /// lookup, look at creating scopes for any \c Decls beyond this number.
  int numberOfDeclsAlreadySeen = 0;

public:
  ASTSourceFileScope *const sourceFileScope;

private:
  /// The last scope to "adopt" deferred nodes.
  /// When adding \c Decls to a scope tree that have been created since the tree
  /// was originally built, add them as children of this scope.
  ASTScopeImpl *newNodeInjectionPoint;

  /// Catch duplicate nodes in the AST
  /// TODO: better to use a shared pointer? Unique pointer?
  Optional<llvm::DenseSet<void*>> _astDuplicates;
  llvm::DenseSet<void*> &astDuplicates;

public:
  ScopeCreator(SourceFile *SF)
      : ctx(SF->getASTContext()),
        sourceFileScope(constructInContext<ASTSourceFileScope>(SF, this)),
        newNodeInjectionPoint(sourceFileScope),
        _astDuplicates(llvm::DenseSet<void *>()),
        astDuplicates(_astDuplicates.getValue()) {}

private:
  explicit ScopeCreator(ScopeCreator &sc)
      : ctx(sc.ctx), sourceFileScope(sc.sourceFileScope),
        astDuplicates(sc.astDuplicates) {}

public:
  ~ScopeCreator() {
    assert(!haveDeferredNodes() && "Should have consumed all deferred nodes");
  }
  ScopeCreator(const ScopeCreator &) = delete;  // ensure no copies
  ScopeCreator(const ScopeCreator &&) = delete; // ensure no moves

  /// Get a half-copy of me for passing down the tree with its own deferred
  /// nodes
  ScopeCreator &withoutDeferrals() { return *(new (ctx) ScopeCreator(*this)); }

public:
  template <typename ClassToConstruct, typename... Args>
  ClassToConstruct *constructInContext(Args... args) {
    return new (ctx) ClassToConstruct(args...);
  }

  void addAnyNewScopesToTree() {
    pushSourceFileDecls(sourceFileScope->SF->Decls);
    if (!haveDeferredNodes())
      return; // an optimization
    newNodeInjectionPoint->clearCachedSourceRangesOfMeAndAncestors();
    auto *const sourceRangeFixupPoint = newNodeInjectionPoint;
    createScopesForDeferredNodes(newNodeInjectionPoint);
    sourceRangeFixupPoint->cacheSourceRangesOfSlice();
  }

public:
  /// For each deferred node, create scopes as needed and add those scopes as
  /// children of the argument.
  /// Since nodes added to the source file in the future need to go where
  /// deferred ndoes go, remember me as an adopter, too.
  void createScopesForDeferredNodes(ASTScopeImpl *parent) {
    // If a scope is a home for the deferred nodes,
    // it's also the place to add
    setNewNodeInjectionPoint(parent);
    // more Decls to the SourceFile
    while (haveDeferredNodes()) {
      Optional<ASTNode> node = popNextDeferredNodeForAdoptionBy(parent);
      if (!node)
        return;
      if (auto declToScope = node->dyn_cast<Decl *>())
        createScopeFor(declToScope, parent);
      else if (auto stmtToScope = node->dyn_cast<Stmt *>())
        createScopeFor(stmtToScope, parent);
      else if (auto exprToScope = node->dyn_cast<Expr *>())
        createScopeFor(exprToScope, parent);
      else
        llvm_unreachable("Impossible case");
    }
  }

  template <typename StmtOrExprOrDecl>
  void createScopeFor(StmtOrExprOrDecl *, ASTScopeImpl *parent);
  
  template <typename StmtOrExpr>
  bool shouldCreateScope(StmtOrExpr *n) const {
    // Cannot ignore implicit statements because implict return can contain
    // scopes in the expression, such as closures.
    return n;
  }

  bool shouldCreateScope(Decl *d) const {
    if (!d)
      return false;
    // Implicit nodes don't have source information for name lookup.
    if (d->isImplicit())
      return false;
    // Have also seen the following in an AST:
    // Source::
    //
    //  func testInvalidKeyPathComponents() {
    //    let _ = \.{return 0} // expected-error* {{}}
    //  }
    //  class X {
    //    class var a: Int { return 1 }
    //  }
    //
    // AST:
    // clang-format off
    //    (source_file "test.swift"
    //     (func_decl range=[test.swift:1:3 - line:3:3] "testInvalidKeyPathComponents()" interface type='() -> ()' access=internal
    //      (parameter_list range=[test.swift:1:36 - line:1:37])
    //      (brace_stmt range=[test.swift:1:39 - line:3:3]
    //       (pattern_binding_decl range=[test.swift:2:5 - line:2:11]
    //        (pattern_any)
    //        (error_expr implicit type='<null>'))
    //
    //       (pattern_binding_decl range=[test.swift:2:5 - line:2:5] <=== SOURCE RANGE WILL CONFUSE SCOPE CODE
    //        (pattern_typed implicit type='<<error type>>'
    //         (pattern_named '_')))
    //      ...
    // clang-format on
    //
    // So test the SourceRange
    //
    // But wait!
    // var z = $x0 + $x1
    // z
    //
    // z has start == end, but must be pushed to expand source range
    //
    // So, only check source ranges for PatternBindingDecls
    if (isa<PatternBindingDecl>(d) && (d->getStartLoc() == d->getEndLoc()))
      return false;
    return true;
  }

  template <typename Scope, typename... Args>
  /// Create a new scope of class ChildScope initialized with a ChildElement,
  /// expandScope it,
  /// add it as a child of the receiver, and return the child and the scope to
  /// receive more decls.
  Scope *createSubtree(ASTScopeImpl *parent, Args... args) {
    auto *child = constructInContext<Scope>(args...);
    parent->addChild(child, ctx);
    NullablePtr<ASTScopeImpl> moreDecls = child->expandMe(*this);
    return moreDecls ? moreDecls.get() : parent;
  }

  template <typename Scope, typename Portion, typename... Args>
  ASTScopeImpl *createSubtree2D(ASTScopeImpl *parent, Args... args) {
    const Portion *portion = constructInContext<Portion>();
    return createSubtree<Scope>(parent, portion, args...);
  }

  void addChildrenForCapturesAndClosuresIn(Expr *expr, ASTScopeImpl *parent) {
    // Use the ASTWalker to find buried captures and closures
    forEachUniqueClosureIn(expr, [&](NullablePtr<CaptureListExpr> captureList,
                                     ClosureExpr *closureExpr) {
      withoutDeferrals().createSubtree<WholeClosureScope>(parent, closureExpr,
                                                          captureList);
    });
  }

private:
  /// Find all of the (non-nested) closures (and associated capture lists)
  /// referenced within this expression.
  void forEachUniqueClosureIn(
      Expr *expr,
      function_ref<void(NullablePtr<CaptureListExpr>, ClosureExpr *)>
          foundUniqueClosure) {
    forEachClosureIn(expr, [&](NullablePtr<CaptureListExpr> captureList,
                               ClosureExpr *closureExpr) {
      if (!isDuplicate(closureExpr))
        foundUniqueClosure(captureList, closureExpr);
    });
  }

  void forEachClosureIn(
      Expr *expr,
      function_ref<void(NullablePtr<CaptureListExpr>, ClosureExpr *)>
          foundClosure);

  static bool hasCustomAttribute(VarDecl *vd) {
    return AttachedPropertyWrapperScope::getCustomAttributesSourceRange(vd)
        .isValid();
  }

public:
  /// If the pattern has an attached property wrapper, create a scope for it
  /// so it can be looked up.

  void createAttachedPropertyWrapperScope(PatternBindingDecl *patternBinding,
                                          ASTScopeImpl *parent) {
    patternBinding->getPattern(0)->forEachVariable([&](VarDecl *vd) {
      // assume all same as first
      if (hasCustomAttribute(vd))
        withoutDeferrals().createSubtree<AttachedPropertyWrapperScope>(parent,
                                                                       vd);
    });
  }

public:
  /// Create the matryoshka nested generic param scopes (if any)
  /// that are subscopes of the receiver. Return
  /// the furthest descendant.
  /// Last GenericParamsScope includes the where clause
  ASTScopeImpl *createGenericParamScopes(Decl *parameterizedDecl,
                                         GenericParamList *generics,
                                         ASTScopeImpl *parent) {
    if (!generics)
      return parent;
    auto *s = parent;
    for (unsigned i : indices(generics->getParams()))
      if (!isDuplicate(generics->getParams()[i])) {
        s = withoutDeferrals().createSubtree<GenericParamScope>(
                                  s, parameterizedDecl, generics, i);
      }
    return s;
  }

  void addChildrenForAllExplicitAccessors(AbstractStorageDecl *asd,
                                          ASTScopeImpl *parent);

  void
  forEachSpecializeAttrInSourceOrder(Decl *declBeingSpecialized,
                                     function_ref<void(SpecializeAttr *)> fn) {
    llvm::SmallVector<SpecializeAttr *, 8> sortedSpecializeAttrs;
    for (auto *attr : declBeingSpecialized->getAttrs()) {
      if (auto *specializeAttr = dyn_cast<SpecializeAttr>(attr)) {
        if (!isDuplicate(specializeAttr))
          sortedSpecializeAttrs.push_back(specializeAttr);
      }
    }
    const auto &srcMgr = declBeingSpecialized->getASTContext().SourceMgr;
    std::sort(sortedSpecializeAttrs.begin(), sortedSpecializeAttrs.end(),
              [&](const SpecializeAttr *a, const SpecializeAttr *b) {
                return srcMgr.isBeforeInBuffer(a->getLocation(),
                                               b->getLocation());
              });
    for (auto *specializeAttr : sortedSpecializeAttrs)
      fn(specializeAttr);
  }

  bool haveDeferredNodes() const { return !deferredNodesInReverse.empty(); }

  void pushIfNecessary(ASTNode n) {
    // Do not defer VarDecls or Accessors because
    // they get created directly by the pattern code
    // and deferring them pushes them to the wrong place.
    // Then, even though they're ignored, they distort the source range
    // of their parents.

    // An optimization; will be ignored later anyway.
    if (ASTScopeImpl::isCreatedDirectly(n))
      return;

    if (!astDuplicates.insert(n.getOpaqueValue()).second)
      return;

    deferredNodesInReverse.push_back(n);
  }

  template <typename ASTNodelike>
  void pushAllNecessaryNodes(ArrayRef<ASTNodelike> nodesToPrepend) {
    for (int i = nodesToPrepend.size() - 1; i >= 0; --i)
      pushIfNecessary(nodesToPrepend[i]);
  }

  void pushSourceFileDecls(ArrayRef<Decl *> declsToPrepend) {
    pushAllNecessaryNodes(declsToPrepend.slice(numberOfDeclsAlreadySeen));
    numberOfDeclsAlreadySeen = declsToPrepend.size();
  }

  bool isDuplicate(void *p, bool registerDuplicate = true) {
    if (registerDuplicate)
      return !astDuplicates.insert(p).second;
    return astDuplicates.count(p);
  }

private:
  ASTNode popNextDeferredNodeForAdoptionBy(ASTScopeImpl *s) {
    auto f = deferredNodesInReverse.back();
    deferredNodesInReverse.pop_back();
    return f;
  }

  // Maintain last adopter so that when we reenter scope tree building
  // after the parser has added more decls to the source file,
  // we can resume building the scope tree where we left off.

  void setNewNodeInjectionPoint(ASTScopeImpl *s) {
    // We get here for any scope that wants to add a deferred node as a child.
    // But after creating a deeper node that has registered as last adopter,
    newNodeInjectionPoint = s;
  }

public:
  void dump() const { print(llvm::errs()); }

  void print(raw_ostream &out) const {
    int i = 0;
    for (auto n : reverse(deferredNodesInReverse)) {
      out << i++ << ": ";
      n.dump(out);
      out << "\n";
    }
  }

  // Make vanilla new illegal for ASTScopes.
  void *operator new(size_t bytes) = delete;
  // Need this because have virtual destructors
  void operator delete(void *data) {}

  // Only allow allocation of scopes using the allocator of a particular source
  // file.
  void *operator new(size_t bytes, const ASTContext &ctx,
                     unsigned alignment = alignof(ScopeCreator));
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
};
} // ast_scope
} // namespace swift

#pragma mark Scope tree creation and extension

ASTScope *ASTScope::createScopeTreeFor(SourceFile *SF) {
  ScopeCreator *scopeCreator = new (SF->getASTContext()) ScopeCreator(SF);
  auto *scope = new (SF->getASTContext()) ASTScope(scopeCreator->sourceFileScope);
  scopeCreator->addAnyNewScopesToTree();
  return scope;
}

void ASTScope::addAnyNewScopesToTree() {
  assert(impl->SF && impl->scopeCreator);
  impl->scopeCreator->addAnyNewScopesToTree();
}

#pragma mark ASTVisitorForScopeCreation

namespace swift {
namespace ast_scope {

class ASTVisitorForScopeCreation
    : public ASTVisitor<ASTVisitorForScopeCreation, void, void, void, void,
                        void, void, ASTScopeImpl *, ScopeCreator &> {
public:

#pragma mark ASTNodes that do not create scopes

  // Even ignored Decls and Stmts must extend the source range of a scope:
  // E.g. a braceStmt with some definitions that ends in a statement that
  // accesses such a definition must resolve as being IN the scope.

#define VISIT_AND_IGNORE(What)                                                 \
  void visit##What(What *w, ASTScopeImpl *p, ScopeCreator &) {                 \
    p->widenSourceRangeForIgnoredASTNode(w);                                   \
  }

  VISIT_AND_IGNORE(ImportDecl)
  VISIT_AND_IGNORE(EnumCaseDecl)
  VISIT_AND_IGNORE(PrecedenceGroupDecl)
  VISIT_AND_IGNORE(InfixOperatorDecl)
  VISIT_AND_IGNORE(PrefixOperatorDecl)
  VISIT_AND_IGNORE(PostfixOperatorDecl)
  VISIT_AND_IGNORE(GenericTypeParamDecl)
  VISIT_AND_IGNORE(AssociatedTypeDecl)
  VISIT_AND_IGNORE(ModuleDecl)
  VISIT_AND_IGNORE(ParamDecl)
  VISIT_AND_IGNORE(EnumElementDecl)
  VISIT_AND_IGNORE(IfConfigDecl)
  VISIT_AND_IGNORE(PoundDiagnosticDecl)
  VISIT_AND_IGNORE(MissingMemberDecl)

  // This declaration is handled from the PatternBindingDecl
  VISIT_AND_IGNORE(VarDecl)

  // This declaration is handled from addChildrenForAllExplicitAccessors
  VISIT_AND_IGNORE(AccessorDecl)

  VISIT_AND_IGNORE(BreakStmt)
  VISIT_AND_IGNORE(ContinueStmt)
  VISIT_AND_IGNORE(FallthroughStmt)
  VISIT_AND_IGNORE(FailStmt)
  VISIT_AND_IGNORE(ThrowStmt)
  VISIT_AND_IGNORE(PoundAssertStmt)

#undef VISIT_AND_IGNORE

#pragma mark simple creation ignoring deferred nodes

#define VISIT_AND_CREATE(What, ScopeClass)                                     \
  void visit##What(What *w, ASTScopeImpl *p, ScopeCreator &scopeCreator) {     \
    scopeCreator.withoutDeferrals().createSubtree<ScopeClass>(p, w);           \
  }

  VISIT_AND_CREATE(SubscriptDecl, SubscriptDeclScope)
  VISIT_AND_CREATE(IfStmt, IfStmtScope)
  VISIT_AND_CREATE(WhileStmt, WhileStmtScope)
  VISIT_AND_CREATE(RepeatWhileStmt, RepeatWhileScope)
  VISIT_AND_CREATE(DoCatchStmt, DoCatchStmtScope)
  VISIT_AND_CREATE(SwitchStmt, SwitchStmtScope)
  VISIT_AND_CREATE(ForEachStmt, ForEachStmtScope)
  VISIT_AND_CREATE(CatchStmt, CatchStmtScope)
  VISIT_AND_CREATE(CaseStmt, CaseStmtScope)

  // Why don't AbstractFunctionDeclScopes inherit deferred nodes and thereby
  // create a subscope in which they are visible?
  // Consider:
  // func f() { func g() {h()}; func h() { g(); }
  // In Swift local functions can be mutually recursive!
  // So, instead of subscopes, we put the AbstractFunctionDeclScope's for
  // g and h in the same BraceStmtScope and get the local binding mechanism find
  // them.
  VISIT_AND_CREATE(AbstractFunctionDecl, AbstractFunctionDeclScope)

#undef VISIT_AND_CREATE

#pragma mark 2D simple creation (ignoring deferred nodes)

#define VISIT_AND_CREATE_WHOLE_PORTION(What, WhatScope)                        \
  void visit##What(What *w, ASTScopeImpl *p, ScopeCreator &scopeCreator) {     \
    scopeCreator.withoutDeferrals()                                            \
        .createSubtree2D<WhatScope, GenericTypeOrExtensionWholePortion>(p, w); \
  }

  VISIT_AND_CREATE_WHOLE_PORTION(ExtensionDecl, ExtensionScope)
  VISIT_AND_CREATE_WHOLE_PORTION(StructDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(ClassDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(EnumDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(TypeAliasDecl, TypeAliasScope)
  VISIT_AND_CREATE_WHOLE_PORTION(OpaqueTypeDecl, OpaqueTypeScope)
#undef VISIT_AND_CREATE_WHOLE_PORTION

  void visitProtocolDecl(ProtocolDecl *e, ASTScopeImpl *p,
                         ScopeCreator &scopeCreator) {
    e->createGenericParamsIfMissing();
    scopeCreator.withoutDeferrals()
        .createSubtree2D<NominalTypeScope, GenericTypeOrExtensionWholePortion>(
            p, e);
  }

#pragma mark simple creation with deferred nodes

  // Each of the following creates a new scope, so that nodes which were parsed
  // after them need to be placed in scopes BELOW them in the tree. So pass down
  // the deferred nodes.
  void visitGuardStmt(GuardStmt *e, ASTScopeImpl *p,
                      ScopeCreator &scopeCreator) {
    scopeCreator.createSubtree<GuardStmtScope>(p, e);
  }
  void visitDoStmt(DoStmt *ds, ASTScopeImpl *p, ScopeCreator &scopeCreator) {
    scopeCreator.createScopeFor(ds->getBody(), p);
  }
  void visitTopLevelCodeDecl(TopLevelCodeDecl *d, ASTScopeImpl *p,
                             ScopeCreator &scopeCreator) {
    scopeCreator.createSubtree<TopLevelCodeScope>(p, d);
  }

#pragma mark special-case creation

  void visitSourceFile(SourceFile *, ASTScopeImpl *, ScopeCreator &) {
    llvm_unreachable("SourceFiles are orphans.");
  }

  void visitYieldStmt(YieldStmt *ys, ASTScopeImpl *p,
                      ScopeCreator &scopeCreator) {
    for (Expr *e : ys->getYields())
      visitExpr(e, p, scopeCreator);
  }

  void visitDeferStmt(DeferStmt *ds, ASTScopeImpl *p,
                      ScopeCreator &scopeCreator) {
    visitFuncDecl(ds->getTempDecl(), p, scopeCreator);
  }

  void visitBraceStmt(BraceStmt *bs, ASTScopeImpl *p,
                      ScopeCreator &scopeCreator) {
    if (p->areDeferredNodesInANewScope())
      scopeCreator.createSubtree<BraceStmtScope>(p, bs);
    else
      scopeCreator.withoutDeferrals().createSubtree<BraceStmtScope>(p, bs);
  }

  void visitPatternBindingDecl(PatternBindingDecl *patternBinding,
                               ASTScopeImpl *parentScope,
                               ScopeCreator &scopeCreator) {
    scopeCreator.createAttachedPropertyWrapperScope(patternBinding,
                                                    parentScope);
    Decl *const pd = parentScope->getDecl().getPtrOrNull();
    const bool isInTypeDecl =
        pd && (isa<NominalTypeDecl>(pd) || isa<ExtensionDecl>(pd));
    const DeclVisibilityKind vis =
        isInTypeDecl ? DeclVisibilityKind::MemberOfCurrentNominal
                     : DeclVisibilityKind::LocalVariable;
    scopeCreator.createSubtree<PatternEntryDeclScope>(parentScope,
                                                      patternBinding, 0, vis);
  }

  void visitReturnStmt(ReturnStmt *rs, ASTScopeImpl *p,
                       ScopeCreator &scopeCreator) {
    if (rs->hasResult())
      visitExpr(rs->getResult(), p, scopeCreator);
  }

  void visitExpr(Expr *expr, ASTScopeImpl *p, ScopeCreator &scopeCreator) {
    if (!expr)
      return;
    p->widenSourceRangeForIgnoredASTNode(expr);
    scopeCreator.addChildrenForCapturesAndClosuresIn(expr, p);
  }
};
} // namespace ast_scope
} // namespace swift

// These definitions are way down here so it can call into
// ASTVisitorForScopeCreation
template <typename StmtOrExprOrDecl>
void ScopeCreator::createScopeFor(StmtOrExprOrDecl *sed, ASTScopeImpl *parent) {
  if (shouldCreateScope(sed))
      ASTVisitorForScopeCreation().visit(sed, parent, *this);
}

void ScopeCreator::addChildrenForAllExplicitAccessors(AbstractStorageDecl *asd,
                                                      ASTScopeImpl *parent) {
  for (auto accessor : asd->getAllAccessors()) {
    if (!accessor->isImplicit() && accessor->getStartLoc().isValid()) {
      // Accessors are always nested within their abstract storage
      // declaration. The nesting may not be immediate, because subscripts may
      // have intervening scopes for generics.
      if (!isDuplicate(accessor) && parent->getEnclosingAbstractStorageDecl() == accessor->getStorage())
        ASTVisitorForScopeCreation().visitAbstractFunctionDecl(accessor, parent,
                                                               *this);
    }
  }
}

#pragma mark creation helpers

void ASTScopeImpl::addChild(ASTScopeImpl *child, ASTContext &ctx) {
  // If this is the first time we've added children, notify the ASTContext
  // that there's a SmallVector that needs to be cleaned up.
  // FIXME: If we had access to SmallVector::isSmall(), we could do better.
  if (storedChildren.empty())
    ctx.addDestructorCleanup(storedChildren);
  storedChildren.push_back(child);
  assert(!child->getParent() && "child should not already have parent");
  child->parent = this;
}



#pragma mark specific implementations of expansion

// Many kinds of scopes do not absorb declarations from above
NullablePtr<ASTScopeImpl> ASTScopeImpl::expandMe(ScopeCreator &scopeCreator) {
  expandNonSplittingMe(scopeCreator);
  return nullptr;
}

void ASTScopeImpl::expandNonSplittingMe(ScopeCreator &) {}

NullablePtr<ASTScopeImpl>
ASTSourceFileScope::expandMe(ScopeCreator &scopeCreator) {
#error here
  // nsertionPoint = scopeCreator.createScopeFor(d, <#ASTScopeImpl *parent#>)
}

  // Create child scopes for every declaration in a body.

void AbstractFunctionDeclScope::expandNonSplittingMe(
    ScopeCreator &scopeCreator) {
  // Create scopes for specialize attributes
  scopeCreator.forEachSpecializeAttrInSourceOrder(
      decl, [&](SpecializeAttr *specializeAttr) {
        scopeCreator.withoutDeferrals().createSubtree<SpecializeAttributeScope>(
            this, specializeAttr, decl);
      });
  // Create scopes for generic and ordinary parameters.
  // For a subscript declaration, the generic and ordinary parameters are in an
  // ancestor scope, so don't make them here.
  ASTScopeImpl *leaf = this;
  if (!isa<AccessorDecl>(decl)) {
    leaf = scopeCreator.createGenericParamScopes(decl, decl->getGenericParams(),
                                                 leaf);
    if (!decl->isImplicit()) {
      leaf = scopeCreator.withoutDeferrals()
                 .createSubtree<AbstractFunctionParamsScope>(
                     leaf, decl->getParameters(), nullptr);
    }
  }
  // Create scope for the body.
  if (decl->getBody()) {
    if (decl->getDeclContext()->isTypeContext())
      scopeCreator.withoutDeferrals().createSubtree<MethodBodyScope>(leaf,
                                                                     decl);
    else
      scopeCreator.withoutDeferrals().createSubtree<PureFunctionBodyScope>(
          leaf, decl);
  }
}

NullablePtr<ASTScopeImpl>
AbstractFunctionParamsScope::expandMe(ScopeCreator &scopeCreator) {
  // Each initializer for a function parameter is its own, sibling, scope.
  // Unlike generic parameters or pattern initializers, it cannot refer to a
  // previous parameter.
  for (ParamDecl *pd : params->getArray()) {
    if (!scopeCreator.isDuplicate(pd) && pd->getDefaultValue())
      scopeCreator.withoutDeferrals()
          .createSubtree<DefaultArgumentInitializerScope>(this, pd);
  }
  return this; // body of func goes under me
}

void AbstractFunctionBodyScope::expandNonSplittingMe(
    ScopeCreator &scopeCreator) {
  BraceStmt *braceStmt = decl->getBody();
  ASTVisitorForScopeCreation().visitBraceStmt(braceStmt, this, scopeCreator);
}

NullablePtr<ASTScopeImpl>
PatternEntryDeclScope::expandMe(ScopeCreator &scopeCreator) {
  auto patternEntry = getPatternEntry();
  // Create a child for the initializer, if present.
  // Cannot trust the source range given in the ASTScopeImpl for the end of the
  // initializer (because of InterpolatedLiteralStrings and EditorPlaceHolders),
  // so compute it ourselves.
  SourceLoc initializerEnd;
  if (patternEntry.getInitAsWritten() &&
      patternEntry.getInitAsWritten()->getSourceRange().isValid()) {
    auto *initializer = scopeCreator.withoutDeferrals()
                            .createSubtree<PatternEntryInitializerScope>(
                                this, decl, patternEntryIndex, vis);
    initializer->cacheSourceRange();
    initializerEnd = initializer->getSourceRange().End;
  }
  // If there are no uses of the declararations, add the accessors immediately.
  // Create unconditionally because more nodes might be added to SourceFile later.
  // Note: the accessors will follow the pattern binding.
  auto *useScope = scopeCreator.createSubtree<PatternEntryUseScope>(
      this, decl, patternEntryIndex, vis, initializerEnd);
  return useScope;
}

NullablePtr<ASTScopeImpl>
PatternEntryInitializerScope::expandMe(ScopeCreator &scopeCreator) {
  // Create a child for the initializer expression.
  ASTVisitorForScopeCreation().visitExpr(getPatternEntry().getInitAsWritten(),
                                         this, scopeCreator.withoutDeferrals());
  return this; // next PatternEntryDeclScope goes under me
}

NullablePtr<ASTScopeImpl>
PatternEntryUseScope::expandMe(ScopeCreator &scopeCreator) {
  // Add accessors for the variables in this pattern.
  forEachVarDeclWithExplicitAccessors(scopeCreator, false, [&](VarDecl *var) {
    scopeCreator.withoutDeferrals().createSubtree<VarDeclScope>(this, var);
  });
  if (!isLastEntry()) {
    return scopeCreator.createSubtree<PatternEntryDeclScope>(
        this, decl, patternEntryIndex + 1, vis);
  } else {
    // no more entries, create the scopes inside the pattern use
    return this;
  }
}

NullablePtr<ASTScopeImpl>
ConditionalClauseScope::expandMe(ScopeCreator &scopeCreator) {
  createSubtreeForCondition(scopeCreator);
  // If there's another conditional clause, add it as the child.
  if (index + 1 < enclosingStmt->getCond().size())
    return scopeCreator.createSubtree<ConditionalClauseScope>(
        this, enclosingStmt, index + 1, stmtAfterAllConditions);
  if (auto *sceps = statementConditionElementPatternScope.getPtrOrNull())
    return sceps;
  return this;
}

ASTScopeImpl *
LabeledConditionalStmtScope::createCondScopes(ScopeCreator &scopeCreator) {
  if (getLabeledConditionalStmt()->getCond().empty())
    return this;
  return scopeCreator.withoutDeferrals().createSubtree<ConditionalClauseScope>(
      this, getLabeledConditionalStmt(), 0, getStmtAfterTheConditions());
}

void IfStmtScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  auto &sc = scopeCreator.withoutDeferrals();
  ASTScopeImpl *lookupParent = createCondScopes(sc);

  // The 'then' branch
  sc.createScopeFor(stmt->getThenStmt(), lookupParent);

  // Add the 'else' branch, if needed.
  sc.createScopeFor(stmt->getElseStmt(), this);
}

void WhileStmtScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  auto &sc = scopeCreator.withoutDeferrals();
  ASTScopeImpl *lookupParent = createCondScopes(sc);
  sc.createScopeFor(stmt->getBody(), lookupParent);
}

NullablePtr<ASTScopeImpl> GuardStmtScope::expandMe(ScopeCreator &scopeCreator) {
  auto &sc = scopeCreator.withoutDeferrals();
  ASTScopeImpl *lookupParent = createCondScopes(sc);
  // Add a child for the 'guard' body, which always exits.
  // Parent is whole guard stmt scope, NOT the cond scopes
  scopeCreator.createScopeFor(stmt->getBody(), this);

  return sc.createSubtree<ConditionalClauseUseScope>(this, lookupParent,
                                                     stmt->getEndLoc());
}

void RepeatWhileScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  scopeCreator.createScopeFor(stmt->getBody(), this);
  ASTVisitorForScopeCreation().visitExpr(stmt->getCond(), this, scopeCreator);
}

void DoCatchStmtScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  scopeCreator.createScopeFor(stmt->getBody(), this);

  for (auto catchClause : stmt->getCatches()) {
    if (!scopeCreator.isDuplicate(catchClause)) {
      ASTVisitorForScopeCreation().visitCatchStmt(catchClause, this,
                                                  scopeCreator);
    }
  }
}

void SwitchStmtScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getSubjectExpr(), this,
                                         scopeCreator);

  for (auto caseStmt : stmt->getCases()) {
    if (!scopeCreator.isDuplicate(caseStmt)) {
    scopeCreator.withoutDeferrals().createSubtree<CaseStmtScope>(this,
                                                                 caseStmt);
    }
  }
}

void ForEachStmtScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getSequence(), this,
                                         scopeCreator);

  // Add a child describing the scope of the pattern.
  scopeCreator.withoutDeferrals().createSubtree<ForEachPatternScope>(this,
                                                                     stmt);
}

void ForEachPatternScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getWhere(), this, scopeCreator);
  ASTVisitorForScopeCreation().visitBraceStmt(stmt->getBody(), this,
                                              scopeCreator);
}

void CatchStmtScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getGuardExpr(), this,
                                         scopeCreator);
  scopeCreator.createScopeFor(stmt->getBody(), this);
}

void CaseStmtScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  for (auto &caseItem : stmt->getMutableCaseLabelItems())
    ASTVisitorForScopeCreation().visitExpr(caseItem.getGuardExpr(), this,
                                           scopeCreator);

  // Add a child for the case body.
  scopeCreator.createScopeFor(stmt->getBody(), this);
}

NullablePtr<ASTScopeImpl> BraceStmtScope::expandMe(ScopeCreator &scopeCreator) {
  scopeCreator.pushAllNecessaryNodes(stmt->getElements());
  return this;
}

void VarDeclScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  assert(!scopeCreator.haveDeferredNodes() &&
         "Decls needing this var go into the PatternEntryUseScope, not here");
  scopeCreator.addChildrenForAllExplicitAccessors(decl, this);
}

void SubscriptDeclScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  auto *sub = decl;
  auto *leaf =
      scopeCreator.createGenericParamScopes(sub, sub->getGenericParams(), this);
  auto *params = scopeCreator.withoutDeferrals()
                     .createSubtree<AbstractFunctionParamsScope>(
                         leaf, sub->getIndices(), sub->getGetter());
  scopeCreator.addChildrenForAllExplicitAccessors(sub, params);
}

void WholeClosureScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  if (auto *cl = captureList.getPtrOrNull())
    scopeCreator.withoutDeferrals().createSubtree<CaptureListScope>(this, cl);
  ASTScopeImpl *bodyParent = this;
  if (closureExpr->getInLoc().isValid())
    bodyParent =
        scopeCreator.withoutDeferrals().createSubtree<ClosureParametersScope>(
            this, closureExpr, captureList);
  scopeCreator.withoutDeferrals().createSubtree<ClosureBodyScope>(
      bodyParent, closureExpr, captureList);
}

void CaptureListScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  // Patterns here are implicit, so need to dig out the intializers
  for (const CaptureListEntry &captureListEntry : expr->getCaptureList()) {
    for (unsigned patternEntryIndex = 0;
         patternEntryIndex < captureListEntry.Init->getNumPatternEntries();
         ++patternEntryIndex) {
      Expr *init = captureListEntry.Init->getInit(patternEntryIndex);
      scopeCreator.addChildrenForCapturesAndClosuresIn(init, this);
    }
  }
}

void ClosureBodyScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  scopeCreator.withoutDeferrals().createSubtree<BraceStmtScope>(
      this, closureExpr->getBody());
}

void TopLevelCodeScope::expandNonSplittingMe(ScopeCreator &scopeCreator) {
  scopeCreator.createSubtree<BraceStmtScope>(this, decl->getBody());
}

void DefaultArgumentInitializerScope::expandNonSplittingMe(
    ScopeCreator &scopeCreator) {
  auto *initExpr = decl->getDefaultValue();
  assert(initExpr);
  ASTVisitorForScopeCreation().visitExpr(initExpr, this, scopeCreator);
}

void GenericTypeOrExtensionScope::expandNonSplittingMe(
    ScopeCreator &scopeCreator) {
  portion->expandScope(this, scopeCreator);
}

void GenericTypeOrExtensionWholePortion::expandScope(
    GenericTypeOrExtensionScope *scope, ScopeCreator &scopeCreator) const {
  // Prevent circular request bugs caused by illegal input and
  // doing lookups that getExtendedNominal in the midst of getExtendedNominal.
  if (scope->shouldHaveABody() && !scope->doesDeclHaveABody())
    return;

  auto *deepestScope = scopeCreator.createGenericParamScopes(
      scope->getDecl().get(), scope->getGenericContext()->getGenericParams(),
      scope);
  if (scope->getGenericContext()->getTrailingWhereClause())
    deepestScope =
        scope->createTrailingWhereClauseScope(deepestScope, scopeCreator);

  scope->createBodyScope(deepestScope, scopeCreator);
}

void IterableTypeBodyPortion::expandScope(GenericTypeOrExtensionScope *scope,
                                          ScopeCreator &scopeCreator) const {
  if (auto *idc = scope->getIterableDeclContext().getPtrOrNull())
    for (auto member : idc->getMembers())
      if (!scopeCreator.isDuplicate(member))
        scopeCreator.createScopeFor(member, scope);
}

#pragma mark createBodyScope

void ExtensionScope::createBodyScope(ASTScopeImpl *leaf,
                                     ScopeCreator &scopeCreator) {
  scopeCreator.withoutDeferrals()
      .createSubtree2D<ExtensionScope, IterableTypeBodyPortion>(leaf, decl);
}
void NominalTypeScope::createBodyScope(ASTScopeImpl *leaf,
                                       ScopeCreator &scopeCreator) {
  scopeCreator.withoutDeferrals()
      .createSubtree2D<NominalTypeScope, IterableTypeBodyPortion>(leaf, decl);
}

#pragma mark createTrailingWhereClauseScope

ASTScopeImpl *
ExtensionScope::createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                               ScopeCreator &scopeCreator) {
  return scopeCreator.withoutDeferrals()
      .createSubtree2D<ExtensionScope, GenericTypeOrExtensionWherePortion>(
          parent, decl);
}
ASTScopeImpl *
NominalTypeScope::createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                                 ScopeCreator &scopeCreator) {
  return scopeCreator.withoutDeferrals()
      .createSubtree2D<NominalTypeScope, GenericTypeOrExtensionWherePortion>(
          parent, decl);
}
ASTScopeImpl *
TypeAliasScope::createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                               ScopeCreator &scopeCreator) {
  return scopeCreator.withoutDeferrals()
      .createSubtree2D<TypeAliasScope, GenericTypeOrExtensionWherePortion>(
          parent, decl);
}

#pragma mark misc

AbstractPatternEntryScope::AbstractPatternEntryScope(
    PatternBindingDecl *declBeingScoped, unsigned entryIndex,
    DeclVisibilityKind vis)
    : decl(declBeingScoped), patternEntryIndex(entryIndex), vis(vis) {
  assert(entryIndex < declBeingScoped->getPatternList().size() &&
         "out of bounds");
}

void AbstractPatternEntryScope::forEachVarDeclWithExplicitAccessors(
    ScopeCreator &scopeCreator, bool dontRegisterAsDuplicate,
    function_ref<void(VarDecl *)> foundOne) const {
  getPatternEntry().getPattern()->forEachVariable([&](VarDecl *var) {
    // Since I'll be called twice, don't register the first time.
    if (scopeCreator.isDuplicate(var, !dontRegisterAsDuplicate))
      return;
    const bool hasAccessors = var->getBracesRange().isValid();
    if (hasAccessors && !var->isImplicit())
      foundOne(var);
  });
}

bool ASTScopeImpl::isCreatedDirectly(const ASTNode n) {
  // See PatternEntryUseScope::expandMe and addChildrenForAllExplicitAccessors
  if (auto *d = n.dyn_cast<Decl*>())
    return isa<VarDecl>(d) || isa<AccessorDecl>(d);
  return false;
}

void ConditionalClauseScope::createSubtreeForCondition(
    ScopeCreator &scopeCreator) {
  const auto &cond = enclosingStmt->getCond()[index];
  switch (cond.getKind()) {
  case StmtConditionElement::CK_Availability:
    return;
  case StmtConditionElement::CK_Boolean:
    ASTVisitorForScopeCreation().visitExpr(cond.getBoolean(), this,
                                           scopeCreator.withoutDeferrals());
    return;
  case StmtConditionElement::CK_PatternBinding:
    statementConditionElementPatternScope =
        scopeCreator.withoutDeferrals()
            .createSubtree<StatementConditionElementPatternScope>(
                this, cond.getPattern());
    ASTVisitorForScopeCreation().visitExpr(cond.getInitializer(), this,
                                           scopeCreator.withoutDeferrals());
    return;
  }
}

bool AbstractPatternEntryScope::isLastEntry() const {
  return patternEntryIndex + 1 == decl->getPatternList().size();
}

ASTScopeImpl *ConditionalClauseScope::findInnermostConditionScope() {
  auto *const deepest = findDeepestConditionalClauseScope();
  auto statementCond = deepest->getStatementConditionElementPatternScope();
  if (ASTScopeImpl *s = statementCond.getPtrOrNull())
    return s;
  return deepest;
}

ConditionalClauseScope *
ConditionalClauseScope::findDeepestConditionalClauseScope() {
  return nextConditionalClause
             ? nextConditionalClause.get()->findDeepestConditionalClauseScope()
             : this;
}

NullablePtr<StatementConditionElementPatternScope>
ConditionalClauseScope::getStatementConditionElementPatternScope() const {
  return statementConditionElementPatternScope;
}

// Following must be after uses to ensure templates get instantiated
#pragma mark getEnclosingAbstractStorageDecl

NullablePtr<AbstractStorageDecl>
ASTScopeImpl::getEnclosingAbstractStorageDecl() const {
  return nullptr;
}

NullablePtr<AbstractStorageDecl>
SpecializeAttributeScope::getEnclosingAbstractStorageDecl() const {
  return getParent().get()->getEnclosingAbstractStorageDecl();
}
NullablePtr<AbstractStorageDecl>
AbstractFunctionDeclScope::getEnclosingAbstractStorageDecl() const {
  return getParent().get()->getEnclosingAbstractStorageDecl();
}
NullablePtr<AbstractStorageDecl>
AbstractFunctionParamsScope::getEnclosingAbstractStorageDecl() const {
  return getParent().get()->getEnclosingAbstractStorageDecl();
}
NullablePtr<AbstractStorageDecl>
GenericParamScope::getEnclosingAbstractStorageDecl() const {
  return getParent().get()->getEnclosingAbstractStorageDecl();
}

void ScopeCreator::forEachClosureIn(
    Expr *expr, function_ref<void(NullablePtr<CaptureListExpr>, ClosureExpr *)>
                    foundClosure) {
  assert(expr);

  /// AST walker that finds top-level closures in an expression.
  class ClosureFinder : public ASTWalker {
    function_ref<void(NullablePtr<CaptureListExpr>, ClosureExpr *)>
        foundClosure;

  public:
    ClosureFinder(
        function_ref<void(NullablePtr<CaptureListExpr>, ClosureExpr *)>
            foundClosure)
        : foundClosure(foundClosure) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (auto *closure = dyn_cast<ClosureExpr>(E)) {
        foundClosure(nullptr, closure);
        return {false, E};
      }
      if (auto *capture = dyn_cast<CaptureListExpr>(E)) {
        foundClosure(capture, capture->getClosureBody());
        return {false, E};
      }
      return {true, E};
    }
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      if (auto *bs = dyn_cast<BraceStmt>(S)) { // closures hidden in here
        return {true, S};
      }
      return {false, S};
    }
    std::pair<bool, Pattern *> walkToPatternPre(Pattern *P) override {
      return {false, P};
    }
    bool walkToDeclPre(Decl *D) override { return false; }
    bool walkToTypeLocPre(TypeLoc &TL) override { return false; }
    bool walkToTypeReprPre(TypeRepr *T) override { return false; }
    bool walkToParameterListPre(ParameterList *PL) override { return false; }
  };

  expr->walk(ClosureFinder(foundClosure));
}

#pragma mark new operators
void *ASTScopeImpl::operator new(size_t bytes, const ASTContext &ctx,
                                 unsigned alignment) {
  return ctx.Allocate(bytes, alignment);
}

void *Portion::operator new(size_t bytes, const ASTContext &ctx,
                             unsigned alignment) {
  return ctx.Allocate(bytes, alignment);
}
void *ASTScope::operator new(size_t bytes, const ASTContext &ctx,
                             unsigned alignment) {
  return ctx.Allocate(bytes, alignment);
}
void *ScopeCreator::operator new(size_t bytes, const ASTContext &ctx,
                                 unsigned alignment) {
  return ctx.Allocate(bytes, alignment);
}

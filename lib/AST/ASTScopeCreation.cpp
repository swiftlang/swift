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

class ScopeCreator {

  /// \c SourceFiles and \c BraceStmts have \c Decls and \c ASTNodes in them
  /// that must be scoped in distant descendants. As scopes are created, pass
  /// down the \c ASTNodes in enclosing \c Decls that really need to be in
  /// subscopes.
  ///
  ///  /// The nodes that must be passed down to deeper levels in the scope
  ///  tree.
  /// In reverse order!
  std::vector<ASTNode> nodesInReverse;

  /// The number of \c Decls in the \c SourceFile that were already processed
  /// into scopes. Since parsing can be interleaved with type-checking, on every
  /// lookup, create scopes for any \c Decls beyond this number.
  int numberOfDeclsAlreadyProcessed = 0;

  ASTSourceFileScope *sourceFileScope;

  /// The last scope to "adopt" deferred nodes.
  /// When adding \c Decls to a scope tree that have been created since the tree
  /// was originally built, add them as children of this scope.
  ASTSourceFileImpl *lastAdopter;

  ASTContext &ctx;

public:
  static createScopeTreeFor(SourceFile *);
  SourceFileScope *const sourceFileScope;

public:
  ScopeCreator(SourceFile *SF) ctx(SF->getASTContext()),
      sourceFileScope(constructScope<SourceFileScope>(SF)),
      lastAdopter(sourceFileScope) {}

  ~ScopeCreator();
  ScopeCreator(const DeferredNodes &) = delete;  // ensure no copies
  ScopeCreator(const DeferredNodes &&) = delete; // ensure no moves
  
  ScopeCreator withoutDeferrals();

  template <typename Scope, typename... Args>
  static ASTScopeImpl *constructScope(Args... args) {
    return new (ctx) Scope(args...);
  }

  void addAnyNewDeclsToTree() {
    pushSourceFileDecls(sourceFileScope->SF->Decls);
    addChildScopesForAnyRemainingDeferredNodesTo(lastAdopter);
  }

protected:
  /// Create and expandScope scopes for any deferred nodes, adding those scopes
  /// as children of the receiver.

  addChildScopesForAnyRemainingDeferredNodesTo(ASTScopeImpl *parent) {
    parent->clearCachedSourceRangesOfAncestors();
    createScopeForNextDeferredNode(parent);
    parent->cacheSourceRangesOfAncestors();
  }

public:
  /// For each deferred node, create scopes as needed and add those scopes as
  /// children of the argument.
  void createScopeForNextDeferredNode(ASTScopeImpl *parent) {
    setLastAdopter(parent); // in case we come back here later after adding
    // more Decls to the SourceFile
    while (!empty()) {
      Optional<ASTNode> node = popNextDeferredNodeForAdoptionBy(this);
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
  bool shouldCreateScope(StmtOrExpr *se) const {
    return se;
  }

  bool shouldCreateScope(Decl *) const {
    if (!d)
      return false;
    // Implicit declarations don't have source information for name lookup.
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
    //  (pattern_binding_decl range=[test.swift:2:3 - line:2:3]
    //   (pattern_typed implicit type='<<error type>>'
    //     (pattern_named '_')))
    //
    // So test the SourceRange
    if (d->getStartLoc() == d->getEndLoc())
      return false;
    return true;
  }
  
template <typename Scope, typename... Args>
  ASTScopeImpl* createSubtree(ASTScope* parent, Args... args) {
    auto *child = constructScope<Scope>(args);
    parent->addChild(child);
    child->expandMeAndCreateScopesFromDeferredNodes(*this);
    return child;
  }
  
  template <typename Scope, typename Portion, typename... Args>
  ASTScopeImpl* createSubtree2D(ASTScope* parent, Args... args) {
    const Portion *portion = new (ctx) Portion();
    return createSubtree(parent, portion, args...);
  }
  
  

  bool empty() const;

  void pushIfNecessary(ASTNode);
  template <typename ASTNodelike>
  void pushAllNecessaryNodes(ArrayRef<ASTNodelike>);
  void pushSourceFileDecls(ArrayRef<Decl *>);

  ASTNode popNextDeferredNodeForAdoptionBy(ASTScopeImpl *);

  NullablePtr<ASTScopeImpl> getLastAdopter() const;
  void setLastAdopter(ASTScopeImpl *);

  void dump() const;
  void print(raw_ostream &) const;

  // Make vanilla new illegal for ASTScopes.
  void *operator new(size_t bytes) = delete;
  // Need this because have virtual destructors
  void operator delete(void *data) {}

  // Only allow allocation of scopes using the allocator of a particular source
  // file.
  void *operator new(size_t bytes, const ASTContext &ctx,
                     unsigned alignment = alignof(DeferredNodes));
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
};



#pragma mark Scope tree creation and extension

ASTScope *ASTScope::createScopeTreeFor(SourceFile *SF) {
  auto *sfs = ASTSourceFileScope::createScopeTreeFor(SF);
  return new (SF->getASTContext()) ASTScope(sfs);
}

ASTSourceFileScope *ScopeCreator : createScopeTreeFor(SourceFile *SF) {
  auto *sfScope = new (SF->getASTContext()) ASTSourceFileScope(SF);
  ScopeCreator s(sfScope);
  s.addAnyNewDeclsToTree();
  return sfScope;
}

ASTSourceFileScope::ASTSourceFileScope(SourceFile *SF)
    : SF(SF), deferred(new (SF->getASTContext())
                           DeferredNodes(__FILE__, __LINE__, *this, SF)) {}

#pragma mark ASTVisitorForScopeCreation

namespace swift {
namespace ast_scope {

class ASTVisitorForScopeCreation
    : public ASTVisitor<ASTVisitorForScopeCreation, void, void, void, void, void, void,
                        ASTScopeImpl *, ScopeCreator &> {
public:

#pragma mark ASTNodes that do not create scopes

  // Even ignored Decls and Stmts must extend the source range of a scope:
  // E.g. a braceStmt with some definitions that ends in a statement that
  // accesses such a definition must resolve as being IN the scope.

#define VISIT_AND_IGNORE(What)                                                 \
  void visit##What(What *w, ASTScopeImpl *p, ScopeCreator &) {                \
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
void visit##What(What *w, ASTScopeImpl *p, ScopeCreator &scopeCreator) { \
         scopeCreator.withoutDeferrals().createSubtree<ScopeClass>(p, w); \
  }
UP TO HERE
  VISIT_AND_CREATE(SubscriptDecl, SubscriptDeclScope)
  VISIT_AND_CREATE(IfStmt, IfStmtScope)
  VISIT_AND_CREATE(RepeatWhileStmt, RepeatWhileScope)
  VISIT_AND_CREATE(DoCatchStmt, DoCatchStmtScope)
  VISIT_AND_CREATE(SwitchStmt, SwitchStmtScope)
  VISIT_AND_CREATE(ForEachStmt, ForEachStmtScope)
  VISIT_AND_CREATE(CatchStmt, CatchStmtScope)
  VISIT_AND_CREATE(CaseStmt, CaseStmtScope)

  // Why don't AbstractFunctionDeclScopes inherit DeferredNodes and thereby
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
  void visit##What(What *w, ASTScopeImpl *p, ScopeCreator &scopeCreator) {                \
UP TO HERE \
    scopeCreator.withoutDeferrals.createSubtree2D<WhatScope, GTXWholePortion>(p, w);                         \
  }

  VISIT_AND_CREATE_WHOLE_PORTION(ExtensionDecl, ExtensionScope)
  VISIT_AND_CREATE_WHOLE_PORTION(StructDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(ClassDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(EnumDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(TypeAliasDecl, TypeAliasScope)
  VISIT_AND_CREATE_WHOLE_PORTION(OpaqueTypeDecl, OpaqueTypeScope)
#undef VISIT_AND_CREATE_WHOLE_PORTION

  void visitProtocolDecl(ProtocolDecl *e, ASTScopeImpl *p, ScopeCreator &scopeCreator) {
    e->createGenericParamsIfMissing();
    UP TO HERE
    scopeCreator.withoutDeferrals.createSubtree2D<NominalTypeScope, GTXWholePortion>(p. e);
  }

#pragma mark simple creation with deferred nodes

  // Each of the following creates a new scope, so that nodes which were parsed
  // after them need to be placed in scopes BELOW them in the tree. So pass down
  // the deferred nodes.
  void visitGuardStmt(GuardStmt *e, ASTScopeImpl *p, ScopeCreator &scopeCreator) {
    scopeCreator.createSubtree<GuardStmtScope>(p, e);
  }
  void visitDoStmt(DoStmt *ds, ASTScopeImpl *p, ScopeCreator &scopeCreator) {
    creationState.createScopeFor(ds->getBody(), p);
  }
  void visitTopLevelCodeDecl(TopLevelCodeDecl *d, ASTScopeImpl *p,
                             ScopeCreator &scopeCreator) {
    scopeCreator.createSubtree<TopLevelCodeScope>(p, d);
  }

#pragma mark special-case creation

  void visitSourceFile(SourceFile *, ASTScopeImpl *, ScopeCreator &) {
    llvm_unreachable("SourceFiles are orphans.");
  }

  void visitYieldStmt(YieldStmt *ys, ASTScopeImpl *p, ScopeCreator &scopeCreator) {
    scopeCreator.pushAllNecessaryNodes(ys->getYields());
    creattionState.createScopeForNextDeferredNode(p);
  }
  void visitDeferStmt(DeferStmt *ds, ASTScopeImpl *p, ScopeCreator &scopeCreator) {
    visitFuncDecl(ds->getTempDecl(), p, scopeCreator);
  }

  void visitBraceStmt(BraceStmt *bs, ASTScopeImpl *p, ScopeCreator &scopeCreator) {
    if (p->areDeferredNodesInANewScope())
      scopeCreator.createSubtree<BraceStmtScope>(p, bs);
    else
      scopeCreator.withoutDeferrals().createSubtree<BraceStmtScope>(p, bs);
  }

  void visitPatternBindingDecl(PatternBindingDecl *patternBinding,
                               ASTScopeImpl *p, ScopeCreator &scopeCreator) {
    p->createAttachedPropertyDelegateScope(patternBinding);
    // scopeCreator will contain any nodes that need to be put into subscopes.
    // In a type decl body, there won't be any.
    // The comment for \c AbstractPatternEntryScope.
    const bool isInTypeDecl =
        p->getDecl() && (isa<NominalTypeDecl>(p->getDecl().get()) ||
                              isa<ExtensionDecl>(p->getDecl().get()));
    if (isInTypeDecl)
      p->createScopesForPatternBindingInATypeDecl(patternBinding);
    else
      p->createScopesForPatternBindingInAFunction(patternBinding,
                                                       scopeCreator);
  }

  void visitReturnStmt(ReturnStmt *rs, ASTScopeImpl *p,
                       ScopeCreator &scopeCreator) {
    if (rs->hasResult())
      visitExpr(rs->getResult(), p, scopeCreator);
  }

  void visitWhileStmt(WhileStmt *ws, ASTScopeImpl *p, ScopeCreator &scopeCreator) {
    if (ws->getCond().empty()) {
      visit(ws->getBody(), p, scopeCreator);
      return;
    }
    scopeCreator.withoutDeferrals().createSubtree<WhileConditionalClauseScope>(p, ws, 0);
  }

  void visitExpr(Expr *expr, ASTScopeImpl *p, ScopeCreator &scopeCreator) {
    if (!expr)
      return;
    p->widenSourceRangeForIgnoredASTNode(expr);
    p->addChildrenForCapturesAndClosuresIn(expr);
  }
};
} // namespace ast_scope
} // namespace swift

// This definition is way down here so it can call into ASTVisitorForScopeCreation
template <typename StmtOrExprOrDecl>
void ScopeCreator::createScopeFor(StmtOrExprOrDecl *sed, ASTScopeImpl *parent) {
  ASTVisitorForScopeCreation().visit(sed, parent, *this);
}



#pragma mark creation helpers

static bool hasCustomAttribute(VarDecl *vd) {
  return AttachedPropertyDelegateScope::getCustomAttributesSourceRange(vd).isValid();
}

void ASTScopeImpl::createAttachedPropertyDelegateScope(
    PatternBindingDecl *patternBinding) {
  patternBinding->getPattern(0)->forEachVariable([&](VarDecl * vd) {
    // assume all same as first
    if (hasCustomAttribute(vd))
      scopeCreator.withoutDeferrals().createSubtree<AttachedPropertyDelegateScope>(this, vd);
  });
}

void ASTScopeImpl::createScopesForPatternBindingInATypeDecl(
    PatternBindingDecl *patternBinding) {
  createSiblingPatternScopes(patternBinding);
}

void ASTScopeImpl::createScopesForPatternBindingInAFunction(
    PatternBindingDecl *patternBinding, DeferredNodes &scopeCreator) {
  createNestedPatternScopes(patternBinding, scopeCreator);
}

void ASTScopeImpl::createSiblingPatternScopes(
    PatternBindingDecl *patternBinding) {
  for (auto entryIndex : range(patternBinding->getNumPatternEntries()))
    if (!patternBinding->getPattern(entryIndex)->isImplicit()) {
      scopeCreator.withoutDeferrals().createSubtree<PatternEntryDeclScope>(this, patternBinding, entryIndex);
    }
}

void ASTScopeImpl::createNestedPatternScopes(PatternBindingDecl *patternBinding,
                                             DeferredNodes &scopeCreator) {
  createRestOfNestedPatternScopes(patternBinding, scopeCreator, 0);
}

void ASTScopeImpl::createRestOfNestedPatternScopes(
    PatternBindingDecl *patternBinding, DeferredNodes &scopeCreator,
    unsigned entryIndex) {
  if (entryIndex >= patternBinding->getPatternList().size()) {
    // no more entries, create the scopes inside the pattern use
    creationState.createScopeForNextDeferredNode(this);
    return;
  }
  // When PatternEntryDeclScope expands, it will create a PatternEntryUseScope
  // which will call createRestOfNestedPatternScopes when it expands.
  scopeCreator.createSubtree<PatternEntryDeclScope>(this, patternBinding,
                                                    entryIndex);
}

// Last GenericParamsScope includes the where clause
ASTScopeImpl *
ASTScopeImpl::createGenericParamScopes(Decl *parameterizedDecl,
                                       GenericParamList *generics) {
  if (!generics)
    return this;
  auto *s = this;
  for (unsigned i : indices(generics->getParams()))
    s = scopeCreator.withoutDeferrals().createSubtree<GenericParamScope>(s, parameterizedDecl, generics, i);
  return s;
}

void ASTScopeImpl::addChild(ASTScopeImpl *child) {
  // If this is the first time we've added children, notify the ASTContext
  // that there's a SmallVector that needs to be cleaned up.
  // FIXME: If we had access to SmallVector::isSmall(), we could do better.
  if (storedChildren.empty())
    getASTContext().addDestructorCleanup(storedChildren);
  storedChildren.push_back(child);
  assert(!child->getParent() && "child should not already have parent");
  child->parent = this;
}

#pragma mark expanding

void ASTScopeImpl::expandMeAndCreateScopesForDeferredNodes(
    ScopeCreator &scopeCreator) {
  expandMe(deferred);
  creationState->addChildScopesForAnyRemainingDeferredNodesTo(this);
}


#pragma mark specific implementations of expansion

// Many kinds of scopes do not expandScope into subscopes
void ASTScopeImpl::expandMe(DeferredNodes &) { dontExpand(); }

void ASTSourceFileScope::expandMe(ScopeCreator &scopeCreator) {
  llvm_unreachable("get expanded from addAnyNewDeclsToTree");
}

  // Create child scopes for every declaration in a body.

void AbstractFunctionDeclScope::expandMe(DeferredNodes &) {
  // Create scopes for specialize attributes
  forEachSpecializeAttrInSourceOrder(decl, [&](SpecializeAttr *specializeAttr) {
    scopeCreator.withoutDeferrals().createSubtree<SpecializeAttributeScope>(this, specializeAttr, decl);
  });

  // Create scopes for generic and ordinary parameters.
  // For a subscript declaration, the generic and ordinary parameters are in an
  // ancestor scope, so don't make them here.
  ASTScopeImpl *leaf = this;
  if (!isa<AccessorDecl>(decl)) {
    leaf = createGenericParamScopes(decl, decl->getGenericParams());
    if (!decl->isImplicit()) {
      leaf = scopeCreator.withoutDeferrals().createSubtree<AbstractFunctionParamsScope>(leaf,
          decl->getParameters(), nullptr);
    }
  }
  // Create scope for the body.
  if (decl->getBody()) {
    if (decl->getDeclContext()->isTypeContext())
      scopeCreator.withoutDeferrals().createSubtree<MethodBodyScope>(leaf, decl);
    else
      scopeCreator.withoutDeferrals().createSubtree<PureFunctionBodyScope>(leaf, decl);
  }
}

void AbstractFunctionBodyScope::expandMe(ScopeCreator &scopeCreator) {
  BraceStmt *braceStmt = decl->getBody();
  ASTVisitorForScopeCreation().visitBraceStmt(braceStmt, this, scopeCreator);
}

void PatternEntryDeclScope::expandMe(ScopeCreator &scopeCreator) {
  auto patternEntry = getPatternEntry();
  // Create a child for the initializer, if present.
  // Cannot trust the source range given in the ASTScopeImpl for the end of the
  // initializer (because of InterpolatedLiteralStrings and EditorPlaceHolders),
  // so compute it ourselves.
  SourceLoc initializerEnd;
  if (patternEntry.getInitAsWritten() &&
      patternEntry.getInitAsWritten()->getSourceRange().isValid()) {
    auto *initializer =
        scopeCreator.withoutDeferrals().createSubtree<PatternEntryInitializerScope>(this, decl, patternEntryIndex);
    initializerEnd = initializer->getSourceRange().End;
  }
  // If there are no uses of the declararations, add the accessors immediately.
  if (scopeCreator.empty())
    addVarDeclScopesAndTheirAccessors(this);
  else
    // Note: the accessors will follow the pattern binding.
    scopeCreator.createSubtree<PatternEntryUseScope>(
        this, decl, patternEntryIndex, initializerEnd);
}

void PatternEntryInitializerScope::expandMe(DeferredNodes &) {
  DeferredNodes noDeferrals(__FILE__, __LINE__, *this, this);
  // Create a child for the initializer expression.
  ASTVisitorForScopeCreation().visitExpr(getPatternEntry().getInitAsWritten(), this,
                           noDeferrals);
}

void PatternEntryUseScope::expandMe(ScopeCreator &scopeCreator) {
  // Add accessors for the variables in this pattern.
  addVarDeclScopesAndTheirAccessors(this);
  // Create a child for the next pattern binding.
  createRestOfNestedPatternScopes(decl, scopeCreator, patternEntryIndex + 1);
}

void ConditionalClauseScope::expandMe(ScopeCreator &scopeCreator) {
  // If this is a boolean conditional not in a guard continuation, add a
  // child for the expression.
  createSubtreeForCondition();
  // If there's another conditional clause, add it as the child.
  if (index + 1 < getContainingStatement()->getCond().size())
    createSubtreeForNextConditionalClause(scopeCreator);
  else {
    // There aren't any additional conditional clauses. Add the appropriate
    // nested scope based on the kind of statement.
    createSubtreeForAfterClauses(scopeCreator);
  }
}

void IfStmtScope::expandMe(ScopeCreator &scopeCreator) {
  // The first conditional clause or, failing that, the 'then' clause.
  if (!stmt->getCond().empty())
    scopeCreator.withoutDeferrals().createSubtree<IfConditionalClauseScope>(this, stmt, 0);
  else
    creationState.createScopeFor(stmt->getThenStmt(), this);

  // Add the 'else' branch, if needed.
  creationState.createScopeFor(stmt->getElseStmt(), this);
}

void RepeatWhileScope::expandMe(ScopeCreator &scopeCreator) {
  creationState.createScopeFor(stmt->getBody(), this);
  ASTVisitorForScopeCreation().visitExpr(stmt->getCond(), this, scopeCreator);
}

void DoCatchStmtScope::expandMe(ScopeCreator &scopeCreator) {
  creationState.createScopeFor(stmt->getBody(), this);

  for (auto catchClause : stmt->getCatches())
    ASTVisitorForScopeCreation().visitCatchStmt(catchClause, this, scopeCreator);
}

void SwitchStmtScope::expandMe(ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getSubjectExpr(), this, scopeCreator);

  for (auto caseStmt : stmt->getCases())
    scopeCreator.withoutDeferrals().createSubtree<CaseStmtScope>(this, caseStmt);
}

void ForEachStmtScope::expandMe(ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getSequence(), this, scopeCreator);

  // Add a child describing the scope of the pattern.
  scopeCreator.withoutDeferrals().createSubtree<ForEachPatternScope>(this, stmt);
}

void ForEachPatternScope::expandMe(ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getWhere(), this, scopeCreator);
  ASTVisitorForScopeCreation().visitBraceStmt(stmt->getBody(), this, scopeCreator);
}

void CatchStmtScope::expandMe(ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getGuardExpr(), this, scopeCreator);
  creationState.createScopeFor(stmt->getBody(), this);
}

void CaseStmtScope::expandMe(ScopeCreator &scopeCreator) {
  for (auto &caseItem : stmt->getMutableCaseLabelItems())
    ASTVisitorForScopeCreation().visitExpr(caseItem.getGuardExpr(), this, scopeCreator);

  // Add a child for the case body.
  creationState.createScopeFor(stmt->getBody(), this);
}

void GuardStmtScope::expandMe(ScopeCreator &scopeCreator) {
  // Add a child to describe the guard condition.
  scopeCreator.withoutDeferrals().createSubtree<GuardConditionalClauseScope>(this, stmt, 0);

  // Add a child for the 'guard' body, which always exits.
  creationState.createScopeFor(stmt->getBody(), this);

  // Add a child to describe the guard condition for the continuation.
  scopeCreator.createSubtree<GuardContinuationScope>(this, stmt, 0);
}

void BraceStmtScope::expandMe(ScopeCreator &scopeCreator) {
  scopeCreator.pushAllNecessaryNodes(stmt->getElements());
  creationState.createScopeForNextDeferredNode(this);
}

void VarDeclScope::expandMe(ScopeCreator &scopeCreator) {
  assert(scopeCreator.empty() && "Decls needing this var go into the PatternEntryUseScope, not here");
  addChildrenForAllExplicitAccessors(decl, scopeCreator);
}

void SubscriptDeclScope::expandMe(ScopeCreator &scopeCreator) {
  auto *sub = decl;
  auto *leaf = createGenericParamScopes(sub, sub->getGenericParams());
  auto *params = scopeCreator.withoutDeferrals().createSubtree<AbstractFunctionParamsScope>(leaf,
      sub->getIndices(), sub->getGetter());
  params->addChildrenForAllExplicitAccessors(sub, scopeCreator);
}

void WholeClosureScope::expandMe(DeferredNodes &) {
  if (auto *cl = captureList.getPtrOrNull())
    scopeCreator.withoutDeferrals().createSubtree<CaptureListScope>(this, cl);
  if (closureExpr->getInLoc().isValid())
    scopeCreator.withoutDeferrals().createSubtree<ClosureParametersScope>(this, captureList, closureExpr);
  scopeCreator.withoutDeferrals().createSubtree<ClosureBodyScope>(this, captureList, closureExpr);
}

void CaptureListScope::expandMe(DeferredNodes &) {
  // Patterns here are implicit, so need to dig out the intializers
  for (const CaptureListEntry &captureListEntry : expr->getCaptureList()) {
    for (unsigned patternEntryIndex = 0;
         patternEntryIndex < captureListEntry.Init->getNumPatternEntries();
         ++patternEntryIndex) {
      Expr *init = captureListEntry.Init->getInit(patternEntryIndex);
      addChildrenForCapturesAndClosuresIn(init);
    }
  }
}

void ClosureBodyScope::expandMe(DeferredNodes &) {
  scopeCreator.withoutDeferrals().createSubtree<BraceStmtScope>(this, closureExpr->getBody());
}

void TopLevelCodeScope::expandMe(ScopeCreator &scopeCreator) {
  scopeCreator.createSubtree<BraceStmtScope>(this, decl->getBody());
  assert(scopeCreator.empty() &&
         "a top level code scope should snarf up all the deferred nodes");
}

void AbstractFunctionParamsScope::expandMe(ScopeCreator &scopeCreator) {
  // Each initializer for a function parameter is its own, sibling, scope.
  // Unlike generic parameters or pattern initializers, it cannot refer to a
  // previous parameter.
  for (ParamDecl *pd : params->getArray()) {
    if (pd->getDefaultValue())
      scopeCreator.withoutDeferrals().createSubtree<DefaultArgumentInitializerScope>(this, pd);
  }
}

void DefaultArgumentInitializerScope::expandMe(ScopeCreator &scopeCreator) {
  auto *initExpr = decl->getDefaultValue();
  assert(initExpr);
  ASTVisitorForScopeCreation().visitExpr(initExpr, this, scopeCreator);
}

void GTXScope::expandMe(ScopeCreator &scopeCreator) {
  portion->expandScope(this, scopeCreator);
}

void GTXWholePortion::expandScope(GTXScope *scope,
                                  ScopeCreator &scopeCreator) const {
  // Prevent circular request bugs caused by illegal input and
  // doing lookups that getExtendedNominal in the midst of getExtendedNominal.
  if (scope->shouldHaveABody() && !scope->doesDeclHaveABody())
    return;

  auto *deepestScope = scope->createGenericParamScopes(
      scope->getDecl().get(), scope->getGenericContext()->getGenericParams());
  if (scope->getGenericContext()->getTrailingWhereClause())
    deepestScope = scope->createTrailingWhereClauseScope(deepestScope);

  scope->createBodyScope(deepestScope);
}

void IterableTypeBodyPortion::expandScope(GTXScope *scope,
                                          ScopeCreator &scopeCreator) const {
  if (auto *idc = scope->getIterableDeclContext().getPtrOrNull())
    for (auto member : idc->getMembers())
      creationState.createScopeFor(member, scope);
}

#pragma mark createBodyScope

void ExtensionScope::createBodyScope(ASTScopeImpl *leaf) {
  scopeCreator.withoutDeferrals.createSubtree2D<ExtensionScope, IterableTypeBodyPortion>(leaf, decl);
}
void NominalTypeScope::createBodyScope(ASTScopeImpl *leaf) {
  scopeCreator.withoutDeferrals.createSubtree2D<NominalTypeScope, IterableTypeBodyPortion>(leaf, decl);
}

#pragma mark createTrailingWhereClauseScope

ASTScopeImpl *
ExtensionScope::createTrailingWhereClauseScope(ASTScopeImpl *parent) {
  return scopeCreator.withoutDeferrals.createSubtree2D<ExtensionScope, GTXWherePortion>(parent, decl);
}
ASTScopeImpl *
NominalTypeScope::createTrailingWhereClauseScope(ASTScopeImpl *parent) {
  return scopeCreator.withoutDeferrals.createSubtree2D<NominalTypeScope, GTXWherePortion>(scopeCreator, decl);
}
ASTScopeImpl *
TypeAliasScope::createTrailingWhereClauseScope(ASTScopeImpl *parent) {
  return scopeCreator.withoutDeferrals.createSubtree2D<TypeAliasScope, GTXWherePortion>(parent, decl);
}

#pragma mark misc

void ASTScopeImpl::forEachClosureIn(
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

void ASTScopeImpl::forEachUniqueClosureIn(
    Expr *expr, function_ref<void(NullablePtr<CaptureListExpr>, ClosureExpr *)>
                    foundUniqueClosure) {
  auto &alreadyHandledClosures = getAlreadyHandledClosures();
  forEachClosureIn(expr, [&](NullablePtr<CaptureListExpr> captureList,
                             ClosureExpr *closureExpr) {
    if (alreadyHandledClosures.insert(closureExpr).second)
      foundUniqueClosure(captureList, closureExpr);
  });
}

llvm::DenseSet<ClosureExpr *> &ASTScopeImpl::getAlreadyHandledClosures() {
  return getParent().get()->getAlreadyHandledClosures();
}
llvm::DenseSet<ClosureExpr *> &ASTSourceFileScope::getAlreadyHandledClosures() {
  return alreadyHandledClosures;
}

void ASTScopeImpl::forEachSpecializeAttrInSourceOrder(
    Decl *declBeingSpecialized, function_ref<void(SpecializeAttr *)> fn) {
  llvm::SmallVector<SpecializeAttr *, 8> sortedSpecializeAttrs;
  for (auto *attr : declBeingSpecialized->getAttrs())
    if (auto *specializeAttr = dyn_cast<SpecializeAttr>(attr))
      sortedSpecializeAttrs.push_back(specializeAttr);
  const auto &srcMgr = declBeingSpecialized->getASTContext().SourceMgr;
  std::sort(sortedSpecializeAttrs.begin(), sortedSpecializeAttrs.end(),
            [&](const SpecializeAttr *a, const SpecializeAttr *b) {
              return srcMgr.isBeforeInBuffer(a->getLocation(),
                                             b->getLocation());
            });
  for (auto *specializeAttr : sortedSpecializeAttrs)
    fn(specializeAttr);
}

AbstractPatternEntryScope::AbstractPatternEntryScope(
  PatternBindingDecl *declBeingScoped, unsigned entryIndex)
: decl(declBeingScoped), patternEntryIndex(entryIndex) {
  assert(entryIndex < declBeingScoped->getPatternList().size() &&
         "out of bounds");
}

void AbstractPatternEntryScope::addVarDeclScopesAndTheirAccessors(
    ASTScopeImpl *parent) const {
  getPatternEntry().getPattern()->forEachVariable([&](VarDecl *var) {
    const bool hasAccessors = var->getBracesRange().isValid();
    if (hasAccessors && !var->isImplicit())
      scopeCreator.withoutDeferrals().createSubtree<VarDeclScope>(parent, var);
  });
}

bool ASTScopeImpl::isCreatedDirectly(const ASTNode n) {
  // See addVarDeclScopesAndTheirAccessors and addChildrenForAllExplicitAccessors
  if (auto *d = n.dyn_cast<Decl*>())
    return isa<VarDecl>(d) || isa<AccessorDecl>(d);
  return false;
}

void ASTScopeImpl::addChildrenForAllExplicitAccessors(AbstractStorageDecl *asd,
                                                      ScopeCreator &scopeCreator) {
  for (auto accessor : asd->getAllAccessors()) {
    if (!accessor->isImplicit() && accessor->getStartLoc().isValid()) {
      // Accessors are always nested within their abstract storage declaration.
      // The nesting may not be immediate, because subscripts may have
      // intervening scopes for generics.
      if (getEnclosingAbstractStorageDecl() == accessor->getStorage())
        ASTVisitorForScopeCreation().visitAbstractFunctionDecl(accessor, this, scopeCreator);
    }
  }
}

void ConditionalClauseScope::createSubtreeForCondition() {
  DeferredNodes noDeferrals(__FILE__, __LINE__, *this, this);
  const auto &cond = getContainingStatement()->getCond()[index];
  switch (cond.getKind()) {
  case StmtConditionElement::CK_Availability:
    return;
  case StmtConditionElement::CK_Boolean:
    ASTVisitorForScopeCreation().visitExpr(cond.getBoolean(), this, noDeferrals);
    return;
  case StmtConditionElement::CK_PatternBinding:
    scopeCreator.withoutDeferrals().createSubtree<StatementConditionElementPatternScope>(parent, cond.getPattern());
    ASTVisitorForScopeCreation().visitExpr(cond.getInitializer(), this, noDeferrals);
    return;
  }
}

void GuardContinuationScope::createSubtreeForCondition() {
  // no condition for this one
}

void WhileConditionalClauseScope::createSubtreeForNextConditionalClause(
    ScopeCreator &scopeCreator) {
  scopeCreator.createSubtree<WhileConditionalClauseScope>(this, stmt,
                                                          index + 1);
}
void IfConditionalClauseScope::createSubtreeForNextConditionalClause(
    ScopeCreator &scopeCreator) {
  scopeCreator.createSubtree<IfConditionalClauseScope>(this, stmt,
                                                       index + 1);
}
void GuardConditionalClauseScope::createSubtreeForNextConditionalClause(
    ScopeCreator &scopeCreator) {
  scopeCreator.createSubtree<GuardConditionalClauseScope>(this, stmt,
                                                          index + 1);
}
void GuardContinuationScope::createSubtreeForNextConditionalClause(
    ScopeCreator &scopeCreator) {
  scopeCreator.createSubtree<GuardContinuationScope>(this, stmt, index + 1);
}

void WhileConditionalClauseScope::createSubtreeForAfterClauses(
    ScopeCreator &scopeCreator) {
  creationState.createScopeFor(stmt->getBody(), this);
}
void IfConditionalClauseScope::createSubtreeForAfterClauses(
    ScopeCreator &scopeCreator) {
  creationState.createScopeFor(stmt->getThenStmt(), this);
}

void GuardConditionalClauseScope::createSubtreeForAfterClauses(
    ScopeCreator &scopeCreator) {
  // There aren't any additional conditional clauses. Add the appropriate
  // nested scope based on the kind of statement.

  // Note: guard statements have the continuation nested under the last
  // condition.
  // The scope *after* the guard statement must include any deferred nodes
  // so that any let vars in the guard are in scope.
  creationState.createScopeForNextDeferredNode(this);
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


#pragma mark addChildrenForCapturesAndClosuresIn

/// add children for any closures
void ASTScopeImpl::addChildrenForCapturesAndClosuresIn(Expr *expr) {
  // Use the ASTWalker to find buried captures and closures
  forEachUniqueClosureIn(expr, [&](NullablePtr<CaptureListExpr> captureList,
                                   ClosureExpr *closureExpr) {
    scopeCreator.withoutDeferrals().createSubtree<WholeClosureScope>(parent, captureList, closureExpr);
  });
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
void *DeferredNodes::operator new(size_t bytes, const ASTContext &ctx,
                                  unsigned alignment) {
  return ctx.Allocate(bytes, alignment);
}


#pragma mark DeferredNodes definitions

#ifndef NDEBUG
DeferredNodes::DeferredNodes(const char *file, int line,
                             const ASTScopeImpl &creator, const void *forWhat)
    : file(file), line(line), creator(creator), forWhat(forWhat) {}
#else
DeferredNodes::DeferredNodes(const char, int, const ASTScopeImpl, const void) {}
#endif

DeferredNodes::~DeferredNodes() {
  assert(nodesInReverse.empty() && "Should have consumed all nodes");
}

ASTNode DeferredNodes::popNextDeferredNodeForAdoptionBy(ASTScopeImpl *s) {
  setLastAdopter(s);
  auto f = nodesInReverse.back();
  nodesInReverse.pop_back();
  return f;
}

// Maintain last adopter so that when we reenter scope tree building
// after the parser has added more decls to the source file,
// we can resume building the scope tree where we left off.

void DeferredNodes::setLastAdopter(ASTScopeImpl *s) {
  // We get here for any scope that wants to add a deferred node as a child.
  // But after creating a deeper node that has registered as last adopter,
  // an ancestor node might try to get more deferred nodes. So,
  // only set it for a deeper node.
  // If scope construction gets called later on for the same SourceFile,
  // we'll always want to add scopes to the deepest adopter.
  if (lastAdopter == s)
    return; // optimization
  if (!lastAdopter || lastAdopter.get()->depth() <= s->depth())
    lastAdopter = s;
}

bool DeferredNodes::empty() const { return nodesInReverse.empty(); }

void DeferredNodes::pushIfNecessary(ASTNode n) {
  // Do not defer VarDecls or Accessors because
  // they get created directly by the pattern code
  // and deferring them pushes them to the wrong place.
  // Then, even though they're ignored, they distort the source range
  // of their parents.
  
  // An optimization; will be ignored later anyway.
  if (ASTScopeImpl::isCreatedDirectly(n))
    return;

  nodesInReverse.push_back(n);
}

template <typename ASTNodelike>
void DeferredNodes::pushAllNecessaryNodes(
    ArrayRef<ASTNodelike> nodesToPrepend) {
  for (int i = nodesToPrepend.size() - 1; i >= 0; --i)
    pushIfNecessary(nodesToPrepend[i]);
}

void DeferredNodes::pushSourceFileDecls(ArrayRef<Decl *> declsToPrepend) {
  pushAllNecessaryNodes(declsToPrepend.slice(numberOfDeclsAlreadyProcessed));
  numberOfDeclsAlreadyProcessed = declsToPrepend.size();
}
NullablePtr<ASTScopeImpl> DeferredNodes::getLastAdopter() const {
  return lastAdopter;
}

void DeferredNodes::dump() const { print(llvm::errs()); }

void DeferredNodes::print(raw_ostream &out) const {
#ifndef NDEBUG
  out << "From " << file << ": " << line << " for: " << forWhat << "\n";
  out << "Scope: ";
  creator.print(out);
  out << "\n";
#endif
  int i = 0;
  for (auto n : reverse(nodesInReverse)) {
    out << i++ << ": ";
    n.dump(out);
    out << "\n";
  }
}

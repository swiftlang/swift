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
namespace swift {
namespace ast_scope {

#pragma mark DeferredNodes

/// \c SourceFiles and \c BraceStmts have \c Decls and \c ASTNodes in them that
/// must be scoped in distant descendants.
/// As scopes are created, pass down the \c ASTNodes in enclosing \c Decls
/// that really need to be in subscopes.
///
/// Because parsing and lookup (typechecking) are interleaved for some files,
/// we need to be able to resume scope tree creation. \c DeferredNodes also
/// helps with this by remembering the \c lastAdopter.

class DeferredNodes final {
  /// The nodes that must be passed down to deeper levels in the scope tree.
  std::deque<ASTNode> nodes;

  /// The number of \c Decls in the \c SourceFile that were already processed
  /// into scopes. Since parsing can be interleaved with type-checking, on every
  /// lookup, create scopes for any \c Decls beyond this number.
  int numberOfDeclsAlreadyProcessed = 0;

  /// The last scope to "adopt" deferred nodes.
  /// When adding \c Decls to a scope tree that have been created since the tree
  /// was originally built, add them as children of this scope.
  NullablePtr<ASTScopeImpl> lastAdopter;

  /// Information about how this object was created for debugging.
  const char *const file;
  const int line;
  const ASTScopeImpl &creator;
  const void *const forWhat;

public:
  DeferredNodes(const char *file, int line, const ASTScopeImpl &creator,
                const void *forWhat);
  ~DeferredNodes();
  DeferredNodes(const DeferredNodes &) = delete;  // ensure no copies
  DeferredNodes(const DeferredNodes &&) = delete; // ensure no moves

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
} // ast_scope
} // namespace swift
using namespace ast_scope;

#pragma mark Scope tree creation and extension

ASTSourceFileScope::ASTSourceFileScope(SourceFile *SF)
    : SF(SF), deferred(new (SF->getASTContext())
                           DeferredNodes(__FILE__, __LINE__, *this, SF)) {}

ASTSourceFileScope *ASTSourceFileScope::createScopeTreeFor(SourceFile *SF) {
  auto *sfScope = new (SF->getASTContext()) ASTSourceFileScope(SF);
  sfScope->addAnyNewDeclsToTree();
  return sfScope;
}

void ASTSourceFileScope::addAnyNewDeclsToTree() {
  deferred->pushSourceFileDecls(SF->Decls);
  auto lastAdopter = deferred->getLastAdopter();
  auto *adopter = lastAdopter ? lastAdopter.get() : this;
  adopter->addChildScopesForAnyRemainingDeferredNodes(*deferred);
}

#pragma mark ScopeCreator

namespace swift {
namespace ast_scope {

class ScopeCreator
    : public ASTVisitor<ScopeCreator, void, void, void, void, void, void,
                        ASTScopeImpl *, DeferredNodes &> {
public:

#pragma mark ASTNodes that do not create scopes

  // Even ignored Decls and Stmts must extend the source range of a scope:
  // E.g. a braceStmt with some definitions that ends in a statement that
  // accesses such a definition must resolve as being IN the scope.
  // clang-format off
                          
  void visitImportDecl(                       ImportDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitEnumCaseDecl(                   EnumCaseDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitPrecedenceGroupDecl(     PrecedenceGroupDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitInfixOperatorDecl(         InfixOperatorDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitPrefixOperatorDecl(       PrefixOperatorDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitPostfixOperatorDecl(     PostfixOperatorDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitGenericTypeParamDecl(   GenericTypeParamDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitAssociatedTypeDecl(       AssociatedTypeDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitModuleDecl(                       ModuleDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitParamDecl(                         ParamDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitEnumElementDecl(             EnumElementDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitIfConfigDecl(                   IfConfigDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitPoundDiagnosticDecl(     PoundDiagnosticDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
  void visitMissingMemberDecl(         MissingMemberDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }

  // This declaration is handled from the PatternBindingDecl
  void visitVarDecl(                VarDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
                          
  // This declaration is handled from addChildrenForAllExplicitAccessors
  void visitAccessorDecl(      AccessorDecl *d, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(d); }
                          
 void visitBreakStmt(             BreakStmt *s, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(s); }
 void visitContinueStmt(       ContinueStmt *s, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(s); }
 void visitFallthroughStmt( FallthroughStmt *s, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(s); }
 void visitFailStmt(               FailStmt *s, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(s); }
 void visitThrowStmt(             ThrowStmt *s, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(s); }
 void visitPoundAssertStmt( PoundAssertStmt *s, ASTScopeImpl *p, DeferredNodes&) { p->widenSourceRangeForIgnoredASTNode(s); }

    // clang-format on
                          

#pragma mark simple creation ignoring deferred nodes
// clang-format off
  void visitSubscriptDecl(SubscriptDecl *e, ASTScopeImpl *p, DeferredNodes &) { p->createSubtree<SubscriptDeclScope>(e); }

  void visitIfStmt(                   IfStmt *e, ASTScopeImpl *p, DeferredNodes &) { p->createSubtree<     IfStmtScope>(e); }
  void visitRepeatWhileStmt( RepeatWhileStmt *e, ASTScopeImpl *p, DeferredNodes &) { p->createSubtree<RepeatWhileScope>(e); }
  void visitDoCatchStmt(         DoCatchStmt *e, ASTScopeImpl *p, DeferredNodes &) { p->createSubtree<DoCatchStmtScope>(e); }
  void visitSwitchStmt(           SwitchStmt *e, ASTScopeImpl *p, DeferredNodes &) { p->createSubtree< SwitchStmtScope>(e); }
  void visitForEachStmt(         ForEachStmt *e, ASTScopeImpl *p, DeferredNodes &) { p->createSubtree<ForEachStmtScope>(e); }
  void visitCatchStmt(             CatchStmt *e, ASTScopeImpl *p, DeferredNodes &) { p->createSubtree<  CatchStmtScope>(e); }
  void visitCaseStmt(               CaseStmt *e, ASTScopeImpl *p, DeferredNodes &) { p->createSubtree<   CaseStmtScope>(e); }
  // clang-format on
  // Why don't AbstractFunctionDeclScopes inherit DeferredNodes and thereby
  // create a subscope in which they are visible?
  // Consider:
  // func f() { func g() {h()}; func h() { g(); }
  // In Swift local functions can be mutually recursive!
  // So, instead of subscopes, we put the AbstractFunctionDeclScope's for
  // g and h in the same BraceStmtScope and get the local binding mechanism find
  // them.
  void visitAbstractFunctionDecl(AbstractFunctionDecl *d, ASTScopeImpl *p,
                                 DeferredNodes &) {
    p->createSubtree<AbstractFunctionDeclScope>(d);
  }

#pragma mark 2D simple creation (ignoring deferred nodes)

  void visitExtensionDecl(ExtensionDecl *e, ASTScopeImpl *p, DeferredNodes &) {
    p->createSubtree2D<ExtensionScope, GTXWholePortion>(e);
  }
  void visitStructDecl(StructDecl *e, ASTScopeImpl *p, DeferredNodes &) {
    p->createSubtree2D<NominalTypeScope, GTXWholePortion>(e);
  }
  void visitClassDecl(ClassDecl *e, ASTScopeImpl *p, DeferredNodes &) {
    p->createSubtree2D<NominalTypeScope, GTXWholePortion>(e);
  }
  void visitEnumDecl(EnumDecl *e, ASTScopeImpl *p, DeferredNodes &) {
    p->createSubtree2D<NominalTypeScope, GTXWholePortion>(e);
  }
  void visitTypeAliasDecl(TypeAliasDecl *e, ASTScopeImpl *p, DeferredNodes &) {
    p->createSubtree2D<TypeAliasScope, GTXWholePortion>(e);
  }
  void visitOpaqueTypeDecl(OpaqueTypeDecl *e, ASTScopeImpl *p,
                           DeferredNodes &) {
    p->createSubtree2D<OpaqueTypeScope, GTXWholePortion>(e);
  }
  void visitProtocolDecl(ProtocolDecl *e, ASTScopeImpl *p, DeferredNodes &) {
    e->createGenericParamsIfMissing();
    p->createSubtree2D<NominalTypeScope, GTXWholePortion>(e);
  }

#pragma mark simple creation with deferred nodes

  // Each of the following creates a new scope, so that nodes which were parsed
  // after them need to be placed in scopes BELOW them in the tree. So pass down
  // the deferred nodes.
  void visitGuardStmt(GuardStmt *e, ASTScopeImpl *p, DeferredNodes &deferred) {
    p->createSubtreeWithDeferrals<GuardStmtScope>(deferred, e);
  }
  void visitDoStmt(DoStmt *ds, ASTScopeImpl *p, DeferredNodes &deferred) {
    p->dispatchAndCreateIfNeeded(ds->getBody(), deferred);
  }
  void visitTopLevelCodeDecl(TopLevelCodeDecl *d, ASTScopeImpl *p,
                             DeferredNodes &deferred) {
    p->createSubtreeWithDeferrals<TopLevelCodeScope>(deferred, d);
  }

#pragma mark special-case creation

  void visitSourceFile(SourceFile *, ASTScopeImpl *, DeferredNodes &) {
    llvm_unreachable("SourceFiles are orphans.");
  }

  void visitYieldStmt(YieldStmt *ys, ASTScopeImpl *p, DeferredNodes &deferred) {
    deferred.pushAllNecessaryNodes(ys->getYields());
    p->dispatchAndCreateAll(deferred);
  }
  void visitDeferStmt(DeferStmt *ds, ASTScopeImpl *p, DeferredNodes &deferred) {
    visitFuncDecl(ds->getTempDecl(), p, deferred);
  }

  void visitBraceStmt(BraceStmt *bs, ASTScopeImpl *p, DeferredNodes &deferred) {
    if (p->areDeferredNodesInANewScope())
      p->createSubtreeWithDeferrals<BraceStmtScope>(deferred, bs);
    else
      p->createSubtree<BraceStmtScope>(bs);
  }

  void visitPatternBindingDecl(PatternBindingDecl *patternBinding,
                               ASTScopeImpl *p, DeferredNodes &deferred) {
    p->createAttachedPropertyDelegateScope(patternBinding);
    // deferred will contain any nodes that need to be put into subscopes.
    // In a type decl body, there won't be any.
    // The comment for \c AbstractPatternEntryScope.
    const bool isInTypeDecl =
        p->getDecl() && (isa<NominalTypeDecl>(p->getDecl().get()) ||
                              isa<ExtensionDecl>(p->getDecl().get()));
    if (isInTypeDecl)
      p->createScopesForPatternBindingInATypeDecl(patternBinding);
    else
      p->createScopesForPatternBindingInAFunction(patternBinding,
                                                       deferred);
  }

  void visitReturnStmt(ReturnStmt *rs, ASTScopeImpl *p,
                       DeferredNodes &deferred) {
    if (rs->hasResult())
      visitExpr(rs->getResult(), p, deferred);
  }

  void visitWhileStmt(WhileStmt *ws, ASTScopeImpl *p, DeferredNodes &deferred) {
    if (ws->getCond().empty()) {
      visit(ws->getBody(), p, deferred);
      return;
    }
    p->createSubtree<WhileConditionalClauseScope>(ws, 0);
  }

  void visitExpr(Expr *expr, ASTScopeImpl *p, DeferredNodes &deferred) {
    if (!expr)
      return;
    p->widenSourceRangeForIgnoredASTNode(expr);
    p->addChildrenForCapturesAndClosuresIn(expr);
  }
};
} // namespace ast_scope
} // namespace swift

#pragma mark dispatch-and-create

void ASTScopeImpl::dispatchAndCreateAll(DeferredNodes &deferred) {
  deferred.setLastAdopter(this); // in case we come back here later after adding
                                 // more Decls to the SourceFile
  while (!deferred.empty()) {
    Optional<ASTNode> node = deferred.popNextDeferredNodeForAdoptionBy(this);
    if (!node)
      return;
    if (auto declToScope = node->dyn_cast<Decl *>())
      dispatchAndCreateIfNeeded(declToScope, deferred);
    else if (auto stmtToScope = node->dyn_cast<Stmt *>())
      dispatchAndCreateIfNeeded(stmtToScope, deferred);
    else if (auto exprToScope = node->dyn_cast<Expr *>())
      dispatchAndCreateIfNeeded(exprToScope, deferred);
    else
      llvm_unreachable("Impossible case");
  }
}

template <typename StmtExpr>
void ASTScopeImpl::dispatchAndCreateIfNeeded(StmtExpr *se,
                                             DeferredNodes &deferred) {
  if (se)
    ScopeCreator().visit(se, this, deferred);
}
void ASTScopeImpl::dispatchAndCreateIfNeeded(Decl *d, DeferredNodes &deferred) {
  // Implicit declarations don't have source information for name lookup.

  // Have also seen the following in an AST:
  // Given
  //  func testInvalidKeyPathComponents() {
  //    let _ = \.{return 0} // expected-error* {{}}
  //  }
  //  class X {
  //    class var a: Int { return 1 }
  //  }
  // Get:
  //  (pattern_binding_decl range=[test.swift:2:3 - line:2:3]
  //   (pattern_typed implicit type='<<error type>>'
  //     (pattern_named '_')))
  // so test the SourceRange
  if (d && !d->isImplicit() &&
      (!isa<PatternBindingDecl>(d) || d->getStartLoc() != d->getEndLoc()))
    ScopeCreator().visit(d, this, deferred);
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
      createSubtree<AttachedPropertyDelegateScope>(vd);
  });
}

void ASTScopeImpl::createScopesForPatternBindingInATypeDecl(
    PatternBindingDecl *patternBinding) {
  createSiblingPatternScopes(patternBinding);
}

void ASTScopeImpl::createScopesForPatternBindingInAFunction(
    PatternBindingDecl *patternBinding, DeferredNodes &deferred) {
  createNestedPatternScopes(patternBinding, deferred);
}

void ASTScopeImpl::createSiblingPatternScopes(
    PatternBindingDecl *patternBinding) {
  for (auto entryIndex : range(patternBinding->getNumPatternEntries()))
    if (!patternBinding->getPattern(entryIndex)->isImplicit()) {
      createSubtree<PatternEntryDeclScope>(patternBinding, entryIndex);
    }
}

void ASTScopeImpl::createNestedPatternScopes(PatternBindingDecl *patternBinding,
                                             DeferredNodes &deferred) {
  createRestOfNestedPatternScopes(patternBinding, deferred, 0);
}

void ASTScopeImpl::createRestOfNestedPatternScopes(
    PatternBindingDecl *patternBinding, DeferredNodes &deferred,
    unsigned entryIndex) {
  if (entryIndex >= patternBinding->getPatternList().size()) {
    // no more entries, create the scopes inside the pattern use
    dispatchAndCreateAll(deferred);
    return;
  }
  // When PatternEntryDeclScope expands, it will create a PatternEntryUseScope
  // which will call createRestOfNestedPatternScopes when it expands.
  createSubtreeWithDeferrals<PatternEntryDeclScope>(deferred, patternBinding,
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
    s = s->createSubtree<GenericParamScope>(parameterizedDecl, generics, i);
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
    DeferredNodes &deferred) {
  expandMe(deferred);
  addChildScopesForAnyRemainingDeferredNodes(deferred);
}

void ASTScopeImpl::addChildScopesForAnyRemainingDeferredNodes(
    DeferredNodes &deferred) {
  clearCachedSourceRangesOfAncestors();
  dispatchAndCreateAll(deferred);
  cacheSourceRangesOfAncestors();
}

#pragma mark specific implementations of expansion

// Many kinds of scopes do not expandScope into subscopes
void ASTScopeImpl::expandMe(DeferredNodes &) { dontExpand(); }

void ASTSourceFileScope::expandMe(DeferredNodes &deferred) {
  llvm_unreachable("get expanded from addAnyNewDeclsToTree");
}

  // Create child scopes for every declaration in a body.

void AbstractFunctionDeclScope::expandMe(DeferredNodes &) {
  // Create scopes for specialize attributes
  forEachSpecializeAttrInSourceOrder(decl, [&](SpecializeAttr *specializeAttr) {
    createSubtree<SpecializeAttributeScope>(specializeAttr, decl);
  });

  // Create scopes for generic and ordinary parameters.
  // For a subscript declaration, the generic and ordinary parameters are in an
  // ancestor scope, so don't make them here.
  ASTScopeImpl *leaf = this;
  if (!isa<AccessorDecl>(decl)) {
    leaf = createGenericParamScopes(decl, decl->getGenericParams());
    if (!decl->isImplicit()) {
      leaf = leaf->createSubtree<AbstractFunctionParamsScope>(
          decl->getParameters(), nullptr);
    }
  }
  // Create scope for the body.
  if (decl->getBody()) {
    if (decl->getDeclContext()->isTypeContext())
      leaf->createSubtree<MethodBodyScope>(decl);
    else
      leaf->createSubtree<PureFunctionBodyScope>(decl);
  }
}

void AbstractFunctionBodyScope::expandMe(DeferredNodes &deferred) {
  BraceStmt *braceStmt = decl->getBody();
  ScopeCreator().visitBraceStmt(braceStmt, this, deferred);
}

void PatternEntryDeclScope::expandMe(DeferredNodes &deferred) {
  auto patternEntry = getPatternEntry();
  // Create a child for the initializer, if present.
  // Cannot trust the source range given in the ASTScopeImpl for the end of the
  // initializer (because of InterpolatedLiteralStrings and EditorPlaceHolders),
  // so compute it ourselves.
  SourceLoc initializerEnd;
  if (patternEntry.getInitAsWritten() &&
      patternEntry.getInitAsWritten()->getSourceRange().isValid()) {
    auto *initializer =
        createSubtree<PatternEntryInitializerScope>(decl, patternEntryIndex);
    initializerEnd = initializer->getSourceRange().End;
  }
  // If there are no uses of the declararations, add the accessors immediately.
  if (deferred.empty())
    addVarDeclScopesAndTheirAccessors(this);
  else
    // Note: the accessors will follow the pattern binding.
    createSubtreeWithDeferrals<PatternEntryUseScope>(
        deferred, decl, patternEntryIndex, initializerEnd);
}

void PatternEntryInitializerScope::expandMe(DeferredNodes &) {
  DeferredNodes noDeferrals(__FILE__, __LINE__, *this, this);
  // Create a child for the initializer expression.
  ScopeCreator().visitExpr(getPatternEntry().getInitAsWritten(), this,
                           noDeferrals);
}

void PatternEntryUseScope::expandMe(DeferredNodes &deferred) {
  // Add accessors for the variables in this pattern.
  addVarDeclScopesAndTheirAccessors(this);
  // Create a child for the next pattern binding.
  createRestOfNestedPatternScopes(decl, deferred, patternEntryIndex + 1);
}

void ConditionalClauseScope::expandMe(DeferredNodes &deferred) {
  // If this is a boolean conditional not in a guard continuation, add a
  // child for the expression.
  createSubtreeForCondition();
  // If there's another conditional clause, add it as the child.
  if (index + 1 < getContainingStatement()->getCond().size())
    createSubtreeForNextConditionalClause(deferred);
  else {
    // There aren't any additional conditional clauses. Add the appropriate
    // nested scope based on the kind of statement.
    createSubtreeForAfterClauses(deferred);
  }
}

void IfStmtScope::expandMe(DeferredNodes &deferred) {
  // The first conditional clause or, failing that, the 'then' clause.
  if (!stmt->getCond().empty()) {
    createSubtree<IfConditionalClauseScope>(stmt, 0);
  } else {
    dispatchAndCreateIfNeeded(stmt->getThenStmt(), deferred);
  }
  // Add the 'else' branch, if needed.
  dispatchAndCreateIfNeeded(stmt->getElseStmt(), deferred);
}

void RepeatWhileScope::expandMe(DeferredNodes &deferred) {
  dispatchAndCreateIfNeeded(stmt->getBody(), deferred);
  ScopeCreator().visitExpr(stmt->getCond(), this, deferred);
}

void DoCatchStmtScope::expandMe(DeferredNodes &deferred) {
  dispatchAndCreateIfNeeded(stmt->getBody(), deferred);

  for (auto catchClause : stmt->getCatches()) {
    ScopeCreator().visitCatchStmt(catchClause, this, deferred);
  }
}

void SwitchStmtScope::expandMe(DeferredNodes &deferred) {
  ScopeCreator().visitExpr(stmt->getSubjectExpr(), this, deferred);

  for (auto caseStmt : stmt->getCases())
    createSubtree<CaseStmtScope>(caseStmt);
}

void ForEachStmtScope::expandMe(DeferredNodes &deferred) {
  ScopeCreator().visitExpr(stmt->getSequence(), this, deferred);

  // Add a child describing the scope of the pattern.
  createSubtree<ForEachPatternScope>(stmt);
}

void ForEachPatternScope::expandMe(DeferredNodes &deferred) {
  ScopeCreator().visitExpr(stmt->getWhere(), this, deferred);
  ScopeCreator().visitBraceStmt(stmt->getBody(), this, deferred);
}

void CatchStmtScope::expandMe(DeferredNodes &deferred) {
  ScopeCreator().visitExpr(stmt->getGuardExpr(), this, deferred);
  dispatchAndCreateIfNeeded(stmt->getBody(), deferred);
}

void CaseStmtScope::expandMe(DeferredNodes &deferred) {
  for (auto &caseItem : stmt->getMutableCaseLabelItems())
    ScopeCreator().visitExpr(caseItem.getGuardExpr(), this, deferred);

  // Add a child for the case body.
  dispatchAndCreateIfNeeded(stmt->getBody(), deferred);
}

void GuardStmtScope::expandMe(DeferredNodes &deferred) {
  // Add a child to describe the guard condition.
  createSubtree<GuardConditionalClauseScope>(stmt, 0);

  // Add a child for the 'guard' body, which always exits.
  dispatchAndCreateIfNeeded(stmt->getBody(), deferred);

  // Add a child to describe the guard condition for the continuation.
  createSubtreeWithDeferrals<GuardContinuationScope>(deferred, stmt, 0);
}

void BraceStmtScope::expandMe(DeferredNodes &deferred) {
  deferred.pushAllNecessaryNodes(stmt->getElements());
  dispatchAndCreateAll(deferred);
}

void VarDeclScope::expandMe(DeferredNodes &deferred) {
  assert(deferred.empty() && "Decls needing this var go into the PatternEntryUseScope, not here");
  addChildrenForAllExplicitAccessors(decl, deferred);
}

void SubscriptDeclScope::expandMe(DeferredNodes &deferred) {
  auto *sub = decl;
  auto *leaf = createGenericParamScopes(sub, sub->getGenericParams());
  auto *params = leaf->createSubtree<AbstractFunctionParamsScope>(
      sub->getIndices(), sub->getGetter());
  params->addChildrenForAllExplicitAccessors(sub, deferred);
}

void WholeClosureScope::expandMe(DeferredNodes &) {
  if (auto *cl = captureList.getPtrOrNull())
    createSubtree<CaptureListScope>(cl);
  if (closureExpr->getInLoc().isValid())
    createSubtree<ClosureParametersScope>(captureList, closureExpr);
  createSubtree<ClosureBodyScope>(captureList, closureExpr);
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
  createSubtree<BraceStmtScope>(closureExpr->getBody());
}

void TopLevelCodeScope::expandMe(DeferredNodes &deferred) {
  createSubtreeWithDeferrals<BraceStmtScope>(deferred, decl->getBody());
  assert(deferred.empty() &&
         "a top level code scope should snarf up all the deferred nodes");
}

void AbstractFunctionParamsScope::expandMe(DeferredNodes &deferred) {
  // Each initializer for a function parameter is its own, sibling, scope.
  // Unlike generic parameters or pattern initializers, it cannot refer to a
  // previous parameter.
  for (ParamDecl *pd : params->getArray()) {
    if (pd->getDefaultValue())
      createSubtree<DefaultArgumentInitializerScope>(pd);
  }
}

void DefaultArgumentInitializerScope::expandMe(DeferredNodes &deferred) {
  auto *initExpr = decl->getDefaultValue();
  assert(initExpr);
  ScopeCreator().visitExpr(initExpr, this, deferred);
}

void GTXScope::expandMe(DeferredNodes &deferred) {
  portion->expandScope(this, deferred);
}

void GTXWholePortion::expandScope(GTXScope *scope,
                                  DeferredNodes &deferred) const {
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
                                          DeferredNodes &deferred) const {
  if (auto *idc = scope->getIterableDeclContext().getPtrOrNull())
    for (auto member : idc->getMembers())
      scope->dispatchAndCreateIfNeeded(member, deferred);
}

#pragma mark createBodyScope

void ExtensionScope::createBodyScope(ASTScopeImpl *leaf) {
  leaf->createSubtree2D<ExtensionScope, IterableTypeBodyPortion>(decl);
}
void NominalTypeScope::createBodyScope(ASTScopeImpl *leaf) {
  leaf->createSubtree2D<NominalTypeScope, IterableTypeBodyPortion>(decl);
}

#pragma mark createTrailingWhereClauseScope

ASTScopeImpl *
ExtensionScope::createTrailingWhereClauseScope(ASTScopeImpl *parent) {
  return parent->createSubtree2D<ExtensionScope, GTXWherePortion>(decl);
}
ASTScopeImpl *
NominalTypeScope::createTrailingWhereClauseScope(ASTScopeImpl *parent) {
  return parent->createSubtree2D<NominalTypeScope, GTXWherePortion>(decl);
}
ASTScopeImpl *
TypeAliasScope::createTrailingWhereClauseScope(ASTScopeImpl *parent) {
  return parent->createSubtree2D<TypeAliasScope, GTXWherePortion>(decl);
}

#pragma mark misc

void ASTScopeImpl::forEachClosureIn(
    Expr *expr,
    function_ref<void(NullablePtr<CaptureListExpr>, ClosureExpr *)>
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
    Expr *expr,
    function_ref<void(NullablePtr<CaptureListExpr>, ClosureExpr *)>
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

void AbstractPatternEntryScope::addVarDeclScopesAndTheirAccessors(
    ASTScopeImpl *parent) const {
  getPatternEntry().getPattern()->forEachVariable([&](VarDecl *var) {
    const bool hasAccessors = var->getBracesRange().isValid();
    if (hasAccessors && !var->isImplicit())
      parent->createSubtree<VarDeclScope>(var);
  });
}

bool ASTScopeImpl::isCreatedDirectly(const ASTNode n) {
  // See addVarDeclScopesAndTheirAccessors and addChildrenForAllExplicitAccessors
  if (auto *d = n.dyn_cast<Decl*>())
    return isa<VarDecl>(d) || isa<AccessorDecl>(d);
  return false;
}

void ASTScopeImpl::addChildrenForAllExplicitAccessors(AbstractStorageDecl *asd,
                                                      DeferredNodes &deferred) {
  for (auto accessor : asd->getAllAccessors()) {
    if (!accessor->isImplicit() && accessor->getStartLoc().isValid()) {
      // Accessors are always nested within their abstract storage declaration.
      // The nesting may not be immediate, because subscripts may have
      // intervening scopes for generics.
      if (getEnclosingAbstractStorageDecl() == accessor->getStorage())
        ScopeCreator().visitAbstractFunctionDecl(accessor, this, deferred);
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
    ScopeCreator().visitExpr(cond.getBoolean(), this, noDeferrals);
    return;
  case StmtConditionElement::CK_PatternBinding:
    createSubtree<StatementConditionElementPatternScope>(cond.getPattern());
    ScopeCreator().visitExpr(cond.getInitializer(), this, noDeferrals);
    return;
  }
}

void GuardContinuationScope::createSubtreeForCondition() {
  // no condition for this one
}

void WhileConditionalClauseScope::createSubtreeForNextConditionalClause(
    DeferredNodes &deferred) {
  createSubtreeWithDeferrals<WhileConditionalClauseScope>(deferred, stmt,
                                                          index + 1);
}
void IfConditionalClauseScope::createSubtreeForNextConditionalClause(
    DeferredNodes &deferred) {
  createSubtreeWithDeferrals<IfConditionalClauseScope>(deferred, stmt,
                                                       index + 1);
}
void GuardConditionalClauseScope::createSubtreeForNextConditionalClause(
    DeferredNodes &deferred) {
  createSubtreeWithDeferrals<GuardConditionalClauseScope>(deferred, stmt,
                                                          index + 1);
}
void GuardContinuationScope::createSubtreeForNextConditionalClause(
    DeferredNodes &deferred) {
  createSubtreeWithDeferrals<GuardContinuationScope>(deferred, stmt, index + 1);
}

void WhileConditionalClauseScope::createSubtreeForAfterClauses(
    DeferredNodes &deferred) {
  dispatchAndCreateIfNeeded(stmt->getBody(), deferred);
}
void IfConditionalClauseScope::createSubtreeForAfterClauses(
    DeferredNodes &deferred) {
  dispatchAndCreateIfNeeded(stmt->getThenStmt(), deferred);
}

void GuardConditionalClauseScope::createSubtreeForAfterClauses(
    DeferredNodes &deferred) {
  // There aren't any additional conditional clauses. Add the appropriate
  // nested scope based on the kind of statement.

  // Note: guard statements have the continuation nested under the last
  // condition.
  // The scope *after* the guard statement must include any deferred nodes
  // so that any let vars in the guard are in scope.
  dispatchAndCreateAll(deferred);
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
    createSubtree<WholeClosureScope>(captureList, closureExpr);
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

#pragma mark creation methods

AbstractPatternEntryScope::AbstractPatternEntryScope(
    PatternBindingDecl *declBeingScoped, unsigned entryIndex)
    : decl(declBeingScoped), patternEntryIndex(entryIndex) {
  assert(entryIndex < declBeingScoped->getPatternList().size() &&
         "out of bounds");
}

template <typename ScopeClass, typename... Args>
ASTScopeImpl *ASTScopeImpl::createSubtree(Args... args) {
  DeferredNodes noDeferrals(__FILE__, __LINE__, *this, nullptr);
  return createSubtreeWithDeferrals<ScopeClass>(noDeferrals, args...);
}

template <typename ScopeClass, typename... Args>
ASTScopeImpl *ASTScopeImpl::createSubtreeWithDeferrals(DeferredNodes &deferred,
                                                       Args... args) {
  auto *child = new (getASTContext()) ScopeClass(args...);
  addChild(child);
  child->expandMeAndCreateScopesForDeferredNodes(deferred);
  return child;
}

template <typename ScopeClass, typename PortionT, typename... Args>
ASTScopeImpl *ASTScopeImpl::createSubtree2D(Args... args) {
  DeferredNodes noDeferrals(__FILE__, __LINE__, *this, nullptr);
  return createSubtreeWithDeferrals2D<ScopeClass, PortionT>(noDeferrals, args...);
}

template <typename ScopeClass, typename PortionT, typename... Args>
ASTScopeImpl *
ASTScopeImpl::createSubtreeWithDeferrals2D(DeferredNodes &deferred,
                                           Args... args) {
  const Portion *portion = new (getASTContext()) PortionT();
  auto *child = new (getASTContext()) ScopeClass(portion, args...);
  addChild(child);
  child->expandMeAndCreateScopesForDeferredNodes(deferred);
  return child;
}

#pragma mark DeferredNodes definitions

DeferredNodes::DeferredNodes(const char *file, int line,
                             const ASTScopeImpl &creator, const void *forWhat)
    : file(file), line(line), creator(creator), forWhat(forWhat) {}
DeferredNodes::~DeferredNodes() {
  assert(nodes.empty() && "Should have consumed all nodes");
}

ASTNode DeferredNodes::popNextDeferredNodeForAdoptionBy(ASTScopeImpl *s) {
  setLastAdopter(s);
  auto f = nodes.front();
  nodes.pop_front();
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
  if (!lastAdopter || lastAdopter.get()->depth() <= s->depth()) {
    lastAdopter = s;
  }
}

bool DeferredNodes::empty() const { return nodes.empty(); }

void DeferredNodes::pushIfNecessary(ASTNode n) {
  // Do not defer VarDecls or Accessors because
  // they get created directly by the pattern code
  // and deferring them pushes them to the wrong place.
  // Then, even though they're ignored, they distort the source range
  // of their parents.
  
  // An optimization; will be ignored later anyway.
  if (ASTScopeImpl::isCreatedDirectly(n))
    return;
  
  nodes.push_front(n);
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
  out << "From " << file << ": " << line << " for: " << forWhat << "\n";
  out << "Scope: ";
  creator.print(out);
  out << "\n";
  for (const auto i : indices(nodes)) {
    out << i << ": ";
    nodes[i].dump(out);
    out << "\n";
  }
}

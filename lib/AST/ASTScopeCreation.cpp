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
  friend class ASTSourceFileScope;
  /// For allocating scopes.
  ASTContext &ctx;

public:
  ASTSourceFileScope *const sourceFileScope;

private:
  /// Catch duplicate nodes in the AST
  /// TODO: better to use a shared pointer? Unique pointer?
  Optional<llvm::DenseSet<void*>> _astDuplicates;
  llvm::DenseSet<void*> &astDuplicates;

public:
  ScopeCreator(SourceFile *SF)
      : ctx(SF->getASTContext()),
        sourceFileScope(new (ctx) ASTSourceFileScope(SF, this)),
        _astDuplicates(llvm::DenseSet<void *>()),
        astDuplicates(_astDuplicates.getValue()) {}

public:
  ScopeCreator(const ScopeCreator &) = delete;  // ensure no copies
  ScopeCreator(const ScopeCreator &&) = delete; // ensure no moves

public:
  /// Given an array of ASTNodes or Decl pointers, add them
  /// Return the resultant insertionPoint
  /// Use a template in order to be able to pass in either an array of ASTNodes
  /// or an array of Decl*s.
  template <typename NodeType>
  ASTScopeImpl *addScopesToTree(ASTScopeImpl *const insertionPoint,
                                ArrayRef<NodeType> nodesOrDeclsToAdd) {
    auto *ip = insertionPoint;
    for (auto nd : nodesOrDeclsToAdd) {
      if (shouldThisNodeBeScopedWhenEncountered(nd))
        ip = createScopeFor(nd, ip);
    }
    return ip;
  }

public:
  /// Return new insertion point
  ASTScopeImpl *createScopeFor(ASTNode, ASTScopeImpl *parent);

  bool shouldCreateScope(ASTNode n) const {
    // Cannot ignore implicit statements because implict return can contain
    // scopes in the expression, such as closures.
    if (!n)
      return false;
    if (n.is<Stmt *>() || n.is<Expr *>())
      return true;

    auto *const d = n.get<Decl *>();
    // Implicit nodes don't have source information for name lookup.
    if (d->isImplicit())
      return false;
    /// In \c Parser::parseDeclVarGetSet fake PBDs are created. Ignore them.
    /// Example:
    /// \code
    /// class SR10903 { static var _: Int { 0 } }
    /// \endcode
    if (const auto *PBD = dyn_cast<PatternBindingDecl>(d))
      if (PBD->isInvalid())
        return false;
    /// In
    /// \code
    /// @propertyWrapper
    /// public struct Wrapper<T> {
    ///   public var value: T
    ///
    ///   public init(body: () -> T) {
    ///     self.value = body()
    ///   }
    /// }
    ///
    /// let globalInt = 17
    ///
    /// @Wrapper(body: { globalInt })
    /// public var y: Int
    /// \endcode
    /// I'm seeing a dumped AST include:
    /// (pattern_binding_decl range=[test.swift:13:8 - line:12:29]
    const auto &SM = d->getASTContext().SourceMgr;
    assert((d->getStartLoc().isInvalid() ||
            !SM.isBeforeInBuffer(d->getEndLoc(), d->getStartLoc())) &&
           "end-before-start will break tree search via location");
    return true;
  }

  template <typename Scope, typename... Args>
  /// Create a new scope of class ChildScope initialized with a ChildElement,
  /// expandScope it,
  /// add it as a child of the receiver, and return the child and the scope to
  /// receive more decls.
  ASTScopeImpl *createSubtree(ASTScopeImpl *parent, Args... args) {
    auto *child = new (ctx) Scope(args...);
    parent->addChild(child, ctx);
    return child->expandMe(*this);
  }

  template <typename Scope, typename PortionClass, typename... Args>
  ASTScopeImpl *createSubtree2D(ASTScopeImpl *parent, Args... args) {
    const Portion *portion = new (ctx) PortionClass();
    return createSubtree<Scope>(parent, portion, args...);
  }

  void addChildrenForCapturesAndClosuresIn(Expr *expr, ASTScopeImpl *parent) {
    // Use the ASTWalker to find buried captures and closures
    forEachUniqueClosureIn(expr, [&](NullablePtr<CaptureListExpr> captureList,
                                     ClosureExpr *closureExpr) {
      createSubtree<WholeClosureScope>(parent, closureExpr, captureList);
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

  // A safe way to discover this, without creating a circular request.
  // Cannot call getAttachedPropertyWrappers.
  static bool hasCustomAttribute(VarDecl *vd) {
    return vd->getAttrs().getAttribute<CustomAttr>();
  }

public:
  /// If the pattern has an attached property wrapper, create a scope for it
  /// so it can be looked up.

  void createAttachedPropertyWrapperScope(PatternBindingDecl *patternBinding,
                                          ASTScopeImpl *parent) {
 
    patternBinding->getPattern(0)->forEachVariable([&](VarDecl *vd) {
        // assume all same as first
      if (hasCustomAttribute(vd))
        createSubtree<AttachedPropertyWrapperScope>(parent, vd);
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
        s = createSubtree<GenericParamScope>(s, parameterizedDecl, generics, i);
      }
    return s;
  }

  void addChildrenForAllExplicitAccessors(AbstractStorageDecl *asd,
                                          ASTScopeImpl *parent);

  void
  forEachSpecializeAttrInSourceOrder(Decl *declBeingSpecialized,
                                     function_ref<void(SpecializeAttr *)> fn) {
    SmallVector<SpecializeAttr *, 8> sortedSpecializeAttrs;
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

  bool shouldThisNodeBeScopedWhenEncountered(ASTNode n) {
    // Do not scope VarDecls or Accessors when encountered because
    // they get created directly by the pattern code.
    // Doing otherwise distorts the source range
    // of their parents.

    if (PatternEntryDeclScope::isHandledSpecially(n))
      return false;

    if (!astDuplicates.insert(n.getOpaqueValue()).second)
      return false;

    return true;
  }

  template <typename ASTNodelike>
  void pushAllNecessaryNodes(ArrayRef<ASTNodelike> nodesToPrepend) {
    for (int i = nodesToPrepend.size() - 1; i >= 0; --i)
      pushIfNecessary(nodesToPrepend[i]);
  }

  bool isDuplicate(void *p, bool registerDuplicate = true) {
    if (registerDuplicate)
      return !astDuplicates.insert(p).second;
    return astDuplicates.count(p);
  }

public:
  void dump() const { print(llvm::errs()); }

  void print(raw_ostream &out) const {
    out << "(swift::ASTSourceFileScope*) " << sourceFileScope << "\n";
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

ASTScope::ASTScope(SourceFile *SF) : impl(createScopeTree(SF)) {}

ASTSourceFileScope *ASTScope::createScopeTree(SourceFile *SF) {
  ScopeCreator *scopeCreator = new (SF->getASTContext()) ScopeCreator(SF);
  scopeCreator->sourceFileScope->addNewDeclsToTree();
  return scopeCreator->sourceFileScope;
}

void ASTSourceFileScope::addNewDeclsToTree() {
  assert(SF && scopeCreator);
  ArrayRef<Decl *> decls = SF->Decls;
  ArrayRef<Decl *> newDecls = decls.slice(numberOfDeclsAlreadySeen);
  // Save source range recalculation work if possible
  if (newDecls.empty())
    return;

  insertionPoint->ensureSourceRangesAreCorrectWhenAddingDescendants([&] {
    insertionPoint = scopeCreator->addScopesToTree(insertionPoint, newDecls);
  });
  numberOfDeclsAlreadySeen = SF->Decls.size();
}

ASTSourceFileScope::ASTSourceFileScope(SourceFile *SF,
                                       ScopeCreator *scopeCreator)
    : SF(SF), scopeCreator(scopeCreator), insertionPoint(this) {}

void ASTScopeImpl::ensureSourceRangesAreCorrectWhenAddingDescendants(
    function_ref<void()> modify) {
  clearCachedSourceRangesOfMeAndAncestors();
  modify();
  cacheSourceRangesOfSlice();
}

#pragma mark ASTVisitorForScopeCreation

namespace swift {
namespace ast_scope {

class ASTVisitorForScopeCreation
    : public ASTVisitor<ASTVisitorForScopeCreation, ASTScopeImpl *,
                        ASTScopeImpl *, ASTScopeImpl *, void, void, void,
                        ASTScopeImpl *, ScopeCreator &> {
public:

#pragma mark ASTNodes that do not create scopes

  // Even ignored Decls and Stmts must extend the source range of a scope:
  // E.g. a braceStmt with some definitions that ends in a statement that
  // accesses such a definition must resolve as being IN the scope.

#define VISIT_AND_IGNORE(What)                                                 \
  ASTScopeImpl *visit##What(What *w, ASTScopeImpl *p, ScopeCreator &) {        \
    p->widenSourceRangeForIgnoredASTNode(w);                                   \
    return p;                                                                  \
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
  ASTScopeImpl *visit##What(What *w, ASTScopeImpl *p,                          \
                            ScopeCreator &scopeCreator) {                      \
    return scopeCreator.createSubtree<ScopeClass>(p, w);                       \
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
  VISIT_AND_CREATE(AbstractFunctionDecl, AbstractFunctionDeclScope)

#undef VISIT_AND_CREATE

#pragma mark 2D simple creation (ignoring deferred nodes)

#define VISIT_AND_CREATE_WHOLE_PORTION(What, WhatScope)                        \
  ASTScopeImpl *visit##What(What *w, ASTScopeImpl *p,                          \
                            ScopeCreator &scopeCreator) {                      \
    return scopeCreator                                                        \
        .createSubtree2D<WhatScope, GenericTypeOrExtensionWholePortion>(p, w); \
  }

  VISIT_AND_CREATE_WHOLE_PORTION(ExtensionDecl, ExtensionScope)
  VISIT_AND_CREATE_WHOLE_PORTION(StructDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(ClassDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(EnumDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(TypeAliasDecl, TypeAliasScope)
  VISIT_AND_CREATE_WHOLE_PORTION(OpaqueTypeDecl, OpaqueTypeScope)
#undef VISIT_AND_CREATE_WHOLE_PORTION

  ASTScopeImpl *visitProtocolDecl(ProtocolDecl *e, ASTScopeImpl *p,
                                  ScopeCreator &scopeCreator) {
    e->createGenericParamsIfMissing();
    return scopeCreator
        .createSubtree2D<NominalTypeScope, GenericTypeOrExtensionWholePortion>(
            p, e);
  }

#pragma mark simple creation with deferred nodes

  // Each of the following creates a new scope, so that nodes which were parsed
  // after them need to be placed in scopes BELOW them in the tree. So pass down
  // the deferred nodes.
  ASTScopeImpl *visitGuardStmt(GuardStmt *e, ASTScopeImpl *p,
                               ScopeCreator &scopeCreator) {
    return scopeCreator.createSubtree<GuardStmtScope>(p, e);
  }
  ASTScopeImpl *visitDoStmt(DoStmt *ds, ASTScopeImpl *p,
                            ScopeCreator &scopeCreator) {
    return scopeCreator.createScopeFor(ds->getBody(), p);
  }
  ASTScopeImpl *visitTopLevelCodeDecl(TopLevelCodeDecl *d, ASTScopeImpl *p,
                                      ScopeCreator &scopeCreator) {
    return scopeCreator.createSubtree<TopLevelCodeScope>(p, d);
  }

#pragma mark special-case creation

  ASTScopeImpl *visitSourceFile(SourceFile *, ASTScopeImpl *, ScopeCreator &) {
    llvm_unreachable("SourceFiles are orphans.");
  }

  ASTScopeImpl *visitYieldStmt(YieldStmt *ys, ASTScopeImpl *p,
                               ScopeCreator &scopeCreator) {
    for (Expr *e : ys->getYields())
      visitExpr(e, p, scopeCreator);
    return p;
  }

  ASTScopeImpl *visitDeferStmt(DeferStmt *ds, ASTScopeImpl *p,
                               ScopeCreator &scopeCreator) {
    visitFuncDecl(ds->getTempDecl(), p, scopeCreator);
    return p;
  }

  ASTScopeImpl *visitBraceStmt(BraceStmt *bs, ASTScopeImpl *p,
                               ScopeCreator &scopeCreator) {
    scopeCreator.createSubtree<BraceStmtScope>(p, bs);
    return p;
  }

  ASTScopeImpl *visitPatternBindingDecl(PatternBindingDecl *patternBinding,
                                        ASTScopeImpl *parentScope,
                                        ScopeCreator &scopeCreator) {
    scopeCreator.createAttachedPropertyWrapperScope(patternBinding,
                                                    parentScope);

    const bool isInTypeDecl = parentScope->isATypeDeclScope();

    const DeclVisibilityKind vis =
        isInTypeDecl ? DeclVisibilityKind::MemberOfCurrentNominal
                     : DeclVisibilityKind::LocalVariable;
    auto *insertionPoint = parentScope;
    for (unsigned i = 0; i < patternBinding->getPatternList().size(); ++i) {
      insertionPoint = scopeCreator.createSubtree<PatternEntryDeclScope>(
          insertionPoint, patternBinding, i, vis);
    }
    // If in a type decl, the type search will find these,
    // but if in a brace stmt, must continue under the last binding.
    return isInTypeDecl ? parentScope : insertionPoint;
  }

  ASTScopeImpl *visitReturnStmt(ReturnStmt *rs, ASTScopeImpl *p,
                                ScopeCreator &scopeCreator) {
    if (rs->hasResult())
      visitExpr(rs->getResult(), p, scopeCreator);
    return p;
  }

  ASTScopeImpl *visitExpr(Expr *expr, ASTScopeImpl *p,
                          ScopeCreator &scopeCreator) {
    if (expr) {
      p->widenSourceRangeForIgnoredASTNode(expr);
      scopeCreator.addChildrenForCapturesAndClosuresIn(expr, p);
    }
    return p;
  }
};
} // namespace ast_scope
} // namespace swift

// These definitions are way down here so it can call into
// ASTVisitorForScopeCreation
ASTScopeImpl *ScopeCreator::createScopeFor(ASTNode n, ASTScopeImpl *parent) {
  if (!shouldCreateScope(n))
    return parent;
  if (auto *p = n.dyn_cast<Decl *>())
    return ASTVisitorForScopeCreation().visit(p, parent, *this);
  if (auto *p = n.dyn_cast<Expr *>())
    return ASTVisitorForScopeCreation().visit(p, parent, *this);
  auto *p = n.get<Stmt *>();
  return ASTVisitorForScopeCreation().visit(p, parent, *this);
}

void ScopeCreator::addChildrenForAllExplicitAccessors(AbstractStorageDecl *asd,
                                                      ASTScopeImpl *parent) {
  for (auto accessor : asd->getAllAccessors()) {
    if (!accessor->isImplicit() && accessor->getStartLoc().isValid()) {
      // Accessors are always nested within their abstract storage
      // declaration. The nesting may not be immediate, because subscripts may
      // have intervening scopes for generics.
      if (!isDuplicate(accessor) &&
          parent->getEnclosingAbstractStorageDecl() == accessor->getStorage())
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

bool PatternEntryDeclScope::isHandledSpecially(const ASTNode n) {
  if (auto *d = n.dyn_cast<Decl *>())
    return isa<VarDecl>(d) || isa<AccessorDecl>(d);
  return false;
}

#pragma mark specific implementations of expansion

  // Do this whole bit so it's easy to see which type of scope is which

#define CREATES_NEW_INSERTION_POINT(Scope)                                     \
  ASTScopeImpl *Scope::expandMe(ScopeCreator &scopeCreator) {                  \
    return expandAScopeThatCreatesANewInsertionPoint(scopeCreator);            \
  }

#define NO_NEW_INSERTION_POINT(Scope)                                          \
  ASTScopeImpl *Scope::expandMe(ScopeCreator &scopeCreator) {                  \
    expandAScopeThatDoesNotCreateANewInsertionPoint(scopeCreator);             \
    return getParent().get();                                                  \
  }

// Return this in particular for GenericParamScope so body is scoped under it
#define NO_EXPANSION(Scope)                                                    \
  ASTScopeImpl *Scope::expandMe(ScopeCreator &) { return this; }

CREATES_NEW_INSERTION_POINT(AbstractFunctionParamsScope)
CREATES_NEW_INSERTION_POINT(ConditionalClauseScope)
CREATES_NEW_INSERTION_POINT(GuardStmtScope)
CREATES_NEW_INSERTION_POINT(PatternEntryDeclScope)
CREATES_NEW_INSERTION_POINT(PatternEntryInitializerScope)
CREATES_NEW_INSERTION_POINT(PatternEntryUseScope)
CREATES_NEW_INSERTION_POINT(GenericTypeOrExtensionScope)
CREATES_NEW_INSERTION_POINT(BraceStmtScope)
CREATES_NEW_INSERTION_POINT(TopLevelCodeScope)

NO_NEW_INSERTION_POINT(AbstractFunctionBodyScope)
NO_NEW_INSERTION_POINT(AbstractFunctionDeclScope)
NO_NEW_INSERTION_POINT(AttachedPropertyWrapperScope)

NO_NEW_INSERTION_POINT(CaptureListScope)
NO_NEW_INSERTION_POINT(CaseStmtScope)
NO_NEW_INSERTION_POINT(CatchStmtScope)
NO_NEW_INSERTION_POINT(ClosureBodyScope)
NO_NEW_INSERTION_POINT(DefaultArgumentInitializerScope)
NO_NEW_INSERTION_POINT(DoCatchStmtScope)
NO_NEW_INSERTION_POINT(ForEachPatternScope)
NO_NEW_INSERTION_POINT(ForEachStmtScope)
NO_NEW_INSERTION_POINT(IfStmtScope)
NO_NEW_INSERTION_POINT(RepeatWhileScope)
NO_NEW_INSERTION_POINT(SubscriptDeclScope)
NO_NEW_INSERTION_POINT(SwitchStmtScope)
NO_NEW_INSERTION_POINT(VarDeclScope)
NO_NEW_INSERTION_POINT(WhileStmtScope)
NO_NEW_INSERTION_POINT(WholeClosureScope)

NO_EXPANSION(GenericParamScope)
NO_EXPANSION(ASTSourceFileScope)
NO_EXPANSION(ClosureParametersScope)
NO_EXPANSION(SpecializeAttributeScope)
// no accessors, unlike PatternEntryUseScope
NO_EXPANSION(ConditionalClausePatternUseScope)
NO_EXPANSION(GuardStmtUseScope)

#undef CREATES_NEW_INSERTION_POINT
#undef NO_NEW_INSERTION_POINT

ASTScopeImpl *
AbstractFunctionParamsScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // Each initializer for a function parameter is its own, sibling, scope.
  // Unlike generic parameters or pattern initializers, it cannot refer to a
  // previous parameter.
  for (ParamDecl *pd : params->getArray()) {
    if (!scopeCreator.isDuplicate(pd) && pd->getDefaultValue())
      scopeCreator.createSubtree<DefaultArgumentInitializerScope>(this, pd);
  }
  return this; // body of func goes under me
}

ASTScopeImpl *PatternEntryDeclScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  auto patternEntry = getPatternEntry();
  // Create a child for the initializer, if present.
  // Cannot trust the source range given in the ASTScopeImpl for the end of the
  // initializer (because of InterpolatedLiteralStrings and EditorPlaceHolders),
  // so compute it ourselves.
  SourceLoc initializerEnd;
  if (patternEntry.getInitAsWritten() &&
      patternEntry.getInitAsWritten()->getSourceRange().isValid()) {
    auto *initializer =
        scopeCreator.createSubtree<PatternEntryInitializerScope>(
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

ASTScopeImpl *
PatternEntryInitializerScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // Create a child for the initializer expression.
  ASTVisitorForScopeCreation().visitExpr(getPatternEntry().getInitAsWritten(),
                                         this, scopeCreator);
  return this;
}

ASTScopeImpl *PatternEntryUseScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // Add accessors for the variables in this pattern.
  forEachVarDeclWithExplicitAccessors(scopeCreator, false, [&](VarDecl *var) {
    scopeCreator.createSubtree<VarDeclScope>(this, var);
  });
  return this;
}

ASTScopeImpl *ConditionalClauseScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  const StmtConditionElement &sec = getStmtConditionElement();
  switch (sec.getKind()) {
  case StmtConditionElement::CK_Availability:
    return this;
  case StmtConditionElement::CK_Boolean:
    ASTVisitorForScopeCreation().visitExpr(sec.getBoolean(), this,
                                           scopeCreator);
    return this;
  case StmtConditionElement::CK_PatternBinding:
    ASTVisitorForScopeCreation().visitExpr(sec.getInitializer(), this,
                                           scopeCreator);
    return scopeCreator.createSubtree<ConditionalClausePatternUseScope>(
        this, sec.getPattern(), endLoc);
  }
}

ASTScopeImpl *GuardStmtScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {

  ASTScopeImpl *conditionLookupParent =
      createNestedConditionalClauseScopes(scopeCreator, stmt->getBody());
  // Add a child for the 'guard' body, which always exits.
  // Parent is whole guard stmt scope, NOT the cond scopes
  scopeCreator.createScopeFor(stmt->getBody(), this);

  return scopeCreator.createSubtree<GuardStmtUseScope>(
      this, conditionLookupParent, stmt->getEndLoc());
}

ASTScopeImpl *
GenericTypeOrExtensionScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  return portion->expandScope(this, scopeCreator);
}

ASTScopeImpl *BraceStmtScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  return scopeCreator.addScopesToTree(this, stmt->getElements());
}

ASTScopeImpl *TopLevelCodeScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  return scopeCreator.createSubtree<BraceStmtScope>(this, decl->getBody());
}

#pragma mark expandAScopeThatDoesNotCreateANewInsertionPoint

void ASTSourceFileScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  llvm_unreachable("expanded by addNewDeclsToTree()");
}

// Create child scopes for every declaration in a body.

void AbstractFunctionDeclScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // Create scopes for specialize attributes
  scopeCreator.forEachSpecializeAttrInSourceOrder(
      decl, [&](SpecializeAttr *specializeAttr) {
        scopeCreator.createSubtree<SpecializeAttributeScope>(
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
      leaf = scopeCreator.createSubtree<AbstractFunctionParamsScope>(
          leaf, decl->getParameters(), nullptr);
    }
  }
  // Create scope for the body.
  // We create body scopes when there is no body for source kit to complete
  // erroneous code in bodies.
  if (decl->getBodySourceRange().isValid()) {
    if (AbstractFunctionBodyScope::isAMethod(decl))
      scopeCreator.createSubtree<MethodBodyScope>(leaf, decl);
    else
      scopeCreator.createSubtree<PureFunctionBodyScope>(leaf, decl);
  }
}

void AbstractFunctionBodyScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // We create body scopes when there is no body for source kit to complete
  // erroneous code in bodies.
  if (BraceStmt *braceStmt = decl->getBody())
    ASTVisitorForScopeCreation().visitBraceStmt(braceStmt, this, scopeCreator);
}

void IfStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  ASTScopeImpl *insertionPoint =
      createNestedConditionalClauseScopes(scopeCreator, stmt->getThenStmt());

  // The 'then' branch
  scopeCreator.createScopeFor(stmt->getThenStmt(), insertionPoint);

  // Add the 'else' branch, if needed.
  scopeCreator.createScopeFor(stmt->getElseStmt(), this);
}

void WhileStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  ASTScopeImpl *insertionPoint =
      createNestedConditionalClauseScopes(scopeCreator, stmt->getBody());
  scopeCreator.createScopeFor(stmt->getBody(), insertionPoint);
}

void RepeatWhileScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.createScopeFor(stmt->getBody(), this);
  ASTVisitorForScopeCreation().visitExpr(stmt->getCond(), this, scopeCreator);
}

void DoCatchStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.createScopeFor(stmt->getBody(), this);

  for (auto catchClause : stmt->getCatches()) {
    if (!scopeCreator.isDuplicate(catchClause)) {
      ASTVisitorForScopeCreation().visitCatchStmt(catchClause, this,
                                                  scopeCreator);
    }
  }
}

void SwitchStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getSubjectExpr(), this,
                                         scopeCreator);

  for (auto caseStmt : stmt->getCases()) {
    if (!scopeCreator.isDuplicate(caseStmt) && !caseStmt->isImplicit()) {
      scopeCreator.createSubtree<CaseStmtScope>(this, caseStmt);
    }
  }
}

void ForEachStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getSequence(), this,
                                         scopeCreator);

  // Add a child describing the scope of the pattern.
  // In error cases such as:
  //    let v: C { for b : Int -> S((array: P { }
  // the body is implicit and it would overlap the source range of the expr
  // above.
  if (!stmt->getBody()->isImplicit())
    scopeCreator.createSubtree<ForEachPatternScope>(this, stmt);
}

void ForEachPatternScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getWhere(), this, scopeCreator);
  ASTVisitorForScopeCreation().visitBraceStmt(stmt->getBody(), this,
                                              scopeCreator);
}

void CatchStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getGuardExpr(), this,
                                         scopeCreator);
  scopeCreator.createScopeFor(stmt->getBody(), this);
}

void CaseStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  for (auto &caseItem : stmt->getMutableCaseLabelItems())
    ASTVisitorForScopeCreation().visitExpr(caseItem.getGuardExpr(), this,
                                           scopeCreator);

  // Add a child for the case body.
  scopeCreator.createScopeFor(stmt->getBody(), this);
}

void VarDeclScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addChildrenForAllExplicitAccessors(decl, this);
}

void SubscriptDeclScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  auto *sub = decl;
  auto *leaf =
      scopeCreator.createGenericParamScopes(sub, sub->getGenericParams(), this);
  auto *params = scopeCreator.createSubtree<AbstractFunctionParamsScope>(
      leaf, sub->getIndices(), sub->getGetter());
  scopeCreator.addChildrenForAllExplicitAccessors(sub, params);
}

void WholeClosureScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  if (auto *cl = captureList.getPtrOrNull())
    scopeCreator.createSubtree<CaptureListScope>(this, cl);
  ASTScopeImpl *bodyParent = this;
  if (closureExpr->getInLoc().isValid())
    bodyParent = scopeCreator.createSubtree<ClosureParametersScope>(
        this, closureExpr, captureList);
  scopeCreator.createSubtree<ClosureBodyScope>(bodyParent, closureExpr,
                                               captureList);
}

void CaptureListScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
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

void ClosureBodyScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.createSubtree<BraceStmtScope>(this, closureExpr->getBody());
}

void DefaultArgumentInitializerScope::
    expandAScopeThatDoesNotCreateANewInsertionPoint(
        ScopeCreator &scopeCreator) {
  auto *initExpr = decl->getDefaultValue();
  assert(initExpr);
  ASTVisitorForScopeCreation().visitExpr(initExpr, this, scopeCreator);
}

void AttachedPropertyWrapperScope::
    expandAScopeThatDoesNotCreateANewInsertionPoint(
        ScopeCreator &scopeCreator) {
  for (auto *attr : decl->getAttrs().getAttributes<CustomAttr>()) {
    if (auto *expr = attr->getArg())
      ASTVisitorForScopeCreation().visitExpr(expr, this, scopeCreator);
  }
}

#pragma mark expandScope

ASTScopeImpl *GenericTypeOrExtensionWholePortion::expandScope(
    GenericTypeOrExtensionScope *scope, ScopeCreator &scopeCreator) const {
  // Prevent circular request bugs caused by illegal input and
  // doing lookups that getExtendedNominal in the midst of getExtendedNominal.
  if (scope->shouldHaveABody() && !scope->doesDeclHaveABody())
    return scope->getParent().get();

  auto *deepestScope = scopeCreator.createGenericParamScopes(
      scope->getDecl().get(), scope->getGenericContext()->getGenericParams(),
      scope);
  if (scope->getGenericContext()->getTrailingWhereClause())
    scope->createTrailingWhereClauseScope(deepestScope, scopeCreator);
  scope->createBodyScope(deepestScope, scopeCreator);
  return scope->getParent().get();
}

ASTScopeImpl *
IterableTypeBodyPortion::expandScope(GenericTypeOrExtensionScope *scope,
                                     ScopeCreator &scopeCreator) const {
  if (auto *idc = scope->getIterableDeclContext().getPtrOrNull()) {
    for (auto member : idc->getMembers()) {
      if (!scopeCreator.isDuplicate(member))
        scopeCreator.createScopeFor(member, scope);
    }
  }
  return scope->getParent().get();
}

ASTScopeImpl *GenericTypeOrExtensionWherePortion::expandScope(
    GenericTypeOrExtensionScope *scope, ScopeCreator &) const {
  return scope->getParent().get();
}

#pragma mark createBodyScope

void ExtensionScope::createBodyScope(ASTScopeImpl *leaf,
                                     ScopeCreator &scopeCreator) {
  scopeCreator.createSubtree2D<ExtensionScope, IterableTypeBodyPortion>(leaf,
                                                                        decl);
}
void NominalTypeScope::createBodyScope(ASTScopeImpl *leaf,
                                       ScopeCreator &scopeCreator) {
  scopeCreator.createSubtree2D<NominalTypeScope, IterableTypeBodyPortion>(leaf,
                                                                          decl);
}

#pragma mark createTrailingWhereClauseScope

ASTScopeImpl *GenericTypeOrExtensionScope::createTrailingWhereClauseScope(
    ASTScopeImpl *parent, ScopeCreator &scopeCreator) {
  return parent;
}

ASTScopeImpl *
ExtensionScope::createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                               ScopeCreator &scopeCreator) {
  return scopeCreator
      .createSubtree2D<ExtensionScope, GenericTypeOrExtensionWherePortion>(
          parent, decl);
}
ASTScopeImpl *
NominalTypeScope::createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                                 ScopeCreator &scopeCreator) {
  return scopeCreator
      .createSubtree2D<NominalTypeScope, GenericTypeOrExtensionWherePortion>(
          parent, decl);
}
ASTScopeImpl *
TypeAliasScope::createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                               ScopeCreator &scopeCreator) {
  return scopeCreator
      .createSubtree2D<TypeAliasScope, GenericTypeOrExtensionWherePortion>(
          parent, decl);
}

#pragma mark misc

ASTScopeImpl *LabeledConditionalStmtScope::createNestedConditionalClauseScopes(
    ScopeCreator &scopeCreator, const Stmt *const afterConds) {
  auto *stmt = getLabeledConditionalStmt();
  ASTScopeImpl *insertionPoint = this;
  for (unsigned i = 0; i < stmt->getCond().size(); ++i) {
    insertionPoint = scopeCreator.createSubtree<ConditionalClauseScope>(
        insertionPoint, stmt, i, afterConds->getStartLoc());
  }
  return insertionPoint;
}

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

bool AbstractPatternEntryScope::isLastEntry() const {
  return patternEntryIndex + 1 == decl->getPatternList().size();
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

bool ASTScopeImpl::isATypeDeclScope() const {
  Decl *const pd = getDecl().getPtrOrNull();
  return pd && (isa<NominalTypeDecl>(pd) || isa<ExtensionDecl>(pd));
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

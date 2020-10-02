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
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/Support/Compiler.h"
#include <algorithm>
#include <unordered_set>

using namespace swift;
using namespace ast_scope;

#pragma mark source range utilities
static bool rangeableIsIgnored(const Decl *d) { return d->isImplicit(); }
static bool rangeableIsIgnored(const Expr *d) {
  return false; // implicit expr may contain closures
}
static bool rangeableIsIgnored(const Stmt *d) {
  return false; // ??
}
static bool rangeableIsIgnored(const ASTNode n) {
  return (n.is<Decl *>() && rangeableIsIgnored(n.get<Decl *>())) ||
         (n.is<Stmt *>() && rangeableIsIgnored(n.get<Stmt *>())) ||
         (n.is<Expr *>() && rangeableIsIgnored(n.get<Expr *>()));
}

template <typename Rangeable>
static SourceRange getRangeableSourceRange(const Rangeable *const p) {
  return p->getSourceRange();
}
static SourceRange getRangeableSourceRange(const ASTNode n) {
  return n.getSourceRange();
}

template <typename Rangeable>
static bool isLocalizable(const Rangeable astElement) {
  return !rangeableIsIgnored(astElement) &&
         getRangeableSourceRange(astElement).isValid();
}

template <typename Rangeable>
static void dumpRangeable(const Rangeable r, llvm::raw_ostream &f) {
  r.dump(f);
}
template <typename Rangeable>
static void dumpRangeable(const Rangeable *r, llvm::raw_ostream &f) {
  r->dump(f);
}
template <typename Rangeable>
static void dumpRangeable(Rangeable *r, llvm::raw_ostream &f) {
  r->dump(f);
}

static void dumpRangeable(const SpecializeAttr *r,
                          llvm::raw_ostream &f) LLVM_ATTRIBUTE_USED;
static void dumpRangeable(const SpecializeAttr *r, llvm::raw_ostream &f) {
  llvm::errs() << "SpecializeAttr\n";
}
static void dumpRangeable(SpecializeAttr *r,
                          llvm::raw_ostream &f) LLVM_ATTRIBUTE_USED;
static void dumpRangeable(SpecializeAttr *r, llvm::raw_ostream &f) {
  llvm::errs() << "SpecializeAttr\n";
}

static void dumpRangeable(const DifferentiableAttr *a,
                          llvm::raw_ostream &f) LLVM_ATTRIBUTE_USED;
static void dumpRangeable(const DifferentiableAttr *a, llvm::raw_ostream &f) {
  llvm::errs() << "DifferentiableAttr\n";
}
static void dumpRangeable(DifferentiableAttr *a,
                          llvm::raw_ostream &f) LLVM_ATTRIBUTE_USED;
static void dumpRangeable(DifferentiableAttr *a, llvm::raw_ostream &f) {
  llvm::errs() << "DifferentiableAttr\n";
}

/// For Debugging
template <typename T>
bool doesRangeableRangeMatch(const T *x, const SourceManager &SM,
                             unsigned start, unsigned end,
                             StringRef file = "") {
  auto const r = getRangeableSourceRange(x);
  if (r.isInvalid())
    return false;
  if (start && SM.getLineAndColumnInBuffer(r.Start).first != start)
    return false;
  if (end && SM.getLineAndColumnInBuffer(r.End).first != end)
    return false;
  if (file.empty())
    return true;
  const auto buf = SM.findBufferContainingLoc(r.Start);
  return SM.getIdentifierForBuffer(buf).endswith(file);
}

#pragma mark end of rangeable

static std::vector<ASTNode> asNodeVector(DeclRange dr) {
  std::vector<ASTNode> nodes;
  llvm::transform(dr, std::back_inserter(nodes),
                  [&](Decl *d) { return ASTNode(d); });
  return nodes;
}

namespace swift {
namespace ast_scope {

namespace {
/// Use me with any ASTNode, Expr*, Decl*, or Stmt*
/// I will yield a void* that is the same, even when given an Expr* and a
/// ClosureExpr* because I take the Expr*, figure its real class, then up
/// cast.
/// Useful for duplicate checking.
class UniquePointerCalculator
    : public ASTVisitor<UniquePointerCalculator, void *, void *, void *, void *,
                        void *, void *> {
public:
  template <typename T> const void *visit(const T *x) {
    return const_cast<T *>(x);
  }

  // Call these only from the superclass
  void *visitDecl(Decl *e) { return e; }
  void *visitStmt(Stmt *e) { return e; }
  void *visitExpr(Expr *e) { return e; }
  void *visitPattern(Pattern *e) { return e; }
  void *visitDeclAttribute(DeclAttribute *e) { return e; }

// Provide default implementations for statements as ASTVisitor does for Exprs
#define STMT(CLASS, PARENT)                                                    \
  void *visit##CLASS##Stmt(CLASS##Stmt *S) { return visitStmt(S); }
#include "swift/AST/StmtNodes.def"

// Provide default implementations for patterns as ASTVisitor does for Exprs
#define PATTERN(CLASS, PARENT)                                                 \
  void *visit##CLASS##Pattern(CLASS##Pattern *S) { return visitPattern(S); }
#include "swift/AST/PatternNodes.def"
};

/// A set that does the right pointer calculation for comparing Decls to
/// DeclContexts, and Exprs
class NodeSet {
  ::llvm::DenseSet<const void *> pointers;

public:
  bool contains(const ASTScopeImpl *const s) {
    if (auto *r = s->getReferrent().getPtrOrNull())
      return pointers.count(r);
    return false; // never exclude a non-checkable scope
  }
  bool insert(const ASTScopeImpl *const s) {
    if (auto *r = s->getReferrent().getPtrOrNull())
      return pointers.insert(r).second;
    return true;
  }
  void erase(const ASTScopeImpl *const s) {
    if (auto *r = s->getReferrent().getPtrOrNull())
      pointers.erase(r);
  }
};
} // namespace

#pragma mark ScopeCreator

class ScopeCreator final {
  friend class ASTSourceFileScope;
  /// For allocating scopes.
  ASTContext &ctx;

public:
  ASTSourceFileScope *const sourceFileScope;
  ASTContext &getASTContext() const { return ctx; }

  /// The AST can have duplicate nodes, and we don't want to create scopes for
  /// those.
  NodeSet scopedNodes;

  ScopeCreator(SourceFile *SF)
      : ctx(SF->getASTContext()),
        sourceFileScope(new (ctx) ASTSourceFileScope(SF, this)) {
    ctx.addDestructorCleanup(scopedNodes);
  }

  ScopeCreator(const ScopeCreator &) = delete;  // ensure no copies
  ScopeCreator(const ScopeCreator &&) = delete; // ensure no moves

  /// Given an array of ASTNodes or Decl pointers, add them
  /// Return the resultant insertionPoint
  ///
  /// \param endLoc The end location for any "scopes until the end" that
  /// we introduce here, such as PatternEntryDeclScope and GuardStmtScope
  ASTScopeImpl *
  addSiblingsToScopeTree(ASTScopeImpl *const insertionPoint,
                         ArrayRef<ASTNode> nodesOrDeclsToAdd,
                         Optional<SourceLoc> endLoc) {
    auto *ip = insertionPoint;
    for (auto nd : nodesOrDeclsToAdd) {
      auto *const newIP =
          addToScopeTreeAndReturnInsertionPoint(nd, ip, endLoc).getPtrOr(ip);
      ip = newIP;
    }
    return ip;
  }

public:
  /// For each of searching, call this unless the insertion point is needed
  void addToScopeTree(ASTNode n, ASTScopeImpl *parent) {
    (void)addToScopeTreeAndReturnInsertionPoint(n, parent, None);
  }
  /// Return new insertion point if the scope was not a duplicate
  /// For ease of searching, don't call unless insertion point is needed
  ///
  /// \param endLoc The end location for any "scopes until the end" that
  /// we introduce here, such as PatternEntryDeclScope and GuardStmtScope
  NullablePtr<ASTScopeImpl>
  addToScopeTreeAndReturnInsertionPoint(ASTNode, ASTScopeImpl *parent,
                                        Optional<SourceLoc> endLoc);

  bool isWorthTryingToCreateScopeFor(ASTNode n) const {
    if (!n)
      return false;
    if (n.is<Expr *>())
      return true;
    // Cannot ignore implicit statements because implict return can contain
    // scopes in the expression, such as closures.
    // But must ignore other implicit statements, e.g. brace statments
    // if they can have no children and no stmt source range.
    // Deal with it in visitBraceStmt
    if (n.is<Stmt *>())
      return true;

    auto *const d = n.get<Decl *>();
    // Implicit nodes may not have source information for name lookup.
    if (!isLocalizable(d))
      return false;

    // Commented out for
    // validation-test/compiler_crashers_fixed/27962-swift-rebindselfinconstructorexpr-getcalledconstructor.swift
    // In that test the invalid PBD -> var decl which contains the desired
    // closure scope
    //    if (const auto *PBD = dyn_cast<PatternBindingDecl>(d))
    //      if (!isLocalizable(PBD))
    //        return false;
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

    // Once we allow invalid PatternBindingDecls (see
    // isWorthTryingToCreateScopeFor), then
    // IDE/complete_property_delegate_attribute.swift fails because we try to
    // expand a member whose source range is backwards.
    (void)SM;
    ASTScopeAssert(d->getStartLoc().isInvalid() ||
                       !SM.isBeforeInBuffer(d->getEndLoc(), d->getStartLoc()),
                   "end-before-start will break tree search via location");
    return true;
  }

  /// Create a new scope of class ChildScope initialized with a ChildElement,
  /// expandScope it,
  /// add it as a child of the receiver, and return the child and the scope to
  /// receive more decls.
  template <typename Scope, typename... Args>
  ASTScopeImpl *constructExpandAndInsertUncheckable(ASTScopeImpl *parent,
                                                    Args... args) {
    ASTScopeAssert(!Scope(args...).getReferrent(),
                   "Not checking for duplicate ASTNode but class supports it");
    return constructExpandAndInsert<Scope>(parent, args...);
  }

  template <typename Scope, typename... Args>
  NullablePtr<ASTScopeImpl>
  ifUniqueConstructExpandAndInsert(ASTScopeImpl *parent, Args... args) {
    Scope dryRun(args...);
    ASTScopeAssert(
        dryRun.getReferrent(),
        "Checking for duplicate ASTNode but class does not support it");
    if (scopedNodes.insert(&dryRun))
      return constructExpandAndInsert<Scope>(parent, args...);
    return nullptr;
  }

private:
  template <typename Scope, typename... Args>
  ASTScopeImpl *constructExpandAndInsert(ASTScopeImpl *parent, Args... args) {
    auto *child = new (ctx) Scope(args...);
    parent->addChild(child, ctx);

    if (auto *ip = child->insertionPointForDeferredExpansion().getPtrOrNull())
      return ip;

    ASTScopeImpl *insertionPoint =
        child->expandAndBeCurrentDetectingRecursion(*this);
    ASTScopeAssert(child->verifyThatThisNodeComeAfterItsPriorSibling(),
                   "Ensure search will work");
    return insertionPoint;
  }

public:
  template <typename Scope, typename PortionClass, typename... Args>
  ASTScopeImpl *constructWithPortionExpandAndInsert(ASTScopeImpl *parent,
                                                    Args... args) {
    const Portion *portion = new (ctx) PortionClass();
    return constructExpandAndInsertUncheckable<Scope>(parent, portion, args...);
  }

  template <typename Scope, typename PortionClass, typename... Args>
  NullablePtr<ASTScopeImpl>
  ifUniqueConstructWithPortionExpandAndInsert(ASTScopeImpl *parent,
                                              Args... args) {
    const Portion *portion = new (ctx) PortionClass();
    return ifUniqueConstructExpandAndInsert<Scope>(parent, portion, args...);
  }

  void addExprToScopeTree(Expr *expr, ASTScopeImpl *parent) {
    // Use the ASTWalker to find buried captures and closures
    ASTScopeAssert(expr,
                 "If looking for closures, must have an expression to search.");

    /// AST walker that finds top-level closures in an expression.
    class ClosureFinder : public ASTWalker {
      ScopeCreator &scopeCreator;
      ASTScopeImpl *parent;

    public:
      ClosureFinder(ScopeCreator &scopeCreator, ASTScopeImpl *parent)
          : scopeCreator(scopeCreator), parent(parent) {}

      std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
        if (auto *closure = dyn_cast<ClosureExpr>(E)) {
          scopeCreator
              .ifUniqueConstructExpandAndInsert<ClosureParametersScope>(
                  parent, closure);
          return {false, E};
        }
        if (auto *capture = dyn_cast<CaptureListExpr>(E)) {
          scopeCreator
              .ifUniqueConstructExpandAndInsert<CaptureListScope>(
                  parent, capture);
          return {false, E};
        }
        return {true, E};
      }
      std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
        if (isa<BraceStmt>(S)) { // closures hidden in here
          return {true, S};
        }
        return {false, S};
      }
      std::pair<bool, Pattern *> walkToPatternPre(Pattern *P) override {
        return {false, P};
      }
      bool walkToDeclPre(Decl *D) override { return false; }
      bool walkToTypeReprPre(TypeRepr *T) override { return false; }
      bool walkToParameterListPre(ParameterList *PL) override { return false; }
    };

    expr->walk(ClosureFinder(*this, parent));
  }

public:
  /// Create the matryoshka nested generic param scopes (if any)
  /// that are subscopes of the receiver. Return
  /// the furthest descendant.
  /// Last GenericParamsScope includes the where clause
  ASTScopeImpl *addNestedGenericParamScopesToTree(Decl *parameterizedDecl,
                                                  GenericParamList *generics,
                                                  ASTScopeImpl *parent) {
    if (!generics)
      return parent;
    auto *s = parent;
    for (unsigned i : indices(generics->getParams()))
      s = ifUniqueConstructExpandAndInsert<GenericParamScope>(
              s, parameterizedDecl, generics, i)
              .getPtrOr(s);
    return s;
  }

  void
  addChildrenForAllLocalizableAccessorsInSourceOrder(AbstractStorageDecl *asd,
                                                     ASTScopeImpl *parent);

  void addChildrenForKnownAttributes(ValueDecl *decl,
                                     ASTScopeImpl *parent);

  /// Add PatternEntryDeclScopes for each pattern binding entry.
  ///
  /// Returns the new insertion point.
  ///
  /// \param endLoc Must be valid iff the pattern binding is in a local
  /// scope, in which case this is the last source location where the
  /// pattern bindings are going to be visible.
  NullablePtr<ASTScopeImpl>
  addPatternBindingToScopeTree(PatternBindingDecl *patternBinding,
                               ASTScopeImpl *parent,
                               Optional<SourceLoc> endLoc);

  /// Remove VarDecls because we'll find them when we expand the
  /// PatternBindingDecls. Remove EnunCases
  /// because they overlap EnumElements and AST includes the elements in the
  /// members.
  std::vector<ASTNode> cull(ArrayRef<ASTNode> input) const {
    // TODO: Investigate whether to move the real EndLoc tracking of
    // SubscriptDecl up into AbstractStorageDecl. May have to cull more.
    std::vector<ASTNode> culled;
    llvm::copy_if(input, std::back_inserter(culled), [&](ASTNode n) {
      ASTScopeAssert(
          !n.isDecl(DeclKind::Accessor),
          "Should not find accessors in iterable types or brace statements");
      return isLocalizable(n) &&
             !n.isDecl(DeclKind::Var) &&
             !n.isDecl(DeclKind::EnumCase) &&
             !n.isDecl(DeclKind::IfConfig);
    });
    return culled;
  }

  /// Templated to work on either ASTNodes, Decl*'s, or whatnot.
  template <typename Rangeable>
  std::vector<Rangeable>
  sortBySourceRange(std::vector<Rangeable> toBeSorted) const {
    auto compareNodes = [&](Rangeable n1, Rangeable n2) {
      return isNotAfter(n1, n2);
    };
    std::stable_sort(toBeSorted.begin(), toBeSorted.end(), compareNodes);
    return toBeSorted;
  }

  template <typename Rangeable>
  bool isNotAfter(Rangeable n1, Rangeable n2) const {
    const auto r1 = getRangeableSourceRange(n1);
    const auto r2 = getRangeableSourceRange(n2);

    const int signum = ASTScopeImpl::compare(r1, r2, ctx.SourceMgr,
                                             /*ensureDisjoint=*/true);
    return -1 == signum;
  }

  SWIFT_DEBUG_DUMP { print(llvm::errs()); }

  void print(raw_ostream &out) const {
    out << "(swift::ASTSourceFileScope*) " << sourceFileScope << "\n";
  }

  // Make vanilla new illegal.
  void *operator new(size_t bytes) = delete;

  // Only allow allocation of scopes using the allocator of a particular source
  // file.
  void *operator new(size_t bytes, const ASTContext &ctx,
                     unsigned alignment = alignof(ScopeCreator));
  void *operator new(size_t Bytes, void *Mem) {
    ASTScopeAssert(Mem, "Allocation failed");
    return Mem;
  }
};
} // ast_scope
} // namespace swift

#pragma mark Scope tree creation and extension

ASTScope::ASTScope(SourceFile *SF) : impl(createScopeTree(SF)) {}

void ASTScope::buildFullyExpandedTree() { impl->buildFullyExpandedTree(); }

void ASTScope::
    buildEnoughOfTreeForTopLevelExpressionsButDontRequestGenericsOrExtendedNominals() {
  impl->buildEnoughOfTreeForTopLevelExpressionsButDontRequestGenericsOrExtendedNominals();
}

void ASTScope::expandFunctionBody(AbstractFunctionDecl *AFD) {
  auto *const SF = AFD->getParentSourceFile();
  SF->getScope().expandFunctionBodyImpl(AFD);
}

void ASTScope::expandFunctionBodyImpl(AbstractFunctionDecl *AFD) {
  impl->expandFunctionBody(AFD);
}

ASTSourceFileScope *ASTScope::createScopeTree(SourceFile *SF) {
  ScopeCreator *scopeCreator = new (SF->getASTContext()) ScopeCreator(SF);
  return scopeCreator->sourceFileScope;
}

void ASTSourceFileScope::buildFullyExpandedTree() {
  expandAndBeCurrentDetectingRecursion(*scopeCreator);
  preOrderChildrenDo([&](ASTScopeImpl *s) {
    s->expandAndBeCurrentDetectingRecursion(*scopeCreator);
  });
}

void ASTSourceFileScope::
    buildEnoughOfTreeForTopLevelExpressionsButDontRequestGenericsOrExtendedNominals() {
      expandAndBeCurrentDetectingRecursion(*scopeCreator);
}

void ASTSourceFileScope::expandFunctionBody(AbstractFunctionDecl *AFD) {
  if (!AFD)
    return;
  auto sr = AFD->getOriginalBodySourceRange();
  if (sr.isInvalid())
    return;
  ASTScopeImpl *bodyScope = findInnermostEnclosingScope(sr.Start, nullptr);
  bodyScope->expandAndBeCurrentDetectingRecursion(*scopeCreator);
}

ASTSourceFileScope::ASTSourceFileScope(SourceFile *SF,
                                       ScopeCreator *scopeCreator)
    : SF(SF), scopeCreator(scopeCreator), insertionPoint(this) {}

#pragma mark NodeAdder

namespace swift {
namespace ast_scope {

class NodeAdder
    : public ASTVisitor<NodeAdder, NullablePtr<ASTScopeImpl>,
                        NullablePtr<ASTScopeImpl>, NullablePtr<ASTScopeImpl>,
                        void, void, void, ASTScopeImpl *, ScopeCreator &> {
  Optional<SourceLoc> endLoc;

public:
  explicit NodeAdder(Optional<SourceLoc> endLoc) : endLoc(endLoc) {}

#pragma mark ASTNodes that do not create scopes

  // Even ignored Decls and Stmts must extend the source range of a scope:
  // E.g. a braceStmt with some definitions that ends in a statement that
  // accesses such a definition must resolve as being IN the scope.

#define VISIT_AND_IGNORE(What)                                                 \
  NullablePtr<ASTScopeImpl> visit##What(What *w, ASTScopeImpl *p,              \
                                        ScopeCreator &) {                      \
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
  VISIT_AND_IGNORE(PoundDiagnosticDecl)
  VISIT_AND_IGNORE(MissingMemberDecl)

  // This declaration is handled from the PatternBindingDecl
  VISIT_AND_IGNORE(VarDecl)

  // These contain nothing to scope.
  VISIT_AND_IGNORE(BreakStmt)
  VISIT_AND_IGNORE(ContinueStmt)
  VISIT_AND_IGNORE(FallthroughStmt)
  VISIT_AND_IGNORE(FailStmt)

#undef VISIT_AND_IGNORE

#pragma mark simple creation ignoring deferred nodes

#define VISIT_AND_CREATE(What, ScopeClass)                                     \
  NullablePtr<ASTScopeImpl> visit##What(What *w, ASTScopeImpl *p,              \
                                        ScopeCreator &scopeCreator) {          \
    return scopeCreator.ifUniqueConstructExpandAndInsert<ScopeClass>(p, w);    \
  }

  VISIT_AND_CREATE(SubscriptDecl, SubscriptDeclScope)
  VISIT_AND_CREATE(IfStmt, IfStmtScope)
  VISIT_AND_CREATE(WhileStmt, WhileStmtScope)
  VISIT_AND_CREATE(RepeatWhileStmt, RepeatWhileScope)
  VISIT_AND_CREATE(DoStmt, DoStmtScope)
  VISIT_AND_CREATE(DoCatchStmt, DoCatchStmtScope)
  VISIT_AND_CREATE(SwitchStmt, SwitchStmtScope)
  VISIT_AND_CREATE(ForEachStmt, ForEachStmtScope)
  VISIT_AND_CREATE(CaseStmt, CaseStmtScope)
  VISIT_AND_CREATE(AbstractFunctionDecl, AbstractFunctionDeclScope)

#undef VISIT_AND_CREATE

#pragma mark 2D simple creation (ignoring deferred nodes)

#define VISIT_AND_CREATE_WHOLE_PORTION(What, WhatScope)                        \
  NullablePtr<ASTScopeImpl> visit##What(What *w, ASTScopeImpl *p,              \
                                        ScopeCreator &scopeCreator) {          \
    return scopeCreator.ifUniqueConstructWithPortionExpandAndInsert<           \
        WhatScope, GenericTypeOrExtensionWholePortion>(p, w);                  \
  }

  VISIT_AND_CREATE_WHOLE_PORTION(ExtensionDecl, ExtensionScope)
  VISIT_AND_CREATE_WHOLE_PORTION(StructDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(ClassDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(ProtocolDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(EnumDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(TypeAliasDecl, TypeAliasScope)
  VISIT_AND_CREATE_WHOLE_PORTION(OpaqueTypeDecl, OpaqueTypeScope)
#undef VISIT_AND_CREATE_WHOLE_PORTION

  // This declaration is handled from
  // addChildrenForAllLocalizableAccessorsInSourceOrder
  NullablePtr<ASTScopeImpl> visitAccessorDecl(AccessorDecl *ad, ASTScopeImpl *p,
                                              ScopeCreator &scopeCreator) {
    return visitAbstractFunctionDecl(ad, p, scopeCreator);
  }

#pragma mark simple creation with deferred nodes

  // Each of the following creates a new scope, so that nodes which were parsed
  // after them need to be placed in scopes BELOW them in the tree. So pass down
  // the deferred nodes.
  NullablePtr<ASTScopeImpl> visitGuardStmt(GuardStmt *e, ASTScopeImpl *p,
                                           ScopeCreator &scopeCreator) {
    ASTScopeAssert(endLoc.hasValue(), "GuardStmt outside of a BraceStmt?");
    return scopeCreator.ifUniqueConstructExpandAndInsert<GuardStmtScope>(
      p, e, *endLoc);
  }
  NullablePtr<ASTScopeImpl> visitTopLevelCodeDecl(TopLevelCodeDecl *d,
                                                  ASTScopeImpl *p,
                                                  ScopeCreator &scopeCreator) {
    return scopeCreator.ifUniqueConstructExpandAndInsert<TopLevelCodeScope>(p,
                                                                            d);
  }

#pragma mark special-case creation

  ASTScopeImpl *visitSourceFile(SourceFile *, ASTScopeImpl *, ScopeCreator &) {
    ASTScope_unreachable("SourceFiles are orphans.");
  }

  NullablePtr<ASTScopeImpl> visitYieldStmt(YieldStmt *ys, ASTScopeImpl *p,
                                           ScopeCreator &scopeCreator) {
    for (Expr *e : ys->getYields())
      visitExpr(e, p, scopeCreator);
    return p;
  }

  NullablePtr<ASTScopeImpl> visitDeferStmt(DeferStmt *ds, ASTScopeImpl *p,
                                           ScopeCreator &scopeCreator) {
    visitFuncDecl(ds->getTempDecl(), p, scopeCreator);
    return p;
  }

  NullablePtr<ASTScopeImpl> visitBraceStmt(BraceStmt *bs, ASTScopeImpl *p,
                                           ScopeCreator &scopeCreator) {
    SmallVector<ValueDecl *, 2> localFuncsAndTypes;
    SmallVector<VarDecl *, 2> localVars;

    // All types and functions are visible anywhere within a brace statement
    // scope. When ordering matters (i.e. var decl) we will have split the brace
    // statement into nested scopes.
    for (auto braceElement : bs->getElements()) {
      if (auto localBinding = braceElement.dyn_cast<Decl *>()) {
        if (auto *vd = dyn_cast<ValueDecl>(localBinding)) {
          if (isa<FuncDecl>(vd)  || isa<TypeDecl>(vd)) {
            localFuncsAndTypes.push_back(vd);
          } else if (auto *var = dyn_cast<VarDecl>(localBinding)) {
            localVars.push_back(var);
          }
        }
      }
    }

    auto maybeBraceScope =
        scopeCreator.ifUniqueConstructExpandAndInsert<BraceStmtScope>(
            p, bs, std::move(localFuncsAndTypes), std::move(localVars));
    if (auto *s = scopeCreator.getASTContext().Stats)
      ++s->getFrontendCounters().NumBraceStmtASTScopes;

    return maybeBraceScope.getPtrOr(p);
  }

  NullablePtr<ASTScopeImpl>
  visitPatternBindingDecl(PatternBindingDecl *patternBinding,
                          ASTScopeImpl *parentScope,
                          ScopeCreator &scopeCreator) {
    return scopeCreator.addPatternBindingToScopeTree(
        patternBinding, parentScope, endLoc);
  }

  NullablePtr<ASTScopeImpl> visitEnumElementDecl(EnumElementDecl *eed,
                                                 ASTScopeImpl *p,
                                                 ScopeCreator &scopeCreator) {
    scopeCreator.constructExpandAndInsertUncheckable<EnumElementScope>(p, eed);
    return p;
  }

  NullablePtr<ASTScopeImpl> visitIfConfigDecl(IfConfigDecl *icd,
                                              ASTScopeImpl *p,
                                              ScopeCreator &scopeCreator) {
    ASTScope_unreachable(
        "Should be handled inside of "
        "expandIfConfigClausesThenCullAndSortElementsOrMembers");
  }

  NullablePtr<ASTScopeImpl> visitReturnStmt(ReturnStmt *rs, ASTScopeImpl *p,
                                            ScopeCreator &scopeCreator) {
    if (rs->hasResult())
      visitExpr(rs->getResult(), p, scopeCreator);
    return p;
  }

  NullablePtr<ASTScopeImpl> visitThrowStmt(ThrowStmt *ts, ASTScopeImpl *p,
                                           ScopeCreator &scopeCreator) {
    visitExpr(ts->getSubExpr(), p, scopeCreator);
    return p;
  }

  NullablePtr<ASTScopeImpl> visitPoundAssertStmt(PoundAssertStmt *pas,
                                                 ASTScopeImpl *p,
                                                 ScopeCreator &scopeCreator) {
    visitExpr(pas->getCondition(), p, scopeCreator);
    return p;
  }

  NullablePtr<ASTScopeImpl> visitExpr(Expr *expr, ASTScopeImpl *p,
                                      ScopeCreator &scopeCreator) {
    if (expr) {
      p->widenSourceRangeForIgnoredASTNode(expr);
      scopeCreator.addExprToScopeTree(expr, p);
    }
    return p;
  }
};
} // namespace ast_scope
} // namespace swift

// These definitions are way down here so it can call into
// NodeAdder
NullablePtr<ASTScopeImpl>
ScopeCreator::addToScopeTreeAndReturnInsertionPoint(ASTNode n,
                                                    ASTScopeImpl *parent,
                                                    Optional<SourceLoc> endLoc) {
  if (!isWorthTryingToCreateScopeFor(n))
    return parent;

  NodeAdder adder(endLoc);
  if (auto *p = n.dyn_cast<Decl *>())
    return adder.visit(p, parent, *this);
  if (auto *p = n.dyn_cast<Expr *>())
    return adder.visit(p, parent, *this);
  auto *p = n.get<Stmt *>();
  return adder.visit(p, parent, *this);
}

void ScopeCreator::addChildrenForAllLocalizableAccessorsInSourceOrder(
    AbstractStorageDecl *asd, ASTScopeImpl *parent) {
  asd->visitParsedAccessors([&](AccessorDecl *ad) {
    assert(asd == ad->getStorage());
    this->addToScopeTree(ad, parent);
  });
}

void ScopeCreator::addChildrenForKnownAttributes(ValueDecl *decl,
                                                 ASTScopeImpl *parent) {
  SmallVector<DeclAttribute *, 2> relevantAttrs;

  for (auto *attr : decl->getAttrs()) {
    if (isa<DifferentiableAttr>(attr)) {
      if (!attr->isImplicit())
        relevantAttrs.push_back(attr);
    }

    if (isa<SpecializeAttr>(attr))
      relevantAttrs.push_back(attr);

    if (isa<CustomAttr>(attr))
      relevantAttrs.push_back(attr);
  }

  // Decl::getAttrs() is a linked list with head insertion, so the
  // attributes are in reverse source order.
  std::reverse(relevantAttrs.begin(), relevantAttrs.end());

  for (auto *attr : relevantAttrs) {
    if (auto *diffAttr = dyn_cast<DifferentiableAttr>(attr)) {
      ifUniqueConstructExpandAndInsert<DifferentiableAttributeScope>(
          parent, diffAttr, decl);
    } else if (auto *specAttr = dyn_cast<SpecializeAttr>(attr)) {
      if (auto *afd = dyn_cast<AbstractFunctionDecl>(decl)) {
        ifUniqueConstructExpandAndInsert<SpecializeAttributeScope>(
            parent, specAttr, afd);
      }
    } else if (auto *customAttr = dyn_cast<CustomAttr>(attr)) {
      if (auto *vd = dyn_cast<VarDecl>(decl)) {
        ifUniqueConstructExpandAndInsert<AttachedPropertyWrapperScope>(
            parent, customAttr, vd);
      }
    }
  }
}

NullablePtr<ASTScopeImpl>
ScopeCreator::addPatternBindingToScopeTree(PatternBindingDecl *patternBinding,
                                           ASTScopeImpl *parentScope,
                                           Optional<SourceLoc> endLoc) {
  if (auto *var = patternBinding->getSingleVar())
    addChildrenForKnownAttributes(var, parentScope);

  auto *insertionPoint = parentScope;
  for (auto i : range(patternBinding->getNumPatternEntries())) {
    bool isLocalBinding = false;
    if (auto *varDecl = patternBinding->getAnchoringVarDecl(i)) {
      isLocalBinding = varDecl->getDeclContext()->isLocalContext();
    }

    Optional<SourceLoc> endLocForBinding = None;
    if (isLocalBinding) {
      endLocForBinding = endLoc;
      ASTScopeAssert(endLoc.hasValue() && endLoc->isValid(),
                     "PatternBindingDecl in local context outside of BraceStmt?");
    }

    insertionPoint =
        ifUniqueConstructExpandAndInsert<PatternEntryDeclScope>(
            insertionPoint, patternBinding, i,
            isLocalBinding, endLocForBinding)
        .getPtrOr(insertionPoint);

    ASTScopeAssert(isLocalBinding || insertionPoint == parentScope,
                   "Bindings at the top-level or members of types should "
                   "not change the insertion point");
  }

  return insertionPoint;
}

#pragma mark creation helpers

void ASTScopeImpl::addChild(ASTScopeImpl *child, ASTContext &ctx) {
  // If this is the first time we've added children, notify the ASTContext
  // that there's a SmallVector that needs to be cleaned up.
  // FIXME: If we had access to SmallVector::isSmall(), we could do better.
  if (storedChildren.empty() && !haveAddedCleanup) {
    ctx.addDestructorCleanup(storedChildren);
    haveAddedCleanup = true;
  }
  storedChildren.push_back(child);
  ASTScopeAssert(!child->getParent(), "child should not already have parent");
  child->parent = this;
  clearCachedSourceRangesOfMeAndAncestors();
}

#pragma mark implementations of expansion

ASTScopeImpl *
ASTScopeImpl::expandAndBeCurrentDetectingRecursion(ScopeCreator &scopeCreator) {
  return evaluateOrDefault(scopeCreator.getASTContext().evaluator,
                           ExpandASTScopeRequest{this, &scopeCreator}, nullptr);
}

ASTScopeImpl *
ExpandASTScopeRequest::evaluate(Evaluator &evaluator, ASTScopeImpl *parent,
                                ScopeCreator *scopeCreator) const {
  auto *insertionPoint = parent->expandAndBeCurrent(*scopeCreator);
  ASTScopeAssert(insertionPoint,
                 "Used to return a null pointer if the insertion point would "
                 "not be used, but it breaks the request dependency hashing");
  return insertionPoint;
}

ASTScopeImpl *ASTScopeImpl::expandAndBeCurrent(ScopeCreator &scopeCreator) {
  ASTScopeAssert(!getWasExpanded(),
                 "Cannot expand the same scope twice");

  auto *insertionPoint = expandSpecifically(scopeCreator);
  ASTScopeAssert(!insertionPointForDeferredExpansion() ||
                     insertionPointForDeferredExpansion().get() ==
                         insertionPoint,
                 "In order for lookups into lazily-expanded scopes to be "
                 "accurate before expansion, the insertion point before "
                 "expansion must be the same as after expansion.");

  setWasExpanded();

  ASTScopeAssert(checkSourceRangeAfterExpansion(scopeCreator.getASTContext()),
                 "Bad range.");
  return insertionPoint;
}

  // Do this whole bit so it's easy to see which type of scope is which

#define CREATES_NEW_INSERTION_POINT(Scope)                                     \
  ASTScopeImpl *Scope::expandSpecifically(ScopeCreator &scopeCreator) {        \
    return expandAScopeThatCreatesANewInsertionPoint(scopeCreator)             \
        .insertionPoint;                                                       \
  }

#define NO_NEW_INSERTION_POINT(Scope)                                          \
  ASTScopeImpl *Scope::expandSpecifically(ScopeCreator &scopeCreator) {        \
    expandAScopeThatDoesNotCreateANewInsertionPoint(scopeCreator);             \
    return getParent().get();                                                  \
  }

// Return this in particular for GenericParamScope so body is scoped under it
#define NO_EXPANSION(Scope)                                                    \
  ASTScopeImpl *Scope::expandSpecifically(ScopeCreator &) { return this; }

CREATES_NEW_INSERTION_POINT(ASTSourceFileScope)
CREATES_NEW_INSERTION_POINT(ConditionalClauseScope)
CREATES_NEW_INSERTION_POINT(GuardStmtScope)
CREATES_NEW_INSERTION_POINT(PatternEntryDeclScope)
CREATES_NEW_INSERTION_POINT(GenericTypeOrExtensionScope)
CREATES_NEW_INSERTION_POINT(BraceStmtScope)
CREATES_NEW_INSERTION_POINT(TopLevelCodeScope)

NO_NEW_INSERTION_POINT(FunctionBodyScope)
NO_NEW_INSERTION_POINT(AbstractFunctionDeclScope)
NO_NEW_INSERTION_POINT(AttachedPropertyWrapperScope)
NO_NEW_INSERTION_POINT(EnumElementScope)
NO_NEW_INSERTION_POINT(ParameterListScope)
NO_NEW_INSERTION_POINT(PatternEntryInitializerScope)

NO_NEW_INSERTION_POINT(CaptureListScope)
NO_NEW_INSERTION_POINT(CaseStmtScope)
NO_NEW_INSERTION_POINT(CaseLabelItemScope)
NO_NEW_INSERTION_POINT(CaseStmtBodyScope)
NO_NEW_INSERTION_POINT(ClosureParametersScope)
NO_NEW_INSERTION_POINT(DefaultArgumentInitializerScope)
NO_NEW_INSERTION_POINT(DoStmtScope)
NO_NEW_INSERTION_POINT(DoCatchStmtScope)
NO_NEW_INSERTION_POINT(ForEachPatternScope)
NO_NEW_INSERTION_POINT(ForEachStmtScope)
NO_NEW_INSERTION_POINT(IfStmtScope)
NO_NEW_INSERTION_POINT(RepeatWhileScope)
NO_NEW_INSERTION_POINT(SubscriptDeclScope)
NO_NEW_INSERTION_POINT(SwitchStmtScope)
NO_NEW_INSERTION_POINT(WhileStmtScope)

NO_EXPANSION(GenericParamScope)
NO_EXPANSION(SpecializeAttributeScope)
NO_EXPANSION(DifferentiableAttributeScope)
NO_EXPANSION(ConditionalClausePatternUseScope)
NO_EXPANSION(LookupParentDiversionScope)

#undef CREATES_NEW_INSERTION_POINT
#undef NO_NEW_INSERTION_POINT

AnnotatedInsertionPoint
ASTSourceFileScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  ASTScopeAssert(SF, "Must already have a SourceFile.");
  ArrayRef<Decl *> decls = SF->getTopLevelDecls();
  // Assume that decls are only added at the end, in source order
  std::vector<ASTNode> newNodes(decls.begin(), decls.end());
  insertionPoint =
      scopeCreator.addSiblingsToScopeTree(insertionPoint,
                                          scopeCreator.sortBySourceRange(
                                            scopeCreator.cull(newNodes)),
                                          None);
  // Too slow to perform all the time:
  //    ASTScopeAssert(scopeCreator->containsAllDeclContextsFromAST(),
  //           "ASTScope tree missed some DeclContexts or made some up");
  return {insertionPoint, "Next time decls are added they go here."};
}

void
ParameterListScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // Each initializer for a function parameter is its own, sibling, scope.
  // Unlike generic parameters or pattern initializers, it cannot refer to a
  // previous parameter.
  for (ParamDecl *pd : params->getArray()) {
    if (pd->hasDefaultExpr())
      scopeCreator
          .constructExpandAndInsertUncheckable<DefaultArgumentInitializerScope>(
              this, pd);
  }
}

AnnotatedInsertionPoint
PatternEntryDeclScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // Initializers come before VarDecls, e.g. PCMacro/didSet.swift 19
  auto patternEntry = getPatternEntry();

  // Create a child for the initializer, if present.
  // Cannot trust the source range given in the ASTScopeImpl for the end of the
  // initializer (because of InterpolatedLiteralStrings and EditorPlaceHolders),
  // so compute it ourselves.
  // Even if this predicate fails, there may be an initContext but
  // we cannot make a scope for it, since no source range.
  if (patternEntry.getOriginalInit() &&
      isLocalizable(patternEntry.getOriginalInit())) {
    ASTScopeAssert(
        !getSourceManager().isBeforeInBuffer(
            patternEntry.getOriginalInit()->getStartLoc(), decl->getStartLoc()),
        "Original inits are always after the '='");
    scopeCreator
        .constructExpandAndInsertUncheckable<PatternEntryInitializerScope>(
            this, decl, patternEntryIndex);
  }

  // Add accessors for the variables in this pattern.
  patternEntry.getPattern()->forEachVariable([&](VarDecl *var) {
    scopeCreator.addChildrenForAllLocalizableAccessorsInSourceOrder(var, this);
  });

  // In local context, the PatternEntryDeclScope becomes the insertion point, so
  // that all any bindings introduecd by the pattern are in scope for subsequent
  // lookups.
  if (isLocalBinding)
    return {this, "All code that follows is inside this scope"};

  return {getParent().get(), "Global and type members do not introduce scopes"};
}

void
PatternEntryInitializerScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // Create a child for the initializer expression.
  scopeCreator.addToScopeTree(ASTNode(getPatternEntry().getOriginalInit()),
                              this);
}

AnnotatedInsertionPoint
ConditionalClauseScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  const StmtConditionElement &sec = getStmtConditionElement();
  switch (sec.getKind()) {
  case StmtConditionElement::CK_Availability:
    return {this, "No introduced variables"};
  case StmtConditionElement::CK_Boolean:
    scopeCreator.addToScopeTree(sec.getBoolean(), this);
    return {this, "No introduced variables"};
  case StmtConditionElement::CK_PatternBinding:
    scopeCreator.addToScopeTree(sec.getInitializer(), this);
    auto *const ccPatternUseScope =
        scopeCreator.constructExpandAndInsertUncheckable<
            ConditionalClausePatternUseScope>(this, sec.getPattern(), endLoc);
    return {ccPatternUseScope,
            "Succeeding code must be in scope of conditional variables"};
  }
  ASTScope_unreachable("Unhandled StmtConditionKind in switch");
}

AnnotatedInsertionPoint
GuardStmtScope::expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &
                                                          scopeCreator) {

  ASTScopeImpl *conditionLookupParent =
      createNestedConditionalClauseScopes(scopeCreator, stmt->getBody());
  // Add a child for the 'guard' body, which always exits.
  // Parent is whole guard stmt scope, NOT the cond scopes
  scopeCreator.addToScopeTree(stmt->getBody(), this);

  auto *const lookupParentDiversionScope =
      scopeCreator
          .constructExpandAndInsertUncheckable<LookupParentDiversionScope>(
              this, conditionLookupParent, stmt->getEndLoc(), endLoc);
  return {lookupParentDiversionScope,
          "Succeeding code must be in scope of guard variables"};
}

AnnotatedInsertionPoint
GenericTypeOrExtensionScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator & scopeCreator) {
  return {portion->expandScope(this, scopeCreator),
          "<X: Foo, Y: X> is legal, so nest these"};
}

AnnotatedInsertionPoint
BraceStmtScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // TODO: remove the sort after fixing parser to create brace statement
  // elements in source order
  auto *insertionPoint =
      scopeCreator.addSiblingsToScopeTree(this,
                                          scopeCreator.sortBySourceRange(
                                            scopeCreator.cull(
                                              stmt->getElements())),
                                          stmt->getEndLoc());
  if (auto *s = scopeCreator.getASTContext().Stats)
    ++s->getFrontendCounters().NumBraceStmtASTScopeExpansions;
  return {
      insertionPoint,
      "For top-level code decls, need the scope under, say a guard statment."};
}

AnnotatedInsertionPoint
TopLevelCodeScope::expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &
                                                             scopeCreator) {

  if (auto *body =
          scopeCreator
              .addToScopeTreeAndReturnInsertionPoint(decl->getBody(), this, None)
              .getPtrOrNull())
    return {body, "So next top level code scope and put its decls in its body "
                  "under a guard statement scope (etc) from the last top level "
                  "code scope"};
  return {this, "No body"};
}

#pragma mark expandAScopeThatDoesNotCreateANewInsertionPoint

// Create child scopes for every declaration in a body.

void AbstractFunctionDeclScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addChildrenForKnownAttributes(decl, this);

  // Create scopes for generic and ordinary parameters.
  // For a subscript declaration, the generic and ordinary parameters are in an
  // ancestor scope, so don't make them here.
  ASTScopeImpl *leaf = this;

  if (!isa<AccessorDecl>(decl)) {
    leaf = scopeCreator.addNestedGenericParamScopesToTree(
        decl, decl->getGenericParams(), leaf);

    auto *params = decl->getParameters();
    if (params->size() > 0) {
      scopeCreator.constructExpandAndInsertUncheckable<ParameterListScope>(
          leaf, params, nullptr);
    }
  }

  // Create scope for the body.
  // We create body scopes when there is no body for source kit to complete
  // erroneous code in bodies.
  if (decl->getBodySourceRange().isValid()) {
    scopeCreator.constructExpandAndInsertUncheckable<FunctionBodyScope>(leaf,
                                                                        decl);
  }
}

void EnumElementScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  if (auto *pl = decl->getParameterList())
    scopeCreator.constructExpandAndInsertUncheckable<ParameterListScope>(
        this, pl, nullptr);
  // The invariant that the raw value expression can never introduce a new scope
  // is checked in Parse.  However, this guarantee is not future-proof.  Compute
  // and add the raw value expression anyways just to be defensive.
  //
  // FIXME: Re-enable this.  It currently crashes for malformed enum cases.
  // scopeCreator.addToScopeTree(decl->getStructuralRawValueExpr(), this);
}

void FunctionBodyScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  expandBody(scopeCreator);
}

void IfStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  ASTScopeImpl *insertionPoint =
      createNestedConditionalClauseScopes(scopeCreator, stmt->getThenStmt());

  // The 'then' branch
  scopeCreator.addToScopeTree(stmt->getThenStmt(), insertionPoint);

  // Add the 'else' branch, if needed.
  scopeCreator.addToScopeTree(stmt->getElseStmt(), this);
}

void WhileStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  ASTScopeImpl *insertionPoint =
      createNestedConditionalClauseScopes(scopeCreator, stmt->getBody());
  scopeCreator.addToScopeTree(stmt->getBody(), insertionPoint);
}

void RepeatWhileScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addToScopeTree(stmt->getBody(), this);
  scopeCreator.addToScopeTree(stmt->getCond(), this);
}

void DoStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addToScopeTree(stmt->getBody(), this);
}

void DoCatchStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addToScopeTree(stmt->getBody(), this);

  for (auto catchClause : stmt->getCatches())
    scopeCreator.addToScopeTree(catchClause, this);
}

void SwitchStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addToScopeTree(stmt->getSubjectExpr(), this);

  for (auto caseStmt : stmt->getCases()) {
    if (isLocalizable(caseStmt))
      scopeCreator.ifUniqueConstructExpandAndInsert<CaseStmtScope>(this,
                                                                   caseStmt);
  }
}

void ForEachStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addToScopeTree(stmt->getSequence(), this);

  // Add a child describing the scope of the pattern.
  // In error cases such as:
  //    let v: C { for b : Int -> S((array: P { }
  // the body is implicit and it would overlap the source range of the expr
  // above.
  if (!stmt->getBody()->isImplicit()) {
    if (isLocalizable(stmt->getBody()))
      scopeCreator.constructExpandAndInsertUncheckable<ForEachPatternScope>(
          this, stmt);
  }
}

void ForEachPatternScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addToScopeTree(stmt->getWhere(), this);
  scopeCreator.addToScopeTree(stmt->getBody(), this);
}

void CaseStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  for (auto &item : stmt->getCaseLabelItems()) {
    if (item.getGuardExpr()) {
      scopeCreator.constructExpandAndInsertUncheckable<CaseLabelItemScope>(
        this, item);
    }
  }

  scopeCreator.constructExpandAndInsertUncheckable<CaseStmtBodyScope>(
      this, stmt);
}

void CaseLabelItemScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addToScopeTree(item.getGuardExpr(), this);
}

void CaseStmtBodyScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addToScopeTree(stmt->getBody(), this);
}

void SubscriptDeclScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addChildrenForKnownAttributes(decl, this);
  auto *leaf = scopeCreator.addNestedGenericParamScopesToTree(
      decl, decl->getGenericParams(), this);
  scopeCreator.constructExpandAndInsertUncheckable<ParameterListScope>(
      leaf, decl->getIndices(), decl->getAccessor(AccessorKind::Get));
  scopeCreator.addChildrenForAllLocalizableAccessorsInSourceOrder(decl, leaf);
}

void CaptureListScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  auto *closureExpr = expr->getClosureBody();
  scopeCreator
      .ifUniqueConstructExpandAndInsert<ClosureParametersScope>(
          this, closureExpr);
}

void ClosureParametersScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  scopeCreator.addToScopeTree(closureExpr->getBody(), this);
}

void DefaultArgumentInitializerScope::
    expandAScopeThatDoesNotCreateANewInsertionPoint(
        ScopeCreator &scopeCreator) {
  auto *initExpr = decl->getStructuralDefaultExpr();
  ASTScopeAssert(initExpr,
                 "Default argument initializer must have an initializer.");
  scopeCreator.addToScopeTree(initExpr, this);
}

void AttachedPropertyWrapperScope::
    expandAScopeThatDoesNotCreateANewInsertionPoint(
        ScopeCreator &scopeCreator) {
  if (auto *expr = attr->getArg())
      scopeCreator.addToScopeTree(expr, this);
}

#pragma mark expandScope

ASTScopeImpl *GenericTypeOrExtensionWholePortion::expandScope(
    GenericTypeOrExtensionScope *scope, ScopeCreator &scopeCreator) const {
  // Get now in case recursion emancipates scope
  auto *const ip = scope->getParent().get();
  
  auto *context = scope->getGenericContext();
  auto *genericParams = (isa<TypeAliasDecl>(context)
                         ? context->getParsedGenericParams()
                         : context->getGenericParams());
  auto *deepestScope = scopeCreator.addNestedGenericParamScopesToTree(
      scope->getDecl(), genericParams, scope);
  if (context->getTrailingWhereClause())
    scope->createTrailingWhereClauseScope(deepestScope, scopeCreator);

  // Prevent circular request bugs caused by illegal input and
  // doing lookups that getExtendedNominal in the midst of getExtendedNominal.
  if (scope->shouldHaveABody() && !scope->doesDeclHaveABody())
    return ip;

  scope->createBodyScope(deepestScope, scopeCreator);
  return ip;
}

ASTScopeImpl *
IterableTypeBodyPortion::expandScope(GenericTypeOrExtensionScope *scope,
                                     ScopeCreator &scopeCreator) const {
  // Get it now in case of recursion and this one gets emancipated
  auto *const ip = scope->getParent().get();
  scope->expandBody(scopeCreator);
  return ip;
}

ASTScopeImpl *GenericTypeOrExtensionWherePortion::expandScope(
    GenericTypeOrExtensionScope *scope, ScopeCreator &) const {
  return scope->getParent().get();
}

#pragma mark createBodyScope

void IterableTypeScope::countBodies(ScopeCreator &scopeCreator) const {
  if (auto *s = scopeCreator.getASTContext().Stats)
    ++s->getFrontendCounters().NumIterableTypeBodyASTScopes;
}

void ExtensionScope::createBodyScope(ASTScopeImpl *leaf,
                                     ScopeCreator &scopeCreator) {
  scopeCreator.constructWithPortionExpandAndInsert<ExtensionScope,
                                                   IterableTypeBodyPortion>(
      leaf, decl);
  countBodies(scopeCreator);
}
void NominalTypeScope::createBodyScope(ASTScopeImpl *leaf,
                                       ScopeCreator &scopeCreator) {
  scopeCreator.constructWithPortionExpandAndInsert<NominalTypeScope,
                                                   IterableTypeBodyPortion>(
      leaf, decl);
  countBodies(scopeCreator);
}

#pragma mark createTrailingWhereClauseScope

ASTScopeImpl *GenericTypeOrExtensionScope::createTrailingWhereClauseScope(
    ASTScopeImpl *parent, ScopeCreator &scopeCreator) {
  return parent;
}

ASTScopeImpl *
ExtensionScope::createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                               ScopeCreator &scopeCreator) {
  return scopeCreator.constructWithPortionExpandAndInsert<
      ExtensionScope, GenericTypeOrExtensionWherePortion>(parent, decl);
}
ASTScopeImpl *
NominalTypeScope::createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                                 ScopeCreator &scopeCreator) {
  return scopeCreator.constructWithPortionExpandAndInsert<
      NominalTypeScope, GenericTypeOrExtensionWherePortion>(parent, decl);
}
ASTScopeImpl *
TypeAliasScope::createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                               ScopeCreator &scopeCreator) {
  return scopeCreator.constructWithPortionExpandAndInsert<
      TypeAliasScope, GenericTypeOrExtensionWherePortion>(parent, decl);
}

#pragma mark misc

ASTScopeImpl *LabeledConditionalStmtScope::createNestedConditionalClauseScopes(
    ScopeCreator &scopeCreator, const Stmt *const afterConds) {
  auto *stmt = getLabeledConditionalStmt();
  ASTScopeImpl *insertionPoint = this;
  for (unsigned i = 0; i < stmt->getCond().size(); ++i) {
    insertionPoint =
        scopeCreator
            .constructExpandAndInsertUncheckable<ConditionalClauseScope>(
                insertionPoint, stmt, i, afterConds->getStartLoc());
  }
  return insertionPoint;
}

AbstractPatternEntryScope::AbstractPatternEntryScope(
    PatternBindingDecl *declBeingScoped, unsigned entryIndex)
    : decl(declBeingScoped), patternEntryIndex(entryIndex) {
  ASTScopeAssert(entryIndex < declBeingScoped->getPatternList().size(),
                 "out of bounds");
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

#pragma mark - expandBody

void FunctionBodyScope::expandBody(ScopeCreator &scopeCreator) {
  scopeCreator.addToScopeTree(decl->getBody(), this);
}

void GenericTypeOrExtensionScope::expandBody(ScopeCreator &) {}

void IterableTypeScope::expandBody(ScopeCreator &scopeCreator) {
  auto nodes = asNodeVector(getIterableDeclContext().get()->getMembers());
  nodes = scopeCreator.sortBySourceRange(scopeCreator.cull(nodes));
  scopeCreator.addSiblingsToScopeTree(this, nodes, None);
  if (auto *s = scopeCreator.getASTContext().Stats)
    ++s->getFrontendCounters().NumIterableTypeBodyASTScopeExpansions;
}

#pragma mark getScopeCreator
ScopeCreator &ASTScopeImpl::getScopeCreator() {
  return getParent().get()->getScopeCreator();
}

ScopeCreator &ASTSourceFileScope::getScopeCreator() { return *scopeCreator; }

#pragma mark getReferrent

  // These are the scopes whose ASTNodes (etc) might be duplicated in the AST
  // getReferrent is the cookie used to dedup them

#define GET_REFERRENT(Scope, x)                                                \
  NullablePtr<const void> Scope::getReferrent() const {                        \
    return UniquePointerCalculator().visit(x);                                 \
  }

GET_REFERRENT(AbstractFunctionDeclScope, getDecl())
// If the PatternBindingDecl is a dup, detect it for the first
// PatternEntryDeclScope; the others are subscopes.
GET_REFERRENT(PatternEntryDeclScope, getPattern())
GET_REFERRENT(TopLevelCodeScope, getDecl())
GET_REFERRENT(SubscriptDeclScope, getDecl())
GET_REFERRENT(GenericParamScope, paramList->getParams()[index])
GET_REFERRENT(AbstractStmtScope, getStmt())
GET_REFERRENT(CaptureListScope, getExpr())
GET_REFERRENT(ClosureParametersScope, getExpr())
GET_REFERRENT(SpecializeAttributeScope, specializeAttr)
GET_REFERRENT(DifferentiableAttributeScope, differentiableAttr)
GET_REFERRENT(AttachedPropertyWrapperScope, attr)
GET_REFERRENT(GenericTypeOrExtensionScope, portion->getReferrentOfScope(this));

const Decl *
Portion::getReferrentOfScope(const GenericTypeOrExtensionScope *s) const {
  return nullptr;
};

const Decl *GenericTypeOrExtensionWholePortion::getReferrentOfScope(
    const GenericTypeOrExtensionScope *s) const {
  return s->getDecl();
};

#undef GET_REFERRENT

#pragma mark currency
NullablePtr<ASTScopeImpl> ASTScopeImpl::insertionPointForDeferredExpansion() {
  return nullptr;
}

NullablePtr<ASTScopeImpl>
FunctionBodyScope::insertionPointForDeferredExpansion() {
  return getParent().get();
}

NullablePtr<ASTScopeImpl>
IterableTypeScope::insertionPointForDeferredExpansion() {
  return portion->insertionPointForDeferredExpansion(this);
}

NullablePtr<ASTScopeImpl>
GenericTypeOrExtensionWholePortion::insertionPointForDeferredExpansion(
    IterableTypeScope *s) const {
  return s->getParent().get();
}
NullablePtr<ASTScopeImpl>
GenericTypeOrExtensionWherePortion::insertionPointForDeferredExpansion(
    IterableTypeScope *) const {
  return nullptr;
}
NullablePtr<ASTScopeImpl>
IterableTypeBodyPortion::insertionPointForDeferredExpansion(
    IterableTypeScope *s) const {
  return s->getParent().get();
}

#pragma mark verification

void ast_scope::simple_display(llvm::raw_ostream &out,
                               const ScopeCreator *scopeCreator) {
  scopeCreator->print(out);
}

//----------------------------------------------------------------------------//
// ExpandASTScopeRequest computation.
//----------------------------------------------------------------------------//

bool ExpandASTScopeRequest::isCached() const {
  ASTScopeImpl *scope = std::get<0>(getStorage());
  return scope->getWasExpanded();
}

Optional<ASTScopeImpl *> ExpandASTScopeRequest::getCachedResult() const {
  return std::get<0>(getStorage());
}

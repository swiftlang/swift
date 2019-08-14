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
  return n.is<Decl *>() && n.get<Decl *>()->isImplicit();
}

template <typename Rangeable>
static SourceRange getRangeableSourceRange(const Rangeable *const p) {
  return p->getSourceRange();
}
static SourceRange getRangeableSourceRange(const SpecializeAttr *a) {
  return a->getRange();
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
/// ClosureExor* because I take the Expr*, figure its real class, then up
/// cast.
class PtrCalc : public ASTVisitor<PtrCalc, void *, void *, void *, void *,
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
        sourceFileScope(new (ctx) ASTSourceFileScope(SF, this)) {}

  ScopeCreator(const ScopeCreator &) = delete;  // ensure no copies
  ScopeCreator(const ScopeCreator &&) = delete; // ensure no moves

  /// Given an array of ASTNodes or Decl pointers, add them
  /// Return the resultant insertionPoint
  ASTScopeImpl *addNodesToTree(ASTScopeImpl *const insertionPoint,
                               ArrayRef<ASTNode> nodesOrDeclsToAdd) {
    auto *ip = insertionPoint;
    for (auto nd :
         expandClausesSortAndCullElementsOrMembers(nodesOrDeclsToAdd)) {
      if (shouldThisNodeBeScopedWhenFoundInSourceFileBraceStmtOrType(nd))
        ip = createScopeFor(nd, ip).getPtrOr(ip);
      else
        ip->widenSourceRangeForIgnoredASTNode(nd);
    }
    return ip;
  }

public:
  /// Return new insertion point if the scope was not a duplicate
  NullablePtr<ASTScopeImpl> createScopeFor(ASTNode, ASTScopeImpl *parent);

  bool shouldCreateScope(ASTNode n) const {
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
    /// In \c Parser::parseDeclVarGetSet fake PBDs are created. Ignore them.
    /// Example:
    /// \code
    /// class SR10903 { static var _: Int { 0 } }
    /// \endcode

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

    // Once we allow invalid PatternBindingDecls (see shouldCreateScope),
    // then IDE/complete_property_delegate_attribute.swift fails because
    // we try to expand a member whose source range is backwards.
    (void)SM;
    assert((d->getStartLoc().isInvalid() ||
            !SM.isBeforeInBuffer(d->getEndLoc(), d->getStartLoc())) &&
           "end-before-start will break tree search via location");
    return true;
  }

  /// Create a new scope of class ChildScope initialized with a ChildElement,
  /// expandScope it,
  /// add it as a child of the receiver, and return the child and the scope to
  /// receive more decls.
  template <typename Scope, typename... Args>
  ASTScopeImpl *createSubtree(ASTScopeImpl *parent, Args... args) {
    Scope dryRun(args...);
    assert(!dryRun.getReferrent() &&
           "Not checking for dup but class supports it");
    return createSubtreeImpl<Scope>(parent, args...);
  }

  template <typename Scope, typename... Args>
  NullablePtr<ASTScopeImpl> createSubtreeIfUnique(ASTScopeImpl *parent,
                                                  Args... args) {
    Scope dryRun(args...);
    assert(dryRun.getReferrent() &&
           "Checking for dup but class does not support it");
    if (scopedNodes.insert(&dryRun))
      return createSubtreeImpl<Scope>(parent, args...);
    return nullptr;
  }

  template <typename Scope, typename... Args>
  ASTScopeImpl *createSubtreeMustBeUnique(ASTScopeImpl *parent, Args... args) {
    if (auto s = createSubtreeIfUnique<Scope>(parent, args...))
      return s.get();
    llvm_unreachable("Scope should have been unique");
  }

private:
  template <typename Scope, typename... Args>
  ASTScopeImpl *createSubtreeImpl(ASTScopeImpl *parent, Args... args) {
    auto *child = new (ctx) Scope(args...);
    parent->addChild(child, ctx);
    if (shouldBeLazy()) {
      if (auto *ip = child->insertionPointForDeferredExpansion().getPtrOrNull())
        return ip;
    }
    ASTScopeImpl *insertionPoint = child->expandAndBeCurrent(*this);
    assert(child->verifyThatThisNodeComeAfterItsPriorSibling() &&
           "Ensure search will work");
    return insertionPoint;
  }

public:
  template <typename Scope, typename PortionClass, typename... Args>
  ASTScopeImpl *createSubtree2D(ASTScopeImpl *parent, Args... args) {
    const Portion *portion = new (ctx) PortionClass();
    return createSubtree<Scope>(parent, portion, args...);
  }

  template <typename Scope, typename PortionClass, typename... Args>
  NullablePtr<ASTScopeImpl> createSubtree2DIfUnique(ASTScopeImpl *parent,
                                                    Args... args) {
    const Portion *portion = new (ctx) PortionClass();
    return createSubtreeIfUnique<Scope>(parent, portion, args...);
  }

  void addChildrenForCapturesAndClosuresIn(Expr *expr, ASTScopeImpl *parent) {
    // Use the ASTWalker to find buried captures and closures
    forEachClosureIn(expr, [&](NullablePtr<CaptureListExpr> captureList,
                               ClosureExpr *closureExpr) {
      createSubtreeIfUnique<WholeClosureScope>(parent, closureExpr,
                                               captureList);
    });
  }

private:
  /// Find all of the (non-nested) closures (and associated capture lists)
  /// referenced within this expression.
  void forEachClosureIn(
      Expr *expr,
      function_ref<void(NullablePtr<CaptureListExpr>, ClosureExpr *)>
          foundClosure);

  // A safe way to discover this, without creating a circular request.
  // Cannot call getAttachedPropertyWrappers.
  static bool hasAttachedPropertyWrapper(VarDecl *vd) {
    return AttachedPropertyWrapperScope::getSourceRangeFor(vd).isValid();
  }

public:
  /// If the pattern has an attached property wrapper, create a scope for it
  /// so it can be looked up.

  void createAttachedPropertyWrapperScope(PatternBindingDecl *patternBinding,
                                          ASTScopeImpl *parent) {
    patternBinding->getPattern(0)->forEachVariable([&](VarDecl *vd) {
      if (hasAttachedPropertyWrapper(vd))
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
      s = createSubtreeIfUnique<GenericParamScope>(s, parameterizedDecl,
                                                   generics, i)
              .getPtrOr(s);
    return s;
  }

  void
  addChildrenForAllLocalizableAccessorsInSourceOrder(AbstractStorageDecl *asd,
                                                     ASTScopeImpl *parent);

  void
  forEachSpecializeAttrInSourceOrder(Decl *declBeingSpecialized,
                                     function_ref<void(SpecializeAttr *)> fn) {
    std::vector<SpecializeAttr *> sortedSpecializeAttrs;
    for (auto *attr : declBeingSpecialized->getAttrs()) {
      if (auto *specializeAttr = dyn_cast<SpecializeAttr>(attr))
        sortedSpecializeAttrs.push_back(specializeAttr);
    }
    // Part of rdar://53921774 rm extra copy
    for (auto *specializeAttr : sortBySourceRange(sortedSpecializeAttrs))
      fn(specializeAttr);
  }

  std::vector<ASTNode>
  expandClausesSortAndCullElementsOrMembers(ArrayRef<ASTNode> input) const {
    auto cleanedupNodes = sortBySourceRange(cull(expandClauses(input)));
    // TODO: uncomment when working on rdar://53627317
    //    findCollidingPatterns(cleanedupNodes);
    return cleanedupNodes;
  }

public:
  /// When ASTScopes are enabled for code completion, rdar://53321156
  /// IfConfigs will pose a challenge because we may need to field lookups into
  /// the inactive clauses, but the AST contains redundancy: the active clause's
  /// elements are present in the members or elements of an IterableTypeDecl or
  /// BraceStmt alongside of the IfConfigDecl. In addition there are two more
  /// complications:
  ///
  /// 1. The active clause's elements may be nested inside an init self
  /// rebinding decl (as in StringObject.self).
  ///
  /// 2. The active clause may be before or after the inactive ones
  ///
  /// So, when encountering an IfConfigDecl, we will expand  the inactive
  /// elements. Also, always sort members or elements so that the child scopes
  /// are in source order (Just one of several reasons we need to sort.)
  ///
  static const bool includeInactiveClausesOfIfConfigDecls = false;

private:
  static std::vector<ASTNode> expandClauses(ArrayRef<ASTNode> input) {
    std::vector<ASTNode> expansion;
    expandClausesInto(expansion, input, /*isInAnActiveNode=*/true);
    return expansion;
  }

  static void expandClausesInto(std::vector<ASTNode> &expansion,
                                ArrayRef<ASTNode> input,
                                const bool isInAnActiveNode) {
    for (auto n : input) {
      if (!n.isDecl(DeclKind::IfConfig)) {
        expansion.push_back(n);
        continue;
      }
      auto *const icd = cast<IfConfigDecl>(n.get<Decl *>());
      for (auto &clause : icd->getClauses()) {
        if (auto *const cond = clause.Cond)
          expansion.push_back(cond);
        if (clause.isActive) {
          // rdar://53922172
          assert(isInAnActiveNode && "Clause should not be marked "
                                     "active unless it's context is "
                                     "active");
          // get inactive nodes that nest in active clauses
          for (auto n : clause.Elements)
            if (auto *const d = n.dyn_cast<Decl *>())
              if (auto *const icd = dyn_cast<IfConfigDecl>(d))
                expandClausesInto(expansion, {d}, true);
        } else if (includeInactiveClausesOfIfConfigDecls) {
          expandClausesInto(expansion, clause.Elements,
                            /*isInAnActiveNode=*/false);
        }
      }
    }
  }

  /// Remove VarDecls because we'll find them when we expand the
  /// PatternBindingDecls. Remove AccessorDecls because they overlap & we'll
  /// find them when we expand the subscripts and var decls. Remove EnunCases
  /// because the overlap EnumElements and AST includes the elements in the
  /// members.
  std::vector<ASTNode> cull(ArrayRef<ASTNode> input) const {
    // When working on rdar://53971116 may have to cull more.
    std::vector<ASTNode> culled;
    llvm::copy_if(input, std::back_inserter(culled), [&](ASTNode n) {
      assert(!n.isDecl(DeclKind::Accessor) &&
             "Should not find accessors in iterable types or brace statements");
      return isLocalizable(n) && !n.isDecl(DeclKind::Var) &&
             !n.isDecl(DeclKind::EnumCase);
    });
    return culled;
  }

  /// TODO: The parser yields two decls at the same source loc with the same
  /// kind. Call me when tackling rdar://53627317, then move this to
  /// ASTVerifier.
  ///
  /// In all cases the first pattern seems to carry the initializer, and the
  /// second, the accessor
  void findCollidingPatterns(ArrayRef<ASTNode> input) const {
    auto dumpPBD = [&](PatternBindingDecl *pbd, const char *which) {
      llvm::errs() << "*** " << which
                   << " pbd isImplicit: " << pbd->isImplicit()
                   << ", #entries: " << pbd->getNumPatternEntries() << " :";
      pbd->getSourceRange().print(llvm::errs(), pbd->getASTContext().SourceMgr,
                                  false);
      llvm::errs() << "\n";
      llvm::errs() << "init: " << pbd->getInit(0) << "\n";
      if (pbd->getInit(0)) {
        llvm::errs() << "SR (init): ";
        pbd->getInit(0)->getSourceRange().print(
            llvm::errs(), pbd->getASTContext().SourceMgr, false);
        llvm::errs() << "\n";
        pbd->getInit(0)->dump(llvm::errs(), 0);
      }
      llvm::errs() << "vars:\n";
      pbd->getPattern(0)->forEachVariable([&](VarDecl *vd) {
        llvm::errs() << "  " << vd->getName()
                     << " implicit: " << vd->isImplicit()
                     << " #accs: " << vd->getAllAccessors().size()
                     << "\nSR (var):";
        vd->getSourceRange().print(llvm::errs(), pbd->getASTContext().SourceMgr,
                                   false);
        llvm::errs() << "\nSR (braces)";
        vd->getBracesRange().print(llvm::errs(), pbd->getASTContext().SourceMgr,
                                   false);
        llvm::errs() << "\n";
        for (auto *a : vd->getAllAccessors()) {
          llvm::errs() << "SR (acc): ";
          a->getSourceRange().print(llvm::errs(),
                                    pbd->getASTContext().SourceMgr, false);
          llvm::errs() << "\n";
          a->dump(llvm::errs(), 0);
        }
      });
    };

    Decl *lastD = nullptr;
    for (auto n : input) {
      auto *d = n.dyn_cast<Decl *>();
      if (!d || !lastD || lastD->getStartLoc() != d->getStartLoc() ||
          lastD->getKind() != d->getKind()) {
        lastD = d;
        continue;
      }
      if (auto *pbd = dyn_cast<PatternBindingDecl>(lastD))
        dumpPBD(pbd, "prev");
      if (auto *pbd = dyn_cast<PatternBindingDecl>(d)) {
        dumpPBD(pbd, "curr");
        llvm_unreachable("found colliding pattern binding decls");
      }
      llvm::errs() << "Two same kind decls at same loc: \n";
      lastD->dump(llvm::errs());
      llvm::errs() << "and\n";
      d->dump(llvm::errs());
      llvm_unreachable("Two same kind decls; unexpected kinds");
    }
  }

  /// See rdar://53921962
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
    auto cmpLoc = [&](const SourceLoc l1, const SourceLoc l2) {
      return l1 == l2 ? 0 : ctx.SourceMgr.isBeforeInBuffer(l1, l2) ? -1 : 1;
    };
    const auto r1 = getRangeableSourceRange(n1);
    const auto r2 = getRangeableSourceRange(n2);
    const int startOrder = cmpLoc(r1.Start, r2.Start);
    const int endOrder = cmpLoc(r1.End, r2.End);

#ifndef NDEBUG
    if (startOrder * endOrder == -1) {
      llvm::errs() << "*** Start order contradicts end order between: ***\n";
      dumpRangeable(n1, llvm::errs());
      llvm::errs() << "\n*** and: ***\n";
      dumpRangeable(n2, llvm::errs());
    }
#endif
    assert(startOrder * endOrder != -1 && "Start order contradicts end order");
    return startOrder + endOrder < 1;
  }

  static bool isVarDeclInPatternBindingDecl(ASTNode n1, ASTNode n2) {
    if (auto *d1 = n1.dyn_cast<Decl *>())
      if (auto *vd = dyn_cast<VarDecl>(d1))
        if (auto *d2 = n2.dyn_cast<Decl *>())
          if (auto *pbd = dyn_cast<PatternBindingDecl>(d2))
            return vd->getParentPatternBinding() == pbd;
    return false;
  }

public:
  bool shouldThisNodeBeScopedWhenFoundInSourceFileBraceStmtOrType(ASTNode n) {
    // Do not scope VarDecls because
    // they get created directly by the pattern code.
    // Doing otherwise distorts the source range
    // of their parents.
    assert(!n.isDecl(DeclKind::Accessor) && "Should not see accessors here");
    return !n.isDecl(DeclKind::Var);
  }

  template <typename ASTNodelike>
  void pushAllNecessaryNodes(ArrayRef<ASTNodelike> nodesToPrepend) {
    for (int i = nodesToPrepend.size() - 1; i >= 0; --i)
      pushIfNecessary(nodesToPrepend[i]);
  }

  bool shouldBeLazy() const { return ctx.LangOpts.LazyASTScopes; }

public:
  /// Return true if scope tree contains all the decl contexts in the AST
  /// May modify the scope tree in order to update obsolete scopes.
  /// Likely slow.
  bool containsAllDeclContextsFromAST() {
    auto allDeclContexts = findLocalizableDeclContextsInAST();
    llvm::DenseMap<const DeclContext *, const ASTScopeImpl *> bogusDCs;
    bool rebuilt = false;
    sourceFileScope->preOrderDo([&](ASTScopeImpl *scope) {
      rebuilt |= scope->reexpandIfObsolete(*this);
    });
    sourceFileScope->postOrderDo([&](ASTScopeImpl *scope) {
      if (auto *dc = scope->getDeclContext().getPtrOrNull()) {
        auto iter = allDeclContexts.find(dc);
        if (iter != allDeclContexts.end())
          ++iter->second;
        else
          bogusDCs.insert({dc, scope});
      }
    });

    auto printDecl = [&](const Decl *d) {
      llvm::errs() << "\ngetAsDecl() -> " << d << " ";
      d->getSourceRange().dump(ctx.SourceMgr);
      llvm::errs() << " : ";
      d->dump(llvm::errs());
      if (rebuilt)
        llvm::errs() << " (rebuilt)";
      llvm::errs() << "\n";
    };
    bool foundOmission = false;
    for (const auto &p : allDeclContexts) {
      if (p.second == 0) {
        if (auto *d = p.first->getAsDecl()) {
          if (isLocalizable(d)) {
            llvm::errs() << "\nASTScope tree omitted DeclContext: " << p.first
                         << " "
                         << ":\n";
            p.first->printContext(llvm::errs());
            printDecl(d);
            foundOmission = true;
          }
        } else {
          // If no decl, no source range, so no scope
        }
      }
    }
    for (const auto dcAndScope : bogusDCs) {
      llvm::errs() << "ASTScope tree confabulated: " << dcAndScope.getFirst()
                   << ":\n";
      dcAndScope.getFirst()->printContext(llvm::errs());
      if (auto *d = dcAndScope.getFirst()->getAsDecl())
        printDecl(d);
      dcAndScope.getSecond()->print(llvm::errs(), 0, false);
    }
    return !foundOmission && bogusDCs.empty();
  }

private:
  /// Return a map of every DeclContext in the AST, and zero in the 2nd element.
  llvm::DenseMap<const DeclContext *, unsigned>
  findLocalizableDeclContextsInAST() const;

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
  // Assume that decls are only added at the end, in source order
  ArrayRef<Decl *> newDecls = decls.slice(numberOfDeclsAlreadySeen);
  std::vector<ASTNode> newNodes(newDecls.begin(), newDecls.end());
  insertionPoint = scopeCreator->addNodesToTree(insertionPoint, newNodes);
  numberOfDeclsAlreadySeen = SF->Decls.size();

  // Too slow to perform all the time:
  //    assert(scopeCreator->containsAllDeclContextsFromAST() &&
  //           "ASTScope tree missed some DeclContexts or made some up");
}

ASTSourceFileScope::ASTSourceFileScope(SourceFile *SF,
                                       ScopeCreator *scopeCreator)
    : SF(SF), scopeCreator(scopeCreator), insertionPoint(this) {}

#pragma mark ASTVisitorForScopeCreation

namespace swift {
namespace ast_scope {

class ASTVisitorForScopeCreation
    : public ASTVisitor<ASTVisitorForScopeCreation, NullablePtr<ASTScopeImpl>,
                        NullablePtr<ASTScopeImpl>, NullablePtr<ASTScopeImpl>,
                        void, void, void, ASTScopeImpl *, ScopeCreator &> {
public:

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
    return scopeCreator.createSubtreeIfUnique<ScopeClass>(p, w);               \
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
  NullablePtr<ASTScopeImpl> visit##What(What *w, ASTScopeImpl *p,              \
                                        ScopeCreator &scopeCreator) {          \
    return scopeCreator.createSubtree2DIfUnique<                               \
        WhatScope, GenericTypeOrExtensionWholePortion>(p, w);                  \
  }

  VISIT_AND_CREATE_WHOLE_PORTION(ExtensionDecl, ExtensionScope)
  VISIT_AND_CREATE_WHOLE_PORTION(StructDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(ClassDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(EnumDecl, NominalTypeScope)
  VISIT_AND_CREATE_WHOLE_PORTION(TypeAliasDecl, TypeAliasScope)
  VISIT_AND_CREATE_WHOLE_PORTION(OpaqueTypeDecl, OpaqueTypeScope)
#undef VISIT_AND_CREATE_WHOLE_PORTION

  // This declaration is handled from addChildrenForAllExplicitAccessors
  NullablePtr<ASTScopeImpl> visitAccessorDecl(AccessorDecl *, ASTScopeImpl *p,
                                              ScopeCreator &scopeCreator) {
    llvm_unreachable("Should not see an accessor decl");
  }

  NullablePtr<ASTScopeImpl> visitProtocolDecl(ProtocolDecl *e, ASTScopeImpl *p,
                                              ScopeCreator &scopeCreator) {
    e->createGenericParamsIfMissing();
    return scopeCreator.createSubtree2DIfUnique<
        NominalTypeScope, GenericTypeOrExtensionWholePortion>(p, e);
  }

#pragma mark simple creation with deferred nodes

  // Each of the following creates a new scope, so that nodes which were parsed
  // after them need to be placed in scopes BELOW them in the tree. So pass down
  // the deferred nodes.
  NullablePtr<ASTScopeImpl> visitGuardStmt(GuardStmt *e, ASTScopeImpl *p,
                                           ScopeCreator &scopeCreator) {
    return scopeCreator.createSubtreeIfUnique<GuardStmtScope>(p, e);
  }
  NullablePtr<ASTScopeImpl> visitDoStmt(DoStmt *ds, ASTScopeImpl *p,
                                        ScopeCreator &scopeCreator) {
    return scopeCreator.createScopeFor(ds->getBody(), p);
  }
  NullablePtr<ASTScopeImpl> visitTopLevelCodeDecl(TopLevelCodeDecl *d,
                                                  ASTScopeImpl *p,
                                                  ScopeCreator &scopeCreator) {
    return scopeCreator.createSubtreeIfUnique<TopLevelCodeScope>(p, d);
  }

#pragma mark special-case creation

  ASTScopeImpl *visitSourceFile(SourceFile *, ASTScopeImpl *, ScopeCreator &) {
    llvm_unreachable("SourceFiles are orphans.");
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
    if (BraceStmtScope::shouldCreateScope(bs))
      scopeCreator.createSubtreeIfUnique<BraceStmtScope>(p, bs);
    return p;
  }

  NullablePtr<ASTScopeImpl>
  visitPatternBindingDecl(PatternBindingDecl *patternBinding,
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
      // TODO: Won't need to do so much work to avoid creating one without
      // a SourceRange once rdar://53627317 is done and getChildlessSourceRange
      // for PatternEntryDeclScope is simplified to use the PatternEntry's
      // source range.
      auto &patternEntry = patternBinding->getPatternList()[i];
      if (!patternEntry.getOriginalInit()) {
        bool found = false;
        patternEntry.getPattern()->forEachVariable([&](VarDecl *vd) {
          if (!vd->isImplicit())
            found = true;
          else
            found |= llvm::any_of(vd->getAllAccessors(), [&](AccessorDecl *a) {
              return isLocalizable(a);
            });
        });
        if (!found)
          continue;
      }
      insertionPoint = scopeCreator
                           .createSubtreeIfUnique<PatternEntryDeclScope>(
                               insertionPoint, patternBinding, i, vis)
                           .getPtrOr(insertionPoint);
    }
    // If in a type decl, the type search will find these,
    // but if in a brace stmt, must continue under the last binding.
    return isInTypeDecl ? parentScope : insertionPoint;
  }

  NullablePtr<ASTScopeImpl> visitEnumElementDecl(EnumElementDecl *eed,
                                                 ASTScopeImpl *p,
                                                 ScopeCreator &scopeCreator) {
    scopeCreator.createSubtree<EnumElementScope>(p, eed);
    return p;
  }

  NullablePtr<ASTScopeImpl> visitIfConfigDecl(IfConfigDecl *icd,
                                              ASTScopeImpl *p,
                                              ScopeCreator &scopeCreator) {
    llvm_unreachable("Should be handled inside of "
                     "expandClausesSortAndCullElementsOrMembers");
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
      scopeCreator.addChildrenForCapturesAndClosuresIn(expr, p);
    }
    return p;
  }
};
} // namespace ast_scope
} // namespace swift

// These definitions are way down here so it can call into
// ASTVisitorForScopeCreation
NullablePtr<ASTScopeImpl> ScopeCreator::createScopeFor(ASTNode n,
                                                       ASTScopeImpl *parent) {
  if (!shouldCreateScope(n))
    return parent;
  if (auto *p = n.dyn_cast<Decl *>())
    return ASTVisitorForScopeCreation().visit(p, parent, *this);
  if (auto *p = n.dyn_cast<Expr *>())
    return ASTVisitorForScopeCreation().visit(p, parent, *this);
  auto *p = n.get<Stmt *>();
  return ASTVisitorForScopeCreation().visit(p, parent, *this);
}

void ScopeCreator::addChildrenForAllLocalizableAccessorsInSourceOrder(
    AbstractStorageDecl *asd, ASTScopeImpl *parent) {
  //  auto  &SM = ctx.SourceMgr;
  //  auto file =
  //  SM.getIdentifierForBuffer(SM.findBufferContainingLoc(parent->getSourceRange().Start));
  //  auto line = SM.getLineNumber(parent->getSourceRange().Start);
  //  bool dumpEm = file.endswith("ArrayBody.swift")  &&  line == 51;
  //

  // Accessors are always nested within their abstract storage
  // declaration. The nesting may not be immediate, because subscripts may
  // have intervening scopes for generics.
  AbstractStorageDecl *const enclosingAbstractStorageDecl =
      parent->getEnclosingAbstractStorageDecl().get();

  std::vector<AccessorDecl *> accessorsToScope;
  // Assume we don't have to deal with inactive clauses of IfConfigs here
  llvm::copy_if(asd->getAllAccessors(), std::back_inserter(accessorsToScope),
                [&](AccessorDecl *ad) {
                  return isLocalizable(ad) &&
                         enclosingAbstractStorageDecl == ad->getStorage();
                });

  // Sort in order to include synthesized ones, which are out of order.
  // Part of rdar://53921774 rm extra copy
  for (auto *accessor : sortBySourceRange(accessorsToScope)) {
    ASTVisitorForScopeCreation().visitAbstractFunctionDecl(accessor, parent,
                                                           *this);
  }
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
  assert(!child->getParent() && "child should not already have parent");
  child->parent = this;
  clearCachedSourceRangesOfMeAndAncestors();
}

void ASTScopeImpl::removeChildren() {
  clearCachedSourceRangesOfMeAndAncestors();
  storedChildren.clear();
}

void ASTScopeImpl::disownDescendants(ScopeCreator &scopeCreator) {
  for (auto *c : getChildren()) {
    c->disownDescendants(scopeCreator);
    c->emancipate();
    scopeCreator.scopedNodes.erase(c);
  }
  removeChildren();
}

#pragma mark implementations of expansion

ASTScopeImpl *ASTScopeImpl::expandAndBeCurrent(ScopeCreator &scopeCreator) {
  auto *insertionPoint = expandSpecifically(scopeCreator);
  if (scopeCreator.shouldBeLazy()) {
    assert(!insertionPointForDeferredExpansion() ||
           insertionPointForDeferredExpansion().get() == insertionPoint);
  }
  beCurrent();
  setChildrenCountWhenLastExpanded();
  assert((getChildlessSourceRange().isValid() || !getChildren().empty()) &&
         "need to be able to find source range");
  assert(verifyThatChildrenAreContainedWithin(getSourceRange()) &&
         "Search will fail");
  return insertionPoint;
}

  // Do this whole bit so it's easy to see which type of scope is which

#define CREATES_NEW_INSERTION_POINT(Scope)                                     \
  ASTScopeImpl *Scope::expandSpecifically(ScopeCreator &scopeCreator) {        \
    return expandAScopeThatCreatesANewInsertionPoint(scopeCreator);            \
  }

#define NO_NEW_INSERTION_POINT(Scope)                                          \
  ASTScopeImpl *Scope::expandSpecifically(ScopeCreator &scopeCreator) {        \
    expandAScopeThatDoesNotCreateANewInsertionPoint(scopeCreator);             \
    return getParent().get();                                                  \
  }

// Return this in particular for GenericParamScope so body is scoped under it
#define NO_EXPANSION(Scope)                                                    \
  ASTScopeImpl *Scope::expandSpecifically(ScopeCreator &) { return this; }

CREATES_NEW_INSERTION_POINT(ParameterListScope)
CREATES_NEW_INSERTION_POINT(ConditionalClauseScope)
CREATES_NEW_INSERTION_POINT(GuardStmtScope)
CREATES_NEW_INSERTION_POINT(PatternEntryDeclScope)
CREATES_NEW_INSERTION_POINT(PatternEntryInitializerScope)
CREATES_NEW_INSERTION_POINT(GenericTypeOrExtensionScope)
CREATES_NEW_INSERTION_POINT(BraceStmtScope)
CREATES_NEW_INSERTION_POINT(TopLevelCodeScope)

NO_NEW_INSERTION_POINT(AbstractFunctionBodyScope)
NO_NEW_INSERTION_POINT(AbstractFunctionDeclScope)
NO_NEW_INSERTION_POINT(AttachedPropertyWrapperScope)
NO_NEW_INSERTION_POINT(EnumElementScope)

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
NO_EXPANSION(ConditionalClausePatternUseScope)
NO_EXPANSION(LookupParentDiversionScope)

#undef CREATES_NEW_INSERTION_POINT
#undef NO_NEW_INSERTION_POINT

ASTScopeImpl *ParameterListScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // Each initializer for a function parameter is its own, sibling, scope.
  // Unlike generic parameters or pattern initializers, it cannot refer to a
  // previous parameter.
  for (ParamDecl *pd : params->getArray()) {
    if (pd->getDefaultValue())
      scopeCreator.createSubtree<DefaultArgumentInitializerScope>(this, pd);
  }
  return this; // body of func goes under me
}

ASTScopeImpl *PatternEntryDeclScope::expandAScopeThatCreatesANewInsertionPoint(
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
    assert(
        !getSourceManager().isBeforeInBuffer(
            patternEntry.getOriginalInit()->getStartLoc(), decl->getStartLoc()) &&
        "Original inits are always after the '='");
    scopeCreator.createSubtree<PatternEntryInitializerScope>(
        this, decl, patternEntryIndex, vis);
  }
  // Add accessors for the variables in this pattern.
  forEachVarDeclWithLocalizableAccessors(scopeCreator, [&](VarDecl *var) {
    scopeCreator.createSubtreeIfUnique<VarDeclScope>(this, var);
  });

  return getParent().get();
}

ASTScopeImpl *
PatternEntryInitializerScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // Create a child for the initializer expression.
  ASTVisitorForScopeCreation().visitExpr(getPatternEntry().getOriginalInit(), this,
                                         scopeCreator);
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

  return scopeCreator.createSubtree<LookupParentDiversionScope>(
      this, conditionLookupParent, stmt->getEndLoc());
}

ASTScopeImpl *
GenericTypeOrExtensionScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  return portion->expandScope(this, scopeCreator);
}

ASTScopeImpl *BraceStmtScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  // TODO: remove the sort after performing rdar://53254395
  return scopeCreator.addNodesToTree(this, stmt->getElements());
}

ASTScopeImpl *TopLevelCodeScope::expandAScopeThatCreatesANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  if (!BraceStmtScope::shouldCreateScope(decl->getBody()))
    return this;
  auto *insertionPoint =
      scopeCreator.createSubtreeIfUnique<BraceStmtScope>(this, decl->getBody())
          .getPtrOr(this);
  return insertionPoint;
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
        scopeCreator.createSubtreeIfUnique<SpecializeAttributeScope>(
            this, specializeAttr, decl);
      });
  // Create scopes for generic and ordinary parameters.
  // For a subscript declaration, the generic and ordinary parameters are in an
  // ancestor scope, so don't make them here.
  ASTScopeImpl *leaf = this;
  if (!isa<AccessorDecl>(decl)) {
    leaf = scopeCreator.createGenericParamScopes(decl, decl->getGenericParams(),
                                                 leaf);
    if (isLocalizable(decl) && getParamsSourceLoc(decl).isValid()) {
      // See rdar://54188611
      // swift::createDesignatedInitOverride just clones the parameters, so they
      // end up with a bogus SourceRange, maybe *before* the start of the
      // function.
      if (!decl->isImplicit()) {
        leaf = scopeCreator.createSubtree<ParameterListScope>(
            leaf, decl->getParameters(), nullptr);
      }
    }
  }
  // Create scope for the body.
  // We create body scopes when there is no body for source kit to complete
  // erroneous code in bodies. But don't let compiler synthesize one.
  if (decl->getBody(false) && decl->getBodySourceRange().isValid()) {
    if (AbstractFunctionBodyScope::isAMethod(decl))
      scopeCreator.createSubtree<MethodBodyScope>(leaf, decl);
    else
      scopeCreator.createSubtree<PureFunctionBodyScope>(leaf, decl);
  }
}

void EnumElementScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  if (auto *pl = decl->getParameterList())
    scopeCreator.createSubtree<ParameterListScope>(this, pl, nullptr);
  if (auto *expr = decl->getRawValueExpr()) // might contain a closure
    ASTVisitorForScopeCreation().visitExpr(expr, this, scopeCreator);
}

void AbstractFunctionBodyScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  expandBody(scopeCreator);
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
    ASTVisitorForScopeCreation().visitCatchStmt(catchClause, this,
                                                scopeCreator);
  }
}

void SwitchStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  ASTVisitorForScopeCreation().visitExpr(stmt->getSubjectExpr(), this,
                                         scopeCreator);

  for (auto caseStmt : stmt->getCases()) {
    if (isLocalizable(caseStmt))
      scopeCreator.createSubtreeIfUnique<CaseStmtScope>(this, caseStmt);
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
  //
  // TODO: refer to rdar://53921962
  if (!stmt->getBody()->isImplicit()) {
    if (isLocalizable(stmt->getBody()))
      scopeCreator.createSubtree<ForEachPatternScope>(this, stmt);
  }
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
  scopeCreator.addChildrenForAllLocalizableAccessorsInSourceOrder(decl, this);
}

void SubscriptDeclScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  auto *sub = decl;
  auto *leaf =
      scopeCreator.createGenericParamScopes(sub, sub->getGenericParams(), this);
  auto *params = scopeCreator.createSubtree<ParameterListScope>(
      leaf, sub->getIndices(), sub->getAccessor(AccessorKind::Get));
  scopeCreator.addChildrenForAllLocalizableAccessorsInSourceOrder(sub, params);
}

void WholeClosureScope::expandAScopeThatDoesNotCreateANewInsertionPoint(
    ScopeCreator &scopeCreator) {
  if (auto *cl = captureList.getPtrOrNull())
    scopeCreator.createSubtreeMustBeUnique<CaptureListScope>(this, cl);
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
  if (BraceStmtScope::shouldCreateScope(closureExpr->getBody())) {
    scopeCreator.createSubtreeMustBeUnique<BraceStmtScope>(
        this, closureExpr->getBody());
  }
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
  // rdar://53972776
  if (scope->shouldHaveABody() && !scope->doesDeclHaveABody())
    return scope->getParent().get();

  auto *deepestScope = scopeCreator.createGenericParamScopes(
      scope->getDecl(), scope->getGenericContext()->getGenericParams(), scope);
  if (scope->getGenericContext()->getTrailingWhereClause())
    scope->createTrailingWhereClauseScope(deepestScope, scopeCreator);
  scope->createBodyScope(deepestScope, scopeCreator);
  return scope->getParent().get();
}

ASTScopeImpl *
IterableTypeBodyPortion::expandScope(GenericTypeOrExtensionScope *scope,
                                     ScopeCreator &scopeCreator) const {
  scope->expandBody(scopeCreator);
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

void AbstractPatternEntryScope::forEachVarDeclWithLocalizableAccessors(
    ScopeCreator &scopeCreator, function_ref<void(VarDecl *)> foundOne) const {
  getPatternEntry().getPattern()->forEachVariable([&](VarDecl *var) {
    if (llvm::any_of(var->getAllAccessors(),
                     [&](AccessorDecl *a) { return isLocalizable(a); }))
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
ParameterListScope::getEnclosingAbstractStorageDecl() const {
  return getParent().get()->getEnclosingAbstractStorageDecl();
}
NullablePtr<AbstractStorageDecl>
GenericParamScope::getEnclosingAbstractStorageDecl() const {
  return getParent().get()->getEnclosingAbstractStorageDecl();
}

bool ASTScopeImpl::isATypeDeclScope() const {
  Decl *const pd = getDeclIfAny().getPtrOrNull();
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

#pragma mark - expandBody

void AbstractFunctionBodyScope::expandBody(ScopeCreator &scopeCreator) {
  BraceStmt *braceStmt = decl->getBody();
  if (braceStmt)
    ASTVisitorForScopeCreation().visitBraceStmt(braceStmt, this, scopeCreator);
}

void GenericTypeOrExtensionScope::expandBody(ScopeCreator &) {}

void IterableTypeScope::expandBody(ScopeCreator &scopeCreator) {
  auto nodes = asNodeVector(getIterableDeclContext().get()->getMembers());
  scopeCreator.addNodesToTree(this, nodes);
}

#pragma mark - reexpandIfObsolete

bool ASTScopeImpl::reexpandIfObsolete(ScopeCreator &scopeCreator) {
  if (isCurrent())
    return false;
  reexpand(scopeCreator);
  return true;
}

void ASTScopeImpl::reexpand(ScopeCreator &scopeCreator) {
  auto scopesToReuse = rescueScopesToReuse();
  disownDescendants(scopeCreator);
  expandAndBeCurrent(scopeCreator);
  addReusedScopes(scopesToReuse);
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
    return PtrCalc().visit(x);                                                 \
  }

GET_REFERRENT(AbstractFunctionDeclScope, getDecl())
// If the PatternBindingDecl is a dup, detect it for the first
// PatternEntryDeclScope; the others are subscopes.
GET_REFERRENT(PatternEntryDeclScope, getPattern())
GET_REFERRENT(TopLevelCodeScope, getDecl())
GET_REFERRENT(SubscriptDeclScope, getDecl())
GET_REFERRENT(VarDeclScope, getDecl())
GET_REFERRENT(GenericParamScope, paramList->getParams()[index])
GET_REFERRENT(AbstractStmtScope, getStmt())
GET_REFERRENT(CaptureListScope, getExpr())
GET_REFERRENT(WholeClosureScope, getExpr())
GET_REFERRENT(SpecializeAttributeScope, specializeAttr)
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
IterableTypeScope::insertionPointForDeferredExpansion() {
  return portion->insertionPointForDeferredExpansion(this);
}
NullablePtr<ASTScopeImpl>
Portion::insertionPointForDeferredExpansion(IterableTypeScope *) const {
  return nullptr;
}
NullablePtr<ASTScopeImpl>
IterableTypeBodyPortion::insertionPointForDeferredExpansion(
    IterableTypeScope *s) const {
  return s->getParent().get();
}
SourceRange ASTScopeImpl::sourceRangeForDeferredExpansion() const {
  return SourceRange();
}
SourceRange IterableTypeScope::sourceRangeForDeferredExpansion() const {
  return portion->sourceRangeForDeferredExpansion(this);
}
SourceRange
Portion::sourceRangeForDeferredExpansion(const IterableTypeScope *) const {
  return SourceRange();
}
SourceRange IterableTypeBodyPortion::sourceRangeForDeferredExpansion(
    const IterableTypeScope *s) const {
  return getChildlessSourceRangeOf(s, false);
}

void ASTScopeImpl::beCurrent() {}
bool ASTScopeImpl::isCurrent() const { return true; }

void IterableTypeScope::beCurrent() { portion->beCurrent(this); }
bool IterableTypeScope::isCurrent() const { return portion->isCurrent(this); }

void Portion::beCurrent(IterableTypeScope *) const {}
bool Portion::isCurrent(const IterableTypeScope *) const { return true; }

void IterableTypeBodyPortion::beCurrent(IterableTypeScope *s) const {
  s->makeBodyCurrent();
}
bool IterableTypeBodyPortion::isCurrent(const IterableTypeScope *s) const {
  return s->isBodyCurrent();
}

void IterableTypeScope::makeBodyCurrent() {
  memberCount = getIterableDeclContext().get()->getMemberCount();
}
bool IterableTypeScope::isBodyCurrent() const {
  return memberCount == getIterableDeclContext().get()->getMemberCount();
}

void AbstractFunctionBodyScope::beCurrent() {
  bodyWhenLastExpanded = decl->getBody();
}
bool AbstractFunctionBodyScope::isCurrent() const {
  return bodyWhenLastExpanded == decl->getBody();
  ;
}

void TopLevelCodeScope::beCurrent() { bodyWhenLastExpanded = decl->getBody(); }
bool TopLevelCodeScope::isCurrent() const {
  return bodyWhenLastExpanded == decl->getBody();
}

void PatternEntryDeclScope::beCurrent() {
  initWhenLastExpanded = getPatternEntry().getOriginalInit();
  unsigned varCount = 0;
  getPatternEntry().getPattern()->forEachVariable(
      [&](VarDecl *) { ++varCount; });
  varCountWhenLastExpanded = 0;
}
bool PatternEntryDeclScope::isCurrent() const {
  if (initWhenLastExpanded != getPatternEntry().getOriginalInit())
    return false;
  unsigned varCount = 0;
  getPatternEntry().getPattern()->forEachVariable(
      [&](VarDecl *) { ++varCount; });
  return varCount == varCountWhenLastExpanded;
}

void WholeClosureScope::beCurrent() {
  bodyWhenLastExpanded = closureExpr->getBody();
}
bool WholeClosureScope::isCurrent() const {
  return bodyWhenLastExpanded == closureExpr->getBody();
}

#pragma mark getParentOfRescuedScopes
NullablePtr<ASTScopeImpl> ASTScopeImpl::getParentOfRescuedScopes() {
  return this;
}
NullablePtr<ASTScopeImpl>
AbstractFunctionBodyScope::getParentOfRescuedScopes() {
  // Reexpansion always creates a new body as the first child
  return getChildren().empty() ? nullptr : getChildren().front();
}
NullablePtr<ASTScopeImpl> TopLevelCodeScope::getParentOfRescuedScopes() {
  // Reexpansion always creates a new body as the first child
  return getChildren().empty() ? nullptr : getChildren().front();
}

#pragma mark rescuing & reusing
std::vector<ASTScopeImpl *> ASTScopeImpl::rescueScopesToReuse() {
  if (auto *p = getParentOfRescuedScopes().getPtrOrNull()) {
    return p->rescueYoungestChildren(p->getChildren().size() -
                                     p->childrenCountWhenLastExpanded);
  }
  return {};
}

void ASTScopeImpl::addReusedScopes(ArrayRef<ASTScopeImpl *> scopesToAdd) {
  auto *p = getParentOfRescuedScopes().getPtrOrNull();
  if (!p) {
    assert(scopesToAdd.empty() && "Non-empty body disappeared?!");
    return;
  }
  auto &ctx = getASTContext();
  for (auto *s : scopesToAdd) {
    p->addChild(s, ctx);
    assert(s->verifyThatThisNodeComeAfterItsPriorSibling() &&
           "Ensure search will work");
  }
}

std::vector<ASTScopeImpl *>
ASTScopeImpl::rescueYoungestChildren(const unsigned int count) {
  std::vector<ASTScopeImpl *> youngestChildren;
  for (unsigned i = getChildren().size() - count; i < getChildren().size(); ++i)
    youngestChildren.push_back(getChildren()[i]);
  // So they don't get disowned and children cleared.
  for (unsigned i = 0; i < count; ++i) {
    storedChildren.back()->emancipate();
    storedChildren.pop_back();
  }
  return youngestChildren;
}

void ASTScopeImpl::setChildrenCountWhenLastExpanded() {
  childrenCountWhenLastExpanded = getChildren().size();
}

bool BraceStmtScope::shouldCreateScope(const BraceStmt *const bs) {
  return isLocalizable(bs);
}

bool AbstractFunctionDeclScope::shouldCreateAccessorScope(
    const AccessorDecl *const ad) {
  return isLocalizable(ad);
}

#pragma mark verification

namespace {
class LocalizableDeclContextCollector : public ASTWalker {

public:
  llvm::DenseMap<const DeclContext *, unsigned> declContexts;

  void record(const DeclContext *dc) {
    if (dc)
      declContexts.insert({dc, 0});
  }

  bool walkToDeclPre(Decl *D) override {
    //    catchForDebugging(D, "DictionaryBridging.swift", 694);
    if (const auto *dc = dyn_cast<DeclContext>(D))
      record(dc);
    if (auto *icd =
            dyn_cast<IfConfigDecl>(D)) { // TODO: fix ASTWalker with option
      walkToClauses(icd);
      return false;
    }
    if (auto *pd = dyn_cast<ParamDecl>(D))
      record(pd->getDefaultArgumentInitContext());
    else if (auto *pbd = dyn_cast<PatternBindingDecl>(D))
      recordInitializers(pbd);
    else if (auto *vd = dyn_cast<VarDecl>(D))
      for (auto *ad : vd->getAllAccessors())
        ad->walk(*this);
    return ASTWalker::walkToDeclPre(D);
  }

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    if (const auto *ce = dyn_cast<ClosureExpr>(E))
      record(ce);
    return ASTWalker::walkToExprPre(E);
  }

private:
  void walkToClauses(IfConfigDecl *icd) {
    for (auto &clause : icd->getClauses()) {
      // Generate scopes for any closures in the condition
      if (ScopeCreator::includeInactiveClausesOfIfConfigDecls &&
          clause.isActive) {
        if (clause.Cond)
          clause.Cond->walk(*this);
        for (auto n : clause.Elements)
          n.walk(*this);
      }
    }
  }

  void recordInitializers(PatternBindingDecl *pbd) {
    for (auto entry : pbd->getPatternList())
      record(entry.getInitContext());
  }

  void catchForDebugging(Decl *D, const char *file, const unsigned line) {
    auto &SM = D->getASTContext().SourceMgr;
    auto loc = D->getStartLoc();
    if (!loc.isValid())
      return;
    auto bufID = SM.findBufferContainingLoc(loc);
    auto f = SM.getIdentifierForBuffer(bufID);
    auto lin = SM.getLineNumber(loc);
    if (f.endswith(file) && lin == line)
      if (auto *v = dyn_cast<PatternBindingDecl>(D))
        llvm::errs() << "HERE catchForDebugging: " << lin << "\n";
  }
};
} // end namespace

llvm::DenseMap<const DeclContext *, unsigned>
ScopeCreator::findLocalizableDeclContextsInAST() const {
  LocalizableDeclContextCollector collector;
  sourceFileScope->SF->walk(collector);
  // Walker omits the top
  collector.record(sourceFileScope->SF);
  return collector.declContexts;
}

bool ASTSourceFileScope::crossCheckWithAST() {
  return scopeCreator->containsAllDeclContextsFromAST();
}

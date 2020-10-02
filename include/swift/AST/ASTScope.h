//===--- ASTScopeImpl.h - Swift AST Object-Oriented Scope --------*- C++-*-===//
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
/// This file defines the ASTScopeImpl class ontology, which
/// describes the scopes that exist within a Swift AST.
///
/// Each scope has four basic functions: printing for debugging, creation of
/// itself and its children, obtaining its SourceRange (for lookup), and looking
/// up names accessible from that scope.
///
/// Invariants:
///   a child's source range is a subset (proper or improper) of its parent's,
///   children are ordered by source range, and do not overlap,
///   all the names visible within a parent are visible within the child, unless
///   the nesting is illegal. For instance, a protocol nested inside of a class
///   does not get to see the symbols in the class or its ancestors.
///
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_AST_SCOPE_H
#define SWIFT_AST_AST_SCOPE_H

#include "swift/AST/ASTNode.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SimpleRequest.h"
#include "swift/Basic/Compiler.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"

/// In case there's a bug in the ASTScope lookup system, suggest that the user
/// try disabling it.
/// \p message must be a string literal
#define ASTScopeAssert(predicate, message)                                     \
  assert((predicate) && message)

#define ASTScope_unreachable(message)                                          \
  llvm_unreachable(message)

namespace swift {

#pragma mark Forward-references

#define DECL(Id, Parent) class Id##Decl;
#define ABSTRACT_DECL(Id, Parent) class Id##Decl;
#include "swift/AST/DeclNodes.def"
#undef DECL
#undef ABSTRACT_DECL

#define EXPR(Id, Parent) class Id##Expr;
#include "swift/AST/ExprNodes.def"
#undef EXPR

#define STMT(Id, Parent) class Id##Stmt;
#define ABSTRACT_STMT(Id, Parent) class Id##Stmt;
#include "swift/AST/StmtNodes.def"
#undef STMT
#undef ABSTRACT_STMT

class GenericParamList;
class TrailingWhereClause;
class ParameterList;
class PatternBindingEntry;
class SpecializeAttr;
class GenericContext;
class DeclName;
class StmtConditionElement;

namespace ast_scope {
class ASTScopeImpl;
class GenericTypeOrExtensionScope;
class IterableTypeScope;
class TypeAliasScope;
class ScopeCreator;

struct AnnotatedInsertionPoint {
  ASTScopeImpl *insertionPoint;
  const char *explanation;
};
} // namespace ast_scope

namespace ast_scope {

void simple_display(llvm::raw_ostream &out, const ASTScopeImpl *);
void simple_display(llvm::raw_ostream &out, const ScopeCreator *);

SourceLoc extractNearestSourceLoc(std::tuple<ASTScopeImpl *, ScopeCreator *>);

#pragma mark the root ASTScopeImpl class

/// Describes a lexical scope within a source file.
///
/// Each \c ASTScopeImpl is a node within a tree that describes all of the
/// lexical scopes within a particular source range. The root of this scope tree
/// is always a \c SourceFile node, and the tree covers the entire source file.
/// The children of a particular node are the lexical scopes immediately
/// nested within that node, and have source ranges that are enclosed within
/// the source range of their parent node. At the leaves are lexical scopes
/// that cannot be subdivided further.
///
/// The tree provides source-location-based query operations, allowing one to
/// find the innermost scope that contains a given source location. Navigation
/// to parent nodes from that scope allows one to walk the lexically enclosing
/// scopes outward to the source file. Given a scope, one can also query the
/// associated \c DeclContext for additional contextual information.
///
/// \code
/// -dump-scope-maps expanded
/// \endcode
class ASTScopeImpl {
  friend class NodeAdder;
  friend class Portion;
  friend class GenericTypeOrExtensionWholePortion;
  friend class NomExtDeclPortion;
  friend class GenericTypeOrExtensionWherePortion;
  friend class GenericTypeOrExtensionWherePortion;
  friend class IterableTypeBodyPortion;
  friend class ScopeCreator;

#pragma mark - tree state
protected:
  using Children = SmallVector<ASTScopeImpl *, 4>;
  /// Whether the given parent is the accessor node for an abstract
  /// storage declaration or is directly descended from it.

private:
  /// Always set by the constructor, so that when creating a child
  /// the parent chain is available.
  ASTScopeImpl *parent = nullptr; // null at the root

  /// Child scopes, sorted by source range.
  /// Must clear source range change whenever this changes
  Children storedChildren;

  bool wasExpanded = false;

  /// Can clear storedChildren, so must remember this
  bool haveAddedCleanup = false;

  // Must be updated after last child is added and after last child's source
  // position is known
  mutable Optional<SourceRange> cachedSourceRange;

  // When ignoring ASTNodes in a scope, they still must count towards a scope's
  // source range. So include their ranges here
  SourceRange sourceRangeOfIgnoredASTNodes;

#pragma mark - constructor / destructor
public:
  ASTScopeImpl(){};
  // TOD: clean up all destructors and deleters
  virtual ~ASTScopeImpl() {}

  ASTScopeImpl(ASTScopeImpl &&) = delete;
  ASTScopeImpl &operator=(ASTScopeImpl &&) = delete;
  ASTScopeImpl(const ASTScopeImpl &) = delete;
  ASTScopeImpl &operator=(const ASTScopeImpl &) = delete;

  // Make vanilla new illegal for ASTScopes.
  void *operator new(size_t bytes) = delete;
  // Need this because have virtual destructors
  void operator delete(void *data) {}

  // Only allow allocation of scopes using the allocator of a particular source
  // file.
  void *operator new(size_t bytes, const ASTContext &ctx,
                     unsigned alignment = alignof(ASTScopeImpl));
  void *operator new(size_t Bytes, void *Mem) {
    ASTScopeAssert(Mem, "Allocation failed");
    return Mem;
  }

#pragma mark - tree declarations
protected:
  NullablePtr<ASTScopeImpl> getParent() { return parent; }
  NullablePtr<const ASTScopeImpl> getParent() const { return parent; }

  const Children &getChildren() const { return storedChildren; }

public:
  void addChild(ASTScopeImpl *child, ASTContext &);

private:
  NullablePtr<ASTScopeImpl> getPriorSibling() const;

public:
  void preOrderDo(function_ref<void(ASTScopeImpl *)>);
  /// Like preorderDo but without myself.
  void preOrderChildrenDo(function_ref<void(ASTScopeImpl *)>);
  void postOrderDo(function_ref<void(ASTScopeImpl *)>);

#pragma mark - source ranges

#pragma mark - source range queries

public:
  /// Return signum of ranges. Centralize the invariant that ASTScopes use ends.
  static int compare(SourceRange, SourceRange, const SourceManager &,
                     bool ensureDisjoint);

  SourceRange getSourceRangeOfScope(bool omitAssertions = false) const;

  /// InterpolatedStringLiteralExprs and EditorPlaceHolders respond to
  /// getSourceRange with the starting point. But we might be asked to lookup an
  /// identifer within one of them. So, find the real source range of them here.
  SourceRange getEffectiveSourceRange(ASTNode) const;

  void computeAndCacheSourceRangeOfScope(bool omitAssertions = false) const;
  bool isSourceRangeCached(bool omitAssertions = false) const;

  bool checkSourceRangeOfThisASTNode() const;

  /// For debugging
  bool doesRangeMatch(unsigned start, unsigned end, StringRef file = "",
                      StringRef className = "");

  unsigned countDescendants() const;

  /// Make sure that when the argument is executed, there are as many
  /// descendants after as before.
  void assertThatTreeDoesNotShrink(function_ref<void()>);

private:
  SourceRange computeSourceRangeOfScope(bool omitAssertions = false) const;
  SourceRange
  computeSourceRangeOfScopeWithChildASTNodes(bool omitAssertions = false) const;
  bool ensureNoAncestorsSourceRangeIsCached() const;

#pragma mark - source range adjustments
private:
  SourceRange widenSourceRangeForIgnoredASTNodes(SourceRange range) const;

  /// If the scope refers to a Decl whose source range tells the whole story,
  /// for example a NominalTypeScope, it is not necessary to widen the source
  /// range by examining the children. In that case we could just return
  /// the childlessRange here.
  /// But, we have not marked such scopes yet. Doing so would be an
  /// optimization.
  SourceRange widenSourceRangeForChildren(SourceRange range,
                                          bool omitAssertions) const;

  /// Even ASTNodes that do not form scopes must be included in a Scope's source
  /// range. Widen the source range of the receiver to include the (ignored)
  /// node.
  void widenSourceRangeForIgnoredASTNode(ASTNode);

private:
  void clearCachedSourceRangesOfMeAndAncestors();

public: // public for debugging
  /// Returns source range of this node alone, without factoring in any
  /// children.
  virtual SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const = 0;

protected:
  SourceManager &getSourceManager() const;
  bool hasValidSourceRange() const;
  bool hasValidSourceRangeOfIgnoredASTNodes() const;
  bool precedesInSource(const ASTScopeImpl *) const;
  bool verifyThatChildrenAreContainedWithin(SourceRange) const;
  bool verifyThatThisNodeComeAfterItsPriorSibling() const;

private:
  bool checkSourceRangeAfterExpansion(const ASTContext &) const;

#pragma mark common queries
public:
  virtual NullablePtr<ClosureExpr> getClosureIfClosureScope() const;
  virtual ASTContext &getASTContext() const;
  virtual NullablePtr<Decl> getDeclIfAny() const { return nullptr; };
  virtual NullablePtr<Stmt> getStmtIfAny() const { return nullptr; };
  virtual NullablePtr<Expr> getExprIfAny() const { return nullptr; };
  virtual NullablePtr<DeclAttribute> getDeclAttributeIfAny() const {
    return nullptr;
  }
  virtual NullablePtr<const void> getReferrent() const { return nullptr; }

#pragma mark - debugging and printing

public:
  virtual const SourceFile *getSourceFile() const;
  virtual std::string getClassName() const = 0;

  /// Print out this scope for debugging/reporting purposes.
  void print(llvm::raw_ostream &out, unsigned level = 0, bool lastChild = false,
             bool printChildren = true) const;

  void printRange(llvm::raw_ostream &out) const;

protected:
  virtual void printSpecifics(llvm::raw_ostream &out) const {}
  virtual NullablePtr<const void> addressForPrinting() const;

public:
  SWIFT_DEBUG_DUMP;

  void dumpOneScopeMapLocation(std::pair<unsigned, unsigned> lineColumn);

private:
  llvm::raw_ostream &verificationError() const;

#pragma mark - Scope tree creation
public:
  /// expandScope me, sending deferred nodes to my descendants.
  /// Return the scope into which to place subsequent decls
  ASTScopeImpl *expandAndBeCurrentDetectingRecursion(ScopeCreator &);

  /// Expand or reexpand the scope if unexpanded or if not current.
  /// There are several places in the compiler that mutate the AST after the
  /// fact, above and beyond adding Decls to the SourceFile.
  ASTScopeImpl *expandAndBeCurrent(ScopeCreator &);

  bool getWasExpanded() const { return wasExpanded; }

protected:
  void setWasExpanded() { wasExpanded = true; }
  virtual ASTScopeImpl *expandSpecifically(ScopeCreator &) = 0;

private:
  /// Compare the pre-expasion range with the post-expansion range and return
  /// false if lazyiness couild miss lookups.
  bool checkLazySourceRange(const ASTContext &) const;

public:
  /// Some scopes can be expanded lazily.
  /// Such scopes must: not change their source ranges after expansion, and
  /// their expansion must return an insertion point outside themselves.
  /// After a node is expanded, its source range (getSourceRangeofThisASTNode
  /// union children's ranges) must be same as this.
  virtual NullablePtr<ASTScopeImpl> insertionPointForDeferredExpansion();
  virtual SourceRange sourceRangeForDeferredExpansion() const;

private:
  virtual ScopeCreator &getScopeCreator();

#pragma mark - lookup

public:
  using DeclConsumer = namelookup::AbstractASTScopeDeclConsumer &;

  /// Entry point into ASTScopeImpl-land for lookups
  static void
  unqualifiedLookup(SourceFile *, SourceLoc, DeclConsumer);

  /// Entry point into ASTScopeImpl-land for labeled statement lookups.
  static llvm::SmallVector<LabeledStmt *, 4>
  lookupLabeledStmts(SourceFile *sourceFile, SourceLoc loc);

  static std::pair<CaseStmt *, CaseStmt *>
  lookupFallthroughSourceAndDest(SourceFile *sourceFile, SourceLoc loc);

#pragma mark - - lookup- starting point
private:
  static const ASTScopeImpl *findStartingScopeForLookup(SourceFile *,
                                                        const SourceLoc where);

protected:
  /// Not const because may reexpand some scopes.
  ASTScopeImpl *findInnermostEnclosingScope(SourceLoc,
                                            NullablePtr<raw_ostream>);
  ASTScopeImpl *findInnermostEnclosingScopeImpl(SourceLoc,
                                                NullablePtr<raw_ostream>,
                                                SourceManager &,
                                                ScopeCreator &);

private:
  NullablePtr<ASTScopeImpl> findChildContaining(SourceLoc loc,
                                                SourceManager &sourceMgr) const;

#pragma mark - - lookup- per scope
protected:
  /// The main (recursive) lookup function:
  /// Tell DeclConsumer about all names found in this scope and if not done,
  /// recurse for enclosing scopes. Stop lookup if about to look in limit.
  ///
  /// If the lookup depends on implicit self, selfDC is its context.
  /// (Names in extensions never depend on self.)
  ///
  /// In a Nominal, Extension, or TypeAliasScope, the lookup can start at either
  /// the body portion (for the first two), the where portion, or a
  /// GenericParamScope. In every case, the generics on the type decl must be
  /// searched, but only once. And they must be searched *before* the generic
  /// parameters. For instance, the following is correct: \code class
  /// ShadowingGenericParameter<T> { \code   typealias T = Int;  func foo (t :
  /// T) {} \code } \code ShadowingGenericParameter<String>().foo(t: "hi")
  ///
  /// So keep track of the last generic param list searched to avoid
  /// duplicating work.
  ///
  /// Look in this scope.
  /// \param limit A scope into which lookup should not go. See \c
  /// getLookupLimit. \param lastListSearched Last list searched.
  /// \param consumer is the object to which found decls are reported.
  void lookup(NullablePtr<const ASTScopeImpl> limit,
              NullablePtr<const GenericParamList> lastListSearched,
              DeclConsumer consumer) const;

protected:
  /// Find either locals or members (no scope has both)
  /// \return True if lookup is done
  virtual bool lookupLocalsOrMembers(DeclConsumer consumer) const;

  /// Returns isDone and the list searched, if any
  std::pair<bool, NullablePtr<const GenericParamList>>
  lookInMyGenericParameters(
      NullablePtr<const GenericParamList> priorListSearched,
      DeclConsumer consumer) const;

  virtual NullablePtr<const GenericParamList> genericParams() const;

  // Consume the generic parameters in the context and its outer contexts
  static bool lookInGenericParametersOf(NullablePtr<const GenericParamList>,
                                        DeclConsumer);

  NullablePtr<const ASTScopeImpl> parentIfNotChildOfTopScope() const {
    const auto *p = getParent().get();
    return p->getParent().isNonNull() ? p : nullptr;
  }

public:
  /// The tree is organized by source location and for most nodes this is also
  /// what obtaines for scoping. However, guards are different. The scope after
  /// the guard else must hop into the innermoset scope of the guard condition.
  virtual NullablePtr<const ASTScopeImpl> getLookupParent() const {
    return parent;
  }

#pragma mark - - lookup- local bindings
protected:
  // A local binding is a basically a local variable defined in that very scope
  // It is not an instance variable or inherited type.

  static bool lookupLocalBindingsInPattern(const Pattern *p,
                                           DeclConsumer consumer);

  /// When lookup must stop before the outermost scope, return the scope to stop
  /// at. Example, if a protocol is nested in a struct, we must stop before
  /// looking into the struct.
  ///
  /// Ultimately, the task of rejecting results found in inapplicable outer
  /// scopes is best moved to the clients of the ASTScope lookup subsystem. It
  /// seems out of place here.
  virtual NullablePtr<const ASTScopeImpl> getLookupLimit() const;

  NullablePtr<const ASTScopeImpl>
  ancestorWithDeclSatisfying(function_ref<bool(const Decl *)> predicate) const;

  /// Whether this scope terminates lookup of labeled statements in the
  /// children below it, because one cannot perform a "break" or a "continue"
  /// in a child that goes outside of this scope.
  virtual bool isLabeledStmtLookupTerminator() const;

}; // end of ASTScopeImpl

#pragma mark - specific scope classes

/// The root of the scope tree.
class ASTSourceFileScope final : public ASTScopeImpl {
public:
  SourceFile *const SF;
  ScopeCreator *const scopeCreator;
  ASTScopeImpl *insertionPoint;

  ASTSourceFileScope(SourceFile *SF, ScopeCreator *scopeCreator);

  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  void buildFullyExpandedTree();
  void
  buildEnoughOfTreeForTopLevelExpressionsButDontRequestGenericsOrExtendedNominals();

  void expandFunctionBody(AbstractFunctionDecl *AFD);

  const SourceFile *getSourceFile() const override;
  NullablePtr<const void> addressForPrinting() const override { return SF; }

  ASTContext &getASTContext() const override;

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

  ScopeCreator &getScopeCreator() override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);
};

class Portion {
public:
  const char *portionName;
  Portion(const char *n) : portionName(n) {}
  virtual ~Portion() {}

  // Make vanilla new illegal for ASTScopes.
  void *operator new(size_t bytes) = delete;
  // Need this because have virtual destructors
  void operator delete(void *data) {}

  // Only allow allocation of scopes using the allocator of a particular source
  // file.
  void *operator new(size_t bytes, const ASTContext &ctx,
                     unsigned alignment = alignof(ASTScopeImpl));
  void *operator new(size_t Bytes, void *Mem) {
    ASTScopeAssert(Mem, "Allocation failed");
    return Mem;
  }

  /// Return the new insertion point
  virtual ASTScopeImpl *expandScope(GenericTypeOrExtensionScope *,
                                    ScopeCreator &) const = 0;

  /// \Returns \c true if this lookup is done looking for results, else \c false.
  virtual SourceRange
  getChildlessSourceRangeOf(const GenericTypeOrExtensionScope *scope,
                            bool omitAssertions) const = 0;

  virtual bool lookupMembersOf(const GenericTypeOrExtensionScope *scope,
                               ASTScopeImpl::DeclConsumer consumer) const;

  virtual NullablePtr<const ASTScopeImpl>
  getLookupLimitFor(const GenericTypeOrExtensionScope *) const;

  virtual const Decl *
  getReferrentOfScope(const GenericTypeOrExtensionScope *s) const;

  virtual NullablePtr<ASTScopeImpl>
  insertionPointForDeferredExpansion(IterableTypeScope *) const = 0;
  virtual SourceRange
  sourceRangeForDeferredExpansion(const IterableTypeScope *) const = 0;
  };

  // For the whole Decl scope of a GenericType or an Extension
  class GenericTypeOrExtensionWholePortion final : public Portion {
  public:
    GenericTypeOrExtensionWholePortion() : Portion("Decl") {}
    virtual ~GenericTypeOrExtensionWholePortion() {}

    // Just for TypeAlias
    ASTScopeImpl *expandScope(GenericTypeOrExtensionScope *,
                              ScopeCreator &) const override;

    SourceRange getChildlessSourceRangeOf(const GenericTypeOrExtensionScope *,
                                          bool omitAssertions) const override;

    NullablePtr<const ASTScopeImpl>
    getLookupLimitFor(const GenericTypeOrExtensionScope *) const override;

    const Decl *
    getReferrentOfScope(const GenericTypeOrExtensionScope *s) const override;

    NullablePtr<ASTScopeImpl>
    insertionPointForDeferredExpansion(IterableTypeScope *) const override;
    SourceRange
    sourceRangeForDeferredExpansion(const IterableTypeScope *) const override;
  };

  /// GenericTypeOrExtension = GenericType or Extension
  class GenericTypeOrExtensionWhereOrBodyPortion : public Portion {
  public:
    GenericTypeOrExtensionWhereOrBodyPortion(const char *n) : Portion(n) {}
    virtual ~GenericTypeOrExtensionWhereOrBodyPortion() {}

    bool lookupMembersOf(const GenericTypeOrExtensionScope *scope,
                         ASTScopeImpl::DeclConsumer consumer) const override;
};

/// Behavior specific to representing the trailing where clause of a
/// GenericTypeDecl or ExtensionDecl scope.
class GenericTypeOrExtensionWherePortion final
    : public GenericTypeOrExtensionWhereOrBodyPortion {
public:
  GenericTypeOrExtensionWherePortion()
      : GenericTypeOrExtensionWhereOrBodyPortion("Where") {}

  bool lookupMembersOf(const GenericTypeOrExtensionScope *scope,
                       ASTScopeImpl::DeclConsumer consumer) const override;

  ASTScopeImpl *expandScope(GenericTypeOrExtensionScope *,
                            ScopeCreator &) const override;

  SourceRange getChildlessSourceRangeOf(const GenericTypeOrExtensionScope *,
                                        bool omitAssertions) const override;

  NullablePtr<ASTScopeImpl>
  insertionPointForDeferredExpansion(IterableTypeScope *) const override;
  SourceRange
  sourceRangeForDeferredExpansion(const IterableTypeScope *) const override;
};

/// Behavior specific to representing the Body of a NominalTypeDecl or
/// ExtensionDecl scope
class IterableTypeBodyPortion final
    : public GenericTypeOrExtensionWhereOrBodyPortion {
public:
  IterableTypeBodyPortion()
      : GenericTypeOrExtensionWhereOrBodyPortion("Body") {}

  ASTScopeImpl *expandScope(GenericTypeOrExtensionScope *,
                            ScopeCreator &) const override;
  SourceRange getChildlessSourceRangeOf(const GenericTypeOrExtensionScope *,
                                        bool omitAssertions) const override;

  NullablePtr<ASTScopeImpl>
  insertionPointForDeferredExpansion(IterableTypeScope *) const override;
  SourceRange
  sourceRangeForDeferredExpansion(const IterableTypeScope *) const override;
};

/// GenericType or Extension scope
/// : Whole type decl, trailing where clause, or body
class GenericTypeOrExtensionScope : public ASTScopeImpl {
public:
  const Portion *const portion;

  GenericTypeOrExtensionScope(const Portion *p) : portion(p) {}
  virtual ~GenericTypeOrExtensionScope() {}

  virtual NullablePtr<IterableDeclContext> getIterableDeclContext() const {
    return nullptr;
  }
  virtual bool shouldHaveABody() const { return false; }

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

public:
  virtual void expandBody(ScopeCreator &);

  virtual Decl *getDecl() const = 0;
  NullablePtr<Decl> getDeclIfAny() const override { return getDecl(); }
  NullablePtr<const void> getReferrent() const override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  /// \c tryBindExtension needs to get the extended nominal, and the DeclContext
  /// is the parent of the \c ExtensionDecl. If the \c SourceRange of an \c
  /// ExtensionScope were to start where the \c ExtensionDecl says, the lookup
  /// source locaiton would fall within the \c ExtensionScope. This inclusion
  /// would cause the lazy \c ExtensionScope to be expanded which would ask for
  /// its generic parameters in order to create those sub-scopes. That request
  /// would cause a cycle because it would ask for the extended nominal. So,
  /// move the source range of an \c ExtensionScope *past* the extended nominal
  /// type, which is not in-scope there anyway.
  virtual SourceRange moveStartPastExtendedNominal(SourceRange) const = 0;

  virtual GenericContext *getGenericContext() const = 0;
  std::string getClassName() const override;
  virtual std::string declKindName() const = 0;
  virtual bool doesDeclHaveABody() const;
  const char *portionName() const { return portion->portionName; }

public:
  // Only for DeclScope, not BodyScope
  // Returns the where clause scope, or the parent if none
  virtual ASTScopeImpl *createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                                       ScopeCreator &);
  virtual NullablePtr<NominalTypeDecl> getCorrespondingNominalTypeDecl() const {
    return nullptr;
  }

  bool areMembersVisibleFromWhereClause() const;

  virtual void createBodyScope(ASTScopeImpl *leaf, ScopeCreator &) {}

protected:
  bool
  lookupLocalsOrMembers(ASTScopeImpl::DeclConsumer consumer) const override;
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  NullablePtr<const ASTScopeImpl> getLookupLimit() const override;
  virtual NullablePtr<const ASTScopeImpl> getLookupLimitForDecl() const;
};

class GenericTypeScope : public GenericTypeOrExtensionScope {
public:
  GenericTypeScope(const Portion *p) : GenericTypeOrExtensionScope(p) {}
  virtual ~GenericTypeScope() {}
  SourceRange moveStartPastExtendedNominal(SourceRange) const override;

protected:
  NullablePtr<const GenericParamList> genericParams() const override;
};

class IterableTypeScope : public GenericTypeScope {
public:
  IterableTypeScope(const Portion *p) : GenericTypeScope(p) {}
  virtual ~IterableTypeScope() {}

  virtual SourceRange getBraces() const = 0;
  bool shouldHaveABody() const override { return true; }
  bool doesDeclHaveABody() const override;
  void expandBody(ScopeCreator &) override;

public:
  NullablePtr<ASTScopeImpl> insertionPointForDeferredExpansion() override;
  SourceRange sourceRangeForDeferredExpansion() const override;

  void countBodies(ScopeCreator &) const;
};

class NominalTypeScope final : public IterableTypeScope {
public:
  NominalTypeDecl *decl;
  NominalTypeScope(const Portion *p, NominalTypeDecl *e)
      : IterableTypeScope(p), decl(e) {}
  virtual ~NominalTypeScope() {}

  std::string declKindName() const override { return "NominalType"; }
  NullablePtr<IterableDeclContext> getIterableDeclContext() const override {
    return decl;
  }
  NullablePtr<NominalTypeDecl>
  getCorrespondingNominalTypeDecl() const override {
    return decl;
  }
  GenericContext *getGenericContext() const override { return decl; }
  Decl *getDecl() const override { return decl; }

  SourceRange getBraces() const override;
  NullablePtr<const ASTScopeImpl> getLookupLimitForDecl() const override;

  void createBodyScope(ASTScopeImpl *leaf, ScopeCreator &) override;
  ASTScopeImpl *createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                               ScopeCreator &) override;
};

class ExtensionScope final : public IterableTypeScope {
public:
  ExtensionDecl *const decl;
  ExtensionScope(const Portion *p, ExtensionDecl *e)
      : IterableTypeScope(p), decl(e) {}
  virtual ~ExtensionScope() {}

  GenericContext *getGenericContext() const override { return decl; }
  NullablePtr<IterableDeclContext> getIterableDeclContext() const override {
    return decl;
  }
  NullablePtr<NominalTypeDecl> getCorrespondingNominalTypeDecl() const override;
  std::string declKindName() const override { return "Extension"; }
  SourceRange getBraces() const override;
  SourceRange moveStartPastExtendedNominal(SourceRange) const override;
  ASTScopeImpl *createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                               ScopeCreator &) override;
  void createBodyScope(ASTScopeImpl *leaf, ScopeCreator &) override;
  Decl *getDecl() const override { return decl; }
  NullablePtr<const ASTScopeImpl> getLookupLimitForDecl() const override;
protected:
  NullablePtr<const GenericParamList> genericParams() const override;
};

class TypeAliasScope final : public GenericTypeScope {
public:
  TypeAliasDecl *const decl;
  TypeAliasScope(const Portion *p, TypeAliasDecl *e)
      : GenericTypeScope(p), decl(e) {}
  virtual ~TypeAliasScope() {}

  std::string declKindName() const override { return "TypeAlias"; }
  ASTScopeImpl *createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                               ScopeCreator &) override;
  GenericContext *getGenericContext() const override { return decl; }
  Decl *getDecl() const override { return decl; }
};

class OpaqueTypeScope final : public GenericTypeScope {
public:
  OpaqueTypeDecl *const decl;
  OpaqueTypeScope(const Portion *p, OpaqueTypeDecl *e)
      : GenericTypeScope(p), decl(e) {}
  virtual ~OpaqueTypeScope() {}

  std::string declKindName() const override { return "OpaqueType"; }
  GenericContext *getGenericContext() const override { return decl; }
  Decl *getDecl() const override { return decl; }
};

/// Since each generic parameter can "see" the preceeding ones,
/// (e.g. <A, B: A>) -- it's not legal but that's how lookup behaves --
/// Each GenericParamScope scopes just ONE parameter, and we next
/// each one within the previous one.
///
/// Here's a wrinkle: for a Subscript, the caller expects this scope (based on
/// source loc) to match requested DeclContexts for starting lookup in EITHER
/// the getter or setter AbstractFunctionDecl (context)
class GenericParamScope final : public ASTScopeImpl {
public:
  /// The declaration that has generic parameters.
  Decl *const holder;
  /// The generic parameters themselves.
  GenericParamList *const paramList;
  /// The index of the current parameter.
  const unsigned index;

  GenericParamScope(Decl *holder, GenericParamList *paramList, unsigned index)
      : holder(holder), paramList(paramList), index(index) {}
  virtual ~GenericParamScope() {}

  /// Actually holder is always a GenericContext, need to test if
  /// ProtocolDecl or SubscriptDecl but will refactor later.
  NullablePtr<const void> getReferrent() const override;
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  NullablePtr<const void> addressForPrinting() const override {
    return paramList;
  }

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;
};

/// Concrete class for a function/initializer/deinitializer
class AbstractFunctionDeclScope final : public ASTScopeImpl {
public:
  AbstractFunctionDecl *const decl;
  AbstractFunctionDeclScope(AbstractFunctionDecl *e) : decl(e) {}
  virtual ~AbstractFunctionDeclScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  virtual NullablePtr<Decl> getDeclIfAny() const override { return decl; }
  Decl *getDecl() const { return decl; }

  NullablePtr<const void> getReferrent() const override;

protected:
  NullablePtr<const GenericParamList> genericParams() const override;
};

/// The parameters for an abstract function (init/func/deinit)., subscript, and
/// enum element
class ParameterListScope final : public ASTScopeImpl {
public:
  ParameterList *const params;
  /// For get functions in subscript declarations,
  /// a lookup into the subscript parameters must count as the get func context.
  const NullablePtr<DeclContext> matchingContext;

  ParameterListScope(ParameterList *params,
                     NullablePtr<DeclContext> matchingContext)
      : params(params), matchingContext(matchingContext) {}
  virtual ~ParameterListScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);
  SourceLoc fixupEndForBadInput(SourceRange) const;

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  NullablePtr<const void> addressForPrinting() const override { return params; }
};

/// Body of functions, methods, constructors, destructors and accessors.
class FunctionBodyScope : public ASTScopeImpl {
public:
  AbstractFunctionDecl *const decl;

  FunctionBodyScope(AbstractFunctionDecl *e) : decl(e) {}
  virtual ~FunctionBodyScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);
  void expandBody(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  virtual NullablePtr<Decl> getDeclIfAny() const override { return decl; }
  Decl *getDecl() const { return decl; }

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;

public:
  std::string getClassName() const override;
  NullablePtr<ASTScopeImpl> insertionPointForDeferredExpansion() override;
  SourceRange sourceRangeForDeferredExpansion() const override;
};

class DefaultArgumentInitializerScope final : public ASTScopeImpl {
public:
  ParamDecl *const decl;

  DefaultArgumentInitializerScope(ParamDecl *e) : decl(e) {}
  ~DefaultArgumentInitializerScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

public:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  virtual NullablePtr<Decl> getDeclIfAny() const override { return decl; }
  Decl *getDecl() const { return decl; }
};

/// Consider:
///  @_propertyWrapper
///  struct WrapperWithInitialValue {
///  }
///  struct HasWrapper {
///    @WrapperWithInitialValue var y = 17
///  }
/// Lookup has to be able to find the use of WrapperWithInitialValue, that's
/// what this scope is for. Because the source positions are screwy.

class AttachedPropertyWrapperScope final : public ASTScopeImpl {
public:
  CustomAttr *attr;
  VarDecl *decl;

  /// Because we have to avoid request cycles, we approximate the test for an
  /// AttachedPropertyWrapper with one based on source location. We might get
  /// false positives, that that doesn't hurt anything. However, the result of
  /// the conservative source range computation doesn't seem to be stable. So
  /// keep the original here, and use it for source range queries.
  const SourceRange sourceRangeWhenCreated;

  AttachedPropertyWrapperScope(CustomAttr *attr, VarDecl *decl)
      : attr(attr), decl(decl),
        sourceRangeWhenCreated(attr->getTypeRepr()->getSourceRange()) {
    ASTScopeAssert(sourceRangeWhenCreated.isValid(),
                   "VarDecls must have ranges to be looked-up");
  }
  virtual ~AttachedPropertyWrapperScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  NullablePtr<const void> addressForPrinting() const override { return decl; }

  NullablePtr<DeclAttribute> getDeclAttributeIfAny() const override {
    return attr;
  }
  NullablePtr<const void> getReferrent() const override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);
};

/// PatternBindingDecl's (PBDs) are tricky (See the comment for \c
/// PatternBindingDecl):
///
/// A PBD contains a list of "patterns", e.g.
///   var (a, b) = foo(), (c,d) = bar() which has two patterns.
///
/// For each pattern, there will be potentially three scopes:
/// always one for the declarations, maybe one for the initializers, and maybe
/// one for users of that pattern.
///
/// If a PBD occurs in code, its initializer can access all prior declarations.
/// Thus, a new scope must be created, nested in the scope of the PBD.
/// In contrast, if a PBD occurs in a type declaration body, its initializer
/// cannot access prior declarations in that body.
///
/// As a further complication, we get VarDecls and their accessors in deferred
/// which really must go into one of the PBD scopes. So we discard them in
/// createIfNeeded, and special-case their creation in
/// addVarDeclScopesAndTheirAccessors.

class AbstractPatternEntryScope : public ASTScopeImpl {
public:
  PatternBindingDecl *const decl;
  const unsigned patternEntryIndex;

  AbstractPatternEntryScope(PatternBindingDecl *, unsigned entryIndex);
  virtual ~AbstractPatternEntryScope() {}

  const PatternBindingEntry &getPatternEntry() const;
  Pattern *getPattern() const;

protected:
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  NullablePtr<Decl> getDeclIfAny() const override { return decl; }
  Decl *getDecl() const { return decl; }
};

class PatternEntryDeclScope final : public AbstractPatternEntryScope {
  const bool isLocalBinding;
  Optional<SourceLoc> endLoc;

public:
  PatternEntryDeclScope(PatternBindingDecl *pbDecl, unsigned entryIndex,
                        bool isLocalBinding, Optional<SourceLoc> endLoc)
      : AbstractPatternEntryScope(pbDecl, entryIndex),
        isLocalBinding(isLocalBinding), endLoc(endLoc) {}
  virtual ~PatternEntryDeclScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  NullablePtr<const void> getReferrent() const override;

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;
  bool isLabeledStmtLookupTerminator() const override;
};

class PatternEntryInitializerScope final : public AbstractPatternEntryScope {
  Expr *initAsWrittenWhenCreated;

public:
  PatternEntryInitializerScope(PatternBindingDecl *pbDecl, unsigned entryIndex)
      : AbstractPatternEntryScope(pbDecl, entryIndex),
        initAsWrittenWhenCreated(pbDecl->getOriginalInit(entryIndex)) {}
  virtual ~PatternEntryInitializerScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;
  NullablePtr<const ASTScopeImpl> getLookupParent() const override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;
};

/// The scope introduced by a conditional clause in an if/guard/while
/// statement.
/// Since there may be more than one "let foo = ..." in (e.g.) an "if",
/// we allocate a matrushka of these.
class ConditionalClauseScope final : public ASTScopeImpl {
public:
  LabeledConditionalStmt *const stmt;
  const unsigned index;
  const SourceLoc endLoc; // cannot get it from the stmt

  ConditionalClauseScope(LabeledConditionalStmt *stmt, unsigned index,
                         SourceLoc endLoc)
      : stmt(stmt), index(index), endLoc(endLoc) {}

  virtual ~ConditionalClauseScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;

protected:
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

private:
  ArrayRef<StmtConditionElement> getCond() const;
  const StmtConditionElement &getStmtConditionElement() const;

protected:
  bool isLabeledStmtLookupTerminator() const override;
};

/// If, while, & guard statements all start with a conditional clause, then some
/// later part of the statement, (then, body, or after the guard) circumvents
/// the normal lookup rule to pass the lookup scope into the deepest conditional
/// clause.
class ConditionalClausePatternUseScope final : public ASTScopeImpl {
  Pattern *const pattern;
  const SourceLoc startLoc;

public:
  ConditionalClausePatternUseScope(Pattern *pattern, SourceLoc startLoc)
      : pattern(pattern), startLoc(startLoc) {}

  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  std::string getClassName() const override;

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;
  bool lookupLocalsOrMembers(DeclConsumer) const override;
  void printSpecifics(llvm::raw_ostream &out) const override;
  bool isLabeledStmtLookupTerminator() const override;
};


/// Capture lists introduce local bindings.
class CaptureListScope final : public ASTScopeImpl {
public:
  CaptureListExpr *const expr;
  CaptureListScope(CaptureListExpr *e) : expr(e) {}
  virtual ~CaptureListScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  NullablePtr<Expr> getExprIfAny() const override { return expr; }
  Expr *getExpr() const { return expr; }
  NullablePtr<const void> getReferrent() const override;
  bool lookupLocalsOrMembers(DeclConsumer) const override;
};

/// For a closure with named parameters, this scope does the local bindings.
class ClosureParametersScope final : public ASTScopeImpl {
public:
  ClosureExpr *const closureExpr;

  ClosureParametersScope(ClosureExpr *closureExpr)
      : closureExpr(closureExpr) {}
  virtual ~ClosureParametersScope() {}

  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  NullablePtr<ClosureExpr> getClosureIfClosureScope() const override {
    return closureExpr;
  }
  NullablePtr<Expr> getExprIfAny() const override { return closureExpr; }
  Expr *getExpr() const { return closureExpr; }
  NullablePtr<const void> getReferrent() const override;

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;
};

class TopLevelCodeScope final : public ASTScopeImpl {
public:
  TopLevelCodeDecl *const decl;

  TopLevelCodeScope(TopLevelCodeDecl *e) : decl(e) {}
  virtual ~TopLevelCodeScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  virtual NullablePtr<Decl> getDeclIfAny() const override { return decl; }
  Decl *getDecl() const { return decl; }
  NullablePtr<const void> getReferrent() const override;
};

/// The \c _@specialize attribute.
class SpecializeAttributeScope final : public ASTScopeImpl {
public:
  SpecializeAttr *const specializeAttr;
  AbstractFunctionDecl *const whatWasSpecialized;

  SpecializeAttributeScope(SpecializeAttr *specializeAttr,
                           AbstractFunctionDecl *whatWasSpecialized)
      : specializeAttr(specializeAttr), whatWasSpecialized(whatWasSpecialized) {
  }
  virtual ~SpecializeAttributeScope() {}

  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  NullablePtr<const void> addressForPrinting() const override {
    return specializeAttr;
  }

  NullablePtr<DeclAttribute> getDeclAttributeIfAny() const override {
    return specializeAttr;
  }
  NullablePtr<const void> getReferrent() const override;

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;
  bool lookupLocalsOrMembers(DeclConsumer) const override;
};

/// A `@differentiable` attribute scope.
///
/// This exists because `@differentiable` attribute may have a `where` clause
/// referring to generic parameters from some generic context.
class DifferentiableAttributeScope final : public ASTScopeImpl {
public:
  DifferentiableAttr *const differentiableAttr;
  ValueDecl *const attributedDeclaration;

  DifferentiableAttributeScope(DifferentiableAttr *diffAttr, ValueDecl *decl)
      : differentiableAttr(diffAttr), attributedDeclaration(decl) {}
  virtual ~DifferentiableAttributeScope() {}

  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  NullablePtr<const void> addressForPrinting() const override {
    return differentiableAttr;
  }

  NullablePtr<DeclAttribute> getDeclAttributeIfAny() const override {
    return differentiableAttr;
  }
  NullablePtr<const void> getReferrent() const override;

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;
  bool lookupLocalsOrMembers(DeclConsumer) const override;
};

class SubscriptDeclScope final : public ASTScopeImpl {
public:
  SubscriptDecl *const decl;

  SubscriptDeclScope(SubscriptDecl *e) : decl(e) {}
  virtual ~SubscriptDeclScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  virtual NullablePtr<Decl> getDeclIfAny() const override { return decl; }
  Decl *getDecl() const { return decl; }
  NullablePtr<const void> getReferrent() const override;

protected:
  NullablePtr<const GenericParamList> genericParams() const override;
};

class EnumElementScope : public ASTScopeImpl {
  EnumElementDecl *const decl;

public:
  EnumElementScope(EnumElementDecl *e) : decl(e) {}

  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  std::string getClassName() const override;
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;
  NullablePtr<Decl> getDeclIfAny() const override { return decl; }
  Decl *getDecl() const { return decl; }

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);
};

class AbstractStmtScope : public ASTScopeImpl {
public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  virtual Stmt *getStmt() const = 0;
  NullablePtr<Stmt> getStmtIfAny() const override { return getStmt(); }
  NullablePtr<const void> getReferrent() const override;

protected:
  bool isLabeledStmtLookupTerminator() const override;
};

class LabeledConditionalStmtScope : public AbstractStmtScope {
public:
  Stmt *getStmt() const override;
  virtual LabeledConditionalStmt *getLabeledConditionalStmt() const = 0;

protected:
  /// Return the lookupParent required to search these.
  ASTScopeImpl *createNestedConditionalClauseScopes(ScopeCreator &,
                                                    const Stmt *afterConds);
};

class IfStmtScope final : public LabeledConditionalStmtScope {
public:
  IfStmt *const stmt;
  IfStmtScope(IfStmt *e) : stmt(e) {}
  virtual ~IfStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  LabeledConditionalStmt *getLabeledConditionalStmt() const override;
};

class WhileStmtScope final : public LabeledConditionalStmtScope {
public:
  WhileStmt *const stmt;
  WhileStmtScope(WhileStmt *e) : stmt(e) {}
  virtual ~WhileStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  LabeledConditionalStmt *getLabeledConditionalStmt() const override;
};

class GuardStmtScope final : public LabeledConditionalStmtScope {
public:
  GuardStmt *const stmt;
  SourceLoc endLoc;
  GuardStmtScope(GuardStmt *e, SourceLoc endLoc) : stmt(e), endLoc(endLoc) {}
  virtual ~GuardStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  LabeledConditionalStmt *getLabeledConditionalStmt() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
};

/// A scope after a guard statement that follows lookups into the conditions
/// Also for:
///  The insertion point of the last statement of an active clause in an #if
///  must be the lookup parent
/// of any following scopes. But the active clause may not be the last clause.
/// In short, this is another case where the lookup parent cannot follow the same
/// nesting as the source order. IfConfigUseScope implements this twist. It
/// follows the IfConfig, wraps all subsequent scopes, and redirects the lookup.
class LookupParentDiversionScope final : public ASTScopeImpl {
public:
  ASTScopeImpl *const lookupParent;
  const SourceLoc startLoc;
  const SourceLoc endLoc;

  LookupParentDiversionScope(ASTScopeImpl *lookupParent,
                             SourceLoc startLoc, SourceLoc endLoc)
      : lookupParent(lookupParent), startLoc(startLoc), endLoc(endLoc) {}

  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  std::string getClassName() const override;

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;
  NullablePtr<const ASTScopeImpl> getLookupParent() const override {
    return lookupParent;
  }
  bool isLabeledStmtLookupTerminator() const override;
};

class RepeatWhileScope final : public AbstractStmtScope {
public:
  RepeatWhileStmt *const stmt;
  RepeatWhileScope(RepeatWhileStmt *e) : stmt(e) {}
  virtual ~RepeatWhileScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  Stmt *getStmt() const override { return stmt; }
};

class DoStmtScope final : public AbstractStmtScope {
public:
  DoStmt *const stmt;
  DoStmtScope(DoStmt *e) : stmt(e) {}
  virtual ~DoStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  Stmt *getStmt() const override { return stmt; }
};

class DoCatchStmtScope final : public AbstractStmtScope {
public:
  DoCatchStmt *const stmt;
  DoCatchStmtScope(DoCatchStmt *e) : stmt(e) {}
  virtual ~DoCatchStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  Stmt *getStmt() const override { return stmt; }
};

class SwitchStmtScope final : public AbstractStmtScope {
public:
  SwitchStmt *const stmt;
  SwitchStmtScope(SwitchStmt *e) : stmt(e) {}
  virtual ~SwitchStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  Stmt *getStmt() const override { return stmt; }
};

class ForEachStmtScope final : public AbstractStmtScope {
public:
  ForEachStmt *const stmt;
  ForEachStmtScope(ForEachStmt *e) : stmt(e) {}
  virtual ~ForEachStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  Stmt *getStmt() const override { return stmt; }
};

class ForEachPatternScope final : public ASTScopeImpl {
public:
  ForEachStmt *const stmt;
  ForEachPatternScope(ForEachStmt *e) : stmt(e) {}
  virtual ~ForEachPatternScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;
  bool isLabeledStmtLookupTerminator() const override;
};

/// The parent scope for a 'case' statement, consisting of zero or more
/// CaseLabelItemScopes, followed by a CaseStmtBodyScope.
///
/// +------------------------------------------------------------------
/// | CaseStmtScope
/// +------------------------------------------------------------------
/// |                               +--------------------------+
/// |                               | CaseLabelItemScope:      |
/// |                               +--------------------------+
/// | case .foo(let x, let y) where | condition(x, y),         |
/// |               ^------^--------------------^--^           |
/// |               this guard expression sees first 'x'/'y'   |
/// |                               +--------------------------+
/// |
/// |                               +--------------------------+
/// |                               | CaseLabelItemScope:      |
/// |                               +--------------------------+
/// |      .foo(let x, let y) where | condition(x, y),         |
/// |               ^------^--------------------^--^           |
/// |               this guard expression sees second 'x'/'y'  |
/// |                               +--------------------------+
/// |
/// |      .bar(let x, let y)
/// |               this case label item doesn't have a guard, so no
/// |               scope is created.
/// |
/// | +----------------------------------------------------------------
/// | | CaseStmtBodyScope:
/// | +----------------------------------------------------------------
/// | | {
/// | |    ... x, y  <-- body sees "joined" 'x'/'y' created by parser
/// | | }
/// | +----------------------------------------------------------------
/// |
/// +------------------------------------------------------------------

class CaseStmtScope final : public AbstractStmtScope {
public:
  CaseStmt *const stmt;
  CaseStmtScope(CaseStmt *e) : stmt(e) {}
  virtual ~CaseStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  Stmt *getStmt() const override { return stmt; }
};

/// The scope used for the guard expression in a case statement. Any
/// variables bound by the case label item's pattern are visible in
/// this scope.
class CaseLabelItemScope final : public ASTScopeImpl {
public:
  CaseLabelItem item;
  CaseLabelItemScope(const CaseLabelItem &item) : item(item) {}
  virtual ~CaseLabelItemScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  bool lookupLocalsOrMembers(ASTScopeImpl::DeclConsumer) const override;
};

/// The scope used for the body of a 'case' statement.
///
/// If the 'case' statement has multiple case label items, each label
/// item's pattern must bind the same variables; the parser creates
/// "fake" variables to represent the join of the variables bound by
/// each pattern.
///
/// These "fake" variables are visible in the 'case' statement body.
class CaseStmtBodyScope final : public ASTScopeImpl {
public:
  CaseStmt *const stmt;
  CaseStmtBodyScope(CaseStmt *e) : stmt(e) {}
  virtual ~CaseStmtBodyScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
protected:
  bool lookupLocalsOrMembers(ASTScopeImpl::DeclConsumer) const override;
  bool isLabeledStmtLookupTerminator() const override;
};

class BraceStmtScope final : public AbstractStmtScope {
  BraceStmt *const stmt;

  /// Declarations which are in scope from the beginning of the statement.
  SmallVector<ValueDecl *, 2> localFuncsAndTypes;

  /// Declarations that are normally in scope only after their
  /// definition.
  SmallVector<VarDecl *, 2> localVars;

public:
  BraceStmtScope(BraceStmt *e,
                 SmallVector<ValueDecl *, 2> localFuncsAndTypes,
                 SmallVector<VarDecl *, 2> localVars)
      : stmt(e),
        localFuncsAndTypes(localFuncsAndTypes),
        localVars(localVars) {}
  virtual ~BraceStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  std::string getClassName() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  NullablePtr<ClosureExpr> parentClosureIfAny() const; // public??
  Stmt *getStmt() const override { return stmt; }

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;
};
} // namespace ast_scope
} // namespace swift

#endif // SWIFT_AST_AST_SCOPE_H

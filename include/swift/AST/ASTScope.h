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
#include "swift/AST/CatchNode.h"
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
#include <optional>

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
class AbstractSpecializeAttr;
class GenericContext;
class DeclName;
class StmtConditionElement;

namespace Lowering {
class SILGenFunction;
}

namespace ast_scope {
class ASTScopeImpl;
class BraceStmtScope;
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

/// Enumerate the different kinds of ASTScope implementation nodes.
enum class ScopeKind {
#define SCOPE_NODE(Name) Name,
#include "swift/AST/ASTScopeNodes.def"
};

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
class ASTScopeImpl : public ASTAllocated<ASTScopeImpl> {
  friend class NodeAdder;
  friend class Portion;
  friend class GenericTypeOrExtensionWholePortion;
  friend class NomExtDeclPortion;
  friend class GenericTypeOrExtensionWherePortion;
  friend class GenericTypeOrExtensionWherePortion;
  friend class IterableTypeBodyPortion;
  friend class ScopeCreator;
  friend class ASTSourceFileScope;
  friend class ABIAttributeScope;
  friend class Lowering::SILGenFunction;

#pragma mark - tree state
protected:
  using Children = SmallVector<ASTScopeImpl *, 4>;
  /// Whether the given parent is the accessor node for an abstract
  /// storage declaration or is directly descended from it.

private:
  /// The kind of scope node.
  ScopeKind kind;

  /// The pointer:
  /// - Always set by the constructor, so that when creating a child
  ///   the parent chain is available. Null at the root.
  /// The int:
  /// - A flag indicating if the scope has been expanded yet or not.
  llvm::PointerIntPair<ASTScopeImpl *, 1> parentAndWasExpanded;

  /// Child scopes, sorted by source range.
  Children storedChildren;

  mutable std::optional<SourceRange> cachedCharSourceRange;

#pragma mark - constructor / destructor
public:
  ASTScopeImpl(ScopeKind kind) : kind(kind) { }

  // TOD: clean up all destructors and deleters
  virtual ~ASTScopeImpl() {}

  ASTScopeImpl(ASTScopeImpl &&) = delete;
  ASTScopeImpl &operator=(ASTScopeImpl &&) = delete;
  ASTScopeImpl(const ASTScopeImpl &) = delete;
  ASTScopeImpl &operator=(const ASTScopeImpl &) = delete;

  // Need this because have virtual destructors
  void operator delete(void *data) {}

#pragma mark - tree declarations
protected:
  NullablePtr<ASTScopeImpl> getParent() {
    return parentAndWasExpanded.getPointer();
  }
  NullablePtr<const ASTScopeImpl> getParent() const {
    return parentAndWasExpanded.getPointer();
  }

public:
  const Children &getChildren() const { return storedChildren; }
  void addChild(ASTScopeImpl *child, ASTContext &);

public:
  void preOrderDo(function_ref<void(ASTScopeImpl *)>);
  /// Like preorderDo but without myself.
  void preOrderChildrenDo(function_ref<void(ASTScopeImpl *)>);
  void postOrderDo(function_ref<void(ASTScopeImpl *)>);

#pragma mark - source ranges

public:
  /// Retrieve the source range of the given scope, where the end location
  /// is adjusted to refer to the end of the token.
  ///
  /// Since the adjustment to the end of the token requires lexing, this
  /// routine also caches the result.
  ///
  /// Note that the start and end locations might be in different source
  /// buffers, so we represent the result as SourceRange rather than
  /// CharSourceRange.
  SourceRange getCharSourceRangeOfScope(SourceManager &SM,
                                        bool omitAssertions = false) const;
  bool isCharSourceRangeCached() const;

  /// Returns source range of this node alone, without factoring in any
  /// children.
  virtual SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const = 0;

protected:
  SourceManager &getSourceManager() const;

private:
  void checkSourceRangeBeforeAddingChild(ASTScopeImpl *child,
                                         const ASTContext &ctx) const;

#pragma mark common queries
public:
  /// Determine the kind of scope node we have.
  ScopeKind getKind() const { return kind; }

  ASTContext &getASTContext() const;
  NullablePtr<Decl> getDeclIfAny() const;
  NullablePtr<Stmt> getStmtIfAny() const;
  NullablePtr<Expr> getExprIfAny() const;

  /// Whether this scope is for a decl attribute.
  bool isDeclAttribute() const;

#pragma mark - debugging and printing

public:
  const SourceFile *getSourceFile() const;
  std::string getClassName() const;

  /// Print out this scope for debugging/reporting purposes.
  void print(llvm::raw_ostream &out, unsigned level = 0, bool lastChild = false,
             bool printChildren = true) const;

  void printRange(llvm::raw_ostream &out) const;

  void printParents(llvm::raw_ostream &out) const;

protected:
  virtual void printSpecifics(llvm::raw_ostream &out) const {}
  virtual NullablePtr<const void> addressForPrinting() const;

public:
  SWIFT_DEBUG_DUMP;
  SWIFT_DEBUG_DUMPER(dumpParents());

  void dumpOneScopeMapLocation(std::pair<unsigned, unsigned> lineColumn);

private:
  [[noreturn]]
  void abortWithVerificationError(
      llvm::function_ref<void(llvm::raw_ostream &)> messageFn) const;

#pragma mark - Scope tree creation
public:
  /// Expand the scope if unexpanded.
  ASTScopeImpl *expandAndBeCurrent(ScopeCreator &);

  bool getWasExpanded() const { return parentAndWasExpanded.getInt(); }

protected:
  void setWasExpanded() { parentAndWasExpanded.setInt(1); }
  virtual ASTScopeImpl *expandSpecifically(ScopeCreator &) = 0;

public:
  /// Some scopes can be expanded lazily.
  /// Such scopes must return an insertion point outside themselves when
  /// expanded.
  virtual NullablePtr<ASTScopeImpl> insertionPointForDeferredExpansion();

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

  static void lookupEnclosingMacroScope(
      SourceFile *sourceFile, SourceLoc loc,
      llvm::function_ref<bool(ASTScope::PotentialMacro)> consume);

  static ABIAttr *lookupEnclosingABIAttributeScope(
      SourceFile *sourceFile, SourceLoc loc);

  static CatchNode lookupCatchNode(ModuleDecl *module, SourceLoc loc);

  /// Scopes that cannot bind variables may set this to true to create more
  /// compact scope tree in the debug info.
  virtual bool ignoreInDebugInfo() const { return false; }

#pragma mark - - lookup- starting point
private:
  static const ASTScopeImpl *findStartingScopeForLookup(SourceFile *,
                                                        const SourceLoc where);

protected:
  /// Not const because may reexpand some scopes.
  ASTScopeImpl *findInnermostEnclosingScope(ModuleDecl *,
                                            SourceLoc,
                                            NullablePtr<raw_ostream>);
  ASTScopeImpl *findInnermostEnclosingScopeImpl(ModuleDecl *,
                                                SourceLoc,
                                                NullablePtr<raw_ostream>,
                                                SourceManager &,
                                                ScopeCreator &);

private:
  NullablePtr<ASTScopeImpl> findChildContaining(ModuleDecl *,
                                                SourceLoc loc,
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
  /// what obtains for scoping. However, guards are different. The scope after
  /// the guard else must hop into the innermoset scope of the guard condition.
  virtual NullablePtr<const ASTScopeImpl> getLookupParent() const {
    return getParent();
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

  ASTSourceFileScope(SourceFile *SF, ScopeCreator *scopeCreator);

  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  void buildFullyExpandedTree();
  void
  buildEnoughOfTreeForTopLevelExpressionsButDontRequestGenericsOrExtendedNominals();

  void expandFunctionBody(AbstractFunctionDecl *AFD);

  NullablePtr<const void> addressForPrinting() const override { return SF; }

  bool ignoreInDebugInfo() const override { return true; }

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

  ScopeCreator &getScopeCreator() override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::ASTSourceFile;
  }
};

class Portion : public ASTAllocated<ASTScopeImpl> {
public:
  const char *portionName;
  Portion(const char *n) : portionName(n) {}
  virtual ~Portion() {}

  // Need this because have virtual destructors
  void operator delete(void *data) {}

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

  virtual NullablePtr<ASTScopeImpl>
  insertionPointForDeferredExpansion(IterableTypeScope *) const = 0;
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

  NullablePtr<ASTScopeImpl>
  insertionPointForDeferredExpansion(IterableTypeScope *) const override;
};

/// Behavior specific to representing the trailing where clause of a
/// GenericTypeDecl or ExtensionDecl scope.
class GenericTypeOrExtensionWherePortion final : public Portion {
public:
  GenericTypeOrExtensionWherePortion() : Portion("Where") {}

  bool lookupMembersOf(const GenericTypeOrExtensionScope *scope,
                       ASTScopeImpl::DeclConsumer consumer) const override;

  ASTScopeImpl *expandScope(GenericTypeOrExtensionScope *,
                            ScopeCreator &) const override;

  SourceRange getChildlessSourceRangeOf(const GenericTypeOrExtensionScope *,
                                        bool omitAssertions) const override;

  NullablePtr<ASTScopeImpl>
  insertionPointForDeferredExpansion(IterableTypeScope *) const override;
};

/// Behavior specific to representing the Body of a NominalTypeDecl or
/// ExtensionDecl scope
class IterableTypeBodyPortion final : public Portion {
public:
  IterableTypeBodyPortion() : Portion("Body") {}

  bool lookupMembersOf(const GenericTypeOrExtensionScope *scope,
                       ASTScopeImpl::DeclConsumer consumer) const override;

  ASTScopeImpl *expandScope(GenericTypeOrExtensionScope *,
                            ScopeCreator &) const override;
  SourceRange getChildlessSourceRangeOf(const GenericTypeOrExtensionScope *,
                                        bool omitAssertions) const override;

  NullablePtr<ASTScopeImpl>
  insertionPointForDeferredExpansion(IterableTypeScope *) const override;
};

/// GenericType or Extension scope
/// : Whole type decl, trailing where clause, or body
class GenericTypeOrExtensionScope : public ASTScopeImpl {
public:
  const Portion *const portion;

  GenericTypeOrExtensionScope(ScopeKind kind, const Portion *p)
      : ASTScopeImpl(kind), portion(p) {}
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

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  /// \c tryBindExtension needs to get the extended nominal, and the DeclContext
  /// is the parent of the \c ExtensionDecl. If the \c SourceRange of an \c
  /// ExtensionScope were to start where the \c ExtensionDecl says, the lookup
  /// source location would fall within the \c ExtensionScope. This inclusion
  /// would cause the lazy \c ExtensionScope to be expanded which would ask for
  /// its generic parameters in order to create those sub-scopes. That request
  /// would cause a cycle because it would ask for the extended nominal. So,
  /// move the source range of an \c ExtensionScope *past* the extended nominal
  /// type, which is not in-scope there anyway.
  virtual SourceRange moveStartPastExtendedNominal(SourceRange) const = 0;

  virtual GenericContext *getGenericContext() const = 0;
  virtual std::string declKindName() const = 0;
  virtual bool doesDeclHaveABody() const;
  const char *portionName() const { return portion->portionName; }

  std::string getClassName() const;

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

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::NominalType ||
           scope->getKind() == ScopeKind::Extension ||
           scope->getKind() == ScopeKind::TypeAlias ||
           scope->getKind() == ScopeKind::OpaqueType;
  }
};

class GenericTypeScope : public GenericTypeOrExtensionScope {
public:
  GenericTypeScope(ScopeKind kind, const Portion *p)
      : GenericTypeOrExtensionScope(kind, p) { }
  virtual ~GenericTypeScope() {}
  SourceRange moveStartPastExtendedNominal(SourceRange) const override;

protected:
  NullablePtr<const GenericParamList> genericParams() const override;
};

class IterableTypeScope : public GenericTypeScope {
public:
  IterableTypeScope(ScopeKind kind, const Portion *p)
      : GenericTypeScope(kind, p) {}
  virtual ~IterableTypeScope() {}

  virtual SourceRange getBraces() const = 0;
  bool shouldHaveABody() const override { return true; }
  bool doesDeclHaveABody() const override;
  void expandBody(ScopeCreator &) override;

public:
  NullablePtr<ASTScopeImpl> insertionPointForDeferredExpansion() override;

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::NominalType ||
        scope->getKind() == ScopeKind::Extension;
  }
};

class NominalTypeScope final : public IterableTypeScope {
public:
  NominalTypeDecl *decl;
  NominalTypeScope(const Portion *p, NominalTypeDecl *e)
      : IterableTypeScope(ScopeKind::NominalType, p), decl(e) {}
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

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::NominalType;
  }
};

class ExtensionScope final : public IterableTypeScope {
public:
  ExtensionDecl *const decl;
  ExtensionScope(const Portion *p, ExtensionDecl *e)
      : IterableTypeScope(ScopeKind::Extension, p), decl(e) {}
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

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::Extension;
  }
};

class TypeAliasScope final : public GenericTypeScope {
public:
  TypeAliasDecl *const decl;
  TypeAliasScope(const Portion *p, TypeAliasDecl *e)
      : GenericTypeScope(ScopeKind::TypeAlias, p), decl(e) {}
  virtual ~TypeAliasScope() {}

  std::string declKindName() const override { return "TypeAlias"; }
  ASTScopeImpl *createTrailingWhereClauseScope(ASTScopeImpl *parent,
                                               ScopeCreator &) override;
  GenericContext *getGenericContext() const override { return decl; }
  Decl *getDecl() const override { return decl; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::TypeAlias;
  }
};

class OpaqueTypeScope final : public GenericTypeScope {
public:
  OpaqueTypeDecl *const decl;
  OpaqueTypeScope(const Portion *p, OpaqueTypeDecl *e)
      : GenericTypeScope(ScopeKind::OpaqueType, p), decl(e) {}
  virtual ~OpaqueTypeScope() {}

  std::string declKindName() const override { return "OpaqueType"; }
  GenericContext *getGenericContext() const override { return decl; }
  Decl *getDecl() const override { return decl; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::OpaqueType;
  }
};

/// Since each generic parameter can "see" the preceding ones,
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
      : ASTScopeImpl(ScopeKind::GenericParam), holder(holder),
        paramList(paramList), index(index) {}
  virtual ~GenericParamScope() {}

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

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::GenericParam;
  }
};

/// Concrete class for a function/initializer/deinitializer
class AbstractFunctionDeclScope final : public ASTScopeImpl {
public:
  AbstractFunctionDecl *const decl;
  AbstractFunctionDeclScope(AbstractFunctionDecl *e)
      : ASTScopeImpl(ScopeKind::AbstractFunctionDecl), decl(e) {}
  virtual ~AbstractFunctionDeclScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  void printSpecifics(llvm::raw_ostream &out) const override;

  bool lookupLocalsOrMembers(DeclConsumer) const override;

public:
  Decl *getDecl() const { return decl; }

protected:
  NullablePtr<const GenericParamList> genericParams() const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::AbstractFunctionDecl;
  }
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
      : ASTScopeImpl(ScopeKind::ParameterList), params(params),
        matchingContext(matchingContext) {}
  virtual ~ParameterListScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  NullablePtr<const void> addressForPrinting() const override { return params; }
  bool ignoreInDebugInfo() const override { return true; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::ParameterList;
  }
};

/// Body of functions, methods, constructors, destructors and accessors.
class FunctionBodyScope : public ASTScopeImpl {
public:
  AbstractFunctionDecl *const decl;

  FunctionBodyScope(AbstractFunctionDecl *e)
      : ASTScopeImpl(ScopeKind::FunctionBody), decl(e) {}
  virtual ~FunctionBodyScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);
  void expandBody(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  Decl *getDecl() const { return decl; }
  bool ignoreInDebugInfo() const override { return true; }

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;

public:
  NullablePtr<ASTScopeImpl> insertionPointForDeferredExpansion() override;

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::FunctionBody;
  }
};

class DefaultArgumentInitializerScope final : public ASTScopeImpl {
public:
  ParamDecl *const decl;

  DefaultArgumentInitializerScope(ParamDecl *e)
      : ASTScopeImpl(ScopeKind::DefaultArgumentInitializer), decl(e) {}
  ~DefaultArgumentInitializerScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

public:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  Decl *getDecl() const { return decl; }
  bool ignoreInDebugInfo() const override { return true; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::DefaultArgumentInitializer;
  }
};

/// The scope for custom attributes and their arguments, such as for
/// attached property wrappers and for attached macros.
///
/// Source locations for the attribute name and its arguments are in the
/// custom attribute, so lookup is invoked from within the attribute
/// itself.
class CustomAttributeScope final : public ASTScopeImpl {
public:
  CustomAttr *attr;
  Decl *decl;

  CustomAttributeScope(CustomAttr *attr,Decl *decl)
      : ASTScopeImpl(ScopeKind::CustomAttribute), attr(attr), decl(decl) {}
  virtual ~CustomAttributeScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  NullablePtr<const void> addressForPrinting() const override { return decl; }

  bool ignoreInDebugInfo() const override { return true; }
  
private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::CustomAttribute;
  }
};

/// The scope for ABI attributes and their arguments.
///
/// Source locations for the attribute name and its arguments are in the
/// custom attribute, so lookup is invoked from within the attribute
/// itself.
class ABIAttributeScope final : public ASTScopeImpl {
public:
  ABIAttr *attr;
  Decl *decl;

  ABIAttributeScope(ABIAttr *attr, Decl *decl)
      : ASTScopeImpl(ScopeKind::ABIAttribute), attr(attr), decl(decl) {}
  virtual ~ABIAttributeScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  NullablePtr<const void> addressForPrinting() const override { return decl; }

  bool ignoreInDebugInfo() const override { return true; }

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::ABIAttribute;
  }
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

  AbstractPatternEntryScope(ScopeKind kind, PatternBindingDecl *,
                            unsigned entryIndex);
  virtual ~AbstractPatternEntryScope() {}

  const PatternBindingEntry &getPatternEntry() const;
  Pattern *getPattern() const;

protected:
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  Decl *getDecl() const { return decl; }
};

class PatternEntryDeclScope final : public AbstractPatternEntryScope {
  const bool isLocalBinding;
  std::optional<SourceLoc> endLoc;

public:
  PatternEntryDeclScope(PatternBindingDecl *pbDecl, unsigned entryIndex,
                        bool isLocalBinding, std::optional<SourceLoc> endLoc)
      : AbstractPatternEntryScope(ScopeKind::PatternEntryDecl, pbDecl,
                                  entryIndex),
        isLocalBinding(isLocalBinding), endLoc(endLoc) {}
  virtual ~PatternEntryDeclScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;
  bool isLabeledStmtLookupTerminator() const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::PatternEntryDecl;
  }
};

class PatternEntryInitializerScope final : public AbstractPatternEntryScope {
  Expr *initAsWrittenWhenCreated;

public:
  PatternEntryInitializerScope(PatternBindingDecl *pbDecl, unsigned entryIndex)
      : AbstractPatternEntryScope(ScopeKind::PatternEntryInitializer, pbDecl,
                                  entryIndex),
        initAsWrittenWhenCreated(pbDecl->isDebuggerBinding() ?
                                 pbDecl->getInit(entryIndex) :
                                 pbDecl->getOriginalInit(entryIndex)) {}
  virtual ~PatternEntryInitializerScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;
  NullablePtr<const ASTScopeImpl> getLookupParent() const override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;
  bool isLabeledStmtLookupTerminator() const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::PatternEntryInitializer;
  }
};

/// The scope introduced by a conditional clause initializer in an
/// if/while/guard statement.
class ConditionalClauseInitializerScope final : public ASTScopeImpl {
public:
  Expr *const initializer;
  const SourceRange bodyRange;

  ConditionalClauseInitializerScope(Expr *initializer)
      : ASTScopeImpl(ScopeKind::ConditionalClauseInitializer),
        initializer(initializer) {}

  virtual ~ConditionalClauseInitializerScope() {}
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;
  NullablePtr<const ASTScopeImpl> getLookupParent() const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::ConditionalClauseInitializer;
  }
};

/// If, while, & guard statements all start with a conditional clause, then some
/// later part of the statement, (then, body, or after the guard) circumvents
/// the normal lookup rule to pass the lookup scope into the deepest conditional
/// clause.
class ConditionalClausePatternUseScope final : public ASTScopeImpl {
  StmtConditionElement sec;
  SourceLoc endLoc;

public:
  ConditionalClausePatternUseScope(StmtConditionElement sec, SourceLoc endLoc)
      : ASTScopeImpl(ScopeKind::ConditionalClausePatternUse), sec(sec),
        endLoc(endLoc) {}

  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;
  bool lookupLocalsOrMembers(DeclConsumer) const override;
  void printSpecifics(llvm::raw_ostream &out) const override;
  bool isLabeledStmtLookupTerminator() const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::ConditionalClausePatternUse;
  }
};


/// Capture lists introduce local bindings.
class CaptureListScope final : public ASTScopeImpl {
public:
  CaptureListExpr *const expr;
  CaptureListScope(CaptureListExpr *e)
      : ASTScopeImpl(ScopeKind::CaptureList), expr(e) {}
  virtual ~CaptureListScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  Expr *getExpr() const { return expr; }
  bool lookupLocalsOrMembers(DeclConsumer) const override;

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::CaptureList;
  }
};

/// For a closure with named parameters, this scope does the local bindings.
class ClosureParametersScope final : public ASTScopeImpl {
public:
  AbstractClosureExpr *const closureExpr;

  ClosureParametersScope(AbstractClosureExpr *closureExpr)
      : ASTScopeImpl(ScopeKind::ClosureParameters), closureExpr(closureExpr) {}
  virtual ~ClosureParametersScope() {}

  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  Expr *getExpr() const { return closureExpr; }
  bool ignoreInDebugInfo() const override { return true; }

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::ClosureParameters;
  }
};

class TopLevelCodeScope final : public ASTScopeImpl {
public:
  TopLevelCodeDecl *const decl;
  SourceLoc endLoc;

  TopLevelCodeScope(TopLevelCodeDecl *e, SourceLoc endLoc)
      : ASTScopeImpl(ScopeKind::TopLevelCode), decl(e), endLoc(endLoc) {}
  virtual ~TopLevelCodeScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  Decl *getDecl() const { return decl; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::TopLevelCode;
  }
};

/// The \c _@specialize attribute.
class SpecializeAttributeScope final : public ASTScopeImpl {
public:
  AbstractSpecializeAttr *const specializeAttr;
  AbstractFunctionDecl *const whatWasSpecialized;

  SpecializeAttributeScope(AbstractSpecializeAttr *specializeAttr,
                           AbstractFunctionDecl *whatWasSpecialized)
      : ASTScopeImpl(ScopeKind::SpecializeAttribute),
        specializeAttr(specializeAttr), whatWasSpecialized(whatWasSpecialized) {
  }
  virtual ~SpecializeAttributeScope() {}

  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  NullablePtr<const void> addressForPrinting() const override {
    return specializeAttr;
  }

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;
  bool lookupLocalsOrMembers(DeclConsumer) const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::SpecializeAttribute;
  }
};

/// A `@differentiable` attribute scope.
///
/// This exists because `@differentiable` attribute may have a `where` clause
/// referring to generic parameters from some generic context.
class DifferentiableAttributeScope final : public ASTScopeImpl {
public:
  DifferentiableAttr *const differentiableAttr;
  Decl *const attributedDeclaration;

  DifferentiableAttributeScope(DifferentiableAttr *diffAttr, Decl *decl)
      : ASTScopeImpl(ScopeKind::DifferentiableAttribute),
        differentiableAttr(diffAttr), attributedDeclaration(decl) {}
  virtual ~DifferentiableAttributeScope() {}

  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  NullablePtr<const void> addressForPrinting() const override {
    return differentiableAttr;
  }

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;
  bool lookupLocalsOrMembers(DeclConsumer) const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::DifferentiableAttribute;
  }
};

class SubscriptDeclScope final : public ASTScopeImpl {
public:
  SubscriptDecl *const decl;

  SubscriptDeclScope(SubscriptDecl *e)
      : ASTScopeImpl(ScopeKind::SubscriptDecl), decl(e) {}

  virtual ~SubscriptDeclScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  Decl *getDecl() const { return decl; }

protected:
  NullablePtr<const GenericParamList> genericParams() const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::SubscriptDecl;
  }
};

class EnumElementScope : public ASTScopeImpl {
  EnumElementDecl *const decl;

public:
  EnumElementScope(EnumElementDecl *e)
      : ASTScopeImpl(ScopeKind::EnumElement), decl(e) {}

  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;
  Decl *getDecl() const { return decl; }

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::EnumElement;
  }
};

class MacroDeclScope final : public ASTScopeImpl {
public:
  MacroDecl *const decl;

  MacroDeclScope(MacroDecl *e) : ASTScopeImpl(ScopeKind::MacroDecl), decl(e) {}
  virtual ~MacroDeclScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  Decl *getDecl() const { return decl; }

protected:
  NullablePtr<const GenericParamList> genericParams() const override;
  bool lookupLocalsOrMembers(DeclConsumer) const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::MacroDecl;
  }
};

/// The scope introduced for the definition of a macro, which follows the `=`.
class MacroDefinitionScope final : public ASTScopeImpl {
public:
  Expr *const definition;

  MacroDefinitionScope(Expr *definition)
      : ASTScopeImpl(ScopeKind::MacroDefinition), definition(definition) {}

  virtual ~MacroDefinitionScope() {}
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::MacroDefinition;
  }
};

class MacroExpansionDeclScope final : public ASTScopeImpl {
public:
  MacroExpansionDecl *const decl;

  MacroExpansionDeclScope(MacroExpansionDecl *e)
      : ASTScopeImpl(ScopeKind::MacroExpansionDecl), decl(e) {}
  virtual ~MacroExpansionDeclScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  void printSpecifics(llvm::raw_ostream &out) const override;

public:
  Decl *getDecl() const { return decl; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::MacroExpansionDecl;
  }
};

class AbstractStmtScope : public ASTScopeImpl {
protected:
  AbstractStmtScope(ScopeKind kind) : ASTScopeImpl(kind) { }

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
  virtual Stmt *getStmt() const = 0;

protected:
  bool isLabeledStmtLookupTerminator() const override;
};

class LabeledConditionalStmtScope : public AbstractStmtScope {
protected:
  LabeledConditionalStmtScope(ScopeKind kind) : AbstractStmtScope(kind) { }

public:
  Stmt *getStmt() const override;
  virtual LabeledConditionalStmt *getLabeledConditionalStmt() const = 0;

protected:
  /// Return the lookupParent required to search these.
  ASTScopeImpl *createNestedConditionalClauseScopes(ScopeCreator &,
                                                    SourceLoc);
};

class IfStmtScope final : public LabeledConditionalStmtScope {
public:
  IfStmt *const stmt;
  IfStmtScope(IfStmt *e)
      : LabeledConditionalStmtScope(ScopeKind::IfStmt), stmt(e) {}
  virtual ~IfStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  LabeledConditionalStmt *getLabeledConditionalStmt() const override;

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::IfStmt;
  }
};

class WhileStmtScope final : public LabeledConditionalStmtScope {
public:
  WhileStmt *const stmt;
  WhileStmtScope(WhileStmt *e)
      : LabeledConditionalStmtScope(ScopeKind::WhileStmt), stmt(e) {}
  virtual ~WhileStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  LabeledConditionalStmt *getLabeledConditionalStmt() const override;

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::WhileStmt;
  }
};

class GuardStmtScope final : public LabeledConditionalStmtScope {
public:
  GuardStmt *const stmt;
  SourceLoc endLoc;
  GuardStmtScope(GuardStmt *e, SourceLoc endLoc) 
      : LabeledConditionalStmtScope(ScopeKind::GuardStmt),
        stmt(e), endLoc(endLoc) {}
  virtual ~GuardStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  LabeledConditionalStmt *getLabeledConditionalStmt() const override;
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::GuardStmt;
  }
};

/// A scope for the body of a guard statement. Lookups from the body must
/// skip the parent scopes for introducing pattern bindings, since they're
/// not visible in the guard body, only after the body ends.
class GuardStmtBodyScope final : public ASTScopeImpl {
public:
  ASTScopeImpl *const lookupParent;
  BraceStmt *const body;

  GuardStmtBodyScope(ASTScopeImpl *lookupParent, BraceStmt *body)
      : ASTScopeImpl(ScopeKind::GuardStmtBody), lookupParent(lookupParent),
        body(body) {}

  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &) override;
  NullablePtr<const ASTScopeImpl> getLookupParent() const override {
    return lookupParent;
  }
  bool isLabeledStmtLookupTerminator() const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::GuardStmtBody;
  }
};

class RepeatWhileScope final : public AbstractStmtScope {
public:
  RepeatWhileStmt *const stmt;
  RepeatWhileScope(RepeatWhileStmt *e)
      : AbstractStmtScope(ScopeKind::RepeatWhile), stmt(e) {}
  virtual ~RepeatWhileScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  Stmt *getStmt() const override { return stmt; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::RepeatWhile;
  }
};

class DoStmtScope final : public AbstractStmtScope {
public:
  DoStmt *const stmt;
  DoStmtScope(DoStmt *e) : AbstractStmtScope(ScopeKind::DoStmt), stmt(e) {}
  virtual ~DoStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  Stmt *getStmt() const override { return stmt; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::DoStmt;
  }
};

class DoCatchStmtScope final : public AbstractStmtScope {
public:
  DoCatchStmt *const stmt;
  DoCatchStmtScope(DoCatchStmt *e)
      : AbstractStmtScope(ScopeKind::DoCatchStmt), stmt(e) {}
  virtual ~DoCatchStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  Stmt *getStmt() const override { return stmt; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::DoCatchStmt;
  }
};

class SwitchStmtScope final : public AbstractStmtScope {
public:
  SwitchStmt *const stmt;
  SwitchStmtScope(SwitchStmt *e)
      : AbstractStmtScope(ScopeKind::SwitchStmt), stmt(e) {}
  virtual ~SwitchStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  Stmt *getStmt() const override { return stmt; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::SwitchStmt;
  }
};

class ForEachStmtScope final : public AbstractStmtScope {
public:
  ForEachStmt *const stmt;
  ForEachStmtScope(ForEachStmt *e)
      : AbstractStmtScope(ScopeKind::ForEachStmt), stmt(e) {}
  virtual ~ForEachStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  Stmt *getStmt() const override { return stmt; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::ForEachStmt;
  }
};

class ForEachPatternScope final : public ASTScopeImpl {
public:
  ForEachStmt *const stmt;
  ForEachPatternScope(ForEachStmt *e)
      : ASTScopeImpl(ScopeKind::ForEachPattern), stmt(e) {}
  virtual ~ForEachPatternScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;
  bool isLabeledStmtLookupTerminator() const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::ForEachPattern;
  }
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
  CaseStmtScope(CaseStmt *e)
      : AbstractStmtScope(ScopeKind::CaseStmt), stmt(e) {}
  virtual ~CaseStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  Stmt *getStmt() const override { return stmt; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::CaseStmt;
  }
};

/// The scope used for the guard expression in a case statement. Any
/// variables bound by the case label item's pattern are visible in
/// this scope.
class CaseLabelItemScope final : public ASTScopeImpl {
public:
  CaseLabelItem item;
  CaseLabelItemScope(const CaseLabelItem &item)
      : ASTScopeImpl(ScopeKind::CaseLabelItem), item(item) {}
  virtual ~CaseLabelItemScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

protected:
  bool lookupLocalsOrMembers(ASTScopeImpl::DeclConsumer) const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::CaseLabelItem;
  }
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
  CaseStmtBodyScope(CaseStmt *e)
      : ASTScopeImpl(ScopeKind::CaseStmtBody), stmt(e) {}
  virtual ~CaseStmtBodyScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  void expandAScopeThatDoesNotCreateANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;
protected:
  bool lookupLocalsOrMembers(ASTScopeImpl::DeclConsumer) const override;
  bool isLabeledStmtLookupTerminator() const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::CaseStmtBody;
  }
};

class BraceStmtScope final : public AbstractStmtScope {
  BraceStmt *const stmt;

  /// Declarations which are in scope from the beginning of the statement.
  ArrayRef<ValueDecl *> localFuncsAndTypes;

  /// Declarations that are normally in scope only after their
  /// definition.
  ArrayRef<VarDecl *> localVars;

  /// The end location for bindings introduced in this scope. This can
  /// extend past the actual end of the BraceStmt in top-level code,
  /// where every TopLevelCodeDecl introduces a new scope through the
  /// end of the buffer.
  SourceLoc endLoc;

public:
  BraceStmtScope(BraceStmt *e,
                 ArrayRef<ValueDecl *> localFuncsAndTypes,
                 ArrayRef<VarDecl *> localVars,
                 SourceLoc endLoc)
      : AbstractStmtScope(ScopeKind::BraceStmt),
        stmt(e),
        localFuncsAndTypes(localFuncsAndTypes),
        localVars(localVars),
        endLoc(endLoc) {}
  virtual ~BraceStmtScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

private:
  AnnotatedInsertionPoint
  expandAScopeThatCreatesANewInsertionPoint(ScopeCreator &);

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  NullablePtr<AbstractClosureExpr> parentClosureIfAny() const; // public??
  BraceStmt *getStmt() const override { return stmt; }

protected:
  bool lookupLocalsOrMembers(DeclConsumer) const override;

public:
  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::BraceStmt;
  }
};

/// Describes a scope introduced by a try/try!/try? expression.
class TryScope final : public ASTScopeImpl {
public:
  AnyTryExpr *const expr;

  /// The end location of the scope. This may be past the TryExpr for
  /// cases where the `try` is at the top-level of an unfolded SequenceExpr. In
  /// such cases, the `try` covers all elements to the right.
  SourceLoc endLoc;

  TryScope(AnyTryExpr *e, SourceLoc endLoc)
      : ASTScopeImpl(ScopeKind::Try), expr(e), endLoc(endLoc) {
    ASSERT(endLoc.isValid());
  }
  virtual ~TryScope() {}

protected:
  ASTScopeImpl *expandSpecifically(ScopeCreator &scopeCreator) override;

public:
  SourceRange
  getSourceRangeOfThisASTNode(bool omitAssertions = false) const override;

  Expr *getExpr() const { return expr; }

  static bool classof(const ASTScopeImpl *scope) {
    return scope->getKind() == ScopeKind::Try;
  }
};

} // namespace ast_scope
} // namespace swift

#endif // SWIFT_AST_AST_SCOPE_H

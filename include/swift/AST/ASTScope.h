//===--- ASTScope.h - Swift AST Scope ---------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the ASTScope class and related functionality, which
// describes the scopes that exist within a Swift AST.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_AST_SCOPE_H
#define SWIFT_AST_AST_SCOPE_H

#include "swift/AST/ASTNode.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/STLExtras.h"

namespace swift {

class AbstractFunctionDecl;
class AbstractStorageDecl;
class ASTContext;
class BraceStmt;
class CaseStmt;
class CatchStmt;
class ClosureExpr;
class Decl;
class DoCatchStmt;
class Expr;
class ForStmt;
class ForEachStmt;
class GenericParamList;
class GuardStmt;
class IfStmt;
class IterableDeclContext;
class LabeledConditionalStmt;
class ParamDecl;
class PatternBindingDecl;
class RepeatWhileStmt;
class SourceFile;
class Stmt;
class StmtConditionElement;
class SwitchStmt;
class TopLevelCodeDecl;
class WhileStmt;

/// Describes kind of scope that occurs within the AST.
enum class ASTScopeKind : uint8_t {
  /// A pre-expanded scope in which we know a priori the children.
  ///
  /// This is a convenience scope that has no direct bearing on the AST.
  Preexpanded,
  /// A source file, which is the root of a scope.
  SourceFile,
  /// The body of a type or extension thereof.
  TypeOrExtensionBody,
  /// The generic parameters of a declaration.
  GenericParams,
  /// A function/initializer/deinitializer.
  AbstractFunctionDecl,
  /// The parameters of a function/initializer/deinitializer.
  AbstractFunctionParams,
  /// The default argument for a parameter.
  DefaultArgument,
  /// A specific pattern binding.
  PatternBinding,
  /// The scope introduced for an initializer of a pattern binding.
  PatternInitializer,
  /// The scope following a particular clause in a pattern binding declaration,
  /// which is the outermost scope in which the variables introduced by that
  /// clause will be visible.
  AfterPatternBinding,
  /// The scope introduced by a brace statement.
  BraceStmt,
  /// The scope introduced by a local declaration.
  LocalDeclaration,
  /// Node describing an "if" statement.
  IfStmt,
  /// The scope introduced by a conditional clause in an if/guard/while
  /// statement.
  ConditionalClause,
  /// Node describing a "guard" statement.
  GuardStmt,
  /// Node describing a repeat...while statement.
  RepeatWhileStmt,
  /// Node describing a for-each statement.
  ForEachStmt,
  /// Describes the scope of the pattern of the for-each statement.
  ForEachPattern,
  /// Describes a do-catch statement.
  DoCatchStmt,
  /// Describes the a catch statement.
  CatchStmt,
  /// Describes a switch statement.
  SwitchStmt,
  /// Describes a 'case' statement.
  CaseStmt,
  /// Describes a C-style 'for' statement.
  ForStmt,
  /// Describes the scope of variables introduced in the initializer of a
  /// a C-style 'for' statement.
  ForStmtInitializer,
  /// Scope for the accessors of an abstract storage declaration.
  Accessors,
  /// Scope for a closure.
  Closure,
  /// Scope for top-level code.
  TopLevelCode,
};

/// Describes a lexical scope within a source file.
///
/// Each \c ASTScope is a node within a tree that describes all of the lexical
/// scopes within a particular source range. The root of this scope tree is
/// always a \c SourceFile node, and the tree covers the entire source file.
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
/// As an implementation detail, the scope tree is lazily constructed as it is
/// queried, and only the relevant subtrees (i.e., trees whose source ranges
/// enclose the queried source location or whose children were explicitly
/// requested by the client) will be constructed. The \c expandAll() operation
/// can be used to fully-expand the tree, constructing all of its nodes, but
/// should only be used for testing or debugging purposes, e.g., via the
/// frontend option
/// \code
/// -dump-scope-maps expanded
/// \endcode
class ASTScope {
  /// The kind of scope this represents.
  ASTScopeKind kind;

  /// The parent scope of this particular scope along with a bit indicating
  /// whether the children of this node have already been expanded.
  mutable llvm::PointerIntPair<const ASTScope *, 1, bool> parentAndExpanded;

  /// The cached source range.
  mutable SourceRange CachedSourceRange;

  /// Union describing the various kinds of AST nodes that can introduce
  /// scopes.
  union {
    /// For \c kind == ASTScopeKind::SourceFile.
    struct {
      // The actual source file.
      SourceFile *file;

      /// The next element that should be considered in the source file.
      ///
      /// This accomodates the expansion of source files.
      mutable unsigned nextElement;
    } sourceFile;

    /// An iterable declaration context, which covers nominal type declarations
    /// and extensions.
    ///
    /// For \c kind == ASTScopeKind::TypeOrExtensionBody.
    IterableDeclContext *iterableDeclContext;

    /// For \c kind == ASTScopeKind::GenericParams.
    struct {
      /// The generic parameters themselves.
      GenericParamList *params;

      /// The declaration that has generic parameters.
      Decl *decl;

      /// The index of the current parameter.
      unsigned index;
    } genericParams;

    /// An abstract function, for \c kind == ASTScopeKind::AbstractFunctionDecl.
    AbstractFunctionDecl *abstractFunction;

    /// An parameter for an abstract function (init/func/deinit).
    ///
    /// For \c kind == ASTScopeKind::AbstractFunctionParams.
    struct {
      /// The function declaration.
      AbstractFunctionDecl *decl;

      /// The index into the function parameter lists.
      unsigned listIndex;

      /// The parameter index into the current function parameter list.
      unsigned paramIndex;
    } abstractFunctionParams;

    /// The parameter whose default argument is being described, i.e.,
    /// \c kind == ASTScopeKind::DefaultArgument.
    ParamDecl *parameter;

    /// For \c kind == ASTScopeKind::PatternBinding,
    /// \c kind == ASTScopeKind::AfterPatternBinding, or
    /// \c kind == ASTScopeKind::PatternInitializer.
    struct {
      PatternBindingDecl *decl;
      unsigned entry;
    } patternBinding;

    /// For \c kind == ASTScopeKind::BraceStmt.
    struct {
      BraceStmt *stmt;

      /// The next element in the brace statement that should be expanded.
      mutable unsigned nextElement;
    } braceStmt;

    /// The declaration introduced within a local scope.
    ///
    /// For \c kind == ASTScopeKind::LocalDeclaration.
    Decl *localDeclaration;

    /// The 'if' statement, for \c kind == ASTScopeKind::IfStmt.
    IfStmt *ifStmt;

    /// For \c kind == ASTScopeKind::ConditionalClause.
    struct {
      /// The statement that contains the conditional clause.
      LabeledConditionalStmt *stmt;

      /// The index of the conditional clause.
      unsigned index;

      /// Whether this conditional clause is being used for the 'guard'
      /// continuation.
      bool isGuardContinuation;
    } conditionalClause;

    /// The 'guard' statement, for \c kind == ASTScopeKind::GuardStmt.
    GuardStmt *guard;

    /// The repeat...while statement, for
    /// \c kind == ASTScopeKind::RepeatWhileStmt.
    RepeatWhileStmt *repeatWhile;

    /// The for-each statement, for
    /// \c kind == ASTScopeKind::ForEachStmt or
    /// \c kind == ASTScopeKind::ForEachPattern.
    ForEachStmt *forEach;

    /// A do-catch statement, for \c kind == ASTScopeKind::DoCatchStmt.
    DoCatchStmt *doCatch;

    /// A catch statement, for \c kind == ASTScopeKind::CatchStmt.
    CatchStmt *catchStmt;

    /// A switch statement, for \c kind == ASTScopeKind::SwitchStmt.
    SwitchStmt *switchStmt;

    /// A case statement, for \c kind == ASTScopeKind::CaseStmt;
    CaseStmt *caseStmt;

    /// A for statement, for \c kind == ASTScopeKind::ForStmt or
    /// \c kind == ASTScopeKind::ForStmtInitializer.
    ForStmt *forStmt;

    /// An abstract storage declaration, for
    /// \c kind == ASTScopeKind::Accessors.
    AbstractStorageDecl *abstractStorageDecl;

    /// The closure, for \c kind == ASTScopeKind::Closure.
    ClosureExpr *closure;

    /// The top-level code declaration for
    /// \c kind == ASTScopeKind::TopLevelCodeDecl.
    TopLevelCodeDecl *topLevelCode;
  };

  /// Child scopes, sorted by source range.
  mutable SmallVector<ASTScope *, 4> storedChildren;

  /// Constructor that only initializes the kind and parent, leaving the
  /// pieces to be initialized by the caller.
  ASTScope(ASTScopeKind kind, const ASTScope *parent)
      : kind(kind), parentAndExpanded(parent, false) { }

  ASTScope(SourceFile *sourceFile, unsigned nextElement)
      : ASTScope(ASTScopeKind::SourceFile, nullptr) {
    this->sourceFile.file = sourceFile;
    this->sourceFile.nextElement = nextElement;
  }

  /// Constructor that initializes a preexpanded node.
  ASTScope(const ASTScope *parent, ArrayRef<ASTScope *> children);

  ASTScope(const ASTScope *parent, IterableDeclContext *idc)
      : ASTScope(ASTScopeKind::TypeOrExtensionBody, parent) {
    this->iterableDeclContext = idc;
  }

  ASTScope(const ASTScope *parent, GenericParamList *genericParams,
           Decl *decl, unsigned index)
      : ASTScope(ASTScopeKind::GenericParams, parent) {
    this->genericParams.params = genericParams;
    this->genericParams.decl = decl;
    this->genericParams.index = index;
  }

  ASTScope(const ASTScope *parent, AbstractFunctionDecl *abstractFunction)
      : ASTScope(ASTScopeKind::AbstractFunctionDecl, parent) {
    this->abstractFunction = abstractFunction;
  }

  ASTScope(const ASTScope *parent, AbstractFunctionDecl *abstractFunction,
           unsigned listIndex, unsigned paramIndex)
      : ASTScope(ASTScopeKind::AbstractFunctionParams, parent) {
    this->abstractFunctionParams.decl = abstractFunction;
    this->abstractFunctionParams.listIndex = listIndex;
    this->abstractFunctionParams.paramIndex = paramIndex;
  }

  ASTScope(const ASTScope *parent, ParamDecl *param)
      : ASTScope(ASTScopeKind::DefaultArgument, parent) {
    this->parameter = param;
  }

  ASTScope(ASTScopeKind kind, const ASTScope *parent, PatternBindingDecl *decl,
           unsigned entry)
      : ASTScope(kind, parent) {
    assert(kind == ASTScopeKind::PatternBinding ||
           kind == ASTScopeKind::PatternInitializer ||
           kind == ASTScopeKind::AfterPatternBinding);
    this->patternBinding.decl = decl;
    this->patternBinding.entry = entry;
  }
  
  ASTScope(const ASTScope *parent, BraceStmt *braceStmt)
      : ASTScope(ASTScopeKind::BraceStmt, parent) {
    this->braceStmt.stmt = braceStmt;
    this->braceStmt.nextElement = 0;
  }

  ASTScope(const ASTScope *parent, IfStmt *ifStmt)
      : ASTScope(ASTScopeKind::IfStmt, parent) {
    this->ifStmt = ifStmt;
  }

  ASTScope(const ASTScope *parent, LabeledConditionalStmt *stmt,
           unsigned index, bool isGuardContinuation)
      : ASTScope(ASTScopeKind::ConditionalClause, parent) {
    this->conditionalClause.stmt = stmt;
    this->conditionalClause.index = index;
    this->conditionalClause.isGuardContinuation = isGuardContinuation;
  }

  ASTScope(const ASTScope *parent, GuardStmt *guard)
      : ASTScope(ASTScopeKind::GuardStmt, parent) {
    this->guard = guard;
  }

  ASTScope(const ASTScope *parent, RepeatWhileStmt *repeatWhile)
      : ASTScope(ASTScopeKind::RepeatWhileStmt, parent) {
    this->repeatWhile = repeatWhile;
  }

  ASTScope(ASTScopeKind kind, const ASTScope *parent, ForEachStmt *forEach)
      : ASTScope(kind, parent) {
    assert(kind == ASTScopeKind::ForEachStmt ||
           kind == ASTScopeKind::ForEachPattern);
    this->forEach = forEach;
  }

  ASTScope(const ASTScope *parent, DoCatchStmt *doCatch)
      : ASTScope(ASTScopeKind::DoCatchStmt, parent) {
    this->doCatch = doCatch;
  }

  ASTScope(const ASTScope *parent, CatchStmt *catchStmt)
      : ASTScope(ASTScopeKind::CatchStmt, parent) {
    this->catchStmt = catchStmt;
  }

  ASTScope(const ASTScope *parent, SwitchStmt *switchStmt)
      : ASTScope(ASTScopeKind::SwitchStmt, parent) {
    this->switchStmt = switchStmt;
  }

  ASTScope(const ASTScope *parent, CaseStmt *caseStmt)
      : ASTScope(ASTScopeKind::CaseStmt, parent) {
    this->caseStmt = caseStmt;
  }

  ASTScope(ASTScopeKind kind, const ASTScope *parent, ForStmt *forStmt)
      : ASTScope(kind, parent) {
    assert(kind == ASTScopeKind::ForStmt ||
           kind == ASTScopeKind::ForStmtInitializer);
    this->forStmt = forStmt;
  }

  ASTScope(const ASTScope *parent, AbstractStorageDecl *abstractStorageDecl)
      : ASTScope(ASTScopeKind::Accessors, parent) {
    this->abstractStorageDecl = abstractStorageDecl;
  }

  ASTScope(const ASTScope *parent, ClosureExpr *closure)
      : ASTScope(ASTScopeKind::Closure, parent) {
    this->closure = closure;
  }

  ASTScope(const ASTScope *parent, TopLevelCodeDecl *topLevelCode)
      : ASTScope(ASTScopeKind::TopLevelCode, parent) {
    this->topLevelCode = topLevelCode;
  }

  ~ASTScope();

  ASTScope(ASTScope &&) = delete;
  ASTScope &operator=(ASTScope &&) = delete;
  ASTScope(const ASTScope &) = delete;
  ASTScope &operator=(const ASTScope &) = delete;

  /// Expand the children of this AST scope so they can be queried.
  void expand() const;

  /// Determine whether the given scope has already been expanded.
  bool isExpanded() const;

  /// Create a new AST scope if one is needed for the given declaration.
  ///
  /// \returns the newly-created AST scope, or \c null if there is no scope
  /// introduced by this declaration.
  static ASTScope *createIfNeeded(const ASTScope *parent, Decl *decl);

  /// Create a new AST scope if one is needed for the given statement.
  ///
  /// \returns the newly-created AST scope, or \c null if there is no scope
  /// introduced by this statement.
  static ASTScope *createIfNeeded(const ASTScope *parent, Stmt *stmt);

  /// Create a new AST scope if one is needed for the given expression.
  ///
  /// \returns the newly-created AST scope, or \c null if there is no scope
  /// introduced by this expression.
  static ASTScope *createIfNeeded(const ASTScope *parent, Expr *Expr);

  /// Create a new AST scope if one is needed for the given AST node.
  ///
  /// \returns the newly-created AST scope, or \c null if there is no scope
  /// introduced by this AST node.
  static ASTScope *createIfNeeded(const ASTScope *parent, ASTNode node);

  /// Determine whether this is a scope from which we can perform a
  /// continuation.
  bool isContinuationScope() const;

  /// Enumerate the continuation scopes for the given parent,
  ///
  /// Statements, such as 'guard' and local declarations, introduce scopes
  /// that extend to the end of an enclosing brace-stmt. This
  /// operation finds each of the "continuation" scopes in the nearest
  /// enclosing brace statement.
  void enumerateContinuationScopes(
         llvm::function_ref<void(ASTScope *)> fn) const;

  /// Compute the source range of this scope.
  SourceRange getSourceRangeImpl() const;

  /// Retrieve the ASTContext in which this scope exists.
  ASTContext &getASTContext() const;

  /// Retrieve the source file in which this scope exists.
  SourceFile &getSourceFile() const;

public:
  /// Create the AST scope for a source file, which is the root of the scope
  /// tree.
  static ASTScope *createRoot(SourceFile *sourceFile);

  /// Determine the kind of AST scope we have.
  ASTScopeKind getKind() const { return kind; }

  /// Retrieve the parent scope that encloses this one.
  const ASTScope *getParent() const { return parentAndExpanded.getPointer(); }

  /// Retrieve the children of this AST scope, expanding if necessary.
  ArrayRef<ASTScope *> children() const {
    if (!isExpanded()) expand();
    return storedChildren;
  }

  /// Determine the source range covered by this scope.
  SourceRange getSourceRange() const {
    if (CachedSourceRange.isInvalid() ||
        getKind() == ASTScopeKind::SourceFile) {
      CachedSourceRange = getSourceRangeImpl();
    }
    return CachedSourceRange;
  }

  /// Retrieve the local declatation when
  /// \c getKind() == ASTScopeKind::LocalDeclaration.
  Decl *getLocalDeclaration() const {
    assert(getKind() == ASTScopeKind::LocalDeclaration);
    return localDeclaration;
  }

  /// Retrieve the abstract function declaration when
  /// \c getKind() == ASTScopeKind::AbstractFunctionDecl;
  AbstractFunctionDecl *getAbstractFunctionDecl() const {
    assert(getKind() == ASTScopeKind::AbstractFunctionDecl);
    return abstractFunction;
  }

  /// Retrieve the abstract storage declaration when
  /// \c getKind() == ASTScopeKind::Accessors;
  AbstractStorageDecl *getAbstractStorageDecl() const {
    assert(getKind() == ASTScopeKind::Accessors);
    return abstractStorageDecl;
  }

  /// Find the innermost enclosing scope that contains this source location.
  const ASTScope *findInnermostEnclosingScope(SourceLoc loc) const;

  /// Retrieve the declaration context directly associated with this scope, or
  /// NULL if there is no such declaration context.
  ///
  /// \seealso getInnermostEnclosingDeclContext().
  DeclContext *getDeclContext() const;

  /// Retrieve the innermost enclosing declaration context in which this
  /// scope
  ///
  /// This is semantically equivalent to calling \c getDeclContext() on this
  /// node and each of its parents until we get a non-null result.
  ///
  /// \seealso getDeclContext().
  DeclContext *getInnermostEnclosingDeclContext() const;

  /// Retrueve the declarations whose names are directly bound by this scope.
  ///
  /// The declarations bound in this scope aren't available in the immediate
  /// parent of this scope, but will still be visible in child scopes (unless
  /// shadowed there).
  ///
  /// Note that this routine does not produce bindings for anything that can
  /// be found via qualified name lookup in a \c DeclContext, such as nominal
  /// type declarations or extensions thereof, or the source file itself. The
  /// client can perform such lookups using the result of \c getDeclContext().
  SmallVector<ValueDecl *, 4> getLocalBindings() const;

  /// Expand the entire scope map.
  ///
  /// Normally, the scope map will be expanded only as needed by its queries,
  /// but complete expansion can be useful for debugging.
  void expandAll() const;

  /// Print out this scope for debugging/reporting purposes.
  void print(llvm::raw_ostream &out, unsigned level = 0,
             bool lastChild = false,
             bool printChildren = true) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");

  // Make vanilla new/delete illegal for Decls.
  void *operator new(size_t bytes) = delete;
  void operator delete(void *data) = delete;

  // Only allow allocation of scopes using the allocator of a particular source
  // file.
  void *operator new(size_t bytes, const ASTContext &ctx,
                     unsigned alignment = alignof(ASTScope));
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
};

}

#endif // SWIFT_AST_AST_SCOPE_H

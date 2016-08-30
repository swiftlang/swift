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
#ifndef SWIFT_AST_SCOPE_MAP_H
#define SWIFT_AST_SCOPE_MAP_H

#include "swift/AST/ASTNode.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/STLExtras.h"

namespace swift {

class AbstractFunctionDecl;
class ASTContext;
class BraceStmt;
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
class PatternBindingDecl;
class RepeatWhileStmt;
class SourceFile;
class Stmt;
class StmtConditionElement;
class WhileStmt;

/// Describes kind of scope that occurs within the AST.
enum class ASTScopeKind : uint8_t {
  /// A source file, which is the root of a scope.
  SourceFile,
  /// The body of a type or extension thereof.
  TypeOrExtensionBody,
  /// The generic parameters of a declaration.
  GenericParams,
  /// The parameters of a function/initializer/deinitializer.
  AbstractFunctionParams,
  /// The scope introduced by a particular clause in a pattern binding
  /// declaration.
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
  /// Note describing a for-each statement.
  ForEachStmt,
  /*
  DoCatchStmt,
  ForEachBody,
   */
};

class ASTScope {
  /// The kind of scope this represents.
  ASTScopeKind kind;

  /// The parent scope of this particular scope along with a bit indicating
  /// whether the children of this node have already been expanded.
  mutable llvm::PointerIntPair<const ASTScope *, 1, bool> parentAndExpanded;

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

    /// An abstract function (init/func/deinit).
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

    /// For \c kind == ASTScopeKind::AfterPatternBinding.
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
    } conditionalClause;

    /// The 'guard' statement, for \c kind == ASTScopeKind::GuardStmt.
    GuardStmt *guard;

    /// The repeat...while statement, for
    /// \c kind == ASTScopeKind::RepeatWhileStmt.
    RepeatWhileStmt *repeatWhile;

    /// The for-each statement, for
    /// \c kind == ASTScopeKind::ForEachStmt.
    ForEachStmt *forEach;
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

  ASTScope(const ASTScope *parent, AbstractFunctionDecl *abstractFunction,
           unsigned listIndex, unsigned paramIndex)
      : ASTScope(ASTScopeKind::AbstractFunctionParams, parent) {
    this->abstractFunctionParams.decl = abstractFunction;
    this->abstractFunctionParams.listIndex = listIndex;
    this->abstractFunctionParams.paramIndex = paramIndex;
  }

  ASTScope(const ASTScope *parent, PatternBindingDecl *decl, unsigned entry)
      : ASTScope(ASTScopeKind::AfterPatternBinding, parent) {
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
           unsigned index)
      : ASTScope(ASTScopeKind::ConditionalClause, parent) {
    this->conditionalClause.stmt = stmt;
    this->conditionalClause.index = index;
  }

  ASTScope(const ASTScope *parent, GuardStmt *guard)
      : ASTScope(ASTScopeKind::GuardStmt, parent) {
    this->guard = guard;
  }

  ASTScope(const ASTScope *parent, RepeatWhileStmt *repeatWhile)
      : ASTScope(ASTScopeKind::RepeatWhileStmt, parent) {
    this->repeatWhile = repeatWhile;
  }

  ASTScope(const ASTScope *parent, ForEachStmt *forEach)
      : ASTScope(ASTScopeKind::ForEachStmt, parent) {
    this->forEach = forEach;
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
  SourceRange getSourceRange() const;

  /// Retrieve the local declatation when
  /// \c getKind() == ASTScopeKind::LocalDeclaration.
  Decl *getLocalDeclaration() const {
    assert(getKind() == ASTScopeKind::LocalDeclaration);
    return localDeclaration;
  }

  /// Expand the entire scope map.
  ///
  /// Normally, the scope map will be expanded only as needed by its queries,
  /// but complete expansion can be useful for debugging.
  void expandAll() const;

  /// Print out this scope for debugging/reporting purposes.
  void print(llvm::raw_ostream &out, unsigned level = 0,
             bool lastChild = false) const;

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

#endif // SWIFT_BASIC_SOURCE_RANGE_MAP_H
